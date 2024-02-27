package expgolomb

import chisel3._
import chisel3.util._

/**
 * Creates and applies a bit-mask to a signal based on the position of its highest least-significant bit,
 * effectively truncating the signal value to in[high:0]
 */
object ApplyHighMask {
  def apply(in: Bits, high: UInt): UInt = {
    in.asUInt & WireDefault(chiselTypeOf(in), (1.U << (high + 1.U)).asUInt - 1.U).asUInt
  }
}

/**
 * Version of Log2 which trims the output bit width to the ceiling logarithm of input bit width
 */
object Log2Trimmed {
  def apply(in: Bits): UInt = {
    require(in.widthKnown, "'in' must have a known width")
    WireDefault(UInt(log2Up(in.getWidth).W), Log2(in))
  }
}

/**
 * Concatenates multiple signals, each by a respective dynamic number of least-significant bits
 */
object DynamicHighCat {
  def apply(in: Seq[(Bits, UInt)]): (UInt, UInt) = {
    val (fields, _) = in.unzip
    require(fields.forall(_.widthKnown), "all fields must have known width")
    val totalWidth = fields.map(_.getWidth).sum

//    var printInfo = Seq[(UInt, UInt, Bits, UInt, UInt, UInt, UInt)]()

    val (result, resultMaskShift) = in.foldRight((0.U(totalWidth.W), 0.U(log2Up(totalWidth).W))) {
      case ((field: Bits, high: UInt), (res: UInt, shift: UInt)) =>
        val fieldMasked = ApplyHighMask(field, high)
        val newRes = res | (fieldMasked << shift).asUInt
        val newShift = shift + high + 1.U

//        printInfo = printInfo.appended((
//          res,
//          shift,
//          field,
//          high,
//          fieldMask,
//          newRes,
//          newShift
//        ))

        (newRes, newShift)
    }

//    printInfo.foreach { case (res, shift, field, high, fieldMask, newRes, newShift) =>
//      printf(cf"res:      $res%b\n")
//      printf(cf"shift:    $shift\n")
//      printf(cf"field:    $field%b[$high:0]\n")
//      printf(cf"fmask:    $fieldMask%b\n")
//      printf(cf"masked:   ${field.asUInt & fieldMask}%b\n")
//      printf(cf"shifted:  ${((field.asUInt & fieldMask) << shift).asUInt}%b\n")
//      printf(cf"newRes:   $newRes%b\n")
//      printf(cf"newShift: $newShift\n")
//      printf("\n")
//    }

    val high = resultMaskShift - 1.U
//    printf(cf"result: $result%b[$high:0]\n")
//    printf(cf"wired:  ${WireDefault(UInt(totalWidth.W), result)}%b[$high:0]\n")
//    printf("\n")
    (WireDefault(UInt(totalWidth.W), result), high)
  }
}

/**
 * Concatenates multiple signals by their significant bits
 */
object Log2Cat {
  def apply(in: Seq[Bits]): UInt = {
    val (cat, _) = DynamicHighCat(in.map { el => (el, Log2(el)) })
    cat
  }
}

/**
 * Returns mean position of highest set bit in a sequence of signals
 */
object MeanHighBit {
  def apply(in: Seq[Bits]): UInt = {
    require(in.nonEmpty && ((in.length & (in.length - 1)) == 0),
      "'in' must be a nonempty sequence with a length of a power of two")
    require(in.forall(_.widthKnown), "All elements of 'in' must have a known width")
    val resWidth = log2Up(Math.ceil(in.map(_.getWidth).sum / in.length.toFloat).toInt)
    val res = (in.map(Log2Trimmed(_)).reduce(_ +& _) >> log2Floor(in.length)).asUInt
//    println(s"Res: $res")
    WireDefault(UInt(resWidth.W), res)
  }
}

/**
 * Encodes or decodes a single integer
 */
object ExpGolombSingle {
  def encode(in: UInt, k: UInt): (UInt, UInt) = {
    require(in.widthKnown, "Input data width must be known")
    val out = WireDefault(UInt((2 * in.getWidth).W), in) + (1.U << k).asUInt
    val outHigh = WireDefault(UInt(log2Up(out.getWidth).W), Log2(out))
    (out, outHigh + outHigh - k)
  }

  def decode(in: Bits, high: UInt, k: UInt): UInt = {
    require(in.widthKnown, "Input data width must be known")
    WireDefault(UInt((in.getWidth / 2).W), ApplyHighMask(in, high)) - (1.U << k).asUInt
  }
}

/**
 * Encodes or decodes a sequence of integers
 */
object ExpGolombMulti {
  def encode(ins: Seq[UInt], kOption: Option[UInt] = None): (UInt, UInt, UInt) = {
    require(ins.forall(_.widthKnown), "Input data width must be known")
    val inputWidth = ins.map(_.getWidth).sum
    val k = kOption.getOrElse(MeanHighBit(ins) +& 1.U)
    val maxWidth = if (kOption.isEmpty) inputWidth + ins.length else 2 * inputWidth
//    println(s"Max Width: $maxWidth")

    val mapped = ins.map(ExpGolombSingle.encode(_, k))
//    printf(mapped.map { case (v, h) => cf"$v%b[$h:0]" }.reduce(_ + _) + "\n")
    val (out, outHigh) = DynamicHighCat(mapped)
//    printf(cf"out:    ${WireDefault(UInt(maxWidth.W), out)}%b[$outHigh:0] (ExpGolombMulti)\n")
    (WireDefault(UInt(maxWidth.W), out), outHigh, k)
  }

  def decode(in: UInt, high: UInt, k: UInt, n: Int): Seq[UInt] = {
    if (n > 0) {
      val sig = ApplyHighMask(in, high)
      val firstSet = Log2(sig)
      val prefixLength = high - firstSet
      val sigLengthMinusOne = prefixLength + prefixLength + k
      val low = high - sigLengthMinusOne
      val decoded = ExpGolombSingle.decode(sig >> low, sigLengthMinusOne, k)
      decoded +: decode(in, low - 1.U, k, n - 1)
    } else Seq()
  }
}
