package expgolomb

import chisel3._
import chisel3.util._

import scala.annotation.tailrec

/**
  * Version of RegEnable which takes an optional enable signal which determines whether the register should be
  * constructed at all.
  */
object RegEnableCond {
  def apply[T <: Data](next: T, cond: Option[Bool]): T = cond.fold(next)(RegEnable(next, _))

  def apply[T <: Data](next: T, init: T, cond: Option[Bool]): T = cond.fold(next)(RegEnable(next, init, _))
}

/**
  * Creates and applies a bit-mask to a signal based on the position of its highest least-significant bit,
  * effectively truncating the signal value to in[high:0].
  */
object ApplyHighMask {
  def apply(in: Bits, high: UInt): UInt = {
    in.asUInt & WireDefault(chiselTypeOf(in), (1.U << (high + 1.U)).asUInt - 1.U).asUInt
  }
}

/**
  * Concatenates multiple signals, each by a respective dynamic number of least-significant bits.
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

//        printInfo = printInfo.appended(
//          (
//            res,
//            shift,
//            field,
//            high,
//            fieldMasked,
//            newRes,
//            newShift
//          )
//        )

        (newRes, newShift)
    }

//    printInfo.foreach {
//      case (res, shift, field, high, fieldMasked, newRes, newShift) =>
//        printf(cf"res:      $res%b\n")
//        printf(cf"shift:    $shift\n")
//        printf(cf"field:    $field%b[$high:0]\n")
//        printf(cf"masked:   $fieldMasked%b\n")
//        printf(cf"shifted:  ${(fieldMasked << shift).asUInt}%b\n")
//        printf(cf"newRes:   $newRes%b\n")
//        printf(cf"newShift: $newShift\n")
//        printf("\n")
//    }

    val high = resultMaskShift - 1.U
//    printf(cf"result: $result%b[$high:0]\n")
//    printf(cf"wired:  ${WireDefault(UInt(totalWidth.W), result)}%b[$high:0]\n")
//    printf("\n")
    (WireDefault(UInt(totalWidth.W), result), high)
  }
}

/**
  * Determines the number of significant bits in a signal.
  */
object BitWidth {
  def apply(in: Bits): UInt = {
    val res = Mux(in === 0.U, 0.U, Log2(in).asUInt +& 1.U)
    in.widthOption.fold(res) { w => WireDefault(UInt(log2Up(w + 1).W), res) }
  }
}

/**
  * Performs unsigned integer division by a constant divisor value.
  */
object DivideByConst {
  def apply(resultMax: Int, divisor: Int)(in: UInt): UInt = {
    require(resultMax >= 0, "resultMax must be greater than or equal to zero")
    require(divisor > 0, "divisor must be greater than zero")

    def muxTree(divMin: Int, divCur: Int, divMax: Int): UInt = {
      if (divCur == divMin) divCur.U
      else
        Mux(
          in <= (divCur * divisor - 1).U,
          muxTree(divMin, (divMin + divCur) / 2, divCur),
          muxTree(divCur, (divCur + divMax + 1) / 2, divMax)
        )
    }

    if (isPow2(divisor)) {
      val shiftRight = log2Floor(divisor)
      if (shiftRight > 0) (in >> log2Floor(divisor)).asUInt
      else in
    } else muxTree(0, resultMax / 2, resultMax)
  }

  def rounded(resultMax: Int, divisor: Int)(in: UInt): UInt = {
    require(resultMax >= 0, "resultMax must be greater than or equal to zero")
    require(divisor > 0, "divisor must be greater than zero")

    def muxTree(divMin: Int, divCur: Int, divMax: Int): UInt = {
      if (divCur == divMax) divCur.U
      else
        Mux(
          in <= ((divCur * divisor + (divCur + 1) * divisor) / 2).U,
          muxTree(divMin, (divMin + divCur) / 2, divCur),
          muxTree(divCur, (divCur + divMax + 1) / 2, divMax)
        )
    }

    if (isPow2(divisor)) {
      val shiftRight = log2Floor(divisor)
      if (shiftRight > 0) (in >> shiftRight).asUInt +& in(shiftRight - 1)
      else in
    } else muxTree(0, resultMax / 2, resultMax)
  }
}

/**
  * Calculates mean (significant) bit width of a sequence of signals.
  */
object MeanBitWidth {
  def apply(in: Seq[Bits], resultMaxCapOption: Option[Int] = None): UInt = {
    require(in.forall(_.widthKnown), "all elements of in must have known width")
    val divisor = in.length
    val resMax = Math.round(in.map(_.getWidth).sum.toDouble / divisor).toInt
    val resMaxCapped = resultMaxCapOption.fold(resMax)(resMax.min(_))
    val res = DivideByConst.rounded(resMaxCapped, divisor)(in.map(BitWidth(_)).reduce(_ +& _))
    WireDefault(UInt(log2Up(resMaxCapped + 1).W), res)
  }
}

/**
  * Drops a fixed number of least-significant bits from each signal until the total signal dynamic width (as dictated
  * each signal's dynamic high value) fits a requested bit-width.
  */
object PackDropLSBs {
  def apply(in: Seq[(Bits, UInt)], outWidth: Int, useRegEnable: Option[Bool]): (Seq[(Bits, UInt)], UInt) = {
    require(in.nonEmpty)
    require(outWidth >= in.length, "outWidth is too small")
    val (fields, _) = in.unzip
    require(fields.forall(_.widthKnown), "All elements of in must have known width")
    val sumWidth = fields.map(_.getWidth).sum
    val maxWidth = fields.map(_.getWidth).max

//    def printRow(row: Seq[(Bits, UInt)]): Unit = {
//      printf(cf"Row: " + row.map { case (field, high) => cf"$field%b[$high:0]" }.reduce(_ + _) + "\n")
//    }

    def shiftBy(field: Bits, high: UInt, enable: Bool, amount: Int): (Bits, UInt) = {
      val shiftAmount = Mux(enable, Mux(amount.U < high, amount.U, high), 0.U)
      (field >> shiftAmount, high - shiftAmount)
    }

    @tailrec
    def shiftLattice(
      row:         Seq[(Bits, UInt)],
      n:           Int,
      i:           Int = 0,
      shiftAmount: UInt = 0.U(log2Up(maxWidth).W)
    ): (Seq[(Bits, UInt)], UInt) = {
      if (n > 0) {
        val lenSum = WireDefault(
          UInt(log2Up(row.map { case (field, _) => field.getWidth }.sum).W),
          row.map { case (_, high) => high }.reduce(_ +& _) +& row.length.U
        )
//        printf(cf"lenSum: $lenSum\n")
        val shiftEnable = RegEnableCond(lenSum, useRegEnable) > outWidth.U
        val newRow = row.map {
          case (field, high) =>
            if (i < field.getWidth) shiftBy(field, high, shiftEnable, 1)
            else (field, high)
        }
        val newShiftAmount = shiftAmount + shiftEnable.asUInt
//        printRow(newRow)
        val outRow = newRow.map {
          case (field, high) =>
            (RegEnableCond(field, useRegEnable), RegEnableCond(high, useRegEnable))
        }
        shiftLattice(outRow, n - 1, i + 1, newShiftAmount)
      } else (row, shiftAmount)
    }

    val (resultRow, shiftAmount) =
      if (sumWidth <= outWidth) (in, 0.U)
      else shiftLattice(in, maxWidth)
    (resultRow, shiftAmount)
  }

  def delay(maxWidth: Int, sumWidth: Int, outWidth: Int): Int = {
    if (sumWidth <= outWidth) 0
    else maxWidth
  }
}

/**
  * Encodes or decodes a single integer.
  */
object ExpGolombSingle {
  def encode(in: UInt, k: UInt): (UInt, UInt) = {
    require(in.widthKnown, "Input data width must be known")
    val out = WireDefault(UInt((2 * in.getWidth).W), in) + (1.U << k).asUInt
    val outHigh = WireDefault(UInt(log2Up(out.getWidth).W), Log2(out))
    (out, outHigh + outHigh - k)
  }

  def decode(in: Bits, k: UInt): UInt = {
    require(in.widthKnown, "Input data width must be known")
    WireDefault(UInt((in.getWidth / 2).W), in.asUInt) - (1.U << k).asUInt
  }
}

/**
  * Encodes a sequence of integers into a block of fixed size, or vice versa, dropping the same number of
  * least-significant bits from each encoded sample as needed. After encoding and dropping LSBs, each sample
  * will be at least one bit wide, and will have at least one set bit.
  */
object ExpGolombBlock {
  def encode(in: Seq[UInt], k: UInt, blockWidth: Int, useRegEnable: Option[Bool]): (UInt, UInt) = {
    require(blockWidth >= in.length, "blockWidth must be at least as large as the length of in")
    val encoded = in
      .map(ExpGolombSingle.encode(_, k))
      .map { case (field, high) => (RegEnableCond(field, useRegEnable), RegEnableCond(high, useRegEnable)) }
    val (shifted, droppedLSBs) = PackDropLSBs(encoded, blockWidth, useRegEnable)
    val adjusted = shifted.map {
      case (field, high) => (field.asUInt | (field === 0.U), high)
    }
    val (cat, catHigh) = DynamicHighCat(adjusted)
    val catShifted = cat << ((blockWidth - 1).U - catHigh)
    (WireDefault(UInt(blockWidth.W), catShifted.asUInt), droppedLSBs)
  }

  def encodeDelay(inWidths: Seq[Int], blockWidth: Int): Int = {
    1 + PackDropLSBs.delay(inWidths.max, inWidths.sum, blockWidth)
  }

  def decode(in: UInt, k: UInt, shift: UInt, numSamples: Int): Seq[UInt] = {
    require(in.widthKnown, "in must have a known width")

    def recoverSamples(n: Int, high: UInt = (in.getWidth - 1).U): Seq[UInt] = {
      if (n > 0) {
        val sig = if (n == numSamples) in else ApplyHighMask(in, high)
        val highestSet = Log2(sig)
        val prefixWidth = high - highestSet
        val sampleWidthMinusOne = prefixWidth + prefixWidth + k
        val samplePackedWidthMinusOne = Mux(sampleWidthMinusOne > shift, sampleWidthMinusOne - shift, 0.U)
        val low = high - samplePackedWidthMinusOne
        val recovered = (sig >> low) << shift
        recovered.asUInt +: recoverSamples(n - 1, low - 1.U)
      } else Seq()
    }

    recoverSamples(numSamples).map(ExpGolombSingle.decode(_, k))
  }
}
