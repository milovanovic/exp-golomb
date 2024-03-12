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
  * Version of ShiftRegister which takes an optional enable signal which determines whether the register should be
  * constructed at all.
  */
object ShiftRegisterCond {
  def apply[T <: Data](in: T, n: Int, cond: Option[Bool]): T = cond.fold(in)(ShiftRegister(in, n, _))

  def apply[T <: Data](in: T, n: Int, resetData: T, cond: Option[Bool]): T =
    cond.fold(in)(ShiftRegister(in, n, resetData, _))
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
  * Performs a width-extending summation which can be pipelined with registers with enable signals,
  * using a divide-and-conquer strategy.
  */
object SumExtended {
  private val divideAndConquerThreshold = 4

  def apply(
    in:           Seq[UInt],
    useRegEnable: Option[Bool]
  ): UInt = {
    require(in.nonEmpty, "in must not be empty")
    require(in.forall(_.widthKnown), "All elements of in must have known width")

    def reduce(in: Seq[UInt]): UInt = {
      in.tail.fold(in.head)(_ +& _)
    }

    useRegEnable.fold(reduce(in)) { enable =>
      def internal(in: Seq[UInt]): UInt = {
        if (in.length <= divideAndConquerThreshold) {
          RegEnable(reduce(in), enable)
        } else {
          val (first, second) = in.splitAt(in.length / 2)
          internal(Seq(first, second).map(internal))
        }
      }
      internal(in)
    }
  }

  def delay(inLength: Int): Int = {
    if (inLength <= divideAndConquerThreshold) 1
    else {
      val first = delay(inLength / 2)
      val second = delay(inLength - inLength / 2)
      assert(first == second)
      first.max(second) + 1
    }
  }
}

/**
  * Concatenates multiple signals, each by a respective dynamic number of least-significant bits.
  */
object DynamicHighCat {
  def apply(in: Seq[(Bits, UInt)], totalWidthOption: Option[Int] = None): (UInt, UInt) = {
    val (fields, highs) = in.unzip
    require(fields.forall(_.widthKnown), "all fields must have known width")
    require(highs.forall(_.widthKnown), "all highs must have known width")
    val totalWidth = totalWidthOption.getOrElse(highs.map(1 << _.getWidth).sum)

//    var printInfo = Seq[(UInt, UInt, Bits, UInt, UInt, UInt, UInt)]()

    val (result, resultMaskShift) = in.foldRight((0.U, 0.U(log2Up(totalWidth).W))) {
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
    Mux(in === 0.U, 0.U, Log2(in).asUInt +& 1.U)
  }
}

/**
  * Performs unsigned integer division by a constant divisor value.
  */
object DivideByConst {
  def apply(divisor: Int, resultMax: Int)(in: UInt): UInt = {
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
      WireDefault(
        UInt(log2Up(resultMax + 1).W),
        if (shiftRight > 0) (in >> log2Floor(divisor)).asUInt
        else in
      )
    } else muxTree(0, resultMax / 2, resultMax)
  }

  def rounded(divisor: Int, resultMax: Int)(in: UInt): UInt = {
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
      WireDefault(
        UInt(log2Up(resultMax + 1).W),
        if (shiftRight > 0) (in >> shiftRight).asUInt +& in(shiftRight - 1)
        else in
      )
    } else muxTree(0, resultMax / 2, resultMax)
  }
}

/**
  * Calculates mean (significant) bit width of a sequence of signals.
  */
object MeanBitWidth {
  def apply(in: Seq[Bits], useRegEnable: Option[Bool], resultMaxCapOption: Option[Int] = None): UInt = {
    require(in.forall(_.widthKnown), "all elements of in must have known width")
    val divisor = in.length
    val resMax = Math.round(in.map(_.getWidth).sum.toDouble / divisor).toInt
    val resMaxCapped = resultMaxCapOption.fold(resMax)(resMax.min(_))
    DivideByConst.rounded(divisor, resMaxCapped)(SumExtended(in.map(BitWidth(_)), useRegEnable))
  }

  def delay(inLength: Int): Int = {
    SumExtended.delay(inLength)
  }
}

/**
  * Drops a fixed number of least-significant bits from each signal until the total signal dynamic width (as dictated
  * each signal's dynamic high value) fits a requested bit-width.
  */
object PackDropLSBs {
  private def calculateMaxShift(widths: Seq[Int], outWidth: Int): Int = {
    @tailrec
    def internal(shiftAmount: Int): Int = {
      assert(shiftAmount < widths.max)
      val shiftedTotalWidth = widths.map(w => (w - shiftAmount).max(1)).sum
      if (shiftedTotalWidth > outWidth) internal(shiftAmount + 1)
      else shiftAmount
    }
    internal(0)
  }

  def apply(in: Seq[(Bits, UInt)], outWidth: Int, useRegEnable: Option[Bool]): (Seq[(Bits, UInt)], UInt) = {
    require(in.nonEmpty)
    require(outWidth >= in.length, "outWidth is too small")
    val (fields, highs) = in.unzip
    require(fields.forall(_.widthKnown), "All elements of in must have known width")
    val widths = fields.map(_.getWidth)

    if (widths.sum <= outWidth) (in, 0.U)
    else {
      val maxShift = calculateMaxShift(widths, outWidth)
      val lenSumDelay = SumExtended.delay(in.length)
      val shiftedLengthsSums: Seq[(UInt, Seq[UInt])] = Seq.tabulate(maxShift + 1) { shiftAmount =>
        val newHighs =
          if (shiftAmount == 0) highs
          else highs.map { high => Mux(high > shiftAmount.U, high - shiftAmount.U, 0.U) }
        val lenSum = SumExtended(newHighs, useRegEnable)
        val newHighsDelayed = newHighs.map { high => ShiftRegisterCond(high, lenSumDelay, useRegEnable) }
        (lenSum, newHighsDelayed)
      }

      def shiftTree(low: Int, high: Int): (Seq[UInt], UInt) = {
        if (low == high) {
          val (_, highsShifted) = shiftedLengthsSums(low)
          (highsShifted, low.U(log2Up(maxShift + 1).W))
        } else {
          val mid = (low + high) / 2
          val (lenSum, _) = shiftedLengthsSums(mid)
          val (rightHighs, rightShift) = shiftTree(mid + 1, high)
          val (leftHighs, leftShift) = shiftTree(low, mid)

          val select: Bool = lenSum > (outWidth - in.length).max(0).U
          (
            rightHighs.zip(leftHighs).map { case (rightHigh, leftHigh) => Mux(select, rightHigh, leftHigh) },
            Mux(select, rightShift, leftShift)
          )
        }
      }

      val (newHighs, shiftAmount) = shiftTree(0, maxShift)
      val newFields = fields.map(ShiftRegisterCond(_, lenSumDelay, useRegEnable) >> shiftAmount)
      (
        newFields.zip(newHighs).map {
          case (field, high) => (RegEnableCond(field, useRegEnable), RegEnableCond(high, useRegEnable))
        },
        RegEnableCond(shiftAmount, useRegEnable)
      )
    }
  }

  def delay(inWidths: Seq[Int], outWidth: Int): Int = {
    if (inWidths.sum <= outWidth) 0
    else {
      1 + SumExtended.delay(inWidths.length)
    }
  }
}

/**
  * Encodes or decodes a single integer.
  */
object ExpGolombSingle {
  def encode(in: UInt, k: UInt): (UInt, UInt) = {
    require(in.widthKnown, "Input data width must be known")
    val out = in +& (1.U << k).asUInt
    val outHighSet = Log2(out)
    val outHighFull = outHighSet +& outHighSet - k
    (out, outHighFull)
  }

  def decode(in: Bits, k: UInt): UInt = {
    require(in.widthKnown, "Input data width must be known")
    in.asUInt - (1.U << k).asUInt
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
    val (cat, catHigh) = DynamicHighCat(adjusted, Some(blockWidth))
    val catShifted = cat << ((blockWidth - 1).U - catHigh)
    (WireDefault(UInt(blockWidth.W), catShifted.asUInt), droppedLSBs)
  }

  def encodeDelay(inWidths: Seq[Int], blockWidth: Int): Int = {
    1 + PackDropLSBs.delay(inWidths, blockWidth)
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
