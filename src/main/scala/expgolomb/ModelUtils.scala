package expgolomb

import scala.annotation.tailrec

object ModelUtils {
  def bitString(n: BigInt, minWidth: Int): String = {
    val bitStr = n.toString(2)
    "0" * (minWidth - bitStr.length) + bitStr
  }

  def bitString(n: Int, minWidth: Int): String = {
    bitString(BigInt(n), minWidth)
  }

  def highMaskBitString(n: BigInt, high: Int): String = {
    bitString(n, high + 1).takeRight(high + 1)
  }

  def highMaskBitString(n: Int, high: Int): String = {
    highMaskBitString(BigInt(n), high)
  }

  def meanBitWidth(in: Seq[Int]): Int = {
    Math.round(in.map(BigInt(_).bitLength).sum.toFloat / in.length)
  }

  def shiftHighs(highs: Seq[Int], blockWidth: Int): (Seq[Int], Int) = {
    require(blockWidth >= highs.length, "block width must be greater than or equal to the number of input elements")

    @tailrec
    def internal(shift: Int, lastHighSum: Option[Int]): (Seq[Int], Int) = {
      val shiftedHighs = highs.map(h => (h - shift).max(0))
      val lenSum = shiftedHighs.sum + highs.length
      if (
        lastHighSum.fold(false)(_ == lenSum) ||
        (lenSum <= blockWidth)
      ) (shiftedHighs, shift)
      else internal(shift + 1, Some(lenSum))
    }

    internal(0, None)
  }

  def totalFields(encodedTotal: BigInt, totalWidth: Int, kWidth: Int, shiftWidth: Int): (Int, Int, BigInt) = {
    require(totalWidth > kWidth + shiftWidth, "totalWidth is too small to fit any encoded samples")
    val encodedWidth = totalWidth - kWidth - shiftWidth
    val (k, shift, encoded) = (
      encodedTotal >> (totalWidth - kWidth),
      (encodedTotal & ((BigInt(1) << (shiftWidth + encodedWidth)) - 1)) >> encodedWidth,
      encodedTotal & ((BigInt(1) << encodedWidth) - 1)
    )
    (k.toInt, shift.toInt, encoded)
  }
}
