package expgolomb

import expgolomb.ModelUtils._

object ExpGolombModel {
  private def encodeSingleUnsigned(in: Int, k: Int): (Int, Int) = {
    require(k >= 0, "k must be non-negative")
    require(in >= 0, "n must be non-negative")

    val res = in + (1 << k)
    val width = BigInt(res).bitLength * 2 - 1 - k
    assert(width > 0)
    (res, width - 1)
  }

  private def encodeSingleSigned(in: Int, k: Int): (Int, Int) = {
    require(k >= 0, "k must be non-negative")

    val invert = in < 0
    val (uRes, uHigh) = encodeSingleUnsigned(if (invert) ~in else in, k)
    (if (!invert) uRes else { uRes | (1 << (uHigh + 1)) }, uHigh + 1)
  }

  private def decodeSingleUnsigned(encoded: Int, k: Int): Int = {
    require(encoded >= (1 << k), "encoded number is too small")
    encoded - (1 << k)
  }

  private def decodeSingleSigned(encoded: Int, high: Int, k: Int): Int = {
    require(high > 0, "high must be greater than zero")
    val encodedVal = encoded & ~(1 << high)
    val invert = encoded != encodedVal
    if (invert) ~decodeSingleUnsigned(encodedVal, k) else decodeSingleUnsigned(encodedVal, k)
  }

  def encodeSingle(in: Int, k: Int, signed: Boolean): (Int, Int) = {
    if (signed) encodeSingleSigned(in, k)
    else encodeSingleUnsigned(in, k)
  }

  def decodeSingle(encoded: Int, k: Int, signedHighOption: Option[Int]): Int = {
    signedHighOption.fold(decodeSingleUnsigned(encoded, k))(decodeSingleSigned(encoded, _, k))
  }

  def encodeBlock(in: Seq[Int], k: Int, blockWidth: Int, signed: Boolean): (BigInt, Int) = {
    val (encodedSeq, encodedHighs) =
      in.map(encodeSingle(_, k, signed)).unzip
    val (shiftedHighs, shift) = shiftHighs(encodedHighs, blockWidth, signed)
    val adjustedSeq =
      if (signed)
        encodedSeq
          .map(_ >> shift)
          .zip(shiftedHighs)
          .map { case (shifted, high) => if (shifted == 0 || shifted == (1 << high)) shifted | 1 else shifted }
      else encodedSeq.map(enc => (enc >> shift).max(1))

    val encodedStr = adjustedSeq
      .zip(shiftedHighs)
      .map { case (field, high) => highMaskBitString(field, high) }
      .mkString
      .padTo(blockWidth, '0')
    assert(encodedStr.length == blockWidth)
    (BigInt(encodedStr, 2), shift)
  }

  def decodeBlock(
    in:         BigInt,
    blockWidth: Int,
    numSamples: Int,
    k:          Int,
    shift:      Int,
    signed:     Boolean
  ): Seq[Int] = {
    require(numSamples > 0, "numSamples must be greater than zero")
    require(shift >= 0, "shift must be greater than or equal to zero")
    def internal(in: String, n: Int): Seq[Int] = {
      if (n > 0) {
        val prefixWidth =
          if (signed) in.indexOf('1', 1) - 1
          else in.indexOf('1')
        require(prefixWidth >= 0, "no data left to decode")
        val width = (prefixWidth * 2 + 1 + k - shift).max(prefixWidth + 1) + (if (signed) 1 else 0)
        val (head, tail) = in.splitAt(width)

        val encodedStr = head + "0" * shift
        val encoded = Integer.parseInt(encodedStr, 2)
        decodeSingle(encoded, k, Option.when(signed)(encodedStr.length - 1)) +: internal(tail, n - 1)
      } else Seq()
    }
    internal(bitString(in, blockWidth), numSamples)
  }

  def encodeTotalBlock(
    in:              Seq[Int],
    totalBlockWidth: Int,
    kWidth:          Int,
    shiftWidth:      Int,
    signed:          Boolean
  ): BigInt = {
    require(
      totalBlockWidth >= minTotalBlockWidth(in.length, None, kWidth, shiftWidth, signed),
      """totalBlockWidth must be large enough to accommodate at least one bit from each input sample
        |along with the values of k and shift amount""".stripMargin.replace('\n', ' ')
    )
    val maxK = (1 << kWidth) - 1
    val maxShift = (1 << shiftWidth) - 1
    val encodedWidth = totalBlockWidth - kWidth - shiftWidth

    val k = meanBitWidth(in).min(maxK)
    val (encoded, shift) = encodeBlock(in, k, encodedWidth, signed)
    require(shift <= maxShift, "sample shift amount cannot fit into shiftWidth bits")

    assert(
      (BigInt(k).bitLength <= kWidth) && (BigInt(shift).bitLength <= shiftWidth) && (encoded.bitLength <= encodedWidth)
    )
    (BigInt(k) << (encodedWidth + shiftWidth)) | (BigInt(shift) << encodedWidth) | encoded
  }

  def decodeTotalBlock(
    in:              BigInt,
    totalBlockWidth: Int,
    numSamples:      Int,
    kWidth:          Int,
    shiftWidth:      Int,
    signed:          Boolean
  ): Seq[Int] = {
    require(
      totalBlockWidth >= minTotalBlockWidth(numSamples, None, kWidth, shiftWidth, signed),
      "totalBlockWidth is too small"
    )
    require(in.bitLength <= totalBlockWidth, "totalBlockWidth is smaller than the bit length of in")
    val (k, shift, encoded) = totalFields(in, totalBlockWidth, kWidth, shiftWidth)
    decodeBlock(encoded, totalBlockWidth - kWidth - shiftWidth, numSamples, k, shift, signed)
  }
}
