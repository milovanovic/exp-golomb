package expgolomb

import ModelUtils._

object ExpGolombModel {
  def encodeSingle(in: Int, k: Int): (Int, Int) = {
    require(k >= 0, "k must be non-negative")
    require(in >= 0, "n must be non-negative")

    val res = in + (1 << k)
    val width = BigInt(res).bitLength * 2 - 1 - k
    assert(width > 0)
    (res, width - 1)
  }

  def decodeSingle(encoded: Int, k: Int): Int = {
    require(encoded >= (1 << k), "encoded number is too small")
    encoded - (1 << k)
  }

  def encodeBlock(in: Seq[Int], k: Int, blockWidth: Int): (BigInt, Int) = {
    val (encodedSeq, encodedHighs) =
      in.map(encodeSingle(_, k)).unzip
    val (shiftedHighs, shift) = shiftHighs(encodedHighs, blockWidth)
    val adjustedSeq = encodedSeq.map(enc => (enc >> shift).max(1))

    val encodedStr = adjustedSeq
      .zip(shiftedHighs)
      .map { case (field, high) => highMaskBitString(field, high) }
      .mkString
      .padTo(blockWidth, '0')
    assert(encodedStr.length == blockWidth)
    (BigInt(encodedStr, 2), shift)
  }

  def decodeBlock(in: BigInt, blockWidth: Int, numSamples: Int, k: Int, shift: Int): Seq[Int] = {
    def internal(in: String, n: Int): Seq[Int] = {
      if (n > 0) {
        val prefixWidth = in.indexOf('1')
        require(prefixWidth >= 0, "no data left to decode")
        val width = (prefixWidth * 2 + 1 + k - shift).max(prefixWidth + 1)
        val (head, tail) = in.splitAt(width)
        decodeSingle(Integer.parseInt(head + "0" * shift, 2), k) +: internal(tail, n - 1)
      } else Seq()
    }
    internal(bitString(in, blockWidth), numSamples)
  }

  def encodeTotalBlock(in: Seq[Int], totalBlockWidth: Int, kWidth: Int, shiftWidth: Int): BigInt = {
    require(
      totalBlockWidth >= in.length + kWidth + shiftWidth,
      """totalBlockWidth must be large enough to accomodate at least one bit from each input sample
        |along with the values of k and shift amount""".stripMargin
    )
    val maxK = (1 << kWidth) - 1
    val maxShift = (1 << shiftWidth) - 1
    val encodedWidth = totalBlockWidth - kWidth - shiftWidth

    val k = meanBitWidth(in).min(maxK)
    val (encoded, shift) = encodeBlock(in, k, encodedWidth)
    require(shift <= maxShift, "sample shift amount cannot fit into shiftWidth bits")

    assert(
      (BigInt(k).bitLength <= kWidth) && (BigInt(shift).bitLength <= shiftWidth) && (encoded.bitLength <= encodedWidth)
    )
    (BigInt(k) << (encodedWidth + shiftWidth)) | (BigInt(shift) << encodedWidth) | encoded
  }

  def decodeTotalBlock(in: BigInt, totalBlockWidth: Int, numSamples: Int, kWidth: Int, shiftWidth: Int): Seq[Int] = {
    require(totalBlockWidth >= numSamples + kWidth + shiftWidth, "totalBlockWidth is too small")
    require(in.bitLength <= totalBlockWidth, "totalBlockWidth is smaller than the bit length of in")
    val (k, shift, encoded) = totalFields(in, totalBlockWidth, kWidth, shiftWidth)
    decodeBlock(encoded, totalBlockWidth - kWidth - shiftWidth, numSamples, k, shift)
  }
}
