package expgolomb

import chisel3.util.log2Up
import expgolomb.ModelUtils._
import org.scalatest.flatspec.AnyFlatSpec

class ExpGolombModelSpec extends AnyFlatSpec {
  val encodingTable = IndexedSeq( // First index: k; Second index: n
    IndexedSeq(
      "1",
      "010",
      "011",
      "00100",
      "00101",
      "00110",
      "00111",
      "0001000",
      "0001001",
      "0001010",
      "0001011",
      "0001100",
      "0001101",
      "0001110",
      "0001111",
      "000010000",
      "000010001",
      "000010010",
      "000010011",
      "000010100",
      "000010101",
      "000010110",
      "000010111",
      "000011000",
      "000011001",
      "000011010",
      "000011011",
      "000011100",
      "000011101",
      "000011110",
      "000011111",
      "00000100000"
    ),
    IndexedSeq(
      "10",
      "11",
      "0100",
      "0101",
      "0110",
      "0111",
      "001000",
      "001001",
      "001010",
      "001011",
      "001100",
      "001101",
      "001110",
      "001111",
      "00010000",
      "00010001",
      "00010010",
      "00010011",
      "00010100",
      "00010101",
      "00010110",
      "00010111",
      "00011000",
      "00011001",
      "00011010",
      "00011011",
      "00011100",
      "00011101",
      "00011110",
      "00011111",
      "0000100000",
      "0000100001"
    ),
    IndexedSeq(
      "100",
      "101",
      "110",
      "111",
      "01000",
      "01001",
      "01010",
      "01011",
      "01100",
      "01101",
      "01110",
      "01111",
      "0010000",
      "0010001",
      "0010010",
      "0010011",
      "0010100",
      "0010101",
      "0010110",
      "0010111",
      "0011000",
      "0011001",
      "0011010",
      "0011011",
      "0011100",
      "0011101",
      "0011110",
      "0011111",
      "000100000",
      "000100001",
      "000100010",
      "000100011"
    ),
    IndexedSeq(
      "1000",
      "1001",
      "1010",
      "1011",
      "1100",
      "1101",
      "1110",
      "1111",
      "010000",
      "010001",
      "010010",
      "010011",
      "010100",
      "010101",
      "010110",
      "010111",
      "011000",
      "011001",
      "011010",
      "011011",
      "011100",
      "011101",
      "011110",
      "011111",
      "00100000",
      "00100001",
      "00100010",
      "00100011",
      "00100100",
      "00100101",
      "00100110",
      "00100111"
    )
  )

  "Singular encoding/decoding" should "encode unsigned integers" in {
    for {
      (tab, k) <- encodingTable.zipWithIndex
      (expected, n) <- tab.zipWithIndex
    } {
      val (encoded, high) = ExpGolombModel.encodeSingle(n, k)
      assertResult(expected)(highMaskBitString(encoded, high))
    }
  }

  it should "decode unsigned integers" in {
    for {
      (tab, k) <- encodingTable.zipWithIndex
      (encodedStr, expected) <- tab.zipWithIndex
    } {
      assertResult(expected)(ExpGolombModel.decodeSingle(Integer.parseInt(encodedStr, 2), k))
    }
  }

  it should "result in original input when decoded after encoding" in {
    for {
      k <- 0 to 16
      n <- 0 until (1 << 16)
    } {
      assertResult(n)(ExpGolombModel.decodeSingle(ExpGolombModel.encodeSingle(n, k)._1, k))
    }
  }

  "Block encoding/decoding" should "produce blocks that comply with the requested block size" in {
    for {
      k <- 0 to 16
      n1 <- (0 to 3).map(_ * 8)
      n2 <- (0 to 3).map(_ * 7)
      n3 <- (0 to 3).map(_ * 6)
      n4 <- (0 to 3).map(_ * 5)
      in = Seq(n1, n2, n3, n4)
      blockWidth <- (1 to 17).map(_ * in.length)
    } {
      val (encoded, _) = ExpGolombModel.encodeBlock(in, k, blockWidth)
      assert(encoded.bitLength <= blockWidth)
    }
  }

  it should "concatenate and left-shift encoded samples when given enough space" in {
    for {
      k <- 0 to 3
      n1 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 20, 21, 24, 27, 29, 32)
      n2 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 20, 21, 24, 27, 29, 32)
      n3 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 20, 21, 24, 27, 29, 32)
      n4 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 20, 21, 24, 27, 29, 32)
      in = Seq(n1, n2, n3, n4)
    } {
      val blockWidth = 64
      val expectedStr = in
        .map(ExpGolombModel.encodeSingle(_, k))
        .map { case (encoded, high) => highMaskBitString(encoded, high) }
        .mkString
        .padTo(blockWidth, '0')
      val (encoded, _) = ExpGolombModel.encodeBlock(in, k, blockWidth)
      val encodedStr = bitString(encoded, blockWidth)
      assertResult(expectedStr)(encodedStr)
    }
  }

  it should "drop enough, but not more than necessary least-significant bits from samples" in {
    for {
      k <- 0 to 3
      n1 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n2 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n3 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n4 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      in = Seq(n1, n2, n3, n4)
      blockWidth <- (12 * in.length until 2 * in.length by -2 * in.length) ++ (2 * in.length to in.length by -1)
    } {
      val (_, dropped) = ExpGolombModel.encodeBlock(in, k, blockWidth)
      val lenSum = in.map(n => 0.max(ExpGolombModel.encodeSingle(n, k)._2 - dropped)).sum + in.length
      assert(lenSum <= blockWidth, "not enough least-significant bits dropped from encoded samples")
      assert(
        (dropped == 0) || (lenSum + in.length > blockWidth),
        "more than necessary least-significant bits dropped from encoded samples"
      )
    }
  }

  it should "ensure at least one set bit in each encoded sample after dropping least-significant bits" in {
    val in = (0 to 16)
    val blockWidth = in.length
    val expected = (1 << in.length) - 1
    for (k <- 0 to 3) {
      val (encoded, _) = ExpGolombModel.encodeBlock(in, k, blockWidth)
      assertResult(expected)(encoded)
    }
  }

  it should "correctly encode and shift samples" in {
    for {
      k <- 0 to 16
      n1 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n2 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n3 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n4 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      in = Seq(n1, n2, n3, n4)
      blockWidth <- (12 * in.length until 2 * in.length by -2 * in.length) ++ (2 * in.length to in.length by -1)
    } {
      val (encoded, shift) = ExpGolombModel.encodeBlock(in, k, blockWidth)
      val encodedStr = bitString(encoded, blockWidth)
      val expectedStr = in
        .map(ExpGolombModel.encodeSingle(_, k))
        .map { case (enc, high) => highMaskBitString(1.max(enc >> shift), 0.max(high - shift)) }
        .mkString
        .padTo(blockWidth, '0')
      assertResult(expectedStr)(encodedStr)
    }
  }

  it should "correctly decode encoded blocks" in {
    for {
      k <- 0 to 16
      n1 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n2 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n3 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n4 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      in = Seq(n1, n2, n3, n4)
      blockWidth <- (12 * in.length until 2 * in.length by -2 * in.length) ++ (2 * in.length to in.length by -1)
    } {
      val (encoded, shift) = ExpGolombModel.encodeBlock(in, k, blockWidth)
      val decoded = ExpGolombModel.decodeBlock(encoded, blockWidth, in.length, k, shift)
      val expected = in
        .map(ExpGolombModel.encodeSingle(_, k))
        .map { case (enc, _) => 1.max(enc >> shift) << shift }
        .map(ExpGolombModel.decodeSingle(_, k))
      assertResult(
        expected,
        s"shift=$shift; k=$k; in=$in; blockWidth=$blockWidth; encoded=" + bitString(encoded, blockWidth)
      )(decoded)
    }
  }

  "Total block encoding/decoding" should "produce encoded blocks that comply with the requested block size" in {
    for {
      n1 <- (0 to 3).map(_ * 8)
      n2 <- (0 to 3).map(_ * 7)
      n3 <- (0 to 3).map(_ * 6)
      n4 <- (0 to 3).map(_ * 5)
      in = Seq(n1, n2, n3, n4)
      encodedWidth <- (1 to 17).map(_ * in.length)
      kWidth = 5
      shiftWidth <- Seq(1.max(log2Up(in.max + 1) * 2 - 2), 5)
      totalWidth = encodedWidth + kWidth + shiftWidth
    } {
      val encoded = ExpGolombModel.encodeTotalBlock(in, totalWidth, kWidth, shiftWidth)
      assert(encoded.bitLength <= totalWidth)
    }
  }

  it should "select the value for k that is equal to the rounded mean significant bit-width of input samples" in {
    for {
      n1 <- (0 to 3).map(_ * 8)
      n2 <- (0 to 3).map(_ * 7)
      n3 <- (0 to 3).map(_ * 6)
      n4 <- (0 to 3).map(_ * 5)
      in = Seq(n1, n2, n3, n4)
      encodedWidth <- (1 to 17).map(_ * in.length)
      kWidth = 5
      shiftWidth <- Seq(1.max(log2Up(in.max + 1) * 2 - 2), 5)
      totalWidth = encodedWidth + kWidth + shiftWidth
    } {
      val total = ExpGolombModel.encodeTotalBlock(in, totalWidth, kWidth, shiftWidth)
      val k = total >> (encodedWidth + shiftWidth)
      val expected = Math.round(in.map(el => if (el == 0) 0 else el.toBinaryString.length).sum.toFloat / in.length)
      assertResult(expected)(k)
    }
  }

  it should "produce encoded blocks in coherence with .encodeBlock()" in {
    for {
      n1 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n2 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n3 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n4 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      in = Seq(n1, n2, n3, n4)
      encodedWidth <- (12 * in.length until 2 * in.length by -2 * in.length) ++ (2 * in.length to in.length by -1)
      kWidth = 5
      shiftWidth = 5
      totalWidth = encodedWidth + kWidth + shiftWidth
    } {
      val total = ExpGolombModel.encodeTotalBlock(in, totalWidth, kWidth, shiftWidth)
      val totalStr = bitString(total, totalWidth)

      val k = Integer.parseInt(totalStr.take(kWidth), 2)
      val shift = Integer.parseInt(totalStr.slice(kWidth, kWidth + shiftWidth), 2)
      val encoded = BigInt(totalStr.drop(kWidth + shiftWidth), 2)
      assertResult((k, shift, encoded))(totalFields(total, totalWidth, kWidth, shiftWidth)) // Testing totalFields

      val (expected, expectedShift) = ExpGolombModel.encodeBlock(in, k, encodedWidth)
      assertResult(expectedShift)(shift)
      assertResult(expected)(encoded)
    }
  }

  it should "decode encoded blocks in coherence with .decodeBlock()" in {
    for {
      n1 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n2 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n3 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n4 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      in = Seq(n1, n2, n3, n4)
      encodedWidth <- (12 * in.length until 2 * in.length by -2 * in.length) ++ (2 * in.length to in.length by -1)
      kWidth = 5
      shiftWidth = 5
      totalWidth = encodedWidth + kWidth + shiftWidth
    } {
      val total = ExpGolombModel.encodeTotalBlock(in, totalWidth, kWidth, shiftWidth)
      val (k, shift, encoded) = totalFields(total, totalWidth, kWidth, shiftWidth)
      val expected = ExpGolombModel.decodeBlock(encoded, encodedWidth, in.length, k, shift)
      val actual = ExpGolombModel.decodeTotalBlock(total, totalWidth, in.length, kWidth, shiftWidth)
      assertResult(expected)(actual)
    }
  }
}
