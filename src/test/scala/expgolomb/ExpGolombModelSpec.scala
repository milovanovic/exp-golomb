package expgolomb

import expgolomb.ModelUtils._
import org.scalatest.flatspec.AnyFlatSpec

class ExpGolombModelSpec extends AnyFlatSpec {
  val encodingTableUnsigned = IndexedSeq( // First index: k; Second index: n
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
  val encodingTableSigned = IndexedSeq( // n: -16 to 15
    IndexedSeq(
      "1000010000",
      "10001111",
      "10001110",
      "10001101",
      "10001100",
      "10001011",
      "10001010",
      "10001001",
      "10001000",
      "100111",
      "100110",
      "100101",
      "100100",
      "1011",
      "1010",
      "11",
      "01",
      "0010",
      "0011",
      "000100",
      "000101",
      "000110",
      "000111",
      "00001000",
      "00001001",
      "00001010",
      "00001011",
      "00001100",
      "00001101",
      "00001110",
      "00001111",
      "0000010000"
    ),
    IndexedSeq(
      "100010001",
      "100010000",
      "1001111",
      "1001110",
      "1001101",
      "1001100",
      "1001011",
      "1001010",
      "1001001",
      "1001000",
      "10111",
      "10110",
      "10101",
      "10100",
      "111",
      "110",
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
      "000010001"
    ),
    IndexedSeq(
      "10010011",
      "10010010",
      "10010001",
      "10010000",
      "101111",
      "101110",
      "101101",
      "101100",
      "101011",
      "101010",
      "101001",
      "101000",
      "1111",
      "1110",
      "1101",
      "1100",
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
      "00010011"
    ),
    IndexedSeq(
      "1010111",
      "1010110",
      "1010101",
      "1010100",
      "1010011",
      "1010010",
      "1010001",
      "1010000",
      "11111",
      "11110",
      "11101",
      "11100",
      "11011",
      "11010",
      "11001",
      "11000",
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
      "0010111"
    )
  )

  behavior.of("Singular encoding/decoding")
  for (signed <- Seq(false, true)) {
    val intType = if (signed) "signed" else "unsigned"
    val table = if (signed) encodingTableSigned else encodingTableUnsigned

    it should s"encode $intType integers" in {
      for {
        (tab, k) <- table.zipWithIndex
        (expected, n) <- tab.zipWithIndex
      } {
        val in = if (signed) n - tab.length / 2 else n
        val (encoded, high) = ExpGolombModel.encodeSingle(in, k, signed)
        assertResult(expected)(highMaskBitString(encoded, high))
      }
    }

    it should s"decode $intType integers" in {
      for {
        (tab, k) <- table.zipWithIndex
        (encodedStr, n) <- tab.zipWithIndex
      } {
        val expected = if (signed) n - tab.length / 2 else n
        assertResult(expected) {
          ExpGolombModel.decodeSingle(Integer.parseInt(encodedStr, 2), k, Option.when(signed)(encodedStr.length - 1))
        }
      }
    }
  }

  it should "result in original input when decoded after encoding" in {
    for {
      signed <- Seq(false, true)
      k <- 0 to 16
      n <- if (signed) -(1 << 8) until (1 << 8) else 0 until (1 << 16)
    } {
      assertResult(n) {
        val (encoded, high) = ExpGolombModel.encodeSingle(n, k, signed)
        ExpGolombModel.decodeSingle(encoded, k, Option.when(signed)(high))
      }
    }
  }

  "Block encoding/decoding" should "produce blocks that comply with the requested block size" in {
    for {
      signed <- Seq(false, true)
      k <- 0 to 16
      n1 <- (0 to 3).map(_ * 8)
      n2 <- (0 to 3).map(_ * 7)
      n3 <- (0 to 3).map(_ * 6)
      n4 <- (0 to 3).map(_ * 5)
      in = Seq(n1, n2, n3, n4)
      blockWidth <- (if (signed) 2 to 17 else 1 to 17).map(_ * in.length)
    } {
      val (encoded, _) = ExpGolombModel.encodeBlock(in, k, blockWidth, signed)
      assert(encoded.bitLength <= blockWidth)
    }
  }

  it should "concatenate and left-shift encoded samples when given enough space" in {
    for {
      signed <- Seq(false, true)
      k <- 0 to 3
      n1 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 20, 21, 24, 27, 29, 32)
      n2 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 20, 21, 24, 27, 29, 32)
      n3 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 20, 21, 24, 27, 29, 32)
      n4 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 20, 21, 24, 27, 29, 32)
      in = Seq(n1, n2, n3, n4)
    } {
      val blockWidth = 64
      val expectedStr = in
        .map(ExpGolombModel.encodeSingle(_, k, signed))
        .map { case (encoded, high) => highMaskBitString(encoded, high) }
        .mkString
        .padTo(blockWidth, '0')
      val (encoded, _) = ExpGolombModel.encodeBlock(in, k, blockWidth, signed)
      val encodedStr = bitString(encoded, blockWidth)
      assertResult(expectedStr)(encodedStr)
    }
  }

  it should "drop enough, but not more than necessary least-significant bits from samples" in {
    for {
      signed <- Seq(false, true)
      k <- 0 to 3
      n1 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n2 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n3 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n4 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      in = Seq(n1, n2, n3, n4)
      blockWidth <-
        if (signed) (13 * in.length until 3 * in.length by -2 * in.length) ++ (3 * in.length to 2 * in.length by -1)
        else (12 * in.length until 2 * in.length by -2 * in.length) ++ (2 * in.length to in.length by -1)
    } {
      val (_, dropped) = ExpGolombModel.encodeBlock(in, k, blockWidth, signed)
      val lenSum = in.map(n => 0.max(ExpGolombModel.encodeSingle(n, k, signed)._2 - dropped)).sum + in.length
      assert(lenSum <= blockWidth, "not enough least-significant bits dropped from encoded samples")
      assert(
        (dropped == 0) || (lenSum + in.length > blockWidth),
        "more than necessary least-significant bits dropped from encoded samples"
      )
    }
  }

  it should "ensure at least one set bit in each encoded sample after dropping least-significant bits" in {
    val uIn = 0 to 16
    val uBlockWidth = uIn.length
    val uExpected = (1 << uIn.length) - 1
    for (k <- 0 to 3) {
      val (encoded, _) = ExpGolombModel.encodeBlock(uIn, k, uBlockWidth, signed = false)
      assertResult(uExpected)(encoded)
    }

    val sIn = -8 to 8
    val sBlockWidth = 2 * sIn.length
    for (k <- 0 to 3) {
      val maskExpected = BigInt("01" * sIn.length, 2)
      val (encoded, _) = ExpGolombModel.encodeBlock(sIn, k, sBlockWidth, signed = true)
      assertResult(maskExpected)(encoded & maskExpected)
    }
  }

  it should "encode and shift samples" in {
    for {
      signed <- Seq(false, true)
      k <- 0 to 16
      n1 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n2 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n3 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n4 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      in = Seq(n1, n2, n3, n4)
      blockWidth <-
        if (signed) (13 * in.length until 3 * in.length by -2 * in.length) ++ (3 * in.length to 2 * in.length by -1)
        else (12 * in.length until 2 * in.length by -2 * in.length) ++ (2 * in.length to in.length by -1)
    } {
      val (encoded, shift) = ExpGolombModel.encodeBlock(in, k, blockWidth, signed)
      val encodedStr = bitString(encoded, blockWidth)
      val expectedStr = in
        .map(ExpGolombModel.encodeSingle(_, k, signed))
        .map {
          case (enc, high) =>
            if (signed) {
              val s = enc >> shift
              highMaskBitString(if (s == 0 || s == (1 << high)) s | 1 else s, 1.max(high - shift))
            } else highMaskBitString(1.max(enc >> shift), 0.max(high - shift))
        }
        .mkString
        .padTo(blockWidth, '0')
      assertResult(expectedStr)(encodedStr)
    }
  }

  it should "decode encoded blocks" in {
    for {
      signed <- Seq(false, true)
      k <- 0 to 16
      n1 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n2 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n3 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n4 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      in = Seq(n1, n2, n3, n4)
      blockWidth <-
        if (signed) (13 * in.length until 3 * in.length by -2 * in.length) ++ (3 * in.length to 2 * in.length by -1)
        else (12 * in.length until 2 * in.length by -2 * in.length) ++ (2 * in.length to in.length by -1)
    } {
      val (encoded, shift) = ExpGolombModel.encodeBlock(in, k, blockWidth, signed)
      val decoded = ExpGolombModel.decodeBlock(encoded, blockWidth, in.length, k, shift, signed)
      val expected = in
        .map(ExpGolombModel.encodeSingle(_, k, signed))
        .map {
          case (enc, high) =>
            if (signed) {
              val s = enc >> shift
              (if (s == 0 || s == (1 << high)) s | 1 else s, 1.max(high - shift))
            } else (1.max(enc >> shift), 0.max(high - shift))
        }
        .map { case (shifted, high) => (shifted << shift, high + shift) }
        .map { case (enc, high) => ExpGolombModel.decodeSingle(enc, k, Option.when(signed)(high)) }
      assertResult(
        expected,
        s"shift=$shift; k=$k; in=$in; blockWidth=$blockWidth; encoded=" + bitString(encoded, blockWidth)
      )(decoded)
    }
  }

  "Total block encoding/decoding" should "produce encoded blocks that comply with the requested block size" in {
    for {
      signed <- Seq(false, true)
      n1 <- (0 to 3).map(_ * 8).map(_ - (if (signed) (3 * 8 + 1) / 2 else 0))
      n2 <- (0 to 3).map(_ * 7).map(_ - (if (signed) (3 * 7 + 1) / 2 else 0))
      n3 <- (0 to 3).map(_ * 6).map(_ - (if (signed) (3 * 6 + 1) / 2 else 0))
      n4 <- (0 to 3).map(_ * 5).map(_ - (if (signed) (3 * 5 + 1) / 2 else 0))
      in = Seq(n1, n2, n3, n4)
      encodedWidth <- ((if (signed) 2 else 1) to 17).map(_ * in.length)
      kWidth = 5
      shiftWidth = 5
      totalWidth = encodedWidth + kWidth + shiftWidth
    } {
      val encoded = ExpGolombModel.encodeTotalBlock(in, totalWidth, kWidth, shiftWidth, signed)
      assert(encoded.bitLength <= totalWidth)
    }
  }

  it should "select the value for k that is equal to the rounded mean significant bit-width of input samples" in {
    for {
      signed <- Seq(false, true)
      n1 <- (0 to 3).map(_ * 8)
      n2 <- (0 to 3).map(_ * 7)
      n3 <- (0 to 3).map(_ * 6)
      n4 <- (0 to 3).map(_ * 5)
      in = Seq(n1, n2, n3, n4)
      encodedWidth <- ((if (signed) 2 else 1) to 17).map(_ * in.length)
      kWidth = 5
      shiftWidth = 5
      totalWidth = encodedWidth + kWidth + shiftWidth
    } {
      val total = ExpGolombModel.encodeTotalBlock(in, totalWidth, kWidth, shiftWidth, signed)
      val k = total >> (encodedWidth + shiftWidth)
      val expected = Math.round(
        in.map(el => if (signed && el < 0) ~el else el)
          .map(el => if (el == 0) 0 else el.toBinaryString.length)
          .sum
          .toFloat / in.length
      )
      assertResult(expected)(k)
    }
  }

  it should "produce encoded blocks in coherence with .encodeBlock()" in {
    for {
      signed <- Seq(false, true)
      n1 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n2 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n3 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n4 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      in = Seq(n1, n2, n3, n4)
      encodedWidth <-
        if (signed) (13 * in.length until 3 * in.length by -2 * in.length) ++ (3 * in.length to 2 * in.length by -1)
        else (12 * in.length until 2 * in.length by -2 * in.length) ++ (2 * in.length to in.length by -1)
      kWidth = 5
      shiftWidth = 5
      totalWidth = encodedWidth + kWidth + shiftWidth
    } {
      val total = ExpGolombModel.encodeTotalBlock(in, totalWidth, kWidth, shiftWidth, signed)
      val totalStr = bitString(total, totalWidth)

      val k = Integer.parseInt(totalStr.take(kWidth), 2)
      val shift = Integer.parseInt(totalStr.slice(kWidth, kWidth + shiftWidth), 2)
      val encoded = BigInt(totalStr.drop(kWidth + shiftWidth), 2)
      assertResult((k, shift, encoded))(totalFields(total, totalWidth, kWidth, shiftWidth)) // Testing totalFields

      val (expected, expectedShift) = ExpGolombModel.encodeBlock(in, k, encodedWidth, signed)
      assertResult(expectedShift)(shift)
      assertResult(expected)(encoded)
    }
  }

  it should "decode encoded blocks in coherence with .decodeBlock()" in {
    for {
      signed <- Seq(false, true)
      n1 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n2 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n3 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      n4 <- Seq(0, 1, 2, 3, 4, 7, 8, 10, 12, 16, 24, 32)
      in = Seq(n1, n2, n3, n4)
      encodedWidth <-
        if (signed) (13 * in.length until 3 * in.length by -2 * in.length) ++ (3 * in.length to 2 * in.length by -1)
        else (12 * in.length until 2 * in.length by -2 * in.length) ++ (2 * in.length to in.length by -1)
      kWidth = 5
      shiftWidth = 5
      totalWidth = encodedWidth + kWidth + shiftWidth
    } {
      val total = ExpGolombModel.encodeTotalBlock(in, totalWidth, kWidth, shiftWidth, signed)
      val (k, shift, encoded) = totalFields(total, totalWidth, kWidth, shiftWidth)
      val expected = ExpGolombModel.decodeBlock(encoded, encodedWidth, in.length, k, shift, signed)
      val actual = ExpGolombModel.decodeTotalBlock(total, totalWidth, in.length, kWidth, shiftWidth, signed)
      assertResult(expected)(actual)
    }
  }
}
