package expgolomb

import chisel3._
import chisel3.util._
import chiseltest._
import expgolomb.ModelUtils._
import org.scalatest.flatspec.AnyFlatSpec

class TestableUnsignedExpGolombEncodeSingle(width: Int, kWidth: Int) extends Module {
  val in = IO(Input(UInt(width.W)))
  val k = IO(Input(UInt(kWidth.W)))
  val out = IO(Output(Bits()))
  val high = IO(Output(UInt()))

  val (resOut, resHigh) = ExpGolombSingle.encodeUnsigned(in, k)
  out := resOut
  high := resHigh
}

class TestableSignedExpGolombEncodeSingle(width: Int, kWidth: Int) extends Module {
  val in = IO(Input(SInt(width.W)))
  val k = IO(Input(UInt(kWidth.W)))
  val out = IO(Output(Bits()))
  val high = IO(Output(UInt()))

  val (resOut, resHigh) = ExpGolombSingle.encodeSigned(in, k)
  out := resOut
  high := resHigh
}

class TestableUnsignedExpGolombDecodeSingle(width: Int, kWidth: Int) extends Module {
  val in = IO(Input(Bits(width.W)))
  val k = IO(Input(UInt(kWidth.W)))
  val out = IO(Output(UInt()))

  out := ExpGolombSingle.decodeUnsigned(in, k)
}

class TestableSignedExpGolombDecodeSingle(width: Int, kWidth: Int) extends Module {
  val in = IO(Input(Bits(width.W)))
  val high = IO(Input(UInt(log2Up(width).W)))
  val k = IO(Input(UInt(kWidth.W)))
  val out = IO(Output(SInt()))

  out := ExpGolombSingle.decodeSigned(in, high, k)
}

class TestableUnsignedExpGolombEncodeBlock(widths: Seq[Int], kWidth: Int, blockWidth: Int, useRegEnable: Option[Bool])
    extends Module {
  val in = widths.map(w => IO(Input(UInt(w.W))))
  val k = IO(Input(UInt(kWidth.W)))
  val out = IO(Output(Bits(blockWidth.W)))
  val shift = IO(Output(UInt()))

  val (resOut, resShift) = ExpGolombBlock.encodeUnsigned(in, k, blockWidth, useRegEnable)
  out := resOut
  shift := resShift
}

class TestableSignedExpGolombEncodeBlock(widths: Seq[Int], kWidth: Int, blockWidth: Int, useRegEnable: Option[Bool])
    extends Module {
  val in = widths.map(w => IO(Input(SInt(w.W))))
  val k = IO(Input(UInt(kWidth.W)))
  val out = IO(Output(Bits(blockWidth.W)))
  val shift = IO(Output(UInt()))

  val (resOut, resShift) = ExpGolombBlock.encodeSigned(in, k, blockWidth, useRegEnable)
  out := resOut
  shift := resShift
}

class TestableUnsignedExpGolombDecodeBlock(blockWidth: Int, n: Int, kWidth: Int, shiftWidth: Int) extends Module {
  val in = IO(Input(Bits(blockWidth.W)))
  val k = IO(Input(UInt(kWidth.W)))
  val shift = IO(Input(UInt(shiftWidth.W)))
  val out = Seq.fill(n)(IO(Output(UInt())))

  out.zip(ExpGolombBlock.decodeUnsigned(in, n, k, shift)).foreach { case (o, d) => o := d }
}

class TestableSignedExpGolombDecodeBlock(blockWidth: Int, n: Int, kWidth: Int, shiftWidth: Int) extends Module {
  val in = IO(Input(Bits(blockWidth.W)))
  val k = IO(Input(UInt(kWidth.W)))
  val shift = IO(Input(UInt(shiftWidth.W)))
  val out = Seq.fill(n)(IO(Output(SInt())))

  out.zip(ExpGolombBlock.decodeSigned(in, n, k, shift)).foreach { case (o, d) => o := d }
}

class ExpGolombSpec extends AnyFlatSpec with ChiselScalatestTester {
  "ExpGolombSingle" should "encode UInts" in test(new TestableUnsignedExpGolombEncodeSingle(10, 5)) { c =>
    for {
      k <- 0 to 16
      in <- 0 to 1023
    } {
      c.in.poke(in)
      c.k.poke(k)
      val (expected, expectedHigh) = ExpGolombModel.encodeSingle(in, k, signed = false)
      c.out.expect(expected)
      c.high.expect(expectedHigh)
    }
  }

  it should "decode UInts" in test(new TestableUnsignedExpGolombDecodeSingle(17, 5)) { c =>
    for {
      k <- 0 to 16
      in <- 0 to 1023
    } {
      val (encoded, _) = ExpGolombModel.encodeSingle(in, k, signed = false)
      c.in.poke(encoded)
      c.k.poke(k)
      c.out.expect(ExpGolombModel.decodeSingle(encoded, k, signedHighOption = None))
    }
  }

  it should "encode SInts" in test(new TestableSignedExpGolombEncodeSingle(10, 5)) { c =>
    for {
      k <- 0 to 16
      in <- -512 until 512
    } {
      c.in.poke(in)
      c.k.poke(k)
      val (expected, expectedHigh) = ExpGolombModel.encodeSingle(in, k, signed = true)
      c.out.expect(expected)
      c.high.expect(expectedHigh)
    }
  }

  it should "decode SInts" in test(new TestableSignedExpGolombDecodeSingle(20, 5)) { c =>
    for {
      k <- 0 to 16
      in <- -512 until 512
    } {
      val (encoded, high) = ExpGolombModel.encodeSingle(in, k, signed = true)
      c.in.poke(encoded)
      c.high.poke(high)
      c.k.poke(k)
      c.out.expect(ExpGolombModel.decodeSingle(encoded, k, signedHighOption = Some(high)))
    }
  }

  "ExpGolombBlock" should "encode sequences of UInts into blocks" in {
    val widths = Seq(1, 2, 4, 7, 8)
    for (blockWidth <- (1 to 4).map(_ * widths.length)) {
      test(new TestableUnsignedExpGolombEncodeBlock(widths, 4, blockWidth, None))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
          for {
            k <- 0 to 8
            a <- Seq(0, 1)
            b <- 0 until 4
            c_ <- (0 until 4).map(_ * 5)
            d <- (0 until 4).map(_ * 42.333).map(Math.round(_).toInt)
            e <- (0 until 4).map(_ * 85)
            in = Seq(a, b, c_, d, e)
          } {
            assert(in.length == widths.length)
            val (expected, shiftExpected) = ExpGolombModel.encodeBlock(in, k, blockWidth, signed = false)

            c.in.zip(in).foreach { case (inHw, in) => inHw.poke(in) }
            c.k.poke(k)
            c.shift.expect(shiftExpected)
            c.out.expect(expected)
          }
        }
    }
  }

  it should "encode sequences of UInts into blocks (pipelined)" in {
    val widths = Seq(1, 2, 4, 7, 8)
    val kWidth = 4
    for (blockWidth <- (1 to 4).map(_ * widths.length)) {
      test(new TestableUnsignedExpGolombEncodeBlock(widths, kWidth, blockWidth, Some(true.B)))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
          val kInSeq = for {
            k <- 0 to 8
            a <- Seq(0, 1)
            b <- 0 until 4
            c_ <- (0 until 4).map(_ * 5)
            d <- (0 until 4).map(_ * 42.333).map(Math.round(_).toInt)
            e <- (0 until 4).map(_ * 85)
          } yield k -> Seq(a, b, c_, d, e)
          assert(kInSeq.head._2.length == widths.length)
          val expectedShiftSeq = kInSeq.map {
            case (k, in) => ExpGolombModel.encodeBlock(in, k, blockWidth, signed = false)
          }
          val ioDelay = ExpGolombBlock.encodeDelay(widths, kWidth, blockWidth)

          for (k -> in <- kInSeq.take(ioDelay)) {
            c.in.zip(in).foreach { case (inHw, in) => inHw.poke(in) }
            c.k.poke(k)
            c.clock.step()
          }

          for (k -> in -> (expected -> shift) <- kInSeq.drop(ioDelay).zip(expectedShiftSeq)) {
            c.in.zip(in).foreach { case (inHw, in) => inHw.poke(in) }
            c.k.poke(k)
            c.shift.expect(shift)
            c.out.expect(expected)
            c.clock.step()
          }

          for (expected -> shift <- expectedShiftSeq.takeRight(ioDelay)) {
            c.shift.expect(shift)
            c.out.expect(expected)
            c.clock.step()
          }
        }
    }
  }

  it should "decode sequences of UInts from encoded blocks" in {
    val widths = Seq(1, 2, 4, 7, 8)
    for (blockWidth <- (1 to 4).map(_ * widths.length)) {
      test(new TestableUnsignedExpGolombDecodeBlock(blockWidth, widths.length, 4, 5))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
          for {
            k <- 0 to 8
            a <- Seq(0, 1)
            b <- 0 until 4
            c_ <- (0 until 4).map(_ * 5)
            d <- (0 until 4).map(_ * 42.333).map(Math.round(_).toInt)
            e <- (0 until 4).map(_ * 85)
            in = Seq(a, b, c_, d, e)
          } {
            assert(in.length == widths.length)
            val (encoded, shift) = ExpGolombModel.encodeBlock(in, k, blockWidth, signed = false)
            val expected = ExpGolombModel.decodeBlock(encoded, blockWidth, in.length, k, shift, signed = false)

            c.in.poke(encoded)
            c.k.poke(k)
            c.shift.poke(shift)
            c.out.zip(expected).foreach { case (o, e) => o.expect(e) }
          }
        }
    }
  }

  it should "encode sequences of SInts into blocks" in {
    val widths = Seq(1, 2, 4, 7, 8)
    for (blockWidth <- (2 to 4).map(_ * widths.length)) {
      test(new TestableSignedExpGolombEncodeBlock(widths, 4, blockWidth, None))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
          for {
            k <- 0 to 8
            a <- Seq(-1, 0)
            b <- -2 until 2
            c_ <- (0 until 4).map(_ * 5).map(_ - 8)
            d <- (0 until 4).map(_ * 42.333).map(Math.round(_).toInt).map(_ - 64)
            e <- (0 until 4).map(_ * 85).map(_ - 128)
            in = Seq(a, b, c_, d, e)
          } {
            assert(in.length == widths.length)
            val (expected, shiftExpected) = ExpGolombModel.encodeBlock(in, k, blockWidth, signed = true)

            c.in.zip(in).foreach { case (inHw, in) => inHw.poke(in) }
            c.k.poke(k)
            c.shift.expect(shiftExpected)
            c.out.expect(expected)
          }
        }
    }
  }

  it should "encode sequences of SInts into blocks (pipelined)" in {
    val widths = Seq(1, 2, 4, 7, 8)
    val kWidth = 4
    for (blockWidth <- (2 to 4).map(_ * widths.length)) {
      test(new TestableSignedExpGolombEncodeBlock(widths, kWidth, blockWidth, Some(true.B)))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
          val kInSeq = for {
            k <- 0 to 8
            a <- Seq(-1, 0)
            b <- -2 until 2
            c_ <- (0 until 4).map(_ * 5).map(_ - 8)
            d <- (0 until 4).map(_ * 42.333).map(Math.round(_).toInt).map(_ - 64)
            e <- (0 until 4).map(_ * 85).map(_ - 128)
          } yield k -> Seq(a, b, c_, d, e)
          assert(kInSeq.head._2.length == widths.length)
          val expectedShiftSeq = kInSeq.map {
            case (k, in) => ExpGolombModel.encodeBlock(in, k, blockWidth, signed = true)
          }
          val ioDelay = ExpGolombBlock.encodeDelay(widths, kWidth, blockWidth)

          for (k -> in <- kInSeq.take(ioDelay)) {
            c.in.zip(in).foreach { case (inHw, in) => inHw.poke(in) }
            c.k.poke(k)
            c.clock.step()
          }

          for (k -> in -> (expected -> shift) <- kInSeq.drop(ioDelay).zip(expectedShiftSeq)) {
            c.in.zip(in).foreach { case (inHw, in) => inHw.poke(in) }
            c.k.poke(k)
            c.shift.expect(shift)
            c.out.expect(expected)
            c.clock.step()
          }

          for (expected -> shift <- expectedShiftSeq.takeRight(ioDelay)) {
            c.shift.expect(shift)
            c.out.expect(expected)
            c.clock.step()
          }
        }
    }
  }

  it should "decode sequences of SInts from encoded blocks" in {
    val widths = Seq(1, 2, 4, 7, 8)
    for (blockWidth <- (2 to 4).map(_ * widths.length)) {
      test(new TestableSignedExpGolombDecodeBlock(blockWidth, widths.length, 4, 5))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
          val masks = c.out.map(p => (1 << p.peek().getWidth) - 1)
          for {
            k <- 0 to 8
            a <- Seq(-1, 0)
            b <- -2 until 2
            c_ <- (0 until 4).map(_ * 5).map(_ - 8)
            d <- (0 until 4).map(_ * 42.333).map(Math.round(_).toInt).map(_ - 64)
            e <- (0 until 4).map(_ * 85).map(_ - 128)
            in = Seq(a, b, c_, d, e)
          } {
            assert(in.length == widths.length)
            val (encoded, shift) = ExpGolombModel.encodeBlock(in, k, blockWidth, signed = true)
            val expected = ExpGolombModel.decodeBlock(encoded, blockWidth, in.length, k, shift, signed = true)

            c.in.poke(encoded)
            c.k.poke(k)
            c.shift.poke(shift)
            c.out.zip(expected).zip(masks).foreach { case o -> e -> m => assertResult(e & m)(o.peek().litValue & m) }
          }
        }
    }
  }

  "UnsignedExpGolombBlockEncoder" should "encode sequences of UInts into blocks" in {
    val n = 5
    val width = 8
    val kWidth = 5
    val shiftWidth = 5
    val maxShift = (1 << shiftWidth).min(width) - 1
    for {
      encodedWidth <- ((width + 1 - maxShift).max(1) to width).map(_ * n)
      totalWidth = encodedWidth + kWidth + shiftWidth
    } {
      test(new UnsignedExpGolombBlockEncoder(n, width, totalWidth, kWidth, shiftWidth))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
          val inSeq = for {
            a <- Seq(0, 1)
            b <- 0 until 4
            c_ <- (0 until 4).map(_ * 5)
            d <- (0 until 4).map(_ * 42.333).map(Math.round(_).toInt)
            e <- (0 until 4).map(_ * 85)
          } yield Seq(a, b, c_, d, e)
          assert(inSeq.head.length == n)
          val expectedSeq =
            inSeq.map(ExpGolombModel.encodeTotalBlock(_, totalWidth, kWidth, shiftWidth, signed = false))
          val ioDelay = c.ioDelay

          c.in.valid.poke(true)
          c.out.ready.poke(true)

          for (in <- inSeq.take(ioDelay)) {
            c.in.bits.zip(in).foreach { case (inHw, in) => inHw.poke(in) }
            c.clock.step()
          }

          def tf(enc: BigInt): (Int, Int, BigInt) = totalFields(enc, totalWidth, kWidth, shiftWidth)

          for (in -> expected <- inSeq.drop(ioDelay).zip(expectedSeq)) {
            c.in.bits.zip(in).foreach { case (inHw, in) => inHw.poke(in) }
            assertResult(tf(expected))(tf(c.out.bits.peek().litValue))
            c.clock.step()
          }

          for (expected <- expectedSeq.takeRight(ioDelay)) {
            assertResult(tf(expected))(tf(c.out.bits.peek().litValue))
            c.clock.step()
          }
        }
    }
  }

  "SignedExpGolombBlockEncoder" should "encode sequences of SInts into blocks" in {
    val n = 5
    val width = 8
    val kWidth = 5
    val shiftWidth = 6
    val maxShift = (1 << shiftWidth).min(width) - 1
    for {
      encodedWidth <- ((width + 1 - maxShift).max(2) to width).map(_ * n)
      totalWidth = encodedWidth + kWidth + shiftWidth
    } {
      test(new SignedExpGolombBlockEncoder(n, width, totalWidth, kWidth, shiftWidth))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
          val inSeq = for {
            a <- Seq(-1, 0)
            b <- -2 until 2
            c_ <- (0 until 4).map(_ * 5).map(_ - 8)
            d <- (0 until 4).map(_ * 42.333).map(Math.round(_).toInt).map(_ - 64)
            e <- (0 until 4).map(_ * 85).map(_ - 128)
          } yield Seq(a, b, c_, d, e)
          assert(inSeq.head.length == n)
          val expectedSeq =
            inSeq.map(ExpGolombModel.encodeTotalBlock(_, totalWidth, kWidth, shiftWidth, signed = true))
          val ioDelay = c.ioDelay

          c.in.valid.poke(true)
          c.out.ready.poke(true)

          for (in <- inSeq.take(ioDelay)) {
            c.in.bits.zip(in).foreach { case (inHw, in) => inHw.poke(in) }
            c.clock.step()
          }

          def tf(enc: BigInt): (Int, Int, BigInt) = totalFields(enc, totalWidth, kWidth, shiftWidth)

          for (in -> expected <- inSeq.drop(ioDelay).zip(expectedSeq)) {
            c.in.bits.zip(in).foreach { case (inHw, in) => inHw.poke(in) }
            assertResult(tf(expected))(tf(c.out.bits.peek().litValue))
            c.clock.step()
          }

          for (expected <- expectedSeq.takeRight(ioDelay)) {
            assertResult(tf(expected))(tf(c.out.bits.peek().litValue))
            c.clock.step()
          }
        }
    }
  }

  "UnsignedExpGolombBlockDecoder" should "decode sequences of UInts from encoded blocks" in {
    val n = 5
    val width = 8
    val kWidth = 5
    val shiftWidth = 5
    val maxShift = (1 << shiftWidth).min(width) - 1
    for {
      encodedWidth <- ((width + 1 - maxShift).max(2) to width).map(_ * n)
      totalWidth = encodedWidth + kWidth + shiftWidth
    } {
      test(new UnsignedExpGolombBlockDecoder(n, width, totalWidth, kWidth, shiftWidth))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
          val inSeq = for {
            a <- Seq(0, 1)
            b <- 0 until 4
            c_ <- (0 until 4).map(_ * 5)
            d <- (0 until 4).map(_ * 42.333).map(Math.round(_).toInt)
            e <- (0 until 4).map(_ * 85)
          } yield Seq(a, b, c_, d, e)
          assert(inSeq.head.length == n)
          val encodedSeq = inSeq.map(ExpGolombModel.encodeTotalBlock(_, totalWidth, kWidth, shiftWidth, signed = false))
          val expectedSeq = encodedSeq.map(
            ExpGolombModel
              .decodeTotalBlock(_, totalWidth, n, kWidth, shiftWidth, signed = false)
              .map(_ & ((1 << width) - 1))
          )
          val ioDelay = c.ioDelay

          c.in.valid.poke(true)
          c.out.ready.poke(true)

          for (encoded <- encodedSeq.take(ioDelay)) {
            c.in.bits.poke(encoded)
            c.clock.step()
          }

          for (encoded -> expected <- encodedSeq.drop(ioDelay).zip(expectedSeq)) {
            c.in.bits.poke(encoded)
            c.out.bits.zip(expected).foreach { case (o, e) => o.expect(e) }
            c.clock.step()
          }

          for (expected <- expectedSeq.takeRight(ioDelay)) {
            c.out.bits.zip(expected).foreach { case (o, e) => o.expect(e) }
            c.clock.step()
          }
        }
    }
  }

  "SignedExpGolombBlockDecoder" should "decode sequences of SInts from encoded blocks" in {
    val n = 5
    val width = 8
    val kWidth = 5
    val shiftWidth = 6
    val maxShift = (1 << shiftWidth).min(width) - 1
    for {
      encodedWidth <- ((width + 1 - maxShift).max(2) to width).map(_ * n)
      totalWidth = encodedWidth + kWidth + shiftWidth
    } {
      test(new SignedExpGolombBlockDecoder(n, width, totalWidth, kWidth, shiftWidth))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
          val mask = (1 << width) - 1
          val inSeq = for {
            a <- Seq(-1, 0)
            b <- -2 until 2
            c_ <- (0 until 4).map(_ * 5).map(_ - 8)
            d <- (0 until 4).map(_ * 42.333).map(Math.round(_).toInt).map(_ - 64)
            e <- (0 until 4).map(_ * 85).map(_ - 128)
          } yield Seq(a, b, c_, d, e)
          assert(inSeq.head.length == n)
          val encodedSeq = inSeq.map(ExpGolombModel.encodeTotalBlock(_, totalWidth, kWidth, shiftWidth, signed = true))
          val expectedSeq =
            encodedSeq.map(ExpGolombModel.decodeTotalBlock(_, totalWidth, n, kWidth, shiftWidth, signed = true))
          val ioDelay = c.ioDelay

          c.in.valid.poke(true)
          c.out.ready.poke(true)

          for (encoded <- encodedSeq.take(ioDelay)) {
            c.in.bits.poke(encoded)
            c.clock.step()
          }

          for (encoded -> expected <- encodedSeq.drop(ioDelay).zip(expectedSeq)) {
            c.in.bits.poke(encoded)
            c.out.bits.zip(expected).foreach { case (o, e) => assertResult(e & mask)(o.peek().litValue & mask) }
            c.clock.step()
          }

          for (expected <- expectedSeq.takeRight(ioDelay)) {
            c.out.bits.zip(expected).foreach { case (o, e) => assertResult(e & mask)(o.peek().litValue & mask) }
            c.clock.step()
          }
        }
    }
  }
}
