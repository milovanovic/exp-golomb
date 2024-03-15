package expgolomb

import chisel3._
import chiseltest._
import expgolomb.ModelUtils._
import org.scalatest.flatspec.AnyFlatSpec

class TestableExpGolombEncodeSingle(width: Int, kWidth: Int) extends Module {
  val in = IO(Input(UInt(width.W)))
  val k = IO(Input(UInt(kWidth.W)))
  val out = IO(Output(UInt()))
  val high = IO(Output(UInt()))

  val (resOut, resHigh) = ExpGolombSingle.encode(in, k)
  out := resOut
  high := resHigh
}

class TestableExpGolombDecodeSingle(width: Int, kWidth: Int) extends Module {
  val in = IO(Input(UInt(width.W)))
  val k = IO(Input(UInt(kWidth.W)))
  val out = IO(Output(UInt()))

  out := ExpGolombSingle.decode(in, k)
}

class TestableExpGolombEncodeBlock(widths: Seq[Int], kWidth: Int, blockWidth: Int, useRegEnable: Option[Bool])
    extends Module {
  val in = widths.map(w => IO(Input(UInt(w.W))))
  val k = IO(Input(UInt(kWidth.W)))
  val out = IO(Output(UInt()))
  val shift = IO(Output(UInt()))

  val (resOut, resShift) = ExpGolombBlock.encode(in, k, blockWidth, useRegEnable)
  out := resOut
  shift := resShift
}

class TestableExpGolombDecodeBlock(blockWidth: Int, n: Int, kWidth: Int, shiftWidth: Int) extends Module {
  val in = IO(Input(UInt(blockWidth.W)))
  val k = IO(Input(UInt(kWidth.W)))
  val shift = IO(Input(UInt(shiftWidth.W)))
  val out = Seq.fill(n)(IO(Output(UInt())))

  out.zip(ExpGolombBlock.decode(in, n, k, shift)).foreach { case (o, d) => o := d }
}

class ExpGolombSpec extends AnyFlatSpec with ChiselScalatestTester {
  "ExpGolombSingle" should "encode UInts" in test(new TestableExpGolombEncodeSingle(10, 5)) { c =>
    for {
      k <- 0 to 16
      in <- 0 to 1023
    } {
      c.in.poke(in)
      c.k.poke(k)
      val (expected, expectedHigh) = ExpGolombModel.encodeSingle(in, k)
      c.out.expect(expected)
      c.high.expect(expectedHigh)
    }
  }

  it should "decode UInts" in test(new TestableExpGolombDecodeSingle(17, 5)) { c =>
    for {
      k <- 0 to 16
      in <- 0 to 1023
    } {
      val (encoded, _) = ExpGolombModel.encodeSingle(in, k)
      c.in.poke(encoded)
      c.k.poke(k)
      c.out.expect(ExpGolombModel.decodeSingle(encoded, k))
    }
  }

  "ExpGolombBlock" should "encode sequences of UInts into blocks" in {
    val widths = Seq(1, 2, 4, 7, 8)
    for (blockWidth <- (1 to 4).map(_ * widths.length)) {
      test(new TestableExpGolombEncodeBlock(widths, 4, blockWidth, None))
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
            val (expected, shiftExpected) = ExpGolombModel.encodeBlock(in, k, blockWidth)

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
      test(new TestableExpGolombEncodeBlock(widths, kWidth, blockWidth, Some(true.B)))
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
            case (k, in) => ExpGolombModel.encodeBlock(in, k, blockWidth)
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
      test(new TestableExpGolombDecodeBlock(blockWidth, widths.length, 4, 5))
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
            val (encoded, shift) = ExpGolombModel.encodeBlock(in, k, blockWidth)
            val expected = ExpGolombModel.decodeBlock(encoded, blockWidth, in.length, k, shift)

            c.in.poke(encoded)
            c.k.poke(k)
            c.shift.poke(shift)
            c.out.zip(expected).foreach { case (o, e) => o.expect(e) }
          }
        }
    }
  }

  "ExpGolombBlockEncoder" should "encode sequences of UInts into blocks" in {
    val n = 5
    val width = 8
    val kWidth = 5
    val shiftWidth = 5
    val maxShift = (1 << shiftWidth).min(width) - 1
    for {
      encodedWidth <- ((width + 1 - maxShift).max(1) to width).map(_ * n)
      totalWidth = encodedWidth + kWidth + shiftWidth
    } {
      test(new ExpGolombBlockEncoder(n, width, totalWidth, kWidth, shiftWidth))
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
          val inSeq = for {
            a <- Seq(0, 1)
            b <- 0 until 4
            c_ <- (0 until 4).map(_ * 5)
            d <- (0 until 4).map(_ * 42.333).map(Math.round(_).toInt)
            e <- (0 until 4).map(_ * 85)
          } yield Seq(a, b, c_, d, e)
          assert(inSeq.head.length == n)
          val expectedSeq = inSeq.map {
            ExpGolombModel.encodeTotalBlock(_, totalWidth, kWidth, shiftWidth)
          }
          val ioDelay = c.ioDelay

          c.in.valid.poke(true.B)
          c.out.ready.poke(true.B)

          for (in <- inSeq.take(ioDelay)) {
            c.in.bits.zip(in).foreach { case (inHw, in) => inHw.poke(in) }
            c.clock.step()
          }
          println()

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
}
