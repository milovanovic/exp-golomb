package expgolomb
import chisel3._
import chisel3.util._
import chiseltest._
import ModelUtils._
import org.scalatest.flatspec.AnyFlatSpec

class TestableExpression[T <: Data, U <: Data](tIns: Seq[T], tOutsOption: Option[Seq[U]])(exp: Seq[T] => Seq[U])
    extends Module {
  val ins = tIns.map(t => IO(Input(t)))
  val res = exp(ins)
  val tOuts = tOutsOption.getOrElse(res.map(chiselTypeOf(_)))
  require(tOuts.length == res.length)
  val outs = tOuts.map(t => IO(Output(t)))
  outs.zip(res).foreach { case (o, r) => o := r }
}

object TestableExpression {
  def apply[T <: Data, U <: Data](tIn: T, tOutOption: Option[U] = None)(exp: T => U) = {
    new TestableExpression[T, U](Seq(tIn), tOutOption.map(Seq(_)))(in => Seq(exp(in.head))) {
      val in = ins.head
      val out = outs.head
    }
  }

  def apply[T <: Data, U <: Data](tIns: Seq[T], tOutsOption: Option[Seq[U]])(exp: Seq[T] => Seq[U]) = {
    new TestableExpression[T, U](tIns, tOutsOption)(exp)
  }
}

class TestableDynamicHighCat[T <: Bits](tIns: Seq[Bits], tHighs: Seq[UInt]) extends Module {
  require(tIns.length == tHighs.length)
  val ins = tIns.map(t => IO(Input(t)))
  val highs = tHighs.map(t => IO(Input(t)))
  val out = IO(Output(UInt()))
  val high = IO(Output(UInt()))

  val (resOut, resHigh) = DynamicHighCat(ins.zip(highs))
  out := resOut
  high := resHigh
}

class Log2CatModule[T <: Bits](tIns: Seq[Bits]) extends Module {
  val ins = tIns.map(t => IO(Input(t)))
  val out = IO(Output(UInt()))
  val high = IO(Output(UInt()))

  val (resOut, resHigh) = DynamicHighCat(ins.map { in => (in, Log2(in)) })
  out := resOut
  high := resHigh
}

class TestablePackDropLSBs[T <: Bits](tIns: Seq[Bits], tHighs: Seq[UInt], blockWidth: Int, useRegEnable: Option[Bool])
    extends Module {
  require(tIns.length == tHighs.length)
  val ins = tIns.map(t => IO(Input(t)))
  val inHighs = tHighs.map(t => IO(Input(t)))
  val outs = Seq.fill(tIns.length)(IO(Output(UInt())))
  val outHighs = Seq.fill(tHighs.length)(IO(Output(UInt())))
  val outShift = IO(Output(UInt()))

  val (res, resShift) = PackDropLSBs(ins.zip(inHighs), blockWidth, useRegEnable)
  val (resOuts, resHighs) = res.unzip
  outs.zip(resOuts).foreach { case (out, resOut) => out := resOut }
  outHighs.zip(resHighs).foreach { case (outHigh, resHigh) => outHigh := resHigh }
  outShift := resShift
}

class UtilsSpec extends AnyFlatSpec with ChiselScalatestTester {
  "BitWidth" should "calculate significant bit width" in test(TestableExpression(UInt(16.W)) {
    BitWidth(_)
  }) { c =>
    for (i <- 0 until (1 << 16)) {
      c.in.poke(i.U)
      c.out.expect(BigInt(i).bitLength)
    }
  }

  "ApplyHighMask" should "apply proper mask based on the position of highest set bit in the mask" in test(
    TestableExpression(
      Seq(UInt(16.W), UInt(5.W)),
      Some(Seq(UInt(16.W)))
    ) { case Seq(in, h) => Seq(ApplyHighMask(in, h)) }
  ) { c =>
    assertResult(c.ins.head.peek().getWidth, "in/out width mismatch")(c.outs.head.peek().getWidth)
    for {
      i <- (0 to 17) ++ (5 to 15).map(1 << _).flatMap(n => n - 1 to n + 1) ++ Seq(65534, 65535)
      h <- 0 until (1 << 5)
    } {
      val mask = BigInt("1" * (h + 1), 2)
      c.ins.zip(Seq(i, h)).foreach { case (inHw, in) => inHw.poke(in.U) }
      c.outs.head.expect(
        i & mask,
        s"0b${i.toBinaryString} & 0b${mask.toString(2)} == 0b${c.outs.head.peek().litValue.toString(2)} " +
          s"(not 0b${(i & mask).toString(2)})"
      )
    }
  }

  "DynamicHighCat" should "concatenate signals, each up to a respective dynamic highest bit" in test(
    new TestableDynamicHighCat[UInt](
      (1 to 16).map { i => UInt(i.W) },
      (1 to 16).map(log2Ceil(_)).map { l => UInt(l.W) }
    )
  ).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
    for {
      a <- 0 to 1
      b <- 0 to 3
      c_ <- Seq(5, 7)
      d <- Seq(10, 15)
      ins = Seq(a, b, c_, d) ++ (5 to 16).map(i => (1 << i) - 1 - b)
      highs <- Seq(0, 0.25, 0.5, 0.67, 1).map { s => (0 until 16).map(_ * s).map(Math.round).map(_.toInt) }
    } {
      c.ins.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
      c.highs.zip(highs).foreach { case (highHw, high) => highHw.poke(high) }

      val expectedStr = ins.zip(highs).map { case (n, h) => highMaskBitString(n, h) }.mkString
      val expectedHigh = highs.sum + highs.length - 1

      c.out.expect(BigInt(expectedStr, 2))
      c.high.expect(expectedHigh)
    }
  }

  it should "concatenate the significant bits of each signal when high bit position is Log2" in test(
    new Log2CatModule[UInt](Seq.fill(6)(UInt(8.W)))
  ).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
    val values = Seq(0, 1, 2, 4, 15, 32, 64, 128, 255)
    val valuesA = values.grouped(3).map(_(0))
    val valuesB = values.grouped(3).map(_(1))
    val valuesC = values.grouped(3).map(_(2))
    for {
      a <- valuesA
      b <- valuesB
      c_ <- valuesC
      d <- valuesA
      e <- valuesB
      f <- valuesC
      ins = Seq(a, b, c_, d, e, f)
    } {
      c.ins.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }

      val expectedStr = ins.map(_.toBinaryString).mkString
      val expectedHigh = ins.map(BigInt(_).bitLength.max(1)).sum - 1

      c.out.expect(BigInt(expectedStr, 2))
      c.high.expect(expectedHigh)
    }
  }

  behavior.of("DivideByConst")
  for {
    (rounded, perform) <- Seq(
      false -> "perform integer division with a constant divisor",
      true -> "perform rounded integer division with a constant divisor"
    )
  } it should perform in {
    for {
      divisor <- 1 to 5
      maxResult <- 0 to 5
    } test(TestableExpression(UInt(5.W)) {
      DivideByConst(divisor, maxResult, rounded)(_)
    }) { c =>
      val capTrunc = divisor * (maxResult + 1) - 1
      val cap = if (rounded) capTrunc - divisor / 2 else capTrunc
      for (in <- 0 to cap.min(31)) {
        val expected =
          if (rounded) Math.round(in.toFloat / divisor)
          else in / divisor
        c.in.poke(in.U)
        c.out.expect(expected, s"in=$in; divisor=$divisor; maxResult=$maxResult")
      }
    }
  }

  "SumExtended" should "calculate sum of max values with increasing widths" in test(
    TestableExpression(
      (1 to 16).map { i => UInt(i.W) },
      None
    ) { ins => Seq(SumExtended(ins, None)) }
  ) { c =>
    val ins = (1 to 16).map(i => (1 << i) - 1)
    c.ins.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
    c.outs.head.expect(ins.sum)
  }

  it should "be pipeline-able with the correct io delay" in test(
    TestableExpression(
      (1 to 16).map { i => UInt(i.W) },
      None
    ) { ins => Seq(SumExtended(ins, Some(true.B))) }
  ) { c =>
    val ioDelay = SumExtended.delay(c.ins.length)
    val insIns = Seq.tabulate(ioDelay) { i => (1 to 16).map(j => ((1 << j) - 1 - i).max(0)) }
    val outs = insIns.map(_.sum)

    for (ins <- insIns) {
      c.ins.zip(ins).foreach { case (inHw, in) => inHw.poke(in) }
      c.clock.step(1)
    }

    for (ins -> expected <- insIns.reverse.zip(outs)) {
      c.ins.zip(ins).foreach { case (inHw, in) => inHw.poke(in) }
      c.outs.head.expect(expected)
      c.clock.step(1)
    }

    for (out <- outs.reverse) {
      c.outs.head.expect(out)
      c.clock.step(1)
    }
  }

  "MeanBitWidth" should "calculate the mean significant bit-width of input samples" in test(
    TestableExpression(Vec(5, UInt(8.W))) {
      MeanBitWidth(_, None, None)
    }
  ).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
    for {
      i <- Seq(0, 1, 2, 4, 8, 16, 32, 64, 128)
      j <- Seq(0, 1, 3, 5, 9, 17, 33, 65, 129)
      k <- Seq(0, 1, 3, 6, 10, 18, 34, 66, 130)
      l <- Seq(0, 1, 3, 7, 11, 19, 35, 67, 131)
      m <- Seq(0, 1, 3, 7, 12, 20, 36, 68, 132)
      ins = Seq(i, j, k, l, m)
    } {
      val expected = meanBitWidth(ins)
      c.in.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
      c.out.expect(expected)
    }
  }

  it should "calculate the mean significant bit-width of input samples, with a predetermined maximum result" in {
    for (maxResult <- 0 to 8) {
      test(
        TestableExpression(Vec(5, UInt(8.W))) {
          MeanBitWidth(_, None, Some(maxResult))
        }
      ).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
        for {
          i <- Seq(0, 1, 2, 4, 8, 16, 32, 64, 128)
          j <- Seq(0, 1, 3, 5, 9, 17, 33, 65, 129)
          k <- Seq(0, 1, 3, 6, 10, 18, 34, 66, 130)
          l <- Seq(0, 1, 3, 7, 11, 19, 35, 67, 131)
          m <- Seq(0, 1, 3, 7, 12, 20, 36, 68, 132)
          ins = Seq(i, j, k, l, m)
        } {
          val expected = meanBitWidth(ins).min(maxResult)
          c.in.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
          c.out.expect(expected)
        }
      }
    }
  }

  it should "be pipeline-able with the correct io delay" in test(TestableExpression(Vec(5, UInt(8.W))) {
    MeanBitWidth(_, Some(true.B), None)
  }).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
    val insIns = for {
      i <- Seq(0, 1, 2)
      j <- Seq(5, 9, 17)
      k <- Seq(34, 66, 130)
      l <- Seq(7, 19, 67)
      m <- Seq(0, 12, 132)
    } yield Seq(i, j, k, l, m)
    val outs = insIns.map(_.map(BigInt(_).bitLength).sum.toFloat / 5).map(Math.round)
    val ioDelay = MeanBitWidth.delay(5)

    for (ins <- insIns.take(ioDelay)) {
      c.in.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
      c.clock.step()
    }

    for (ins -> expected <- insIns.drop(ioDelay).zip(outs)) {
      c.in.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
      c.out.expect(expected)
      c.clock.step()
    }

    for (expected <- outs.takeRight(ioDelay)) {
      c.out.expect(expected)
      c.clock.step()
    }
  }

  it should "be pipeline-able with the correct io delay, with a predetermined maximum result" in {
    for (maxResult <- 0 to 8) {
      test(TestableExpression(Vec(5, UInt(8.W))) {
        MeanBitWidth(_, Some(true.B), Some(maxResult))
      }).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
        val insIns = for {
          i <- Seq(0, 1, 2)
          j <- Seq(5, 9, 17)
          k <- Seq(34, 66, 130)
          l <- Seq(7, 19, 67)
          m <- Seq(0, 12, 132)
        } yield Seq(i, j, k, l, m)
        val outs = insIns.map(_.map(BigInt(_).bitLength).sum.toFloat / 5).map(Math.round(_).min(maxResult))
        val ioDelay = MeanBitWidth.delay(5)

        for (ins <- insIns.take(ioDelay)) {
          c.in.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
          c.clock.step()
        }

        for (ins -> expected <- insIns.drop(ioDelay).zip(outs)) {
          c.in.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
          c.out.expect(expected)
          c.clock.step()
        }

        for (expected <- outs.takeRight(ioDelay)) {
          c.out.expect(expected)
          c.clock.step()
        }
      }
    }
  }

  "PackDropLSBs" should
    "shift each signal until their dynamic widths can fit within a requested block size" in {
    val ins = Seq(13, 67, 150, 188, 52, 108, 110, 164)

    for (blockWidth <- (8 to 1 by -1).map(_ * ins.length))
      test(
        new TestablePackDropLSBs[UInt](
          Seq.fill(ins.length)(UInt(8.W)),
          Seq.fill(ins.length)(UInt(3.W)),
          blockWidth,
          None
        )
      ).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
        for {
          a <- Seq(0, 3)
          b <- Seq(1, 4)
          c_ <- Seq(2, 5)
          d <- Seq(3, 6)
          e <- Seq(4, 7)
          f <- Seq(0, 2)
          g <- Seq(3, 5)
          h <- Seq(6, 7)
          highs = Seq(a, b, c_, d, e, f, g, h)
        } {
          assert(ins.length == highs.length)
          val (expectedHighs, expectedShift) = shiftHighs(highs, blockWidth, signed = false)
          val expected = ins.map(_ >> expectedShift)

          c.ins.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
          c.inHighs.zip(highs).foreach { case (highHw, high) => highHw.poke(high) }
          c.outShift.expect(expectedShift)
          c.outHighs.zip(expectedHighs).foreach { case (highOut, highExp) => highOut.expect(highExp) }
          c.outs.zip(expected).foreach { case (out, exp) => out.expect(exp) }
        }
      }
  }

  it should "be pipeline-able with the correct io delay" in {
    val ins = Seq(13, 67, 150, 188, 52, 108, 110, 164)

    for (blockWidth <- (8 to 1 by -1).map(_ * ins.length)) {
      test(
        new TestablePackDropLSBs[UInt](
          Seq.fill(ins.length)(UInt(8.W)),
          Seq.fill(ins.length)(UInt(3.W)),
          blockWidth,
          Some(true.B)
        )
      ).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
        val highsSeq = for {
          a <- Seq(0, 3)
          b <- Seq(1, 4)
          c_ <- Seq(2, 5)
          d <- Seq(3, 6)
          e <- Seq(4, 7)
          f <- Seq(0, 2)
          g <- Seq(3, 5)
          h <- Seq(6, 7)
        } yield Seq(a, b, c_, d, e, f, g, h)
        assert(ins.length == highsSeq.head.length)
        val ioDelay = PackDropLSBs.delay(Seq.fill(ins.length)(8), blockWidth)

        val (expectedHighsSeq, expectedShiftSeq) = highsSeq.map(shiftHighs(_, blockWidth, signed = false)).unzip
        val expectedSeq = expectedShiftSeq.map(s => ins.map(_ >> s))

        c.ins.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }

        for (highs <- highsSeq.take(ioDelay)) {
          c.inHighs.zip(highs).foreach { case (highHw, high) => highHw.poke(high) }
          c.clock.step()
        }

        for (
          highs -> expectedShift -> expectedHighs -> expected <-
            highsSeq.drop(ioDelay).zip(expectedShiftSeq).zip(expectedHighsSeq).zip(expectedSeq)
        ) {
          c.inHighs.zip(highs).foreach { case (highHw, high) => highHw.poke(high) }
          c.outShift.expect(expectedShift)
          c.outHighs.zip(expectedHighs).foreach { case (highOut, highExp) => highOut.expect(highExp) }
          c.outs.zip(expected).foreach { case (out, exp) => out.expect(exp) }
          c.clock.step()
        }

        for (
          expectedShift -> expectedHighs -> expected <-
            expectedShiftSeq.zip(expectedHighsSeq).zip(expectedSeq).takeRight(ioDelay)
        ) {
          c.outShift.expect(expectedShift)
          c.outHighs.zip(expectedHighs).foreach { case (highOut, highExp) => highOut.expect(highExp) }
          c.outs.zip(expected).foreach { case (out, exp) => out.expect(exp) }
          c.clock.step()
        }
      }
    }
  }
}
