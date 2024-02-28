package expgolomb

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/**
  * Concatenates multiple signals by their significant bits.
  */
object Log2Cat {
  def apply(in: Seq[Bits]): UInt = {
    val (cat, _) = DynamicHighCat(in.map { el => (el, Log2(el)) })
    cat
  }
}

/**
  * Encodes or decodes a sequence of integers.
  */
object ExpGolombMulti {
  def encodeSeq(ins: Seq[UInt], kOption: Option[UInt] = None): (Seq[(UInt, UInt)], UInt) = {
    require(ins.forall(_.widthKnown), "Input data width must be known")
    val k = kOption.getOrElse(MeanHighBit(ins) +& 1.U)
    (ins.map(ExpGolombSingle.encode(_, k)), k)
  }

  def encode(ins: Seq[UInt], kOption: Option[UInt] = None): (UInt, UInt, UInt) = {
    require(ins.forall(_.widthKnown), "Input data width must be known")
    val inputWidth = ins.map(_.getWidth).sum
    val maxWidth = if (kOption.isEmpty) inputWidth + ins.length else 2 * inputWidth

    val (encodedSeq, k) = encodeSeq(ins, kOption)
    val (out, outHigh) = DynamicHighCat(encodedSeq)
    //    printf(encodedSeq.map { case (v, h) => cf"$v%b[$h:0]" }.reduce(_ + _) + "\n")
    //    printf(cf"out:    ${WireDefault(UInt(maxWidth.W), out)}%b[$outHigh:0] (ExpGolombMulti)\n")
    (WireDefault(UInt(maxWidth.W), out), outHigh, k)
  }

  def decode(in: UInt, high: UInt, k: UInt, n: Int): Seq[UInt] = {
    if (n > 0) {
      val sig = ApplyHighMask(in, high)
      val firstSet = Log2(sig)
      val prefixLength = high - firstSet
      val sigLengthMinusOne = prefixLength + prefixLength + k
      val low = high - sigLengthMinusOne
      val decoded = ExpGolombSingle.decode(sig >> low, sigLengthMinusOne, k)
      decoded +: decode(in, low - 1.U, k, n - 1)
    } else Seq()
  }
}

class TestableExpression[T <: Data, U <: Data](tIn: T, tOutOption: Option[U] = None)(exp: T => U) extends Module {
  val in = IO(Input(tIn))
  val out = IO(Output(tOutOption.getOrElse(tIn)))
  out := exp(in)
}

class DynamicHighCatModule(n: Int) extends Module {
  val ins = IO(Input(Vec(n, UInt(8.W))))
  val inHighs = IO(Input(Vec(n, UInt(3.W))))
  val out = IO(Output(UInt((n * 8).W)))
  val outHigh = IO(Output(UInt(log2Up(n * 8).W)))

  val res = DynamicHighCat(ins.zip(inHighs))
  out := res._1
  outHigh := res._2
}

class Log2CatModule(n: Int, w: Int)
    extends TestableExpression(
      Vec(n, UInt(w.W)),
      tOutOption = Some(UInt((n * w).W))
    )(Log2Cat(_))

class ExpGolombEncodeSingleModule(width: Int) extends Module {
  val in = IO(Input(UInt(width.W)))
  val k = IO(Input(UInt(log2Up(width).W)))
  val out = IO(Output(UInt((2 * width).W)))
  val outHigh = IO(Output(UInt(log2Up(2 * width).W)))

  val res = ExpGolombSingle.encode(in, k)
  out := res._1
  outHigh := res._2
}

class ExpGolombDecodeSingleModule(width: Int) extends Module {
  val in = IO(Input(UInt(width.W)))
  val high = IO(Input(UInt(log2Up(width).W)))
  val k = IO(Input(UInt(log2Up(width).W)))
  val out = IO(Output(UInt(width.W)))

  out := ExpGolombSingle.decode(in, high, k)
}

class ExpGolombEncodeMultiModule(n: Int, width: Int) extends Module {
  val ins = IO(Input(Vec(n, UInt(width.W))))
  val out = IO(Output(UInt(((n + 1) * width).W)))
  val outHigh = IO(Output(UInt(log2Up((n + 1) * width).W)))
  val outK = IO(Output(UInt(log2Up(width + 1).W)))

  val res = ExpGolombMulti.encode(ins)
  out := res._1
  outHigh := res._2
  outK := res._3
//  printf(cf"out:    $out%b[$outHigh:0] (module)\n")
//  printf(cf"out:    $out[$outHigh:0] (module)\n")
}

class ExpGolombDecodeMultiModule(n: Int, width: Int) extends Module {
  val in = IO(Input(UInt(((n + 1) * width).W)))
  val high = IO(Input(UInt(log2Up((n + 1) * width).W)))
  val k = IO(Input(UInt(log2Up(width + 1).W)))
  val outs = IO(Output(Vec(n, UInt(width.W))))

  outs := ExpGolombMulti.decode(in, high, k, n)
}

class PackDropLSBsModule(n: Int, width: Int, outWidth: Int) extends Module {
  val ins = IO(Input(Vec(n, UInt(width.W))))
  val inHighs = IO(Input(Vec(n, UInt(log2Up(width).W))))
  val outs = IO(Output(Vec(n, UInt(width.W))))
  val outHighs = IO(Output(Vec(n, UInt(log2Up(width).W))))
  val outShift = IO(Output(UInt(log2Up(width).W)))

  val (resRow, resShift) = PackDropLSBs(ins.zip(inHighs), outWidth)
  val (resOuts, resHighs) = resRow.unzip
  outs := VecInit(resOuts)
  outHighs := VecInit(resHighs)
  outShift := resShift
}

object Golomb {
  def encode(n: Int, k: Int): String = {
    require(n >= 0)
    require(k >= 0)
    val encodedBinary = (n + (1 << k)).toBinaryString
    val encodedUnary = "0" * (encodedBinary.length - k - 1)
    encodedUnary + encodedBinary
  }
}

object StrBits {
  def apply(num: BigInt, high: BigInt): String = {
    val len = high + 1
    val binStr = num.toString(2)
    val outPadLen = len - binStr.length
    s"${"0" * outPadLen.toInt}$binStr"
  }

  def apply[T <: Data](data: T, high: UInt): String = {
    apply(data.peek().litValue, high.peek().litValue)
  }

  def apply[T <: Data](data: T): String = {
    data.peek().litValue.toString(2)
  }
}

class Playground extends AnyFlatSpec with ChiselScalatestTester {
//  it should "Log2" in test(new TestableExpression(UInt(8.W))(Log2(_: UInt))) { c =>
//    for (i <- 0 to 64) {
//      c.in.poke(i.U)
//      val res = c.out.peek().litValue.toInt
//      println(s"${i.toBinaryString} <> ${(1 << res).toBinaryString} <--> : Log2($i) == $res")
//    }
//  }

//  it should "DynamicHighCat" in {
//    val ins = Seq((2, 3), (15, 1), (14, 3), (10, 1))
//
//    test(new DynamicHighCatModule(ins.length)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
//      ins.zipWithIndex.foreach {
//        case ((v, h), i) =>
//          c.ins(i).poke(v.U(8.W))
//          c.inHighs(i).poke(h.U(3.W))
//      }
//
//      println(
//        s"Concat: ${ins.map { case (v, h) => s"${v.toBinaryString}[$h:0]" }.mkString(" ")}; " +
//          s"Result: <${c.out.peek().getWidth}>${StrBits(c.out, c.outHigh)}[${c.outHigh.peek()}:0]"
//      )
//    }
//  }

//  it should "Log2Cat" in {
//    val ins = Seq(2, 15, 14, 10)
//
//    test(new Log2CatModule(ins.length, 8)) { c =>
//      c.in.zip(ins).foreach { case (inHW, in) => inHW.poke(in.U) }
//      println(
//        s"Concat: ${ins.map(_.toBinaryString).mkString(" ")}; " +
//          s"Result: <${c.out.peek().getWidth}>${StrBits(c.out)}"
//      )
//    }
//  }

//  it should "ExpGolombSingle.encode" in {
//    test(new ExpGolombEncodeSingleModule(8)) { c =>
//      for {
//        k <- 0 to 3
//        i <- 0 to 29
//      } {
//        c.k.poke(k.U)
//        c.in.poke(i.U)
//        println(s"(k == $k) $i <--> ${StrBits(c.out, c.outHigh)}")
//      }
//    }
//  }

//  it should "ExpGolombSingle.decode" in {
//    test(new ExpGolombDecodeSingleModule(12)) { c =>
//      for {
//        k <- 0 to 3
//        i <- 0 to 29
//      } {
//        val in = i + (1 << k)
//        val high = 2 * log2Floor(in) - k
//        c.in.poke(in.U)
//        c.high.poke(high.U)
//        c.k.poke(k.U)
//        println(s"(k == $k) ${StrBits(in, high)} <--> ${c.out.peek().litValue}")
//      }
//    }
//  }

//  it should "MeanHighBit" in {
////    val ins = Seq(2, 7, 12, 14, 15, 18, 35, 63)
//    val ins = Seq(255, 254, 253, 252)
//    test(
//      new TestableExpression[Vec[UInt], UInt](Vec(ins.length, UInt(8.W)), tOutOption = Option(UInt()))(v =>
//        MeanHighBit(v.toSeq)
//      )
//    ) { c =>
//      c.in.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
//      val expected = ins.map(log2Floor(_)).sum / ins.length
//      println(s"In: ${c.in.peek().mkString(" ")}; Out: ${c.out.peek()}; Expected: $expected")
//    }
//  }

//  it should "ExpGolombMulti.encode" in {
//    val ins = Seq(2, 7, 12, 14, 15, 18, 35, 63)
////    val ins = Seq(255, 254, 253, 252)
//    test(new ExpGolombEncodeMultiModule(ins.length, 8)) { c =>
//      c.ins.zip(ins).foreach { case (inHW: UInt, in: Int) => inHW.poke(in.U) }
//      c.clock.step()
//      val k = c.outK.peek().litValue.toInt
//      println(
//        s"In: ${ins.mkString(" ")}; Out: (k == $k) " +
//          s"${c.out.peek()}[${c.outHigh.peek()}:0] ==> ${StrBits(c.out, c.outHigh)}"
//      )
//      println(s"Expected: ${ins.map(Golomb.encode(_, k)).mkString}")
//    }
//  }

//  it should "ExpGolombMulti.decode" in {
//    val in = "100101011111100111101111101000100110011001001111"
//    val k = 4
//    val n = 8
//    val expectedOuts = Seq(2, 7, 12, 14, 15, 18, 35, 63)
////    val in = "111111111111111110111111101111111100"
////    val k = 8
////    val n = 4
////    val expectedOuts = Seq(255, 254, 253, 252)
//    test(new ExpGolombDecodeMultiModule(n, 8)) { c =>
//      c.in.poke(s"b$in".U)
//      c.high.poke((in.length - 1).U)
//      c.k.poke(k)
//      c.clock.step()
//
//      println(
//        s"In: ${c.in.peek().litValue.toString(2)}; Outs: " + c.outs.map(_.peek()).mkString(" ") +
//          "; Expected: " + expectedOuts.mkString(" ")
//      )
//    }
//  }

  it should "PackDropLSBs" in {
//    val ins = 8 to 15
//    val width = 8
//    val highs = ins.map(in => log2Up(in + 1) - 1)
//    val outWidth = ins.length * 2
    val ins = Seq(12, 0, 13, 1, 14, 2, 15, 3)
    val width = 8
    val highs = Seq(3, 1, 3, 1, 3, 1, 3, 1)
    val outWidth = 20
    test(new PackDropLSBsModule(ins.length, width, outWidth)) { c =>
      c.ins.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
      c.inHighs.zip(highs).foreach { case (highHw, high) => highHw.poke(high.U) }
      c.clock.step()

      println(s"Ins: " + ins.map(_.toBinaryString).mkString(" "))
      println(
        s"Outs: " + c.outs
          .zip(c.outHighs)
          .map {
            case (out, outHigh) => s"${out.peek()}{${out.peek().litValue.toString(2)}}[${outHigh.peek()}:0]"
          }
          .mkString(" ")
      )
      println(s"Shift: ${c.outShift.peek()}")
    }
  }
}
