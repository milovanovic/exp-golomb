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
  * Returns mean position of highest set bit in a sequence of signals.
  */
object MeanHighBit {
  def apply(in: Seq[Bits]): UInt = {
    require(isPow2(in.length), "in must be a nonempty sequence with a length of a power of two")
    require(in.forall(_.widthKnown), "All elements of in must have a known width")
    val resWidth = log2Up(Math.ceil(in.map(_.getWidth).sum / in.length.toFloat).toInt)
    val res = (in.map(Log2(_)).reduce(_ +& _) >> log2Floor(in.length)).asUInt
    //    println(s"Res: $res")
    WireDefault(UInt(resWidth.W), res)
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
      val decoded = ExpGolombSingle.decode(sig >> low, k)
      decoded +: decode(in, low - 1.U, k, n - 1)
    } else Seq()
  }
}

class TestableExpression[T <: Data, U <: Data](tIn: T, tOutOption: Option[U] = None)(exp: T => U) extends Module {
  val in = IO(Input(tIn))
  val res = exp(in)
  val out = IO(Output(tOutOption.getOrElse(chiselTypeOf(res))))
  out := res
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
  val k = IO(Input(UInt(log2Up(width).W)))
  val out = IO(Output(UInt(width.W)))

  out := ExpGolombSingle.decode(in, k)
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

  val (resRow, resShift) = PackDropLSBs(ins.zip(inHighs), outWidth, None)
  val (resOuts, resHighs) = resRow.unzip
  outs := VecInit(resOuts)
  outHighs := VecInit(resHighs)
  outShift := resShift
}

class ExpGolombEncodeBlockModule(n: Int, width: Int, blockWidth: Int) extends Module {
  val ins = IO(Input(Vec(n, UInt(width.W))))
  val k = IO(Input(UInt(log2Up(width).W)))
  val out = IO(Output(UInt(blockWidth.W)))
  val outShift = IO(Output(UInt(log2Up(width).W)))

  val (resOut, resShift) = ExpGolombBlock.encode(ins, k, blockWidth, None)
  out := resOut
  outShift := resShift
}

class ExpGolombDecodeBlockModule(n: Int, width: Int, blockWidth: Int) extends Module {
  val in = IO(Input(UInt(blockWidth.W)))
  val k = IO(Input(UInt(log2Up(width).W)))
  val shift = IO(Input(UInt(log2Up(width).W)))
  val outs = IO(Output(Vec(n, UInt(width.W))))

  outs := VecInit(ExpGolombBlock.decode(in, k, shift, n))
}

class MeanBitWidthModule(n: Int, width: Int) extends Module {
  val ins = IO(Input(Vec(n, UInt(width.W))))
  val out = IO(Output(UInt(log2Up(width + 1).W)))

  out := MeanBitWidth(ins)
}

object Golomb {
  def encodeInt(n: Int, k: Int): Int = {
    require(n >= 0)
    require(k >= 0)
    n + (1 << k)
  }

  def encode(n: Int, k: Int): String = {
    val encodedBinary = encodeInt(n, k).toBinaryString
    val encodedUnary = "0" * (encodedBinary.length - k - 1)
    encodedUnary + encodedBinary
  }

  def decodeInt(n: Int, k: Int): Int = {
    require(n >= (1 << k))
    require(k >= 0)
    n - (1 << k)
  }

  def decode(s: String, k: Int): Int = {
    decodeInt(Integer.parseInt(s, 2), k)
  }
}

object StrBits {
  def apply(num: BigInt, high: BigInt): String = {
    val len = high + 1
    val binStr = num.toString(2)
    val outPadLen = len - binStr.length
    s"${"0" * outPadLen.toInt}$binStr"
  }

  def apply[T <: Data](data: T, high: Int): String = {
    apply(data.peek().litValue, high)
  }

  def apply[T <: Data](data: T, high: UInt): String = {
    apply(data.peek().litValue, high.peek().litValue)
  }

  def apply[T <: Data](data: T): String = {
    data.peek().litValue.toString(2)
  }
}

object StrGroupedSeq {
  def apply(s: String, takeSeq: Seq[Int]): Seq[String] = {
    if (takeSeq.isEmpty) Seq()
    else s.take(takeSeq.head) +: apply(s.drop(takeSeq.head), takeSeq.tail)
  }
}

class Playground extends AnyFlatSpec with ChiselScalatestTester {
//  it should "Log2" in test(new TestableExpression[UInt, UInt](UInt(8.W))(Log2(_))) { c =>
//    for (i <- 0 to 64) {
//      c.in.poke(i.U)
//      val res = c.out.peek().litValue.toInt
//      println(s"${i.toBinaryString} <> ${(1 << res).toBinaryString} <--> : Log2($i) == ${c.out.peek()}")
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
//        c.in.poke(in.U)
//        c.k.poke(k.U)
//        println(s"(k == $k) ${in.toBinaryString} <--> ${c.out.peek().litValue}")
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

//  it should "PackDropLSBs" in {
////    val ins = 8 to 15
////    val width = 8
////    val highs = ins.map(in => log2Up(in + 1) - 1)
////    val outWidth = ins.length * 2
//    val ins = Seq(12, 0, 13, 1, 14, 2, 15, 3)
//    val width = 8
//    val highs = Seq(3, 1, 3, 1, 3, 1, 3, 1)
//    val outWidth = 20
//    test(new PackDropLSBsModule(ins.length, width, outWidth)) { c =>
//      c.ins.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
//      c.inHighs.zip(highs).foreach { case (highHw, high) => highHw.poke(high.U) }
//      c.clock.step()
//
//      println(s"Ins: " + ins.map(_.toBinaryString).mkString(" "))
//      println(
//        s"Outs: " + c.outs
//          .zip(c.outHighs)
//          .map {
//            case (out, outHigh) => s"${out.peek()}{${out.peek().litValue.toString(2)}}[${outHigh.peek()}:0]"
//          }
//          .mkString(" ")
//      )
//      println(s"Shift: ${c.outShift.peek()}")
//    }
//  }

//  it should "ExpGolombBlock.encode" in {
////    val ins = 8 to 15
////    val width = 8
////    val k = 3
////    val outWidth = ins.length * 6
//    val ins = Seq(12, 0, 13, 1, 14, 2, 15, 3)
//    val width = 8
//    val k = 3
//    val outWidth = 32
//
//    val encoded = ins.map(Golomb.encode(_, k))
//    test(new ExpGolombEncodeBlockModule(ins.length, width, outWidth)) { c =>
//      c.ins.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
//      c.k.poke(k.U)
//      c.clock.step()
//      val outBits =
//        StrGroupedSeq(
//          StrBits(c.out, outWidth - 1),
//          encoded.map { s => 1.max(s.length - c.outShift.peek().litValue.toInt) }
//        )
//      val (outPadded, encodedPadded) = outBits
//        .zip(encoded)
//        .map { case (sHw, s) => (sHw.padTo(s.length, " ").mkString, s.padTo(sHw.length, " ").mkString) }
//        .unzip
//      println(s"In:      " + ins.map(_.toBinaryString).mkString(" "))
//      println(s"Encoded: " + encodedPadded.mkString(" "))
//      println(
//        s"Out:     " + outPadded.mkString("", " ", " ") +
//          s"(${c.out.peek()}) " +
//          s"(>> ${c.outShift.peek()})".stripMargin
//      )
//    }
//  }

//  it should "ExpGolombBlock.decode" in {
//    def shiftUnshift(n: Int, shift: Int): Int = {
//      require(shift >= 0)
//      1.max(n >> shift) << shift
//    }
//
////    val n = 8
////    val width = 8
////    val k = 3
////    val encoded = "010000010001010010010011010100010101010110010111"
////    val shift = 0
//////    val encoded = "0100001000010010100101010010100101101011"
//////    val shift = 1
//////    val encoded = "01000100010001000101010101010101"
//////    val shift = 2
//////    val encoded = "010010010010010010010010"
//////    val shift = 3
//////    val encoded = "0101010101010101"
//////    val shift = 4
//////    val encoded = "11111111"
//////    val shift = 5
////    val expected = (8 to 15)
////      .map(Golomb.encodeInt(_, k))
////      .map(shiftUnshift(_, shift))
////      .map(Golomb.decodeInt(_, k))
//
//    val n = 8
//    val width = 8
//    val k = 3
//    val encoded = "0101001000010101100101011010100101111011"
//    val shift = 0
////    val encoded = "01010100010101000101110101011101"
////    val shift = 1
////    val encoded = "010110010110010110010110"
////    val shift = 2
////    val encoded = "0101010101010101"
////    val shift = 3
////    val encoded = "11111111"
////    val shift = 5
//    val expected = Seq(12, 0, 13, 1, 14, 2, 15, 3)
//      .map(Golomb.encodeInt(_, k))
//      .map(shiftUnshift(_, shift))
//      .map(Golomb.decodeInt(_, k))
//
//    val expectedBits = expected.map(_.toBinaryString)
//    test(new ExpGolombDecodeBlockModule(n, width, encoded.length)) { c =>
//      c.in.poke(s"b$encoded".U)
//      c.k.poke(k.U)
//      c.shift.poke(shift.U)
//      c.clock.step()
//      val outs = c.outs.map(_.peek().litValue.toInt)
//      val outsBits = c.outs.zip(expectedBits).map { case (out, exp) => StrBits(out, exp.length - 1) }
//      println("In:       " + encoded)
////      println("Out:      " + outsBits.mkString(" "))
////      println("Expected: " + expectedBits.mkString(" "))
//      println("Out:      " + outs.mkString(" "))
//      println("Expected: " + expected.mkString(" "))
//    }
//  }

//  it should "DivideByConst" in {
//    val width = 8
//    val divisor = 7
//    val maxIn = (1 << width) - 1
//    val maxOut = maxIn / divisor
//    test(new TestableExpression[UInt, UInt](UInt(width.W))(DivideByConst.rounded(maxOut, divisor)(_))) { c =>
//      for (in <- 0 to maxIn) {
//        c.in.poke(in.U)
//        c.clock.step()
//        val out = c.out.peek().litValue.toInt
////        val exp = in / divisor
//        val exp = Math.round(in.toDouble / divisor)
//        println(s"In: $in; Out: $out; Expected: $exp")
//        assertResult(exp)(out)
//      }
//    }
//  }

//  it should "BitWidth" in {
//    val width = 8
//    val maxIn = (1 << width) - 1
//    test(new TestableExpression[UInt, UInt](UInt(width.W))(BitWidth(_))) { c =>
//      for (in <- 0 to maxIn) {
//        c.in.poke(in.U)
//        c.clock.step()
//        val out = c.out.peek().litValue.toInt
//        val exp = if (in == 0) 0 else in.toBinaryString.length
//        println(s"In: $in; Out: $out; Expected: $exp")
//        assertResult(exp)(out)
//      }
//    }
//  }

  it should "MeanBitWidth" in {
    def bitWidth(n: Int): Int = {
      if (n == 0) 0 else n.toBinaryString.length
    }

    val width = 8
    val n = 5
    test(new MeanBitWidthModule(n, width)).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      for {
        i <- Seq(0, 1, 2, 4, 8, 16, 32, 64, 128)
        j <- Seq(0, 1, 3, 5, 9, 17, 33, 65, 129)
        k <- Seq(0, 1, 3, 6, 10, 18, 34, 66, 130)
        l <- Seq(0, 1, 3, 7, 11, 19, 35, 67, 131)
        m <- Seq(0, 1, 3, 7, 12, 20, 36, 68, 132)
      } {
        val ins = Seq(i, j, k, l, m).take(n)
        c.ins.zip(ins).foreach { case (inHw, in) => inHw.poke(in.U) }
//        c.clock.step()
        val out = c.out.peek().litValue.toInt
        val exp = Math.round(ins.map(bitWidth).sum.toFloat / n)
//        println(s"Ins: ${ins.map(_.toBinaryString).mkString(" ")}; Out: $out; Expected: $exp")
        assertResult(out)(exp)
      }
    }
  }
}
