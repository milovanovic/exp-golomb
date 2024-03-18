package expgolomb

import chisel3._
import chisel3.util._
import ModelUtils.minTotalBlockWidth

abstract class GenericExpGolombBlockDecoder[T <: Data](tOut: Vec[T], totalBlockWidth: Int, kWidth: Int, shiftWidth: Int)
    extends Module {
  require(tOut.nonEmpty, "tOut must not be an empty sequence")
  require(tOut.isWidthKnown, "width of tOut's elements must be known")

  protected def blockDecode(in: Bits, k: UInt, shift: UInt): Seq[T]
  protected def blockDecodeDelay: Int = 0

  val in = IO(Flipped(Decoupled(Bits(totalBlockWidth.W))))
  val out = IO(Decoupled(tOut))

  private val validResult = Wire(Bool())
  private val globalEnable = out.ready || !validResult
  in.ready := globalEnable
  out.valid := validResult

  private val k = in.bits.head(kWidth)
  private val shift = in.bits.tail(kWidth).head(shiftWidth)
  private val encoded = in.bits.tail(kWidth + shiftWidth)
  out.bits.zip(blockDecode(encoded, k, shift)).foreach {
    case (o, r) => o := RegEnable(r, globalEnable)
  }

  val ioDelay = 1 + blockDecodeDelay
  validResult := ShiftRegister(in.valid, ioDelay, globalEnable)
}

class UnsignedExpGolombBlockDecoder(n: Int, elemWidth: Int, totalBlockWidth: Int, kWidth: Int, shiftWidth: Int)
    extends GenericExpGolombBlockDecoder[UInt](Vec(n, UInt(elemWidth.W)), totalBlockWidth, kWidth, shiftWidth) {
  require(
    totalBlockWidth >= minTotalBlockWidth(n, Some(elemWidth), kWidth, shiftWidth, signed = false),
    "totalBlockWidth is too small"
  )

  override protected def blockDecode(in: Bits, k: UInt, shift: UInt): Seq[UInt] = {
    ExpGolombBlock.decodeUnsigned(in, n, k, shift)
  }
}

class SignedExpGolombBlockDecoder(n: Int, elemWidth: Int, totalBlockWidth: Int, kWidth: Int, shiftWidth: Int)
    extends GenericExpGolombBlockDecoder[SInt](Vec(n, SInt(elemWidth.W)), totalBlockWidth, kWidth, shiftWidth) {
  require(
    totalBlockWidth >= minTotalBlockWidth(n, Some(elemWidth), kWidth, shiftWidth, signed = false),
    "totalBlockWidth is too small"
  )

  override protected def blockDecode(in: Bits, k: UInt, shift: UInt): Seq[SInt] = {
    ExpGolombBlock.decodeSigned(in, n, k, shift)
  }
}
