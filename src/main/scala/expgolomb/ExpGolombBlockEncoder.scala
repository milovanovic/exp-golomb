package expgolomb

import chisel3._
import chisel3.util._
import expgolomb.ModelUtils.minTotalBlockWidth

abstract class GenericExpGolombBlockEncoder[T <: Data](
  tIn:             Vec[T],
  totalBlockWidth: Int,
  kWidth:          Int,
  shiftWidth:      Int)
    extends Module {
  require(tIn.nonEmpty, "tIn must not be an empty sequence")
  require(tIn.isWidthKnown, "width of tIn's elements must be known")

  protected def meanBitWidth(in: Vec[T], maxResult: Int): UInt
  protected def meanBitWidthDelay: Int =
    MeanBitWidth.delay(in.bits.length)
  protected def blockEncode(in: Vec[T], k: UInt, blockWidth: Int): (Bits, UInt)
  protected def blockEncodeDelay: Int =
    ExpGolombBlock.encodeDelay(in.bits.map(_.getWidth), k.getWidth, encodedBlockWidth)

  private val maxK = (1 << kWidth) - 1
  private val encodedBlockWidth = totalBlockWidth - kWidth - shiftWidth

  val in = IO(Flipped(Decoupled(tIn)))
  val out = IO(Decoupled(Bits(totalBlockWidth.W)))

  private val validResult = Wire(Bool())
  protected val globalEnable = out.ready || !validResult
  in.ready := globalEnable
  out.valid := validResult

  private val k = RegEnable(meanBitWidth(in.bits, maxK), globalEnable)
  assert(k.getWidth <= kWidth)
  private val kDelay = meanBitWidthDelay + 1
  private val inDelayed = ShiftRegister(in.bits, kDelay, globalEnable)

  private val (encoded, shift) = blockEncode(inDelayed, k, encodedBlockWidth)
  require(shift.getWidth <= shiftWidth, "shiftWidth is too small")
  assert(encoded.getWidth == encodedBlockWidth)
  private val encodeDelay = blockEncodeDelay
  private val kDelayed = ShiftRegister(k, encodeDelay, globalEnable)

  out.bits := Cat(
    WireDefault(UInt(kWidth.W), kDelayed),
    WireDefault(UInt(shiftWidth.W), shift),
    encoded
  )

  val ioDelay = kDelay + encodeDelay
  validResult := ShiftRegister(in.valid, ioDelay, false.B, globalEnable)
}

class UnsignedExpGolombBlockEncoder(n: Int, elemWidth: Int, totalBlockWidth: Int, kWidth: Int, shiftWidth: Int)
    extends GenericExpGolombBlockEncoder[UInt](Vec(n, UInt(elemWidth.W)), totalBlockWidth, kWidth, shiftWidth) {
  require(n > 0, "n must be greater than zero")
  require(elemWidth > 0, "elemWidth must be greater than zero")
  private val minBlockWidth = minTotalBlockWidth(n, Some(elemWidth), kWidth, shiftWidth, signed = false)
  require(totalBlockWidth >= minBlockWidth, s"totalBlockWidth is too small (try $minBlockWidth)")

  override protected def meanBitWidth(in: Vec[UInt], maxResult: Int): UInt = {
    MeanBitWidth(in, Some(globalEnable), Some(maxResult))
  }

  override protected def blockEncode(in: Vec[UInt], k: UInt, blockWidth: Int): (Bits, UInt) = {
    ExpGolombBlock.encodeUnsigned(in, k, blockWidth, Some(globalEnable))
  }
}

class SignedExpGolombBlockEncoder(n: Int, elemWidth: Int, totalBlockWidth: Int, kWidth: Int, shiftWidth: Int)
    extends GenericExpGolombBlockEncoder[SInt](Vec(n, SInt(elemWidth.W)), totalBlockWidth, kWidth, shiftWidth) {
  require(n > 0, "n must be greater than zero")
  require(elemWidth > 0, "elemWidth must be greater than zero")
  private val minBlockWidth = minTotalBlockWidth(n, Some(elemWidth), kWidth, shiftWidth, signed = true)
  require(totalBlockWidth >= minBlockWidth, s"totalBlockWidth is too small (try $minBlockWidth)")

  override protected def meanBitWidth(in: Vec[SInt], maxResult: Int): UInt = {
    MeanBitWidth(in.map { el => Mux(el < 0.S, ~el, el) }, Some(globalEnable), Some(maxResult))
  }

  override protected def blockEncode(in: Vec[SInt], k: UInt, blockWidth: Int): (Bits, UInt) = {
    ExpGolombBlock.encodeSigned(in, k, blockWidth, Some(globalEnable))
  }
}
