package expgolomb

import chisel3._
import chisel3.util._

class ExpGolombBlockEncoder(n: Int, elemWidth: Int, totalBlockWidth: Int, kWidth: Int, shiftWidth: Int) extends Module {
  require(n > 0, "n must be greater than zero")
  require(elemWidth > 0, "elemWidth must be greater than zero")
  private val maxK = (1 << kWidth) - 1
  private val maxShift = (1 << shiftWidth).min(elemWidth) - 1
  private val minBlockWidth = n * (elemWidth + 1 - maxShift).max(1) + kWidth + shiftWidth
  require(totalBlockWidth >= minBlockWidth, s"totalBlockWidth is too small (try $minBlockWidth)")
  private val encodedBlockWidth = totalBlockWidth - kWidth - shiftWidth

  val in = IO(Flipped(Decoupled(Vec(n, UInt(elemWidth.W)))))
  val out = IO(Decoupled(UInt(totalBlockWidth.W)))
  val ioDelay = 1 + ExpGolombBlock.encodeDelay(Seq.fill(n)(elemWidth), encodedBlockWidth)

  private val validResult = RegInit(false.B)
  private val globalEnable = out.ready || !validResult
  validResult := ShiftRegister(in.valid, ioDelay - 1, false.B, globalEnable)

  in.ready := globalEnable
  out.valid := validResult

  private val k = RegEnable(MeanBitWidth(in.bits, Some(maxK)), globalEnable)
  assert(k.getWidth <= kWidth)

  private val (encoded, shift) =
    ExpGolombBlock.encode(
      RegEnable(in.bits, globalEnable),
      k,
      encodedBlockWidth,
      Some(globalEnable)
    )
  assert(encoded.getWidth == encodedBlockWidth)

  out.bits := Cat(
    if (k.getWidth != kWidth) WireDefault(UInt(kWidth.W), k) else k,
    if (shift.getWidth != shiftWidth) WireDefault(UInt(shiftWidth.W), shift) else shift,
    encoded
  )
}
