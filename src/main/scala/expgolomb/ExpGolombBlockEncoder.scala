package expgolomb

import chisel3._
import chisel3.util._
import ModelUtils.minTotalBlockWidth

class ExpGolombBlockEncoder(n: Int, elemWidth: Int, totalBlockWidth: Int, kWidth: Int, shiftWidth: Int) extends Module {
  require(n > 0, "n must be greater than zero")
  require(elemWidth > 0, "elemWidth must be greater than zero")
  private val maxK = (1 << kWidth) - 1
  private val minBlockWidth = minTotalBlockWidth(n, elemWidth, kWidth, shiftWidth)
  require(totalBlockWidth >= minBlockWidth, s"totalBlockWidth is too small (try $minBlockWidth)")
  private val encodedBlockWidth = totalBlockWidth - kWidth - shiftWidth

  val in = IO(Flipped(Decoupled(Vec(n, UInt(elemWidth.W)))))
  val out = IO(Decoupled(UInt(totalBlockWidth.W)))

  private val validResult = RegInit(false.B)
  private val globalEnable = out.ready || !validResult
  in.ready := globalEnable
  out.valid := validResult

  private val k = RegEnable(MeanBitWidth(in.bits, Some(globalEnable), Some(maxK)), globalEnable)
  assert(k.getWidth <= kWidth)
  private val kDelay = MeanBitWidth.delay(in.bits.length) + 1
  private val inDelayed = ShiftRegister(in.bits, kDelay, globalEnable)

  private val (encoded, shift) =
    ExpGolombBlock.encode(
      inDelayed,
      k,
      encodedBlockWidth,
      Some(globalEnable)
    )
  require(shift.getWidth <= shiftWidth, "shiftWidth is too small")
  assert(encoded.getWidth == encodedBlockWidth)
  private val encodeDelay = ExpGolombBlock.encodeDelay(Seq.fill(n)(elemWidth), k.getWidth, encodedBlockWidth)
  private val kDelayed = ShiftRegister(k, encodeDelay, globalEnable)

  out.bits := Cat(
    WireDefault(UInt(kWidth.W), kDelayed),
    WireDefault(UInt(shiftWidth.W), shift),
    encoded
  )

  val ioDelay = kDelay + encodeDelay
  validResult := ShiftRegister(in.valid, ioDelay - 1, false.B, globalEnable)
}
