package expgolomb

import chisel3._
import chisel3.util._
import ModelUtils.minTotalBlockWidth

class ExpGolombBlockDecoder(n: Int, elemWidth: Int, totalBlockWidth: Int, kWidth: Int, shiftWidth: Int) extends Module {
  require(totalBlockWidth >= minTotalBlockWidth(n, elemWidth, kWidth, shiftWidth), "totalBlockWidth is too small")
  val in = IO(Flipped(Decoupled(UInt(totalBlockWidth.W))))
  val out = IO(Decoupled(Vec(n, UInt(elemWidth.W))))

  private val validResult = Wire(Bool())
  private val globalEnable = out.ready || !validResult
  in.ready := globalEnable
  out.valid := validResult

  private val k = in.bits.head(kWidth)
  private val shift = in.bits.tail(kWidth).head(shiftWidth)
  private val encoded = in.bits.tail(kWidth + shiftWidth)
  out.bits.zip(ExpGolombBlock.decode(encoded, n, k, shift)).foreach {
    case (o, r) => o := RegEnable(r, globalEnable)
  }

  val ioDelay = 1
  validResult := ShiftRegister(in.valid, ioDelay, globalEnable)
}
