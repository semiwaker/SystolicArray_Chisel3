package systolic_array

import chisel3._
import chisel3.util._

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class TestModule extends Module {
  val io = IO(new Bundle {
    val in    = Input(Bool())
    val start = Input(Bool())
    val out   = Output(UInt(8.W))
  })
  val reg = RegNext(io.out, init = 0.U)
  val i   = Reg(UInt(8.W))
  io.out := reg
  reg := reg
  val c = ForLoop(i, 0.U, 3.U, "Main")(CodeBlock("LoopBody")(Code((sig_fin) => {
    reg := i
    sig_fin := 1.B
  }, "LoopBodyCode")))
  c.generate()
  c.sig_reset := io.start
  c.sig_en := 1.B
  printf("Step en %d fin %d reg %d out %d\n", c.sig_en, c.sig_fin, reg, io.out)
}

class Tester(t: TestModule) extends PeekPokeTester(t) {
  poke(t.io.start, 1)
  step(1)
  poke(t.io.start, 0)
  step(10)
}

object Main extends App {
  DebugSwitch.on()
  iotesters.Driver.execute(args, () => new TestModule) { c =>
    new Tester(c)
  }
}
