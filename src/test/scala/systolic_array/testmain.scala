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
  val i   = RegNext(io.out)
  val j   = RegNext(io.out)
  io.out := reg
  reg := reg
  val c = ForLoop(i, 0.U, 6.U, 2.U, "Outer")(
    ForLoop(j, 1.U, 7.U, 3.U, "Inner")(
      CodeBlock("Body")(
        OneStepCode("Add") {
          reg := i + j
        } :: OneStepCode("Mul") {
          reg := i * j
        } :: Nil
      )
    )
  )
  c.generate()
  c.sig_reset := io.start
  c.sig_en := 1.B
  printf("Step out %d\n", io.out)
}

class Tester(t: TestModule) extends PeekPokeTester(t) {
  poke(t.io.start, 1)
  step(1)
  poke(t.io.start, 0)
  step(30)
}

object Main extends App {
  // DebugSwitch.on()
  iotesters.Driver.execute(args, () => new TestModule) { c =>
    new Tester(c)
  }
}
