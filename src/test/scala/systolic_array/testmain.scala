package systolic_array

import chisel3._
import chisel3.util._

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class TestModule extends Module {
  val io = IO(new Bundle {
    val in    = Input(UInt(1.W))
    val start = Input(Bool())
    val out   = Output(UInt(1.W))
  })
  val reg = RegNext(io.in)
  io.out := reg
  val c = CodeBlock(Code((sig_end) => {
    reg := 1.U
    printf(p"$io\n")
    sig_end := 1.B
  }))
  c.add(Code((sig_end) => {
    reg := 2.U
    printf(p"$io\n")
    sig_end := 1.B
  }))
  c.generate()
  c.sig_start := io.start
  printf("Step %d %d %d %d\n", c.sig_start, c.sig_end, reg, io.out)
}

class Tester(t: TestModule) extends PeekPokeTester(t) {
  poke(t.io.start, 1)
  expect(t.io.out, 0)
  step(1)
  poke(t.io.start, 0)
  step(1)
  step(1)
  step(1)
  step(1)
  step(1)
}

object Main extends App {
  iotesters.Driver.execute(args, () => new TestModule) { c =>
    new Tester(c)
  }
}
