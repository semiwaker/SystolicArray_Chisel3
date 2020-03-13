package systolic_array

import scala.util.Random

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}

class SystolicArrayTester(t: SystolicArray) extends PeekPokeTester(t) {

  def conv(in: Array[Array[Int]], weight: Array[Array[Int]]): Array[Array[Int]] = {
    val m   = in.length
    val n   = in(0).length
    val h   = weight.length
    val w   = weight(0).length
    var out = Array.ofDim[Int](m - h + 1, n - w + 1)

    for (i <- 0 until m - h + 1; j <- 0 until n - w + 1)
      out(i)(j) = (for (x <- 0 until h; y <- 0 until w) yield in(i + x)(j + y) * weight(x)(y)).sum

    out
  }

  val rand = new Random(1)
  val m    = 64
  val n    = 64
  val h    = 32
  val w    = 32

  val a = Array.ofDim[Int](m, n)
  val b = Array.ofDim[Int](h, w)

  for (i <- 0 until m; j <- 0 until n)
    a(i)(j) = rand.nextInt() % 6 + 5
  for (i <- 0 until h; j <- 0 until w)
    b(i)(j) = rand.nextInt() % 6 + 5

  val c = conv(a, b)

  val i_addr = 0
  val w_addr = n * m
  val o_addr = n * m + w * h

  // println(s"A m $m n $n")
  // for (i <- 0 until m) {
  //   for (j <- 0 until n)
  //     print(s"${a(i)(j)} ")
  //   println("")
  // }
  // println(s"B h $h w $w")
  // for (i <- 0 until h) {
  //   for (j <- 0 until w)
  //     print(s"${b(i)(j)} ")
  //   println("")
  // }
  // println(s"C m_o ${m - h} n_o ${n - w}")
  // for (i <- 0 until m - h + 1) {
  //   for (j <- 0 until n - w + 1)
  //     print(s"${c(i)(j)} ")
  //   println("")
  // }

  poke(t.io.is_wr, 1)
  for (i <- 0 until m; j <- 0 until n) {
    poke(t.io.rdwr_addr, i * n + j + i_addr)
    poke(t.io.wr_value, a(i)(j))
    step(1)
  }
  for (i <- 0 until h; j <- 0 until w) {
    poke(t.io.rdwr_addr, i * w + j + w_addr)
    poke(t.io.wr_value, b(i)(j))
    step(1)
  }
  poke(t.io.is_wr, 0)
  poke(t.io.is_rd, 1)
  for (i <- 0 until m; j <- 0 until n) {
    poke(t.io.rdwr_addr, i * n + j + i_addr)
    step(1)
    expect(t.io.rd_value, a(i)(j))
  }
  for (i <- 0 until h; j <- 0 until w) {
    poke(t.io.rdwr_addr, i * w + j + w_addr)
    step(1)
    expect(t.io.rd_value, b(i)(j))
  }
  poke(t.io.is_rd, 0)

  poke(t.io.n, n)
  poke(t.io.m, m)
  poke(t.io.w, w)
  poke(t.io.h, h)
  poke(t.io.i_addr, i_addr)
  poke(t.io.w_addr, w_addr)
  poke(t.io.o_addr, o_addr)
  poke(t.io.start, 1)
  step(1)
  poke(t.io.start, 0)

  while (peek(t.io.valid) != 1) {
    step(1)
  }
  poke(t.io.is_rd, 1)
  for (i <- 0 until m - h + 1; j <- 0 until n - w + 1) {
    poke(t.io.rdwr_addr, i * (n - w + 1) + j + o_addr)
    step(1)
    expect(t.io.rd_value, c(i)(j))
  }
  poke(t.io.is_rd, 0)
}

object SystolicArrayTestMain extends App {
  // DebugSwitch.on()
  iotesters.Driver.execute(args, () => new SystolicArray(32, 32, 8, 8, 1048576, WeightStationary)) { c =>
    new SystolicArrayTester(c)
  }
}
