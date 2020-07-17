package systolic_array

import chisel3._
import chisel3.util._

import chisel3.iotesters
import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import breeze.linalg.DenseMatrix

object InterfaceMain extends App {
  val spec = new WorkloadSpec {
    val i :: j :: k :: Nil = genIterators(3)
    i.setUpperbound(10)
    j.setUpperbound(20)
    k.setUpperbound(30)
    val a :: b :: Nil = genTensor(2)
    setExpr(a(1 + i)(3 * k + 2 * j + 5) * b(k)(j + 2 * (i + 2)))
  }
  for (mat <- spec.accessMats) println(mat)
  for (c   <- spec.accessConst) println(c)
  for (u   <- spec.upperbounds) println(u)
}
