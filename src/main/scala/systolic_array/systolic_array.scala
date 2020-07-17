package systolic_array

import chisel3._
import chisel3.util._
import chisel3.internal.firrtl.Index

import breeze.linalg._
import breeze.numerics._

class SystolicArray(
    dataW: Int,
    addrW: Int,
    array_W: Int,
    array_H: Int,
    memory_size: Int,
    workloadSpec: WorkloadSpec,
    DMat: DenseMatrix[Int],
    TMat: DenseMatrix[Int]
) extends Module {
  val io = IO(new Bundle {
    val n         = Input(UInt(dataW.W))
    val m         = Input(UInt(dataW.W))
    val w         = Input(UInt(dataW.W))
    val h         = Input(UInt(dataW.W))
    val start     = Input(Bool())
    val i_addr    = Input(UInt(addrW.W))
    val w_addr    = Input(UInt(addrW.W))
    val o_addr    = Input(UInt(addrW.W))
    val is_wr     = Input(Bool())
    val is_rd     = Input(Bool())
    val wr_value  = Input(UInt(dataW.W))
    val rdwr_addr = Input(UInt(addrW.W))
    val valid     = Output(Bool())
    val rd_value  = Output(UInt(dataW.W))
  })
}
