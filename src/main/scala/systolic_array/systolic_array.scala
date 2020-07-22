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
) {}
