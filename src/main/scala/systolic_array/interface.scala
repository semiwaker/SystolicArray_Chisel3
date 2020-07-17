package systolic_array

import chisel3._
import chisel3.util._
import chisel3.internal.firrtl.Index

import breeze.linalg._
import breeze.numerics._

class IteratorExpr(numIter: Int, spec: WorkloadSpec) {
  var coef = DenseVector.zeros[Int](numIter)
  var id   = -1
  var c    = 0

  def getNumIters()             = numIter
  def setUpperbound(bound: Int) = spec.setUpperbound(id, bound)

  def +(other: IteratorExpr): IteratorExpr = {
    assert(numIter == other.getNumIters())
    val result = new IteratorExpr(numIter, spec)
    result.coef = coef + other.coef
    result.c = c + other.c
    result
  }
  def +(other: Int): IteratorExpr = {
    val result = new IteratorExpr(numIter, spec)
    result.coef = coef
    result.c = other + c
    result
  }
  def +:(other: Int): IteratorExpr = this + c
  def *(k: Int): IteratorExpr = {
    val result = new IteratorExpr(numIter, spec)
    result.coef = k * coef
    result.c = k * c
    result
  }
  def *:(k: Int): IteratorExpr = this * k
}

object IteratorExpr {
  def apply(numIter: Int, spec: WorkloadSpec) = new IteratorExpr(numIter, spec)
  def apply(id: Int, numIter: Int, spec: WorkloadSpec) = {
    val x = new IteratorExpr(numIter, spec)
    x.id = id
    x.coef(id) = 1
    x
  }
}

abstract class TensorExpr {
  def generate(spec: WorkloadSpec): UInt
  def findIndexes(spec: WorkloadSpec): List[TensorIndex]
}

class NilTensorExpr extends TensorExpr {
  def generate(spec: WorkloadSpec)    = throw new Exception("Can't generate empty expression")
  def findIndexes(spec: WorkloadSpec) = Nil
}

abstract class BinaryTensorExpr(a: TensorExpr, b: TensorExpr) extends TensorExpr {
  def findIndexes(spec: WorkloadSpec): List[TensorIndex] = a.findIndexes(spec) ++ b.findIndexes(spec)
}

class TensorAddExpr(a: TensorExpr, b: TensorExpr) extends BinaryTensorExpr(a, b) {
  def generate(spec: WorkloadSpec): UInt = {
    val left  = a.generate(spec)
    val right = b.generate(spec)
    left + right
  }
}
class TensorMulExpr(a: TensorExpr, b: TensorExpr) extends BinaryTensorExpr(a, b) {
  def generate(spec: WorkloadSpec): UInt = {
    val left  = a.generate(spec)
    val right = b.generate(spec)
    left * right
  }
}

class TensorIndex(indexes: List[IteratorExpr], tensor: Tensor) extends TensorExpr {
  def getIndexes()                                       = indexes
  def getTensor()                                        = tensor
  def apply(idx: IteratorExpr): TensorIndex              = new TensorIndex(indexes :+ idx, tensor)
  def +(other: TensorIndex)                              = new TensorAddExpr(this, other)
  def *(other: TensorIndex)                              = new TensorMulExpr(this, other)
  def generate(spec: WorkloadSpec): UInt                 = spec.getSignal(tensor)
  def findIndexes(spec: WorkloadSpec): List[TensorIndex] = this :: Nil
}
class Tensor(id: Int) extends TensorExpr {
  def getID() = id
  def apply(idx: IteratorExpr): TensorIndex = {
    new TensorIndex(idx :: Nil, this)
  }
  def generate(spec: WorkloadSpec): UInt                 = throw new Exception("Can't generate a Tensor without index")
  def findIndexes(spec: WorkloadSpec): List[TensorIndex] = Nil
}

class WorkloadSpec {
  var numIter          = 0
  var iterList         = List[IteratorExpr]()
  var upperbounds      = Array[Int]()
  var numTensor        = 0
  var tensorList       = List[Tensor]()
  var signals          = List[UInt]().toVector
  var expr: TensorExpr = new NilTensorExpr()

  var accessMats  = List[DenseMatrix[Int]]().toVector
  var accessConst = List[DenseVector[Int]]().toVector

  def genIterators(n: Int): List[IteratorExpr] = {
    numIter = n
    upperbounds = Array.ofDim[Int](n)
    iterList = (for (i <- 0 until n) yield IteratorExpr(i, n, this)).toList
    iterList
  }
  def setUpperbound(id: Int, bound: Int): Unit = {
    assert(0 <= id && id < numIter)
    upperbounds(id) = bound
  }
  def genTensor(n: Int): List[Tensor] = {
    numTensor = n
    tensorList = (for (i <- 0 until n) yield new Tensor(i)).toList
    tensorList
  }
  def getSignal(t: Tensor) = signals(t.getID)

  def setExpr(e: TensorExpr) = {
    expr = e
    val idxs = e.findIndexes(this)
    accessMats = (for (idx <- idxs)
      yield {
        val rows = idx.getIndexes()
        DenseMatrix.tabulate[Int](rows.length, numIter) { case (i, j) => rows(i).coef(j) }
      }).toVector
    accessConst = idxs.map((x) => DenseVector.tabulate[Int](x.getIndexes().length)(x.getIndexes()(_).c)).toVector
  }
  def genExpr(sig: List[UInt]) = {
    signals = sig.toVector
    expr.generate(this)
  }

  implicit class IntTImesIteratorExpr(x: Int) {
    def *(y: IteratorExpr) = y * x
  }
  implicit class IntPlusIteratorExpr(x: Int) {
    def +(y: IteratorExpr) = y + x
  }
}

object Helpers {
  implicit class IntTImesIteratorExpr(x: Int) {
    def *(y: IteratorExpr) = y * x
  }
  implicit class IntPlusIteratorExpr(x: Int) {
    def +(y: IteratorExpr) = y + x
  }
}
