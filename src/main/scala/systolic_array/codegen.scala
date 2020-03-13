package systolic_array

import chisel3._
import chisel3.util._

import scala.collection.immutable.Seq

object DebugSwitch {
  var switch           = false
  def apply(): Boolean = switch
  def on(): Unit       = { switch = true }
  def off(): Unit      = { switch = false }
}

object UnnamedCode {
  var cnt = 0
  def apply(): Int = {
    cnt += 1
    cnt
  }
}

abstract class CodeGenerator(name: String = s"Unnamed Code ${UnnamedCode()}") {
  val sig_en    = Wire(Bool())
  val sig_fin   = RegInit(0.B)
  val sig_reset = Wire(Bool())

  sig_reset := 0.B

  def generate(): Unit

  def code_name = name

  def dprintf(pable: Printable): Unit =
    if (DebugSwitch())
      printf(pable)
}

class Code(name: String = s"Unnamed Code ${UnnamedCode()}")(func: (UInt) => Unit) extends CodeGenerator(name) {
  def generate(): Unit = {
    when(sig_en) {
      func(sig_fin)
    }
    when(sig_reset) {
      dprintf(p"$name reset\n")
      sig_fin := 0.B
    }
  }
}

object Code {
  def apply(name: String = s"Unnamed Code ${UnnamedCode()}")(func: (UInt) => Unit) = new Code(name)(func)
}

object OneStepCode {
  def apply(name: String = s"Unnamed Code ${UnnamedCode()}")(func: => Unit) =
    Code(name)((sig_fin) => {
      func
      sig_fin := 1.B
    })
}

class CodeBlock(name: String = s"Unnamed Code Block ${UnnamedCode()}") extends CodeGenerator(name) {
  var stages = 0
  var blocks = List[CodeGenerator]()

  def add(code: CodeGenerator): Unit = {
    blocks :+= code
    stages += 1
  }
  def generate(): Unit = {
    val code_states = Enum(stages + 1).toArray
    val state       = RegInit(code_states(0))
    for (i <- 0 until stages)
      blocks(i).generate()
    for (i <- 0 until stages)
      blocks(i).sig_en := 0.B
    when(sig_reset) {
      dprintf(p"$name Reset\n")
      state := code_states(0)
      blocks(0).sig_reset := 1.B
      sig_fin := 0.B
    }
    when(sig_en) {
      dprintf(p"$name State $state\n")
      for (i <- 0 until stages)
        when(state === code_states(i)) {
          when(blocks(i).sig_fin) {
            state := code_states(i + 1)
            if (i != stages - 1)
              blocks(i + 1).sig_reset := 1.B
            else {
              sig_fin := 1.B
              // dprintf(p"$name Finished\n")
            }
          }.otherwise {
            blocks(i).sig_en := 1.B
            state := code_states(i)
          }
        }
      when(state === code_states(stages)) {
        sig_fin := 1.B
      }
    }
  }
}

object CodeBlock {
  def apply(name: String = s"Unnamed Code Block ${UnnamedCode()}")(codes: List[CodeGenerator]) = {
    val c = new CodeBlock(name)
    for (code <- codes)
      c.add(code)
    c
  }
}

class ForLoop(
    variable: UInt,
    start: UInt,
    end: UInt,
    stride: UInt = 1.U,
    name: String = s"Unnamed ForLoop ${UnnamedCode()}"
)(body: CodeGenerator)
    extends CodeGenerator(name) {
  def generate(): Unit = {
    body.generate()
    body.sig_en := 0.B
    variable := variable
    when(sig_reset) {
      dprintf(p"$name reset\n")
      variable := start
      body.sig_reset := 1.B
      sig_fin := 0.B
    }
    when(sig_en) {
      dprintf(p"$name Var $variable\n")
      when(variable >= end) {
        sig_fin := 1.B
      }.otherwise {
        when(body.sig_fin) {
          variable := variable + stride
          body.sig_reset := 1.B
        }.otherwise {
          body.sig_en := 1.B
        }
      }
    }
  }
}

object ForLoop {
  def apply(
      variable: UInt,
      start: UInt,
      end: UInt,
      stride: UInt = 1.U,
      name: String = s"Unnamed ForLoop ${UnnamedCode()}"
  )(
      body: CodeGenerator
  ) =
    new ForLoop(variable, start, end, stride, name)(body)
}
