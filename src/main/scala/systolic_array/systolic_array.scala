package systolic_array

import chisel3._
import chisel3.util._

import scala.collection.immutable.Seq

sealed trait SystolicArrayMode;

case object NotSpecified     extends SystolicArrayMode;
case object InputStationary  extends SystolicArrayMode;
case object OutputStationary extends SystolicArrayMode;
case object WeightStationary extends SystolicArrayMode;

class SystolicArray(
    dataW: Int,
    addrW: Int,
    array_W: Int,
    array_H: Int,
    memory_size: Int,
    mode: SystolicArrayMode = NotSpecified
) extends Module {
  val io = IO(new Bundle {
    val n      = Input(UInt(dataW.W))
    val m      = Input(UInt(dataW.W))
    val w      = Input(UInt(dataW.W))
    val h      = Input(UInt(dataW.W))
    val start  = Input(Bool())
    val i_addr = Input(UInt(addrW.W))
    val w_addr = Input(UInt(addrW.W))
    val o_addr = Input(UInt(addrW.W))
    val valid  = Output(Bool())
  })

  // These memories should be outside of this module, but I haven't figure out how
  val data_i = Mem(memory_size, UInt(dataW.W))
  val data_w = Mem(memory_size, UInt(dataW.W))
  val data_o = Mem(memory_size, UInt(dataW.W))

  val n   = RegNext(io.n)
  val m   = RegNext(io.m)
  val w   = RegNext(io.w)
  val h   = RegNext(io.h)
  val n_o = RegNext(io.n)
  val m_o = RegNext(io.m)

  when(io.start) {
    n := io.n
    m := io.m
    w := io.w
    h := io.h
    n_o := io.n - io.w
    m_o := io.m - io.h
  }

  // The work state of a cell
  val s_idle :: s_init :: s_work :: s_output :: Nil = Enum(3)

  // Defining a cell in the array
  class Cell extends Module {
    val io = IO(new Bundle {
      val up_in    = Input(UInt(dataW.W))
      val up_bub   = Input(Bool())
      val left_in  = Input(UInt(dataW.W))
      val left_bub = Input(Bool())
      val state_in = Input(UInt())

      val out       = Output(UInt(dataW.W))
      val down_out  = Output(UInt(dataW.W))
      val down_bub  = Output(Bool())
      val right_out = Output(UInt(dataW.W))
      val right_bub = Output(Bool())
      val state_out = Output(UInt())
    })

    val reg      = Reg(UInt(dataW.W))
    val out_reg  = Reg(UInt(dataW.W))
    val vert_reg = Reg(UInt(dataW.W))
    val vert_bub = Reg(Bool())
    val hori_reg = Reg(UInt(dataW.W))
    val hori_bub = Reg(Bool())
    vert_reg := io.up_in
    io.down_out := vert_reg
    vert_bub := io.up_bub
    io.down_bub := vert_bub

    hori_reg := io.left_in
    io.right_out := hori_reg
    hori_bub := io.left_bub
    io.right_bub := hori_bub

    io.state_out := io.state_in

    when(io.state_in === s_work) {
      mode match {
        case InputStationary  => {}
        case OutputStationary => {}
        case WeightStationary => {
          when(!vert_bub) { io.out := reg * vert_reg }
            .otherwise { io.out := 0.U }
        }
        case NotSpecified => {}
      }
    }.elsewhen(io.state_in === s_init) {
        reg := io.up_in
      }
      .elsewhen(io.state_in === s_output) {
        io.down_out := out_reg
        out_reg := io.up_in
      }

  }

  val cellArray = (
    for (i <- 0 until array_H; j <- 0 until array_W) yield Module(new Cell)
  ).toArray
  for (i <- 0 until array_H; j <- 0 until array_W) {
    if (i != 0) {
      cellArray(i * array_H + j).io.up_in := cellArray((i - 1) * array_H + j).io.down_out
      cellArray(i * array_H + j).io.up_bub := cellArray((i - 1) * array_H + j).io.down_bub
      cellArray(i * array_H + j).io.state_in := cellArray((i - 1) * array_H + j).io.state_out
    } else {
      if (j != 0)
        cellArray(i * array_H + j).io.state_in := cellArray(i * array_H + (j - 1)).io.state_out
    }

    if (j != 0) {
      cellArray(i * array_H + j).io.left_in := cellArray(i * array_H + (j - 1)).io.right_out
      cellArray(i * array_H + j).io.left_bub := cellArray(i * array_H + (j - 1)).io.right_bub
    }
  }
  val read_addr_reg  = (for (i <- 0 until array_W) yield RegInit(0.U(addrW.W))).toArray
  val write_addr_reg = (for (i <- 0 until array_W) yield RegInit(0.U(addrW.W))).toArray

  // A helper function to make an addition tree when output reduce is needed
  def make_add_tree() = {
    def makeTree(l: Int, r: Int, a: Array[UInt]): Tuple2[UInt, Int] = {
      val reg = Reg(UInt(dataW.W))
      if (l == r) {
        reg := a(l)
        (reg, 1)
      } else if (l == r + 1) {
        reg := a(l) + a(r)
        (reg, 1)
      } else {
        val m               = (l + r) / 2
        val (lreg, lheight) = makeTree(l, m, a)
        val (rreg, rheight) = makeTree(m + 1, r, a)
        reg := lreg + rreg
        (reg, if (lheight > rheight) lheight + 1 else rheight + 1)
      }
    }
    val lines = (
      for (i <- 0 until array_H) yield {
        val row = (for (j <- 0 until array_W) yield cellArray(i * array_H + j).io.out).toArray
        makeTree(0, array_W - 1, row)
      }
    ).toArray
    val col       = { for ((reg, h) <- lines) yield reg }.toArray
    val h1        = { for ((reg, h) <- lines) yield h }.max
    val (reg, h2) = makeTree(0, array_H - 1, col)
    (reg, h1 + h2)
  }

  val state_reg = RegInit(s_idle)
  cellArray(0).io.state_in := state_reg

  mode match {
    case InputStationary  => {}
    case OutputStationary => {}
    case WeightStationary => {
      val x           = RegInit(0.U(dataW.W))
      val y           = RegInit(0.U(dataW.W))
      val loading     = RegInit(0.B)
      val calculating = RegInit(0.B)
      val load_x      = RegInit(0.U(dataW.W))
      val load_y      = RegInit(0.U(dataW.W))
      val write_addr_reg = RegInit(UInt(addrW.W))

      val (sum_reg, delay) = make_add_tree()
      val stop_m           = RegNext(m)
      val stop_n           = RegNext(n)
      stop_m := m + delay.U
      stop_n := n - array_W.U

      val mainCode = CodeBlock("mainCode")(
        ForLoop(y, 0.U, m_o , name="Clear_y_loop")(
          ForLoop(x, 0.U, n_o, name="Clear_x_loop")(
            CodeBlock("Clear_body")(
              OneStepCode("Clear_Prepare")
              {
                write_addr_reg := y * n_o + io.o_addr
              } :: OneStepCode("Clear_work"){
                data_o(write_addr_reg) := 0.U
                write_addr_reg := write_addr_reg + 1.U
              } :: Nil
            )
          )
        ):: ForLoop(y, 0.U, h, array_H.U, "y_loop")(
          ForLoop(x, 0.U, w, array_W.U, "x_loop")(
            CodeBlock("Body")(
              OneStepCode("Prepare") {
                state_reg := s_init
                for (i <- 0 until array_W)
                  read_addr_reg(i) := y * w + x + i.U + io.w_addr
              }::ForLoop(load_y, 0.U, array_H.U, 1.U, "Load_loop")(
                OneStepCode("Load") {
                  for (i <- 0 until array_W) {
                    cellArray(i).io.up_in := data_w(read_addr_reg(i))
                    read_addr_reg(i) := read_addr_reg(i) + w
                  }
                }
              ) :: OneStepCode("LoadFin") {
                state_reg := s_work
              } :: ForLoop(load_x, x, stop_n, 1.U, "Calc_x_loop")(
                CodeBlock("Calc")(
                  OneStepCode("Calc_prepare"){
                    for (i <- 0 until array_W)
                      read_addr_reg(i) := load_x + i.U + io.i_addr
                    write_addr_reg := load_x - x
                  } :: ForLoop(load_y, y, stop_m, 1.U, "Calc_y_loop")(
                    OneStepCode("Calc"){
                      when(load_y < m){
                        for (i<-0 until array_W){
                          cellArray(i).io.up_in := data_w(read_addr_reg(i))
                          read_addr_reg(i) := read_addr_reg(i) + n
                        }
                      }
                      when(load_y >= y + delay.U + array_H.U){
                        data_o(write_addr_reg) := data_o(write_addr_reg) + sum_reg
                        write_addr_reg := write_addr_reg + n_o
                      }
                    }
                  ) :: Nil
                )
              ) :: Nil
            )
          )
        ) :: Nil
      )

      mainCode.generate()

      io.valid := mainCode.sig_fin
      mainCode.sig_en := 1.B
      mainCode.sig_reset := io.start
    }
    case _ => {}
  }

}
