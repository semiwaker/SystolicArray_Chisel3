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

  io.rd_value := 0.U
  io.valid := 0.B

  val data = Mem(memory_size, UInt(dataW.W))

  val n   = RegNext(io.n)
  val m   = RegNext(io.m)
  val w   = RegNext(io.w)
  val h   = RegNext(io.h)
  val n_o = RegNext(io.n)
  val m_o = RegNext(io.m)

  n := n
  m := m
  w := w
  h := w
  n_o := n_o
  m_o := m_o

  val start_reg = RegInit(0.B)
  start_reg := start_reg

  when(io.is_wr) {
    // printf(p"Write ${io.wr_value} at ${io.rdwr_addr}\n")
    data(io.rdwr_addr) := io.wr_value
  }
  when(io.is_rd) {
    io.rd_value := data(io.rdwr_addr)
    // printf(p"Read ${io.rd_value} at ${io.rdwr_addr}\n")
  }

  when(io.start) {
    n := io.n
    m := io.m
    w := io.w
    h := io.h
    n_o := io.n - io.w + 1.U
    m_o := io.m - io.h + 1.U
    start_reg := 1.B
  }

  // The work state of a cell
  val s_idle :: s_init :: s_work :: s_output :: Nil = Enum(4)

  val v_pass = Wire(Bool())
  val h_pass = Wire(Bool())
  v_pass := 0.B
  h_pass := 1.B

  // Defining a cell in the array
  class Cell {
    val up_in    = Wire(UInt(dataW.W))
    val up_bub   = Wire(Bool())
    val left_in  = Wire(UInt(dataW.W))
    val left_bub = Wire(Bool())
    val state_in = Wire(UInt())

    val out       = Wire(UInt(dataW.W))
    val down_out  = Wire(UInt(dataW.W))
    val down_bub  = Wire(Bool())
    val right_out = Wire(UInt(dataW.W))
    val right_bub = Wire(Bool())
    val state_out = Wire(UInt())

    val w_reg    = Reg(UInt(dataW.W))
    val out_reg  = Reg(UInt(dataW.W))
    val vert_reg = Reg(UInt(dataW.W))
    val vert_bub = Reg(Bool())
    val hori_reg = Reg(UInt(dataW.W))
    val hori_bub = Reg(Bool())

    down_out := vert_reg
    down_bub := vert_bub
    when(v_pass) {
      vert_reg := up_in
      vert_bub := up_bub
    }.otherwise {
      vert_reg := vert_reg
      vert_bub := vert_bub
    }

    right_out := hori_reg
    right_bub := hori_bub
    when(h_pass) {
      hori_reg := left_in
      hori_bub := left_bub
    }.otherwise {
      hori_reg := hori_reg
      hori_bub := hori_bub
    }

    state_out := state_in

    w_reg := w_reg
    out_reg := out_reg

    out := out_reg

    when(state_in === s_work) {
      mode match {
        case InputStationary  => {}
        case OutputStationary => {}
        case WeightStationary => {
          when(!vert_bub) { out_reg := w_reg * vert_reg }
            .otherwise { out_reg := 0.U }
        }
        case NotSpecified => {}
      }
    }.elsewhen(state_in === s_init) {
        w_reg := vert_reg
      }
      .elsewhen(state_in === s_output) {
        down_out := out_reg
        out_reg := up_in
      }

  }

  val cellArray = (for (i <- 0 until array_H) yield (for (j <- 0 until array_W) yield new Cell()).toArray).toArray
  // val cellArray = Array.ofDim[Cell](array_H, array_W)
  for (i <- 0 until array_H; j <- 0 until array_W) {
    if (i != 0) {
      cellArray(i)(j).up_in := cellArray(i - 1)(j).down_out
      cellArray(i)(j).up_bub := cellArray(i - 1)(j).down_bub
      cellArray(i)(j).state_in := cellArray(i - 1)(j).state_out
    } else {
      cellArray(i)(j).up_in := 0.U
      cellArray(i)(j).up_bub := 0.B
      if (j != 0)
        cellArray(i)(j).state_in := cellArray(i)(j - 1).state_out
    }

    if (j != 0) {
      cellArray(i)(j).left_in := cellArray(i)(j - 1).right_out
      cellArray(i)(j).left_bub := cellArray(i)(j - 1).right_bub
    } else {
      cellArray(i)(j).left_in := 0.U
      cellArray(i)(j).left_bub := 0.B
    }
  }
  val read_addr_reg = (for (i <- 0 until array_W) yield RegInit(0.U(addrW.W))).toArray

  for (i <- 0 until array_W) read_addr_reg(i) := read_addr_reg(i)

  val add_tree_move = Wire(Bool())
  add_tree_move := 0.B

  // A helper function to make an addition tree when output reduce is needed
  def make_add_tree() = {
    def makeTree(l: Int, r: Int, a: Array[UInt]): Tuple2[UInt, Int] = {
      val reg = Reg(UInt(dataW.W))
      reg := reg
      if (l == r) {
        when(add_tree_move) {
          reg := a(l)
        }
        (reg, 1)
      } else if (r == l + 1) {
        when(add_tree_move) {
          reg := a(l) + a(r)
        }
        (reg, 1)
      } else {
        val m               = (l + r) / 2
        val (lreg, lheight) = makeTree(l, m, a)
        val (rreg, rheight) = makeTree(m + 1, r, a)
        when(add_tree_move) {
          reg := lreg + rreg
        }
        if (lheight == rheight)
          (reg, lheight + 1)
        else {
          val mid_reg = RegNext(reg)
          mid_reg := mid_reg
          when(add_tree_move) {
            if (lheight > rheight)
              mid_reg := rreg
            else
              mid_reg := lreg
          }
          (mid_reg, if (lheight > rheight) lheight + 1 else rheight + 1)
        }
      }
    }
    val lines = (
      for (i <- 0 until array_H) yield {
        val row = (for (j <- 0 until array_W) yield cellArray(i)(j).out).toArray
        makeTree(0, array_W - 1, row)
      }
    ).toArray
    val col       = { for ((reg, h) <- lines) yield reg }.toArray
    val h1        = { for ((reg, h) <- lines) yield h }.max
    val (reg, h2) = makeTree(0, array_H - 1, col)
    (reg, h1 + h2 - 1)
  }

  val state_reg = RegInit(s_idle)
  cellArray(0)(0).state_in := state_reg

  mode match {
    case InputStationary  => {}
    case OutputStationary => {}
    case WeightStationary => {
      val x              = RegInit(0.U(dataW.W))
      val y              = RegInit(0.U(dataW.W))
      val clear_x        = RegInit(0.U(dataW.W))
      val clear_y        = RegInit(0.U(dataW.W))
      val load_y         = RegInit(0.U(dataW.W))
      val calc_x         = RegInit(0.U(dataW.W))
      val calc_y         = RegInit(0.U(dataW.W))
      val write_addr_reg = Reg(UInt(addrW.W))

      write_addr_reg := write_addr_reg

      val (sum_reg, delay) = make_add_tree()
      // println(s"Delay $delay")
      val stop_m = RegNext(m)
      val stop_n = RegNext(m)

      stop_m := stop_m
      stop_n := stop_n

      val mainCode = CodeBlock("mainCode")(
        ForLoop(clear_y, 0.U, m_o, 1.U, "Clear_y_loop")(
          ForLoop(clear_x, 0.U, n_o, 1.U, "Clear_x_loop")(
            CodeBlock("Clear_body")(
              OneStepCode("Clear_Prepare") {
                write_addr_reg := clear_y * n_o + io.o_addr
              } :: OneStepCode("Clear_work") {
                data(write_addr_reg) := 0.U
                write_addr_reg := write_addr_reg + 1.U
              } :: Nil
            )
          )
        ) :: ForLoop(y, 0.U, h, array_H.U, "y_loop")(
          ForLoop(x, 0.U, w, array_W.U, "x_loop")(
            CodeBlock("Body")(
              OneStepCode("Prepare") {
                // printf(p"y $y x $x\n")
                state_reg := s_init
                for (i <- 0 until array_W)
                  read_addr_reg(i) := y * w + x + i.U + io.w_addr
              } :: ForLoop(load_y, 0.U, array_H.U, 1.U, "Load_loop")(
                OneStepCode("Load") {
                  v_pass := 1.B
                  for (i <- 0 until array_W) {
                    cellArray(0)(i).up_in := data(read_addr_reg(i))
                    read_addr_reg(i) := read_addr_reg(i) + w
                  }
                }
              ) :: OneStepCode("LoadFin") {
                state_reg := s_work
                stop_m := m_o + delay.U + y + array_H.U + 1.U
                stop_n := n_o + x
              } :: ForLoop(calc_x, x, stop_n, 1.U, "Calc_x_loop")(
                CodeBlock("Calc_x")(
                  OneStepCode("Calc_prepare") {
                    for (i <- 0 until array_W)
                      read_addr_reg(i) := y * n + calc_x + i.U + io.i_addr
                    write_addr_reg := calc_x - x + io.o_addr
                  } :: ForLoop(calc_y, y, stop_m, 1.U, "Calc_y_loop")(
                    OneStepCode("Calc_y") {
                      v_pass := 1.B
                      add_tree_move := 1.B
                      when(calc_y < m) {
                        for (i <- 0 until array_W) {
                          cellArray(0)(i).up_in := data(read_addr_reg(i))
                          read_addr_reg(i) := read_addr_reg(i) + n
                        }
                      }
                      when(calc_y > y + delay.U + array_H.U) {
                        // printf(p"Add $sum_reg at $write_addr_reg\n")
                        data(write_addr_reg) := data(write_addr_reg) + sum_reg
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
      mainCode.sig_en := start_reg
      mainCode.sig_reset := io.start

      // when(start_reg) {
      //   for (i <- 0 until array_H) {
      //     for (j <- 0 until array_W)
      //       printf(p"${cellArray(i)(j).vert_reg}")
      //     printf("\n")
      //   }
      //   printf("\n")
      // }

    }
    case _ => {}
  }

}
