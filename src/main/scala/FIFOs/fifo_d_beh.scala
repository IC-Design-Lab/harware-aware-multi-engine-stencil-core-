package fifo_behavior

import chisel3._
import chisel3.util.{Cat, MuxCase, ShiftRegister, is, log2Ceil, switch}
import java.io._

/*
 * Generates the behavioral SystemVerilog FIFO used for ModelSim.
 *
 * Arguments:
 *   args(0) = data width
 *   args(1) = delay depth
 *
 * Examples:
 *   runMain fifo_behavior.fifo_d_beh_gen 32 28
 *   runMain fifo_behavior.fifo_d_beh_gen 32 120
 */
object fifo_d_beh_gen {

  def main(args: Array[String]): Unit = {

    val bw =
      args.lift(0).map(_.toInt).getOrElse(32)

    val depth =
      args.lift(1).map(_.toInt).getOrElse(28)

    val dir = new File("verification/fifos/size 8 - k - 2")
    dir.mkdirs()

    /*
     * The same filename is overwritten each time.
     *
     */
    val outputFile =
      new File(dir, "fifo_d_beh.sv")

    val writer = new PrintWriter(outputFile)

    try {
      writer.println(
        circt.stage.ChiselStage.emitSystemVerilog(
          new fifo_d_beh(
            bw = bw,
            depth = depth
          )
        )
      )
    } finally {
      writer.close()
    }

    println(
      s"Generated ${outputFile.getPath}: bw=$bw, delay=$depth"
    )
  }
}

/*
 * Fixed-delay FIFO behavioral model.
 *
 * This module is intended for large stencil delays.
 *
 * The FIFO first stores 'depth' input values.
 * After that initial filling phase, one old value is produced and
 * one new value is stored on each advancing cycle.
 *
 * With continuous wr_en and rd_en:
 *
 *   dout[n] = din[n - depth]
 */
/***class fifo_d_beh(
  val bw: Int,
  val depth: Int
) extends Module {



  val io = IO(new Bundle {

    val din   = Input(UInt(bw.W))
    val wr_en = Input(Bool())
    val rd_en = Input(Bool())

    val dout  = Output(UInt(bw.W))

    val full  = Output(Bool())
    val empty = Output(Bool())

    val wr_rst_busy = Output(Bool())
    val rd_rst_busy = Output(Bool())
  })

  /*
   * Asynchronous-read memory gives first-word-fall-through-like
   * behavior and avoids adding another output-register cycle.
   */
  val mem = Mem(depth, UInt(bw.W))

  /*
   * Circular memory pointer.
   *
   * It identifies:
   *   1. the oldest stored value
   *   2. the location overwritten by the newest input
   */
  val ptrWidth =
    math.max(1, log2Ceil(depth))

  val ptr =
    RegInit(0.U(ptrWidth.W))

  /*
   * Counts the initial number of accepted inputs.
   *
   * It stops counting when the required delay has been filled.
   */
  val fillCountWidth =
    math.max(1, log2Ceil(depth + 1))

  val fillCount =
    RegInit(0.U(fillCountWidth.W))

  val primed =
    fillCount === depth.U

  /*
   * During initial filling:
   *   a write advances the FIFO even when rd_en is low.
   *
   * After filling:
   *   write and read must occur together so the FIFO continues
   *   containing exactly 'depth' values.
   */
  val advance =
    io.wr_en && (!primed || io.rd_en)

  /*
   * empty remains high until 'depth' values have been received.
   */
  io.empty := !primed

  /*
   * Once the delay is filled, a new input cannot be accepted unless
   * the delayed output is also being consumed.
   *
   * Here, full means:
   *
   *   "This delay unit cannot advance in the current cycle."
   */
  io.full :=
    primed && !io.rd_en

  /*
   * Memory contents are not valid during the initial filling phase,
   * so dout is forced to zero until the FIFO has been primed.
   */
  io.dout :=
    Mux(
      primed,
      mem(ptr),
      0.U
    )

  /*
   * The behavioral model does not model the reset recovery delay
   * of the physical Vivado FIFO IP.
   */
  io.wr_rst_busy := false.B
  io.rd_rst_busy := false.B

  when(advance) {

    /*
     * Before this clock edge, mem(ptr) contains the oldest value.
     *
     * At this clock edge, that location is overwritten with the
     * newest input value.
     */
    mem(ptr) := io.din

    /*
     * Circular pointer increment.
     */
    ptr := Mux(
      ptr === (depth - 1).U,
      0.U,
      ptr + 1.U
    )

    /*
     * fillCount is updated only during startup.
     *
     * After reaching depth, it remains constant.
     */
    when(!primed) {
      fillCount := fillCount + 1.U
    }
  }
} */

class fifo_d_beh(
  val bw: Int,
  val depth: Int
) extends Module {

  override def desiredName: String =
    s"fifo_d_beh_b${bw}_d${depth}"

  require(depth > 0)

  val io = IO(new Bundle {
    val din   = Input(UInt(bw.W))
    val wr_en = Input(Bool())
    val rd_en = Input(Bool())

    val dout  = Output(UInt(bw.W))

    val full  = Output(Bool())
    val empty = Output(Bool())

    val wr_rst_busy = Output(Bool())
    val rd_rst_busy = Output(Bool())
  })

  val ptrWidth =
    math.max(1, log2Ceil(depth))

  val countWidth =
    math.max(1, log2Ceil(depth + 1))

  val mem =
    Mem(depth, UInt(bw.W))

  val wrPtr =
    RegInit(0.U(ptrWidth.W))

  val rdPtr =
    RegInit(0.U(ptrWidth.W))

  val count =
    RegInit(0.U(countWidth.W))

  def inc(ptr: UInt): UInt =
    Mux(ptr === (depth - 1).U, 0.U, ptr + 1.U)

  io.empty := count === 0.U
  io.full  := count === depth.U

  // FWFT behavior: oldest value is visible whenever FIFO is not empty.
  io.dout := Mux(io.empty, 0.U, mem(rdPtr))

  val doRead =
    io.rd_en && !io.empty

  val doWrite =
    io.wr_en && (!io.full || doRead)

  when (doWrite) {
    mem(wrPtr) := io.din
    wrPtr := inc(wrPtr)
  }

  when (doRead) {
    rdPtr := inc(rdPtr)
  }

  when (doWrite && !doRead) {
    count := count + 1.U
  } .elsewhen (!doWrite && doRead) {
    count := count - 1.U
  }

  io.wr_rst_busy := false.B
  io.rd_rst_busy := false.B
}