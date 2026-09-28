package Stencils

import chisel3._
import chisel3.util.{Cat, MuxCase, ShiftRegister, is, log2Ceil, switch}
import java.io._
import FloatingPoint._
import FloatingPoint.fpu._



object dp {
  def main(args: Array[String]): Unit = {
    val dir = new File("verification/dut")
    dir.mkdirs()
    val sw2 = new PrintWriter(new File(dir, "sc_r3_k3_2d.sv"))
    sw2.println(circt.stage.ChiselStage.emitSystemVerilog(new SODA_2d(32, 2 ,8, 3,10,13,5,"cross")))
    sw2.close()
  }
}

object dp3 {
  def main(args: Array[String]): Unit = {
    val dir = new File("verification/dut")
    dir.mkdirs()
    val sw2 = new PrintWriter(new File(dir, "sc_r1_k2_3d.sv"))
    sw2.println(circt.stage.ChiselStage.emitSystemVerilog(new SODA_3d(32,2,8,8,7,1,10,13,7,1)))
    sw2.close()
  }
}

object StencilSchedule {

  def offsets3D(x: Int, y: Int): Seq[Int] = {
    Seq(
      -x * y,
      -x,
      -1,
      0,
      1,
      x,
      x * y
    )
  }

  def expandedOffsets3D(
    x: Int,
    y: Int,
    k: Int
  ): Seq[Int] = {

    require(k > 0, "k must be greater than zero")

    val offsets = offsets3D(x, y)

    (for {
      shift <- 0 until k
      point <- offsets
    } yield point + shift).distinct.sorted
  }

  def chains3D(
    x: Int,
    y: Int,
    k: Int
  ): Seq[Seq[Int]] = {

    val expandedOffsets =
      expandedOffsets3D(x, y, k)

    (0 until k).map { chainId =>
      expandedOffsets.filter { point =>
        Math.floorMod(point, k) == chainId
      }
    }
  }

  def fifoDepthsReversed3D(
    x: Int,
    y: Int,
    k: Int
  ): Seq[Seq[Int]] = {

    val chains =
      chains3D(x, y, k)

    chains.map { chain =>

      chain
        .sliding(2)
        .map {
          case Seq(a, b) =>

            val difference = b - a

            require(
              difference % k == 0,
              s"Offset difference $difference is not divisible by k=$k"
            )

            difference / k
        }
        .toSeq
        .reverse
    }
  }

  def maxChainDelay3D(
    x: Int,
    y: Int,
    k: Int
  ): Int = {

    val fifoDepths =
      fifoDepthsReversed3D(x, y, k)

    if (fifoDepths.isEmpty) {
      0
    } else {
      fifoDepths.map(_.sum).max
    }
  }

  def ceilDiv(a: Int, b: Int): Int = {
    (a + b - 1) / b
  }

}



object s_dual {
  def main(args: Array[String]): Unit = {
    val bw = 32
    val k = 48 // streaming width
    val x = 64 
    val y = 64
    val z = 64
    val r = 1
    val mult_pd = 7               
    val add_pd = 10
    val points = 7
    val time_steps = 5
    val dir = new File("verification/dut_64")
    dir.mkdirs()
    val sw2 = new PrintWriter(new File(dir, Seq(
      "soda_dual_test",
      s"k$k",
      s"x${x}_y${y}_z${z}",
      s"t${time_steps}",".sv"
    ).mkString("_")))
    sw2.println(circt.stage.ChiselStage.emitSystemVerilog(new SODA_dual_core(bw, k,x,y ,z,r,mult_pd,add_pd,points,time_steps)))
    sw2.close()
  }
}

object s_quad {
  def main(args: Array[String]): Unit = {
    val bw = 32
    val k = 56 // streaming width
    val x = 64  
    val y = 64
    val z = 64
    val r = 1
    val mult_pd = 7               
    val add_pd = 10
    val points = 7
    val time_steps = 5
    val dir = new File("verification/dut_multi_engine_64")
    dir.mkdirs()
    val sw2 = new PrintWriter(new File(dir, Seq(
      "soda_quad_test",
      s"k$k",
      s"x${x}_y${y}_z${z}",
      s"t${time_steps}",".sv"
    ).mkString("_")))
    sw2.println(circt.stage.ChiselStage.emitSystemVerilog(new SODA_quad_engine(bw, k,x,y ,z,r,mult_pd,add_pd,points,time_steps)))
    sw2.close()
  }
}

class SODA_quad_engine(

    bw: Int,
    k: Int,
    x: Int,
    y: Int,
    z: Int,
    r: Int,
    mult_pd: Int,
    add_pd: Int,
    points: Int,
    time_steps: Int

) extends Module {

  val number_of_engines = 4

  val io = IO(new Bundle() {

    val in_matrix_d = Input(Vec(number_of_engines, UInt((bw * k).W)))
    val in_weight_d = Input(Vec(number_of_engines, UInt((bw * points).W)))
    val in_ready_d = Input(Vec(number_of_engines, Bool()))

    val out_data_d = Output(Vec(number_of_engines, UInt((bw * k).W)))
    val out_valid_d = Output(Vec(number_of_engines, UInt(k.W)))

  })

  val engines = Seq.tabulate(number_of_engines) { i => 
  
    val e = Module(new SODA_dual_core(

      bw = bw, 
      k = k, 
      x = x, 
      y = y, 
      z = z, 
      r = r, 
      mult_pd = mult_pd, 
      add_pd = add_pd, 
      points = points, 
      time_steps = time_steps
    
    ))

    e.suggestName(s"engine_${i}")
    e

  }

   for (i <- 0 until number_of_engines) {

      engines(i).io.in_matrix_d := io.in_matrix_d(i)
      engines(i).io.in_weight_d := io.in_weight_d(i)
      engines(i).io.in_ready_d := io.in_ready_d(i)

      io.out_data_d(i) := engines(i).io.out_data_d
      io.out_valid_d(i) := engines(i).io.out_valid_d

    }

}

class SODA_dual_core(bw: Int, k: Int, x:Int, y:Int,z:Int, r: Int, mult_pd: Int, add_pd: Int, points: Int, time_steps:Int) extends Module {
  val io = IO(new Bundle() {

    val in_matrix_d = Input(UInt((bw * k).W))
    val in_weight_d = Input(UInt((bw * points).W))
    val in_ready_d = Input(Bool())
    val out_data_d = Output(UInt((bw * k).W))
    val out_valid_d = Output(UInt(k.W))

  })

  //override def desiredName: String =
  s"soda_dual_core_k${k}_x${x}_y${y}_z${z}_ts${time_steps}"

  val core_1 = Module(new SODA_3d(bw,k,x,y,z, r, mult_pd, add_pd, points, time_steps))
  val core_2 = Module(new SODA_3d(bw,k,x,y,z, r, mult_pd, add_pd, points, time_steps))

  val ADD_CC = 10   // FP_add latency
  val MULT_CC = 7  // FP_mult latency
  val ADD_TREE_CC = log2Ceil(points) * ADD_CC // latency of the cascading adders except for the last one
  val DOT_CC = MULT_CC + ADD_TREE_CC // latency of the full dot product
  val counter_c = RegInit(1.U(bw.W))
  val dram_load  = RegInit(true.B)
  val count_flag = RegInit(false.B)
  val input_size = StencilSchedule.ceilDiv((x*y*z)-2*(x+r), k)
  val pipeline_flush = StencilSchedule.ceilDiv(((x*y)-(r+x))+(x*y)+(2*(x+r)), k)
  // val core1_valid_latency = (x*y) + DOT_CC
  val maxChainDelay = StencilSchedule.maxChainDelay3D(x,y,k)
  val core1_valid_latency = maxChainDelay + DOT_CC
  val stage_latency = DOT_CC + pipeline_flush
  val delay_latency = math.max(1,input_size - (core1_valid_latency + stage_latency) + 1)  
  val core1_steps = (time_steps+1)/2
  val core2_steps = time_steps/2
  val core1_valid = Array.fill(core1_steps)(0)
  val core2_valid = Array.fill(core2_steps)(0)
  var wrap_cnt = 0
  val wrap_stride = 1
  core1_valid(0) = core1_valid_latency  // time step 1
  core2_valid(0) = core1_valid(0) + stage_latency + delay_latency   // time step 2
  wrap_cnt = (wrap_cnt +2)% k    // k moves by two strides for the first time step
  var core1_index = 1
  var core2_index = 1

  for(t <- 2 until time_steps) { // ping pong between the two cores
    if(t % 2 == 0){ // core1 turn
      if(wrap_cnt == 0){
        core1_valid(core1_index) = core2_valid(core2_index-1) + DOT_CC +  pipeline_flush + wrap_stride}
      else {
        core1_valid(core1_index) = core2_valid(core2_index-1)  + DOT_CC + pipeline_flush
      }
      wrap_cnt = (wrap_cnt + 1) % k
      core1_index += 1
    } else { // core2 turn
      if(wrap_cnt == 0){
        core2_valid(core2_index) = core1_valid(core1_index-1) + DOT_CC+ pipeline_flush + delay_latency + wrap_stride}
      else {
        core2_valid(core2_index) = core1_valid(core1_index-1) + DOT_CC+ pipeline_flush + delay_latency}
      wrap_cnt = (wrap_cnt + 1) % k
      core2_index += 1
    }
  }

  val first_valid =
    if (time_steps % 2 == 1) {
      core1_valid(core1_steps - 1)}
    else {
      core2_valid(core2_steps - 1)
    }

  val core1_out_select =
    if(time_steps % 2 == 1){
      true.B
    }else{
      false.B
    }

  val grid_counters = RegInit(VecInit((0 until k).map(_ => 0.U(bw.W))))
  val grid_counters_r = RegNext(RegNext(grid_counters))
  val ix = Wire(Vec(k, UInt(log2Ceil(x).W)))
  val iy = Wire(Vec(k, UInt(log2Ceil(y).W)))
  val iz = Wire(Vec(k, UInt(log2Ceil(z).W)))

  val grid_offset = (((time_steps + 1)/2) - 3).S  // needs to be parameterized per k
  val first_valid_signed = first_valid.asSInt     // Convert first_valid (UInt) to SInt
  val first_valid_offset = first_valid_signed + grid_offset

  // println(first_valid_unsigned)
  val offset = (r * time_steps).U
  val start_index = offset + offset * x.U + offset * x.U * y.U -1.U
  val last_index = (x*y*z - (r+x) - 1).U
  val lane_offset = (k - (time_steps % k)) % k // determine which lane is the first valid at
  val done_matrix = grid_counters(k-1) >= last_index
  val done_matrix_d = RegNext(done_matrix, false.B)
  val matrix_fi = RegInit(false.B)
  when(done_matrix_d) {
    matrix_fi := true.B
  }

  println(grid_offset)
  when((counter_c === first_valid_offset.asUInt-1.U) && !matrix_fi) { // grid count initilization
    for(i <- 0 until k) {
      grid_counters(i) := start_index + i.U + ((i+lane_offset) % k).U
    }
  }.elsewhen((counter_c > first_valid_offset.asUInt -1.U) && !matrix_fi) { // grid count until last point
    for(i <- 0 until k) {
      grid_counters(i) := grid_counters(i) + k.U
    }
  }

  for(i <- 0 until k) {
    ix(i) := grid_counters_r(i) % x.U
    iy(i) := (grid_counters_r(i)/x.U) % y.U
    iz(i) := grid_counters_r(i)/(x.U * y.U)
  }

  val lower_x = (r.asUInt*time_steps.asUInt)
  val upper_x = (x.asUInt - 1.U) - (r.asUInt*time_steps.asUInt)
  val lower_y = (r.asUInt*time_steps.asUInt)
  val upper_y = (y.asUInt - 1.U) - (r.asUInt*time_steps.asUInt)
  val lower_z = (r.asUInt*time_steps.asUInt)
  val upper_z = (z.asUInt - 1.U) - (r.asUInt*time_steps.asUInt)
  val valid_vec = Wire(Vec(k, Bool()))

  for(i <- 0 until k) {
    valid_vec(i) :=
      ix(i) >= lower_x && ix(i) <= upper_x &&
        iy(i) >= lower_y && iy(i) <= upper_y &&
        iz(i) >= lower_z && iz(i) <= upper_z
  }

  when(io.in_ready_d){count_flag := true.B}
  when(count_flag && !matrix_fi){counter_c := counter_c + 1.U}
  when(counter_c <= input_size.U-2.U){
    dram_load := true.B
  }.otherwise{
    dram_load := false.B
  }

  val input_counter = RegInit(0.U(bw.W))
  val dram_active   = RegInit(true.B)
  when(io.in_ready_d && dram_active && !matrix_fi){input_counter := input_counter + 1.U}
  when(input_counter === input_size.U){dram_active := false.B}
  when(matrix_fi){
    dram_active := false.B
    dram_load := false.B
  }
  val feedback_valid = core_2.io.out_valid
  val core1_input = Mux(dram_active, io.in_matrix_d, RegNext(core_2.io.out_data))
  val core1_valid_flag = Mux(dram_active, io.in_ready_d, feedback_valid)

  core_1.io.in_matrix := core1_input
  core_1.io.in_ready  := RegNext(core1_valid_flag)
  core_1.io.in_weight := io.in_weight_d
  // core_2.io.in_matrix := ShiftRegister(core_1.io.out_data,delay_latency)
  // core_2.io.in_ready  := RegNext(core_1.io.out_valid)
  // val core1ToCore2Delay =


  val core1ToCore2Delay =
       Module(new FIFO_r(bw * k, delay_latency))
    // Module(new FixedDelayFIFOChain( width = bw * k, totalDelay = delay_latency, maxFifoDepth = 32768))

  core1ToCore2Delay.io.in := core_1.io.out_data

  core_2.io.in_matrix := core1ToCore2Delay.io.out

  core_2.io.in_ready := RegNext(core_1.io.out_valid)

  // val core1DataDelay =
  //   Module(new FIFO_r(bw * k, delay_latency))

  // val core1ValidDelay =
  //   Module(new FIFO_r(1, delay_latency))

  // core1DataDelay.io.in :=
  //   core_1.io.out_data

  // core1ValidDelay.io.in :=
  //   core_1.io.out_valid.asUInt

  // core_2.io.in_matrix :=
  //   core1DataDelay.io.out

  // core_2.io.in_ready :=
  //   core1ValidDelay.io.out.asBool
  
  core_2.io.in_weight := io.in_weight_d
  io.out_data_d:= Mux(core1_out_select, core_1.io.out_data, core_2.io.out_data)
  io.out_valid_d := (valid_vec.asUInt)
  dontTouch(valid_vec)
}


class FixedDelayFIFOChain(
  width: Int,
  totalDelay: Int,
  maxFifoDepth: Int = 32768
) extends Module {

  val io = IO(new Bundle {
    val in  = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })

  require(totalDelay >= 0)
  require(maxFifoDepth > 0)

  def splitDelay(delay: Int): Seq[Int] = {
    if (delay == 0) {
      Seq.empty[Int]
    } else {
      val fullFifos = delay / maxFifoDepth
      val rem       = delay % maxFifoDepth

      Seq.fill(fullFifos)(maxFifoDepth) ++
        (if (rem > 0) Seq(rem) else Seq.empty[Int])
    }
  }

  val depths = splitDelay(totalDelay)

  if (depths.isEmpty) {
    io.out := io.in
  } else {
    val stages = depths.zipWithIndex.map {
      case (d, i) =>
        val fifo = Module(new FIFO_r(width, d))
        fifo.suggestName(s"fixed_delay_stage${i}_d${d}")
        fifo
    }

    stages.head.io.in := io.in

    for (i <- 1 until stages.length) {
      stages(i).io.in := stages(i - 1).io.out
    }

    io.out := stages.last.io.out
  }
}


class fifo_d_beh(val bw: Int, val depth: Int) extends BlackBox {

  override def desiredName: String =
    s"fifo_d_beh_b${bw}_d${depth}"
  val io = IO(new Bundle {

    val clk = Input(Clock())
    val srst = Input(Bool())

    val din  = Input(UInt(bw.W))
    val wr_en = Input(Bool())
    val rd_en = Input(Bool())

    val dout = Output(UInt(bw.W))
    val full = Output(Bool())
    val empty = Output(Bool())

    val wr_rst_busy = Output(Bool())
    val rd_rst_busy = Output(Bool())
  })
}

class FIFO_r(val bw: Int, val depth: Int) extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(bw.W))
    val out = Output(UInt(bw.W))
  })

  if (depth >= 16){  //64 -> 
    val fifo = Module(new fifo_d_beh(bw, depth))

    fifo.io.clk := clock
    fifo.io.srst := reset.asBool

    fifo.io.din := io.in

    val count = RegInit(0.U(log2Ceil(depth + 1).W))
    val filled = count === depth.U

    fifo.io.wr_en := true.B
    fifo.io.rd_en := filled

    when (!filled && !fifo.io.full){
      count := count + 1.U
    }

    io.out := Mux(filled, fifo.io.dout, 0.U)
  } else {

  val regs = RegInit(VecInit(Seq.fill(depth)(0.U(bw.W))))
  regs(0) := io.in
  for(i <- 1 until depth){
    regs(i) := regs(i-1)
  }
  io.out := regs.last
  
  }

}



class SODA_3d(
  bw: Int,
  k: Int,
  x: Int,
  y: Int,
  z: Int,
  r: Int,
  mult_pd: Int,
  add_pd: Int,
  points: Int,
  time_steps: Int
) extends Module {

  val io = IO(new Bundle {

    val in_matrix = Input(UInt((bw * k).W))

    val in_weight = Input(UInt((bw * points).W))

    val in_ready = Input(Bool())

    val out_data = Output(UInt((bw * k).W))
      
    val out_valid =  Output(Bool())
     
  })

  val ADD_CC  = 10
  val MULT_CC = 7

  val ADD_TREE_CC = log2Ceil(points) * ADD_CC
    

  val DOT_CC = MULT_CC + ADD_TREE_CC

  val W_SIZE = points

  val init_g = x + r

  val t = RegInit(1.U(bw.W))
    
  /*
   * Store stencil weights.
   */
  val weights = Reg(Vec(W_SIZE, UInt(bw.W)))
    

  for (i <- 0 until W_SIZE) {

    weights(i) :=
      io.in_weight(
        W_SIZE * bw - (i * bw) - 1,
        W_SIZE * bw - (bw * (i + 1))
      )
  }

  val counter = RegInit(0.U(bw.W))
    

  val count_f = RegInit(false.B)
    

  when(io.in_ready) {
    count_f := true.B
  }

  when(count_f) {
    counter := counter + 1.U
  }

  /*
   * One dot-product unit is created for every stream lane.
   *
   * k = 4 means four dot-product units.
   */
  val scs =
    Seq.fill(k) {
      Module(
        new dot(
          bw,
          points,
          mult_pd,
          add_pd
        )
      )
    }

  /*
   * Original seven-point stencil offsets.
   */
  val a: Seq[Int] =
    StencilSchedule.offsets3D(x, y)

  /*
   * Expanded offsets for k parallel lanes.
   */
  val a_k: Seq[Int] =
    StencilSchedule.expandedOffsets3D(
      x,
      y,
      k
    )

  /*
   * Divide expanded offsets into k modulus chains.
   */
  val chains: Seq[Seq[Int]] =
    StencilSchedule.chains3D(
      x,
      y,
      k
    )

  /*
   * Calculate FIFO/shift-register depths.
   */
  val fifo_depths_reversed: Seq[Seq[Int]] =
    StencilSchedule.fifoDepthsReversed3D(
      x,
      y,
      k
    )

  fifo_depths_reversed.zipWithIndex.foreach {
    case (depths, chainId) =>
      println(
        s"Chain $chainId depths = $depths"
      )
  }

  /*
   * Longest complete delay chain.
   *
   * k = 2, x = 8, y = 8:
   * maxChainDelay = 64
   *
   * k = 4, x = 8, y = 8:
   * maxChainDelay = 32
   */
  val maxChainDelay =
    fifo_depths_reversed
      .map(_.sum)
      .max

  println(
    s"Maximum chain delay = $maxChainDelay"
  )

  /*
   * Output-valid latency:
   *
   * delay-line latency + dot-product latency
   */

  io.out_valid :=
    ShiftRegister(
      io.in_ready,
      maxChainDelay + DOT_CC
    )

  // val outValidDelay =
  //   Module(new FIFO_r(1, DOT_CC + maxChainDelay))

  // outValidDelay.io.in := io.in_ready.asUInt
  // io.out_valid := outValidDelay.io.out.asBool

  /*
   * Remove the final k expanded offsets because those values are
   * directly supplied from the current input lanes.
   */
  val a_k_trimmed = a_k.dropRight(k)

  /*
   * Map every FIFO output to its corresponding stencil offset.
   */
  val offset_map: Seq[Seq[Int]] =
    chains.map { chain =>
      chain
        .reverse
        .filter(a_k_trimmed.contains)
    }

  val fifo_offset_map = scala.collection.mutable.Map[Int, FIFO_r]()

  println("---- FIFO chain debug ----")

  chains.zipWithIndex.foreach {
    case (chain, chainId) =>
      println(s"chain $chainId        = ${chain.mkString("[", ", ", "]")}")
      println(s"chain $chainId reverse= ${chain.reverse.mkString("[", ", ", "]")}")
      println(s"offset_map $chainId   = ${offset_map(chainId).mkString("[", ", ", "]")}")
      println(s"depths $chainId       = ${fifo_depths_reversed(chainId).mkString("[", ", ", "]")}")
  }

  for (chainId <- chains.indices) {
  require(
    offset_map(chainId).length == fifo_depths_reversed(chainId).length,
    s"chain $chainId: offset_map length does not match FIFO depth list length"
  )
}

  /*
   * Construct all FIFO/shift-register delay elements.
   */
  val fifos: Seq[Seq[FIFO_r]] =
    fifo_depths_reversed.zipWithIndex.map {

      case (depths, chainId) =>

        depths.zipWithIndex.map {

          case (depth, fifoIndex) =>

            val offsetForName =
              offset_map(chainId)(fifoIndex)

            val fifo =
              Module(
                new FIFO_r(
                  bw,
                  depth
                )
              )

            fifo.suggestName(
              s"chain${chainId}_fifo${offsetForName}"
            )

            fifo_offset_map(offsetForName) =
              fifo

            fifo
        }
    }

  val fifo_map: Map[Int, FIFO_r] =
    fifo_offset_map.toMap

  /*
   * Connect delay-line taps to each dot-product unit.
   */
  scs.zipWithIndex.foreach {

    case (sc, scId) =>

      val sc_offsets = a.map(_ + scId)
        
      val fifo_offsets =  sc_offsets.dropRight(1)
       
      fifo_offsets.zipWithIndex.foreach {

        case (offset, inputIndex) =>

          sc.io.in_matrix(inputIndex) :=
            fifo_map(offset).io.out

          sc.io.in_weight(inputIndex) :=
            weights(inputIndex)
      }

      /*
       * The final stencil input comes directly from the current
       * input lane.
       */
      val lastInput = fifo_offsets.length
        

      sc.io.in_matrix(lastInput) :=
        io.in_matrix(
          k * bw - (scId * bw) - 1,
          k * bw - (bw * (scId + 1))
        )

      sc.io.in_weight(lastInput) := weights(lastInput)
        
  }

  /*
   * Connect each input lane to its corresponding delay chain.
   */
  for (chainId <- fifos.indices) {

    for (fifoIndex <- fifos(chainId).indices) {

      if (fifoIndex == 0) {

        fifos(chainId)(fifoIndex).io.in :=
          io.in_matrix(
            k * bw - (chainId * bw) - 1,
            k * bw - (bw * (chainId + 1))
          )

      } else {

        fifos(chainId)(fifoIndex).io.in :=
          fifos(chainId)(fifoIndex - 1).io.out
      }
    }
  }

  /*
   * Collect the k dot-product results.
   */
  val sc_outputs =  Wire(Vec(k, UInt(bw.W)))
   
  for (i <- scs.indices) {

    sc_outputs(
      math.abs(i - (k - 1))
    ) := scs(i).io.out_data
  }

  io.out_data :=
    sc_outputs.asUInt
}

class SODA_2d(bw: Int, k: Int, m:Int, r:Int, mult_pd: Int, add_pd: Int, points: Int, shape: String) extends Module {
  val io = IO(new Bundle() {
    val in_matrix = Input(UInt((bw * k).W))
    val in_weight = Input(UInt((bw *points).W))
    val in_ready = Input(Bool())
    val out_data = Output(UInt((bw * k).W))
    val out_valid = Output(Bool())
  })
  
  val d = 2*r+1
  val ADD_CC = 10
  val MULT_CC = 7
  val ADD_TREE_CC = log2Ceil(points - 1) * ADD_CC // latency of the cascading adders except for the last one
  val DOT_CC = MULT_CC + ADD_TREE_CC + ADD_CC // latency of the full dot product
  val LATENCY = ((d-1)*(m/k))+DOT_CC
  val W_SIZE = points
  val weights = Reg(Vec(W_SIZE, UInt(bw.W)))
  for(i <- 0 until (W_SIZE)){
    weights(i) := io.in_weight(W_SIZE * bw - (i*bw) - 1, (W_SIZE * bw - (bw*(i+1))))
  }
  io.out_valid := ShiftRegister(io.in_ready, LATENCY)

  // gen the compute units
  val scs = Seq.fill(k) {
    Module(new dot(bw, points, mult_pd, add_pd))
  }
  // gen starting sequence
  val a: Seq[Int] = Seq(-m, -1, 0, 1, m) // cross
  val a_k = (for {shift <- 0 until k    // expand based on k
                  point <- a} yield point + shift).distinct.sorted
  val chains =
    for (i <- 0 until k)
      yield a_k.iterator.filter(point => ((point % k + k) % k) == i).toSeq // gen the chains based on modulo
  val fifo_depths_reversed: Seq[Seq[Int]] =
    chains.map { chain =>
      chain.sliding(2).map { case Seq(a, b) => (b - a) / k }.toSeq.reverse}
  val a_k_trimmed =  a_k.dropRight(k)
  val offset_map: Seq[Seq[Int]] = chains.map { chain => chain.reverse.filter(a_k_trimmed.contains) }
  val fifo_offset_map = scala.collection.mutable.Map[Int, FIFO_r]()
  val fifos: Seq[Seq[FIFO_r]] =
    fifo_depths_reversed.zipWithIndex.map { case (depths, chainid) =>
      depths.zipWithIndex.map { case (depth, i) =>
        val offsetForName = offset_map(chainid)(i)
        val fifo = Module(new FIFO_r(bw, depth))
        fifo.suggestName(s"chain${chainid}_fifo${offsetForName}")
        fifo_offset_map(offsetForName) = fifo
        fifo
      }
    }

  val fifo_map: Map[Int, FIFO_r] = fifo_offset_map.toMap
  scs.zipWithIndex.foreach { case (sc, sc_id) =>
    val sc_offsets = a.map(_ + sc_id)
    val fifo_offsets = sc_offsets.dropRight(1).filter(a_k_trimmed.contains)
    val chain_offset = sc_offsets.last // last one
    fifo_offsets.zipWithIndex.foreach { case (offset, in) =>
      sc.io.in_matrix(in) := fifo_map(offset).io.out
      sc.io.in_weight(in) := weights(in)
    }
    val last_input = fifo_offsets.length
    sc.io.in_matrix(last_input) := io.in_matrix(k * bw - (sc_id * bw) - 1, (k * bw - (bw * (sc_id + 1))))
    sc.io.in_weight(last_input) := weights(last_input)
  }

  val sc_outputs = Wire(Vec(k, UInt(bw.W)))

  for (i <- fifos.indices) { // i index of chain
    for (j <- fifos(i).indices) { // j index of fifo inside chain
      if (j == 0) {
        fifos(i)(j).io.in := io.in_matrix(k*bw-(i*bw) - 1, (k*bw - (bw*(i+1))))
      }
      else {
        fifos(i)(j).io.in := fifos(i)(j - 1).io.out
      }
    }
  }

  for (i <- scs.indices) {
    sc_outputs(math.abs(i-(k-1))) := scs(i).io.out_data
  }
  io.out_data := sc_outputs.asUInt
}

class  dot(bw: Int, sw: Int, mult_pd: Int, add_pd: Int ) extends Module{

  val io = IO(new Bundle(){
    val in_matrix = Input(Vec(sw, UInt(bw.W)))
    val in_weight = Input(Vec(sw, UInt(bw.W)))
    val out_data = Output(UInt(bw.W))
  })

  val ADD_CC = 10
  val MULT_CC = 7
  val ADD_TREE_CC = log2Ceil(sw) * ADD_CC
  val DOT_CC = MULT_CC + ADD_TREE_CC

  val matrix = (0 until sw).map(i =>
    Cat(io.in_matrix).asUInt(sw*bw-(i*bw)-1, (sw*bw - (bw*(i+1))))
  )

  val weights = (0 until sw).map(i =>
    Cat(io.in_weight).asUInt(sw*bw-(i*bw)-1, (sw*bw - (bw*(i+1))))
  )

  val terms = (0 until sw).map { i =>

    // FP multiplier
    val mult = Module(new FP_mult(FP32, mult_pd))

    // Always-ready streaming mode
    mult.io.in_valid := true.B
    mult.io.out_ready := true.B

    mult.io.in_a := matrix(i)
    mult.io.in_b := weights(i)

    mult.io.out_s
  }

  // Reduction tree of FP adders
  def Redux_Tree(inputs: Seq[UInt], depth: Int = 0): UInt = {

    if(inputs.length == 1) {
      inputs.head
    }

    else {

      val pairs = inputs.grouped(2).toList

      val next = pairs.map {

        case Seq(a, b) =>

          // FP adder
          val add = Module(new FP_add(FP32, add_pd))

          // Always-ready streaming mode
          add.io.in_valid := true.B
          add.io.out_ready := true.B

          add.io.in_a := a
          add.io.in_b := b

          add.io.out_s

        case Seq(single) =>

          ShiftRegister(single, ADD_CC)
      }

      Redux_Tree(next)
    }
  }

  io.out_data := Redux_Tree(terms)
}


class jacobi_3d(bw: Int, sw: Int, mult_pd: Int, add_pd: Int ) extends Module{

  val io = IO(new Bundle(){
    val in_matrix = Input(Vec(sw, UInt(bw.W)))
    val in_weight = Input(Vec(sw, UInt(bw.W)))
    val out_data = Output(UInt(bw.W))
  })

  val ADD_CC = 10
  val MULT_CC = 7
  val ADD_TREE_CC = log2Ceil(sw) * ADD_CC  // 3 * 10 = 30
  val DOT_CC = MULT_CC + ADD_TREE_CC       // 7 + 30 = 37

  val matrix = (0 until sw).map(i =>
    Cat(io.in_matrix).asUInt(sw*bw-(i*bw)-1, (sw*bw - (bw*(i+1))))
  )

  // val weights = (0 until sw).map(i =>
  //   Cat(io.in_weight).asUInt(sw*bw-(i*bw)-1, (sw*bw - (bw*(i+1))))
  // )
  

  // Reduction tree of FP adders
  def Redux_Tree(inputs: Seq[UInt], depth: Int = 0): UInt = {

    if(inputs.length == 1) {
      inputs.head
    }

    else {

      val pairs = inputs.grouped(2).toList

      val next = pairs.map {

        case Seq(a, b) =>

          // FP adder
          val add = Module(new FP_add(FP32, add_pd))

          // Always-ready streaming mode
          add.io.in_valid := true.B
          add.io.out_ready := true.B

          add.io.in_a := a
          add.io.in_b := b

          add.io.out_s

        case Seq(single) =>

          ShiftRegister(single, ADD_CC)
      }

      Redux_Tree(next)
    }
  }

    val sum_7_terms = Redux_Tree(matrix)

    val one_by_7 = "h3E124925".U(32.W)  // 1/7 in FP32

    // FP multiplier
    val mult = Module(new FP_mult(FP32, mult_pd))

    // Always-ready streaming mode
    mult.io.in_valid := true.B
    mult.io.out_ready := true.B

    mult.io.in_a := sum_7_terms
    mult.io.in_b := one_by_7

    mult.io.out_s

  io.out_data := mult.io.out_s
}