package FloatingPoint
import Primitives.primitives._
import Primitives.convert._
import chisel3._
import chisel3.util.{Counter, ShiftRegister, log2Ceil}

object fpu {

  // Digit recurrence based division
  class FP_div(FORMAT: FloatingPointFormat, L: Int, latency: Int) extends FPModule(FORMAT) {
    val io = IO(new Bundle() {
      val in_ready = Output(Bool())
      val out_ready = Input(Bool())
      val in_valid = Input(Bool())
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val out_s = Output(UInt(bw.W))
      val out_valid = Output(Bool())
    })
    override def desiredName = s"${FORMAT}_divider_${L}_${latency}"
    // Instantiate divider first to get its ready signal
    val frac_divider = Module(new divider((mantissa + 2), L, latency, true)).io
    frac_divider.out_ready := io.out_ready

    // Pipeline enable derived from divider's ready
    val pipe_enable = frac_divider.in_ready

    // Propagate ready signal
    io.in_ready := frac_divider.in_ready

    // get the sign bit of the two inputs
    val sign_wire = Wire(Vec(2, UInt(1.W)))
    sign_wire(0) := io.in_a(bw - 1)
    sign_wire(1) := io.in_b(bw - 1)

    // get the exponents of the two inputs
    val exp_wire = Wire(Vec(2, UInt(exponent.W)))
    when(io.in_a(bw - 2, mantissa) > max_exp) {
      exp_wire(0) := max_exp
    }.elsewhen(io.in_a(bw-2,mantissa) < min_exp){ // saturating inputs
      exp_wire(0) := min_exp
    }.otherwise {
      exp_wire(0) := io.in_a(bw - 2, mantissa)
    }
    when(io.in_b(bw - 2, mantissa) > max_exp) {
      exp_wire(1) := max_exp
    }.elsewhen(io.in_b(bw-2,mantissa) < min_exp){
      exp_wire(1) := min_exp
    }.otherwise {
      exp_wire(1) := io.in_b(bw - 2, mantissa)
    }

    // get the mantissa parts of the two inputs
    val frac_wire = Wire(Vec(2, UInt(mantissa.W)))
    frac_wire(0) := io.in_a(mantissa - 1, 0)
    frac_wire(1) := io.in_b(mantissa - 1, 0)

    // 1.0 + mantissa part of the two numbers
    val whole_frac_wire = Wire(Vec(2, UInt((mantissa + 1).W)))
    whole_frac_wire(0) := 1.U ## frac_wire(0)
    whole_frac_wire(1) := 1.U ## frac_wire(1)

    val new_sign_wire = Wire(UInt(1.W))
    new_sign_wire := sign_wire(0) ^ sign_wire(1)

    // compute result exponent: exp_a - exp_b + bias = exp_a - (exp_b - bias)
    val postProcess_exp_subtractor = Module(new full_subtractor(exponent))
    postProcess_exp_subtractor.io.in_a := exp_wire(0) // dividend exponent
    postProcess_exp_subtractor.io.in_b := exp_wire(1) - bias // divisor exponent minus bias
    postProcess_exp_subtractor.io.in_c := 0.U

    frac_divider.in_valid := io.in_valid
    frac_divider.in_a := whole_frac_wire(0) ## (0.U((1).W))
    frac_divider.in_b := whole_frac_wire(1) ## (0.U((1).W))

    val uo_check = ShiftRegister(exp_wire(1) < bias, latency, pipe_enable) //  if yes means we add to exp(0) else we do regular subtraction
    val carry_flag = ShiftRegister(postProcess_exp_subtractor.io.out_c.asBool, latency, pipe_enable) // if(y0check) then lookout for cflag low implies overflow, else cflag high implies underflow
    val msb_check = frac_divider.out_s((mantissa + 1))
    val exp_sum = ShiftRegister(postProcess_exp_subtractor.io.out_s, latency, pipe_enable)

    val new_sign_reg = ShiftRegister(new_sign_wire,latency,pipe_enable)


    val o_flag_reg = Mux(uo_check,!carry_flag || ((exp_sum -& (!msb_check).asUInt) > max_exp), false.B) // if true, overflow detected
    val u_flag_reg = Mux(!uo_check,carry_flag || ((exp_sum -& (!msb_check).asUInt) < min_exp), false.B) // if true, underflow detected
    val new_exp_reg = exp_sum
    val new_frac_reg = frac_divider.out_s

    io.out_valid := frac_divider.out_valid
    val out_sign = new_sign_reg
    val out_exp = Mux(o_flag_reg, max_exp, Mux(u_flag_reg, min_exp, Mux(msb_check, new_exp_reg, new_exp_reg - 1.U)))
    val out_frac = Mux(o_flag_reg, max_frac, Mux(u_flag_reg, min_frac,Mux(msb_check, new_frac_reg(mantissa, 1), new_frac_reg((mantissa - 1), 0))))
    io.out_s := out_sign ## out_exp ## out_frac
  }

  // digit recurrence based fp square root
  class FP_sqrt(FORMAT: FloatingPointFormat, L: Int, latency: Int) extends FPModule(FORMAT) {
    require(L <= mantissa)
    val io = IO(new Bundle() {
      val in_ready = Output(Bool())
      val out_ready = Input(Bool())
      val in_valid = Input(Bool())
      val in_a = Input(UInt(bw.W))
      val out_s = Output(UInt(bw.W))
      val out_valid = Output(Bool())
    })
    override def desiredName = s"${FORMAT}_sqrt_${L}_${latency}"

    // Instantiate sqrt first to get its ready signal
    val fsqrt = Module(new frac_sqrt(mantissa,L, latency)).io
    fsqrt.out_ready := io.out_ready

    // Pipeline enable derived from sqrt's ready
    val pipe_enable = fsqrt.in_ready

    // Propagate ready signal
    io.in_ready := fsqrt.in_ready

    val sign = io.in_a(bw - 1)
    val exp = io.in_a(bw-2, mantissa) -& bias
    val frac = 1.U(2.W) ## io.in_a(mantissa - 1, 0)

    val exp_new = Mux(exp(0).asBool,(exp - 1.U) >> 1, exp >> 1).asUInt

    val ref_frac = Mux(exp(0).asBool, frac << 1, frac).asUInt

    fsqrt.in_valid := io.in_valid
    fsqrt.in_a := ref_frac

    val out_sign = ShiftRegister(sign, latency, pipe_enable)
    val out_exp = ShiftRegister(exp_new, latency, pipe_enable) + bias
    val out_frac = fsqrt.out_s

    io.out_valid := fsqrt.out_valid
    io.out_s := out_sign ## out_exp(exponent - 1, 0) ## out_frac
  }

  // multiplier
  class FP_mult(FORMAT: FloatingPointFormat, latency: Int) extends FPModule(FORMAT) {
    require(latency >= 1, "Pipeline depth must be at least 1")
    val io = IO(new Bundle() {
      val in_ready = Output(Bool())
      val out_ready = Input(Bool())
      val in_valid = Input(Bool())
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val out_s = Output(UInt(bw.W))
      val out_valid = Output(Bool())
    })
    override def desiredName = s"${FORMAT}_mult_${latency}"

    // 10 logical pipeline stage boundaries between combinational blocks:
    // 0: input registration
    // 1: after field extraction / before sign computation
    // 2: after sign XOR / before exponent add
    // 3: after exponent add / before fraction multiply
    // 4: after fraction multiply / before overflow/underflow check
    // 5: after overflow/underflow signals
    // 6: after flag computation
    // 7: after normalization mux
    // 8: after overflow saturation mux
    // 9: after underflow saturation / output registration
    
    private val N_STAGES = 10
    private val pipe_skip = if (latency >= N_STAGES) 1 else N_STAGES / latency
    val sr_array = Array.fill(N_STAGES)(0)
    for (i <- 0 until latency) {
      sr_array((i * pipe_skip) % N_STAGES) += 1
    }

    // Track valid through pipeline using shift register
    val out_valid_reg = ShiftRegister(io.in_valid && io.in_ready, latency, io.out_ready || !io.out_valid)
    io.out_valid := out_valid_reg

    // Pipeline enable: advance when output is consumed or no valid output
    val pipe_enable = io.out_ready || !io.out_valid

    // Ready when pipeline can advance
    io.in_ready := pipe_enable

    val in_a = ShiftRegister(io.in_a, sr_array(0) , pipe_enable)
    val in_b = ShiftRegister(io.in_b, sr_array(0), pipe_enable)

    val sign_wire = Wire(Vec(2, UInt(1.W)))
    sign_wire(0) := in_a(bw - 1)
    sign_wire(1) := in_b(bw - 1)

    // get the exponents of the two inputs
    val exp_wire = Wire(Vec(2, UInt(exponent.W)))
    when(in_a(bw - 2, mantissa) > max_exp) {
      exp_wire(0) := max_exp
    }.elsewhen(in_a(bw-2,mantissa) < min_exp){ // saturating inputs
      exp_wire(0) := min_exp
    }.otherwise {
      exp_wire(0) := in_a(bw - 2, mantissa)
    }
    when(in_b(bw - 2, mantissa) > max_exp) {
      exp_wire(1) := max_exp
    }.elsewhen(in_b(bw-2,mantissa) < min_exp){
      exp_wire(1) := min_exp
    }.otherwise {
      exp_wire(1) := in_b(bw - 2, mantissa)
    }

    // get the mantissa parts of the two inputs
    val frac_wire = Wire(Vec(2, UInt(mantissa.W)))
    frac_wire(0) := in_a(mantissa - 1, 0)
    frac_wire(1) := in_b(mantissa - 1, 0)

    // 1.0 + mantissa part of the two numbers
    val whole_frac_wire = Wire(Vec(2, UInt((mantissa + 1).W)))
    whole_frac_wire(0) := 1.U ## frac_wire(0)
    whole_frac_wire(1) := 1.U ## frac_wire(1)

    // delays sign, exponent, and mantissa to align with multiplier output
    val whole_frac_sr = ShiftRegister(whole_frac_wire, sr_array(1), pipe_enable)
    val sign_sr = ShiftRegister(sign_wire, sr_array(1), pipe_enable)
    val exp_sr = ShiftRegister(exp_wire, sr_array(1), pipe_enable)

    // sign xor, floating point multiplication rule
    val new_sign_wire = Wire(UInt(1.W))
    new_sign_wire := sign_sr(0) ^ sign_sr(1)

    // align sign, exponent and mantissa before exponent addition
    val new_sign_sr = ShiftRegister(new_sign_wire, sr_array(2), pipe_enable)
    val exp_sr_2 = ShiftRegister(exp_sr, sr_array(2), pipe_enable)
    val whole_frac_sr_2 = ShiftRegister(whole_frac_sr, sr_array(2), pipe_enable)

    // FP multiplication: exponent formula exp_out = exp_a + exp_b - bias
    val exp_adder = Module(new full_adder(exponent))
    exp_adder.io.in_a := exp_sr_2(0)
    exp_adder.io.in_b := exp_sr_2(1) - bias
    exp_adder.io.in_c := 0.U

    // registers the outputs of results of exp_adder, sign, original exp, and whole fractions before mantissa multiplication
    val exp_adder_outs_sr = ShiftRegister(exp_adder.io.out_s, sr_array(3), pipe_enable)
    val exp_adder_outc_sr = ShiftRegister(exp_adder.io.out_c, sr_array(3), pipe_enable)
    val new_sign_sr_2 = ShiftRegister(new_sign_sr, sr_array(3), pipe_enable)
    val exp_sr_3 = ShiftRegister(exp_sr_2, sr_array(3), pipe_enable)
    val whole_frac_sr_3 = ShiftRegister(whole_frac_sr_2, sr_array(3), pipe_enable)

    // mantissa multiplication
    val frac_multiplier = Module(new multiplier((mantissa + 1))).io
    frac_multiplier.in_a := whole_frac_sr_3(0)
    frac_multiplier.in_b := whole_frac_sr_3(1)

    // Stage 4 registers | align mantissa product, exponent sum, carry flag | sign | original exponents
    val frac_mult_out_sr = ShiftRegister(frac_multiplier.out_s, sr_array(4), pipe_enable)
    val exp_adder_outs_sr_2 = ShiftRegister(exp_adder_outs_sr, sr_array(4), pipe_enable)
    val exp_adder_outc_sr_2 = ShiftRegister(exp_adder_outc_sr, sr_array(4), pipe_enable)
    val new_sign_sr_3 = ShiftRegister(new_sign_sr_2, sr_array(4), pipe_enable)
    val exp_sr_4 = ShiftRegister(exp_sr_3, sr_array(4), pipe_enable)

    // Overflow/ Underflow prep | checks exp_b < bias | resgisters carry out of exp addition | registers exp_sum | sign | registers full mantissa multiplication result
    val uo_check = ShiftRegister(exp_sr_4(1),sr_array(5), pipe_enable) < bias
    val carry_flag = ShiftRegister(exp_adder_outc_sr_2.asBool, sr_array(5), pipe_enable)
    val exp_sum = ShiftRegister(exp_adder_outs_sr_2, sr_array(5), pipe_enable)
    val new_sign_reg = ShiftRegister(new_sign_sr_3,sr_array(5),pipe_enable)
    val msb_check = ShiftRegister(frac_mult_out_sr((mantissa + 1) * 2 - 1),sr_array(5),pipe_enable)
    val new_frac_reg = ShiftRegister(frac_mult_out_sr,sr_array(5),pipe_enable)


    // Overflow/Underflow check: if underflow check, look for carry low or exp_sum + msb_check < min_exp, else if overflow check, look for carry high or exp_sum + msb_check > max_exp
    val u_flag_reg = ShiftRegister(Mux(uo_check,!carry_flag || ((exp_sum +& msb_check.asUInt) < min_exp), false.B), sr_array(6), pipe_enable) // if true, underflow detected
    val o_flag_reg = ShiftRegister(Mux(!uo_check,carry_flag || ((exp_sum +& msb_check.asUInt) > max_exp), false.B), sr_array(6), pipe_enable) // if true, overflow detected
    // aligns exp_sum, sign, and mantissa with the flags
    val new_exp_reg = ShiftRegister(exp_sum,sr_array(6),pipe_enable)
    val new_sign_reg_2 = ShiftRegister(new_sign_reg,sr_array(6),pipe_enable)
    val new_frac_reg_2 = ShiftRegister(new_frac_reg,sr_array(6),pipe_enable)



    // Normalization | if msb of mantissa product is 1, no normalization needed, else shift left by 1 and add 1 to exponent | also if underflow detected, force normalization to produce smallest representable number
    val out_exp_innermux = ShiftRegister(Mux(new_frac_reg_2((mantissa + 1) * 2 - 1).asBool, new_exp_reg + 1.U, new_exp_reg), sr_array(7), pipe_enable)
    val out_frac_innermux = ShiftRegister(Mux(new_frac_reg_2((mantissa + 1) * 2 - 1).asBool, new_frac_reg_2((mantissa + 1) * 2 - 2, mantissa + 1), new_frac_reg_2((mantissa + 1) * 2 - 3, mantissa)), sr_array(7), pipe_enable)
    // Aligns flag, and sign with normalized outputs
    val u_flag_reg_2 = ShiftRegister(u_flag_reg, sr_array(7), pipe_enable) // if true, underflow detected
    val o_flag_reg_2 = ShiftRegister(o_flag_reg, sr_array(7), pipe_enable) // if true, overflow detected
    val new_sign_reg_3 = ShiftRegister(new_sign_reg_2,sr_array(7),pipe_enable)


    // Overflow saturation mux | if overflow detected, saturate to max value, else pass through normalized value
    val out_exp_innermux_2 = ShiftRegister(Mux(o_flag_reg_2, max_exp, out_exp_innermux), sr_array(8), pipe_enable)
    val out_frac_innermux_2 = ShiftRegister(Mux(o_flag_reg_2, max_frac, out_frac_innermux), sr_array(8), pipe_enable)
    val new_sign_reg_4 = ShiftRegister(new_sign_reg_3,sr_array(8),pipe_enable)
    val u_flag_reg_3 = ShiftRegister(u_flag_reg_2, sr_array(8), pipe_enable) // if true, underflow detected

    val out_sign = new_sign_reg_4 // final sign
    val out_exp = Mux(u_flag_reg_3, min_exp, out_exp_innermux_2) // if underflow detected, saturate to min exp, else pass through normalized exp (previous result)
    val out_frac = Mux(u_flag_reg_3, min_frac, out_frac_innermux_2) // if underflow detected, saturate to min frac, else pass through normalized frac (previous result)
    io.out_s := ShiftRegister(out_sign ## out_exp ## out_frac,sr_array(9),pipe_enable) // final result - concatenate sign, exp, and frac and register at output
  }

  // adder
  class FP_add(FORMAT: FloatingPointFormat, latency: Int) extends FPModule(FORMAT) {
    require(latency >= 1, "Pipeline depth must be at least 1")
    val io = IO(new Bundle() {
      val in_ready = Output(Bool())
      val out_ready = Input(Bool())
      val in_valid = Input(Bool())
      val in_a = Input(UInt(bw.W))
      val in_b = Input(UInt(bw.W))
      val out_s = Output(UInt(bw.W))
      val out_valid = Output(Bool())
    })
    override def desiredName = s"${FORMAT}_add_${latency}"

    // 10 logical pipeline stage boundaries between combinational blocks:
    // 0: input registration
    // 1: after field extraction / before exponent subtraction
    // 2: after exponent subtraction / before equal-exponent arrangement
    // 3: after arrangement / before mantissa alignment and fraction add setup
    // 4: after alignment / before fraction addition
    // 5: after fraction addition / before sign resolution
    // 6: after sign resolution / before LZC
    // 7: after LZC / before normalization muxes
    // 8: after normalization / before final output mux
    // 9: output registration
    private val N_STAGES = 10
    private val pipe_skip = if (latency >= N_STAGES) 1 else N_STAGES / latency
    val sr_array = Array.fill(N_STAGES)(0)
    for (i <- 0 until latency) {
      sr_array((i * pipe_skip) % N_STAGES) += 1
    }

    // Track valid through pipeline using shift register
    val out_valid_reg = ShiftRegister(io.in_valid && io.in_ready, latency, io.out_ready || !io.out_valid)
    io.out_valid := out_valid_reg

    // Pipeline enable: advance when output is consumed or no valid output
    val pipe_enable = io.out_ready || !io.out_valid

    // Ready when pipeline can advance
    io.in_ready := pipe_enable

    val in_a = ShiftRegister(io.in_a,sr_array(0),pipe_enable)
    val in_b = ShiftRegister(io.in_b,sr_array(0),pipe_enable)


    // get the sign bit of the two inptus
    val sign_wire = Wire(Vec(2, UInt(1.W)))
    sign_wire(0) := in_a(bw - 1)
    sign_wire(1) := in_b(bw - 1)

    // get the exponents of the two inputs
    val exp_wire = Wire(Vec(2, UInt(exponent.W)))
    when(in_a(bw - 2, mantissa) > max_exp) {
      exp_wire(0) := max_exp
    }.elsewhen(in_a(bw-2,mantissa) < min_exp){ // saturating inputs
      exp_wire(0) := min_exp
    }.otherwise {
      exp_wire(0) := in_a(bw - 2, mantissa)
    }
    when(in_b(bw - 2, mantissa) > max_exp) {
      exp_wire(1) := max_exp
    }.elsewhen(in_b(bw-2,mantissa) < min_exp){
      exp_wire(1) := min_exp
    }.otherwise {
      exp_wire(1) := in_b(bw - 2, mantissa)
    }

    // get the mantissa parts of the two inputs
    val frac_wire = Wire(Vec(2, UInt(mantissa.W)))
    frac_wire(0) := in_a(mantissa - 1, 0)
    frac_wire(1) := in_b(mantissa - 1, 0)

    // 1.0 + mantissa part of the two numbers
    val whole_frac_wire = Wire(Vec(2, UInt((mantissa + 1).W)))
    whole_frac_wire(0) := 1.U ## frac_wire(0)
    whole_frac_wire(1) := 1.U ## frac_wire(1)


    val exp_sr = ShiftRegister(exp_wire, sr_array(1), pipe_enable)
    val whole_frac_sr = ShiftRegister(whole_frac_wire, sr_array(1), pipe_enable)
    val sign_sr = ShiftRegister(sign_wire, sr_array(1), pipe_enable)

    val exp_subtractor = Module(new full_subtractor(exponent))
    exp_subtractor.io.in_a := exp_sr(0)
    exp_subtractor.io.in_b := exp_sr(1)
    exp_subtractor.io.in_c := 0.U

    val exp_sub_out_c = ShiftRegister(exp_subtractor.io.out_c, sr_array(2), pipe_enable)
    val exp_sub_out_s = ShiftRegister(exp_subtractor.io.out_s, sr_array(2), pipe_enable)
    val exp_sr_2 = ShiftRegister(exp_sr, sr_array(2), pipe_enable)
    val whole_frac_sr_2 = ShiftRegister(whole_frac_sr, sr_array(2), pipe_enable)
    val sign_sr_2 = ShiftRegister(sign_sr, sr_array(2), pipe_enable)

    val eqexp_arrange = ShiftRegister(Mux(exp_sr_2(0)===exp_sr_2(1),whole_frac_sr_2(1) > whole_frac_sr_2(0),false.B), sr_array(3), pipe_enable)  // special case in which exponents are equal, check if mantissa1 > mantissa0
    val exp_sr_3 = ShiftRegister(exp_sr_2, sr_array(3), pipe_enable)
    val whole_frac_sr_3 = ShiftRegister(whole_frac_sr_2, sr_array(3), pipe_enable)
    val sign_sr_3 = ShiftRegister(sign_sr_2, sr_array(3), pipe_enable)
    val exp_sub_out_c_2 = ShiftRegister(exp_sub_out_c, sr_array(3), pipe_enable)
    val exp_sub_out_s_2 = ShiftRegister(exp_sub_out_s, sr_array(3), pipe_enable)


    val redundant_op = ShiftRegister(Mux(exp_sub_out_c_2.asBool, (~exp_sub_out_s_2).asUInt + 1.U, exp_sub_out_s_2) > mantissa.U,sr_array(4),pipe_enable) // difference between exponents overly large for alignment (could work around this by having more bits)
    val fracadd_in_a = ShiftRegister(Mux(exp_sub_out_c_2.asBool, whole_frac_sr_3(1), whole_frac_sr_3(eqexp_arrange)),sr_array(4),pipe_enable) // selects the larger mantissa part of the two
    val fracadd_in_b = ShiftRegister(Mux(exp_sub_out_c_2.asBool, whole_frac_sr_3(0) >> ((~exp_sub_out_s_2).asUInt + 1.U), whole_frac_sr_3(!eqexp_arrange) >> exp_sub_out_s_2),sr_array(4),pipe_enable) // whichever mantissa is smaller is aligned with respect to larger mantissa
    val ref_exp = ShiftRegister(Mux(exp_sub_out_c_2.asBool, exp_sr_3(1),exp_sr_3(0)),sr_array(4),pipe_enable) // the larger exp of the two is reference
    val ref_sign = ShiftRegister(Mux(exp_sub_out_c_2.asBool, sign_sr_3(1),sign_sr_3(eqexp_arrange)),sr_array(4),pipe_enable) // same with sign and so on
    val diff_sign = ShiftRegister(sign_sr_3(0) ^ sign_sr_3(1), sr_array(4), pipe_enable) // need to check if signs are diff, if yes then we know result cannot grow so look out for leading zeros in frac sum else result may grow so check for frac sum carry


    val frac_adder = Module(new full_adder((mantissa + 1))).io
    frac_adder.in_a := fracadd_in_a
    frac_adder.in_b := Mux(diff_sign.asBool, (~fracadd_in_b).asUInt + 1.U, fracadd_in_b) // note that if signs are diff we need to twos complement the smaller inp regardless if it is actually negative or not
    frac_adder.in_c := 0.U

    val frac_adder_out_c = ShiftRegister(frac_adder.out_c, sr_array(5), pipe_enable)
    val frac_adder_out_s = ShiftRegister(frac_adder.out_s, sr_array(5), pipe_enable)
    val diff_sign_2 = ShiftRegister(diff_sign, sr_array(5), pipe_enable)
    val redundant_op_2 = ShiftRegister(redundant_op, sr_array(5), pipe_enable)
    val ref_sign_2 = ShiftRegister(ref_sign, sr_array(5), pipe_enable)
    val ref_exp_2 = ShiftRegister(ref_exp, sr_array(5), pipe_enable)

    val sign_out = ShiftRegister(Mux(diff_sign_2.asBool && !redundant_op_2, Mux(frac_adder_out_c.asBool, ref_sign_2, !ref_sign_2), ref_sign_2),sr_array(6),pipe_enable) // from observation, if diff signs, then frac sum carry = 0 implies a sign change, otherwise same sign stays
    val fracadd_outs = ShiftRegister(frac_adder_out_s,sr_array(6),pipe_enable)
    val fracadd_outc = ShiftRegister(frac_adder_out_c,sr_array(6),pipe_enable)
    val diff_sign_3 = ShiftRegister(diff_sign_2, sr_array(6), pipe_enable)
    val ref_exp_3 = ShiftRegister(ref_exp_2, sr_array(6), pipe_enable)

    val LZC_inst = Module(new LZC(bw,2)).io
    LZC_inst.in_d := fracadd_outs ## (VecInit.fill(bw - (mantissa + 1))(true.B)).asUInt
    val leadzeroindex = ShiftRegister(LZC_inst.out_c, sr_array(7), pipe_enable) // this lead zero index is computed because diff_sign implies result could be smaller which may introduce lead zeros in fracsum
    val ref_exp_4 = ShiftRegister(ref_exp_3, sr_array(7), pipe_enable)
    val fracadd_outs_2 = ShiftRegister(fracadd_outs, sr_array(7), pipe_enable)
    val diff_sign_4 = ShiftRegister(diff_sign_3, sr_array(7), pipe_enable)
    val fracadd_outc_2 = ShiftRegister(fracadd_outc, sr_array(7), pipe_enable)
    val sign_out_2 = ShiftRegister(sign_out, sr_array(7), pipe_enable)

    val red = ref_exp_4 -& leadzeroindex
    val inc = ref_exp_4 +& 1.U

    val shifted_left = fracadd_outs_2(mantissa - 1, 0) << leadzeroindex

    val diff_sign_5 = ShiftRegister(diff_sign_4, sr_array(8), pipe_enable)
    val innermux_frac_true = ShiftRegister(Mux(fracadd_outs_2(mantissa).asBool, fracadd_outs_2(mantissa-1,0), Mux(red(exponent) || (red < min_exp), min_frac, shifted_left)), sr_array(8), pipe_enable)
    val innermux_frac_false = ShiftRegister(Mux(fracadd_outc_2.asBool, Mux(inc(exponent) || (inc > max_exp), max_frac, fracadd_outs_2(mantissa,1)), fracadd_outs_2(mantissa-1,0)), sr_array(8), pipe_enable)
    val innermux_exp_true = ShiftRegister(Mux(fracadd_outs_2(mantissa).asBool, ref_exp_4, Mux(red(exponent) || (red < min_exp), min_exp, red(exponent-1,0))), sr_array(8), pipe_enable)
    val innermux_exp_false = ShiftRegister(Mux(fracadd_outc_2.asBool, Mux(inc(exponent) || (inc > max_exp), max_exp, inc(exponent-1,0)), ref_exp_4), sr_array(8), pipe_enable)
    val sign_out_3 = ShiftRegister(sign_out_2, sr_array(8), pipe_enable)

    val norm_out_frac = (ShiftRegister(Mux(diff_sign_5.asBool,
      innermux_frac_true,
      innermux_frac_false),
      sr_array(9), pipe_enable))(mantissa - 1,0)

    val norm_out_exp = ShiftRegister(Mux(diff_sign_5.asBool,
      innermux_exp_true,
      innermux_exp_false),
      sr_array(9), pipe_enable)
    val norm_out_sign = ShiftRegister(sign_out_3, sr_array(9), pipe_enable)

    io.out_s := norm_out_sign ## norm_out_exp ## norm_out_frac
  }

  class FP_acc(FORMAT: FloatingPointFormat, iters: Int, ExpExp: Int, ExpMSB: Int, LSB: Int) extends FPModule(FORMAT) {
    require(LSB >= mantissa)
    require(ExpMSB >= 1)
    val io = IO(new Bundle() {
      val in_ready = Output(Bool())
      val out_ready = Input(Bool())
      val in_valid = Input(Bool())
      val in_a = Input(UInt(bw.W))
      val out_s = Output(UInt(bw.W))
      val out_valid = Output(Bool())
    })
    override def desiredName = s"${FORMAT}_acc"

    val ResultRegWidth = Math.pow(2, log2Ceil(ExpMSB + LSB)).toInt

    // Pipeline enable: advance when output is consumed or no valid output
    val pipe_enable = io.out_ready || !io.out_valid

    // Ready when pipeline can advance
    io.in_ready := pipe_enable

    // --------------------------------------------------------------

    val input_a = ShiftRegister(io.in_a, 1, pipe_enable) // store in input a


    // max and min values for saturating use
    val max_expected_exp = (ExpExp.U(exponent.W) + bias - 1.U) // reference value for aligning input a (we expect the input to be smaller than this value)

    // get the sign bit
    val sign_wire = Wire(UInt(1.W))
    sign_wire := input_a(bw - 1)


    // get the exponent and mantissa and saturate
    val exp_wire = Wire(UInt(exponent.W))
    val frac_wire = Wire(UInt((mantissa + 1).W))
    when(input_a(bw - 2, mantissa) > max_expected_exp) {
      exp_wire := max_expected_exp
      frac_wire := max_frac
    }.elsewhen(input_a(bw-2,mantissa) < min_exp){
      exp_wire := min_exp
      frac_wire := min_frac
    }.otherwise {
      exp_wire := input_a(bw - 2, mantissa)
      frac_wire := 1.U ## input_a(mantissa - 1, 0)
    }



    // converting fractional part into internal format
    val whole_frac_wire = Wire( UInt((ExpMSB+LSB).W))
    whole_frac_wire := (0.U((ExpMSB - 1).W)) ## frac_wire ## (0.U((LSB - mantissa).W))


    // find the difference between exponents, we use this to right shift the input value for proper alignment
    val exp_subtractor = Module(new full_subtractor(exponent))
    exp_subtractor.io.in_a := max_expected_exp
    exp_subtractor.io.in_b := exp_wire
    exp_subtractor.io.in_c := 0.U

    // --------------------------------------------------------------

    // then the shifting on the input mantissa is performed
    val shifted_frac = (ShiftRegister(whole_frac_wire,1,pipe_enable) >> ShiftRegister(exp_subtractor.io.out_s,1,pipe_enable)).asUInt

    // --------------------------------------------------------------

    // take 1s complement of shifted frac in case it is negative
    val inv_shif_frac = (~ShiftRegister(shifted_frac,1,pipe_enable)).asUInt

    // if negative , we consider the 1s complemented frac else the normal one
    val frac_add_adjusted = Mux(ShiftRegister(sign_wire,2,pipe_enable).asBool, inv_shif_frac,ShiftRegister(shifted_frac,1,pipe_enable))

    // --------------------------------------------------------------

    // internally stored result for accumulation results
    val acc_sign = RegInit(0.U)
    val acc_wfrac = RegInit(0.U((ResultRegWidth).W))

    val inp_sign = ShiftRegister(sign_wire, 3, pipe_enable)

    // compare signs of internal result and the input sign
    val diff_sign = (acc_sign ^ inp_sign)

    // add the internal result with the aligned input
    val frac_adder = Module(new full_adder(ExpMSB + LSB)).io
    frac_adder.in_a := acc_wfrac
    frac_adder.in_b := ShiftRegister(frac_add_adjusted, 1, pipe_enable)
    frac_adder.in_c := inp_sign // take into account the input sign, which will basically twos complement the second input if negative. Resulting in a subtraction from addition

    // if the signs were different and we had no carry, then it is implied that the internal sign should invert
    //    val new_sign = Mux(diff_sign === 1.U, Mux(frac_adder.out_c.asBool, acc_sign, !acc_sign), acc_sign)
    val new_sign = Mux(diff_sign === 1.U, Mux(inp_sign === 1.U, Mux(!frac_adder.out_c.asBool, !acc_sign, acc_sign), Mux(frac_adder.out_c.asBool, !acc_sign, acc_sign)), acc_sign)

    // these regs are just for carrying over the results into normalization after all accumulation iterations are complete
    val temp_sign = RegInit(false.B)
    val temp_frac = RegInit(0.U((ResultRegWidth).W))

    // counter of iterations and a register for indicating output validity
    val cnt_iter = new Counter(iters)
    val out_valid = RegInit(false.B)

    // count the iters and update internal result register accordingly
    when(pipe_enable && ShiftRegister(io.in_valid && io.in_ready, 4, pipe_enable)){
      cnt_iter.inc()
      when( cnt_iter.value === (iters-1).U){ // if all iterations are done
        acc_sign := 0.U // clear accumulation registers
        acc_wfrac := 0.U
        out_valid := true.B // activate sequence for valid output
        temp_sign := new_sign // push data to temp registers for normalization
        temp_frac := frac_adder.out_s
      }.otherwise{
        out_valid := false.B
        acc_sign := new_sign
        acc_wfrac := frac_adder.out_s
      }
    }

    // --------------------------------------------------------------

    // take 2s complement of temp frac reg value in case of negativity
    val tfrac_sr = ShiftRegister(temp_frac, 1, pipe_enable)
    val two_comp_temp_frac = (~tfrac_sr(ExpMSB+LSB-1,0)).asUInt + 1.U
    // if temp sign is negative, take the 2s complemented result
    val lzc_in = Mux(ShiftRegister(temp_sign, 1, pipe_enable), two_comp_temp_frac, tfrac_sr)

    // --------------------------------------------------------------

    // pass the temp_frac value over to the leading zero counter
    val LZC_inst = Module(new LZC(ResultRegWidth,2)).io
    LZC_inst.in_d := ShiftRegister(lzc_in, 1, pipe_enable)

    // --------------------------------------------------------------
    // compute where the index for the leading one is
    val leadzeroindex = (ResultRegWidth - 1).U - ShiftRegister(LZC_inst.out_c, 1, pipe_enable)
    // there are two possible shifts for normalization
    val sleft = ShiftRegister(lzc_in, 2, pipe_enable) << (LSB.U - leadzeroindex) // shift left in case index is below LSB
    val sright = ShiftRegister(lzc_in, 2, pipe_enable) >> (leadzeroindex - LSB.U) // shift right in case index is above LSB
    val red = max_expected_exp -& (LSB.U - leadzeroindex) // subtract from exp in case index is below LSB
    val inc = max_expected_exp +& (leadzeroindex - LSB.U) // add to exp in case index is above LSB
    // final step is to check for overflows/underflows
    val adj_frac = Mux(leadzeroindex > (LSB).U, // for mantissa
      Mux(inc(exponent) || (inc > max_exp), max_frac,sright), // overflow
      Mux(red(exponent) || (red < min_exp), min_frac,sleft)) // underflow
    val adj_exp = Mux(leadzeroindex > (LSB).U, // for exponent
      Mux(inc(exponent) || (inc > max_exp), max_exp,inc), // overflow
      Mux(red(exponent) || (red < min_exp), min_exp,red)) // underflow

    // regs for storing the output value
    val out_sign = RegInit(false.B)
    val out_exp = RegInit(0.U(exponent.W))
    val out_frac = RegInit(0.U(mantissa.W))

    when(pipe_enable){
      out_sign := ShiftRegister(temp_sign, 3, pipe_enable)
      out_exp := adj_exp(exponent-1, 0)
      out_frac := adj_frac(LSB-1, LSB - mantissa)
    }

    // --------------------------------------------------------------

    // setting output port values
    io.out_valid := ShiftRegister(out_valid,4,pipe_enable)
    io.out_s := out_sign ## out_exp ## out_frac
  }

  class FP_cos(FORMAT: FloatingPointFormat, iters: Int) extends FPModule(FORMAT) {
    val io = IO(new Bundle{
      val in_ready = Output(Bool())
      val out_ready = Input(Bool())
      val in_valid = Input(Bool())
      val in_angle = Input(UInt(bw.W))
      val out_valid = Output(Bool())
      val out_cos = Output(UInt(bw.W))
      val out_sin = Output(UInt(bw.W))
    })
    override def desiredName = s"${FORMAT}_cos_$iters"
    val TWOPI = convert_string_to_IEEE_754((Math.PI * 2 ).toString, FORMAT).U(bw.W)

    // Chain modules with ready-valid handshaking
    val fpdiv = Module(new FP_div(FORMAT,bw/2-1,bw/2-1)).io
    val fpfloor = Module(new FP_floor(FORMAT)).io
    val fpmult = Module(new FP_mult(FORMAT,3)).io
    val floatfixed = Module(new FloatTOFixed(FORMAT, bw-mantissa, mantissa)).io
    val cordic = Module(new cordic(bw, false, mantissa, iters, iters)).io
    val fixedfloat_cos = Module(new FixedTOFloat(FORMAT, bw-mantissa, mantissa)).io
    val fixedfloat_sin = Module(new FixedTOFloat(FORMAT, bw-mantissa, mantissa)).io

    // Connect ready signals backwards through the chain
    fixedfloat_cos.out_ready := io.out_ready
    fixedfloat_sin.out_ready := io.out_ready
    cordic.out_ready := fixedfloat_cos.in_ready && fixedfloat_sin.in_ready
    floatfixed.out_ready := cordic.in_ready
    fpmult.out_ready := floatfixed.in_ready
    fpfloor.out_ready := fpmult.in_ready
    fpdiv.out_ready := fpfloor.in_ready

    // Propagate ready to input
    io.in_ready := fpdiv.in_ready

    // Pipeline enable for side-path shift registers
    val pipe_enable = fpfloor.in_ready

    // Connect valid and data signals forward through the chain
    fpdiv.in_valid := io.in_valid
    fpdiv.in_a := io.in_angle
    fpdiv.in_b := TWOPI

    fpfloor.in_valid := fpdiv.out_valid
    fpfloor.in_data := 0.U(1.W) ## fpdiv.out_s(bw-2,0)
    val sign = ShiftRegister(fpdiv.out_s(bw-1), 1, pipe_enable)

    fpmult.in_valid := fpfloor.out_valid
    fpmult.in_a := sign ## fpfloor.out_frac(bw-2,0)
    fpmult.in_b := TWOPI

    floatfixed.in_valid := fpmult.out_valid
    floatfixed.in_float := fpmult.out_s

    cordic.in_valid := floatfixed.out_valid
    cordic.in_d := floatfixed.out_fixed

    fixedfloat_cos.in_valid := cordic.out_valid
    fixedfloat_cos.in_fixed := cordic.out_x

    fixedfloat_sin.in_valid := cordic.out_valid
    fixedfloat_sin.in_fixed := cordic.out_y

    io.out_valid := fixedfloat_cos.out_valid
    io.out_cos := fixedfloat_cos.out_float
    io.out_sin := fixedfloat_sin.out_float
  }

  class FP_atan(FORMAT: FloatingPointFormat, iters: Int) extends FPModule (FORMAT) {
    val io = IO(new Bundle{
      val in_ready = Output(Bool())
      val out_ready = Input(Bool())
      val in_valid = Input(Bool())
      val in_tan = Input(UInt(bw.W))
      val out_valid = Output(Bool())
      val out_atan = Output(UInt(bw.W))
    })
    override def desiredName = s"${FORMAT}_atan_$iters"

    // Chain modules with ready-valid handshaking
    val floatfixed = Module(new FloatTOFixed(FORMAT, bw/2, bw/2)).io
    val cordic = Module(new cordic(bw, true, bw/2, iters, iters)).io
    val fixedfloat_atan = Module(new FixedTOFloat(FORMAT, bw/2, bw/2)).io

    // Connect ready signals backwards through the chain
    fixedfloat_atan.out_ready := io.out_ready
    cordic.out_ready := fixedfloat_atan.in_ready
    floatfixed.out_ready := cordic.in_ready

    // Propagate ready to input
    io.in_ready := floatfixed.in_ready

    // Connect valid and data signals forward through the chain
    floatfixed.in_valid := io.in_valid
    floatfixed.in_float := io.in_tan

    cordic.in_valid := floatfixed.out_valid
    cordic.in_d := floatfixed.out_fixed

    fixedfloat_atan.in_valid := cordic.out_valid
    fixedfloat_atan.in_fixed := cordic.out_z

    io.out_valid := fixedfloat_atan.out_valid
    io.out_atan := fixedfloat_atan.out_float
  }

  class FloatTOFixed(FORMAT: FloatingPointFormat, ibits: Int, fbits: Int) extends FPModule(FORMAT) {
    val fixed_width = ibits+fbits

    val io = IO(new Bundle{
      val in_ready = Output(Bool())
      val out_ready = Input(Bool())
      val in_valid = Input(Bool())
      val in_float = Input(UInt(bw.W))
      val out_valid = Output(Bool())
      val out_fixed = Output(SInt((fixed_width+1).W))
    })

    // Track valid through pipeline
    val out_valid_reg = ShiftRegister(io.in_valid && io.in_ready, 2, io.out_ready || !io.out_valid)
    io.out_valid := out_valid_reg

    // Pipeline enable: advance when output is consumed or no valid output
    val pipe_enable = io.out_ready || !io.out_valid

    // Ready when pipeline can advance
    io.in_ready := pipe_enable

    val sign = io.in_float(bw-1)
    val exp = io.in_float(bw-2,mantissa)
    val frac = 1.U(1.W) ## io.in_float(mantissa-1,0)

    val norm_frac = Mux(sign, (~frac).asUInt + 1.U, frac)

    val result_wire = Wire(UInt((ibits+fbits).W))
    result_wire := (if(fbits > mantissa) 0.U((ibits - 1).W) ## frac ## 0.U((fbits - mantissa).W) else 0.U((ibits-1).W) ## frac(mantissa, mantissa-fbits))
    val exp_diff = exp -& bias
    val twoscomped = (~exp_diff).asUInt + 1.U
    val result_reg = ShiftRegister(Mux(exp_diff(exponent).asBool, result_wire >> (twoscomped), result_wire << (exp_diff))(fixed_width-1,0), 1, pipe_enable)
    val sign_sr = ShiftRegister(sign, 1, pipe_enable)
    val output = Mux(sign_sr, (~result_reg).asUInt + 1.U, result_reg.asUInt)(fixed_width-1,0)
    io.out_fixed := ShiftRegister((sign_sr ## output).asSInt, 1, pipe_enable)
  }

  class FixedTOFloat(FORMAT: FloatingPointFormat, ibits: Int, fbits: Int) extends FPModule(FORMAT) {
    val fixed_width = ibits+fbits

    val io = IO(new Bundle{
      val in_ready = Output(Bool())
      val out_ready = Input(Bool())
      val in_valid = Input(Bool())
      val in_fixed = Input(SInt((fixed_width+1).W))
      val out_valid = Output(Bool())
      val out_float = Output(UInt(bw.W))
    })

    // Track valid through pipeline
    val out_valid_reg = ShiftRegister(io.in_valid && io.in_ready, 2, io.out_ready || !io.out_valid)
    io.out_valid := out_valid_reg

    // Pipeline enable: advance when output is consumed or no valid output
    val pipe_enable = io.out_ready || !io.out_valid

    // Ready when pipeline can advance
    io.in_ready := pipe_enable

    val sign = io.in_fixed(fixed_width)
    val input = Mux(sign, (~io.in_fixed).asUInt + 1.U, io.in_fixed.asUInt)(fixed_width-1,0)
    val lzc = Module(new LZC(fixed_width,2)).io
    lzc.in_d := input
    val inputsr = ShiftRegister(input, 1, pipe_enable)
    val signsr = ShiftRegister(sign, 1, pipe_enable)
    val lzcout = ShiftRegister(lzc.out_c, 1, pipe_enable)
    val lead_index = (fixed_width - 1).U - lzcout
    val shifted_frac = Mux(lead_index < fbits.U, inputsr << (fbits.U - lead_index), inputsr >> (lead_index - fbits.U))
    val shifted_exp = Mux(lead_index < fbits.U, bias - (fbits.U - lead_index), bias + (lead_index - fbits.U))(exponent-1,0)
    io.out_float := ShiftRegister(signsr ##  shifted_exp ## (if(fbits > mantissa) shifted_frac(fbits-1, fbits-mantissa) else shifted_frac(fbits-1, 0) ## 0.U((mantissa - fbits).W)), 1, pipe_enable)
  }

  class FP_floor(FORMAT: FloatingPointFormat) extends FPModule(FORMAT) {
    val io = IO(new Bundle {
      val in_ready = Output(Bool())
      val out_ready = Input(Bool())
      val in_valid = Input(Bool())
      val in_data = Input(UInt(bw.W))
      val out_valid = Output(Bool())
      val out_whole = Output(UInt((exponent + 1).W))
      val out_frac = Output(UInt(bw.W))
    })
    override def desiredName = s"${FORMAT}_floor"

    // Track valid through pipeline
    val out_valid_reg = ShiftRegister(io.in_valid && io.in_ready, 1, io.out_ready || !io.out_valid)
    io.out_valid := out_valid_reg

    // Pipeline enable: advance when output is consumed or no valid output
    val pipe_enable = io.out_ready || !io.out_valid

    // Ready when pipeline can advance
    io.in_ready := pipe_enable

    val sign_wire = Wire(UInt(1.W))
    val exp_wire = Wire(UInt(exponent.W))
    val frac_wire = Wire(UInt((mantissa + exponent).W))

    sign_wire := io.in_data(bw - 1)
    exp_wire := io.in_data(bw - 2, mantissa)
    frac_wire := 1.U((exponent).W) ## io.in_data(mantissa - 1,0) // appending 1 at msb to complete the fixed point representation of 1.mantissa

    val exp_diff = exp_wire - bias // compute difference between input exponent and bias
    val one = 1.U(1.W) ## 0.U(mantissa.W)

    val shifted_frac = Mux(exp_wire < bias, (frac_wire >> ((~exp_diff).asUInt + 1.U)),(frac_wire << (exp_diff))).asUInt // right/left shifting
    val temp_whole_sign = Wire(UInt((1).W))
    val temp_whole = Wire(UInt((exponent).W))
    val temp_frac = Wire(UInt(mantissa.W))

    temp_whole_sign := sign_wire
    temp_whole := shifted_frac(mantissa+exponent-1,mantissa) + sign_wire // retrieve the whole part from the shifted result
    temp_frac := Mux(sign_wire.asBool, one - shifted_frac(mantissa-1,0), shifted_frac(mantissa-1,0)) // store the remaining mantissa bits

    val lzc = Module(new LZC(bw,2)).io
    lzc.in_d := temp_frac ## (VecInit.fill(bw-mantissa)(1.U)).asUInt

    val out_frac = ShiftRegister(temp_frac << (lzc.out_c + 1.U),1,pipe_enable)
    val out_exp = ShiftRegister(bias - (lzc.out_c + 1.U),1,pipe_enable)

    val out_whole = ShiftRegister(temp_whole_sign ## temp_whole, 1, pipe_enable)

    io.out_whole := out_whole
    io.out_frac := 0.U(1.W) ## out_exp(exponent-1,0) ## out_frac(mantissa-1,0)

  }

  class FP_exp(FORMAT: FloatingPointFormat) extends FPModule(FORMAT) {
    val io = IO(new Bundle{
      val in_ready = Output(Bool())
      val out_ready = Input(Bool())
      val in_valid = Input(Bool())
      val in_data = Input(UInt(bw.W))
      val out_valid = Output(Bool())
      val out_data = Output(UInt(bw.W))
    })
    override def desiredName = s"${FORMAT}_exp"

    // Canonical Signed Digit (CSD) encoding for multiplierless constant multiplication.
    // Returns (sign, shift_position) pairs where sign is +1 or -1.
    // CSD guarantees no two consecutive non-zero digits, minimizing add/sub operations.
    def csdTerms(n0: BigInt): Array[(Int, Int)] = {
      if (n0 == 0) return Array.empty

      val globalSign = if (n0 < 0) -1 else 1
      var k = n0.abs
      var i = 0
      val out = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]

      while (k > 0) {
        if ((k & 1) == 1) {
          val mod4 = (k & 3).toInt // 1 or 3 when k is odd
          val ui = if (mod4 == 1) 1 else -1
          out += ((globalSign * ui, i))
          k -= ui                    // subtract (+1) or subtract (-1) ≡ add 1
        }
        k = k >> 1
        i += 1
      }

      out.toArray
    }

    // --- Named parameters for the internal fixed-point pipeline ---

    // Float-to-fixed conversion: ibits integer bits, fbits fractional bits
    // ibits = exponent - 1: covers the max input magnitude (unbiased exp up to exponent-2)
    // fbits = mantissa + 1: preserves full mantissa precision + implicit leading 1
    val f2f_ibits = exponent - 1
    val f2f_fbits = mantissa + 1

    // CSD constant scale factor: constants are scaled by 2^(mantissa+1) for fixed-point precision
    val csd_scale_bits = mantissa + 1

    // CORDIC fractional bits: maximizes precision within bw bits (bw - 1 sign bit - 1 integer bit + 1)
    val cordic_fbits = mantissa + exponent - 1 // = bw - 2 for standard formats
    val cordic_iters = bw
    val cordic_latency = bw

    // Latency of each CSD multiplier stage (ln2_inv multiply and ln2 multiply)
    val csd_multiply_latency = 1
    // Total CSD pipeline stages before CORDIC input
    val csd_total_stages = 2

    // Delay for the whole-part register to align with CORDIC output
    val w_fy_delay = csd_multiply_latency + cordic_latency

    // Input range clamping:
    // max_exp: bias + exponent - 2 limits the max input magnitude so e^x doesn't overflow too far
    // min_exp: bias - mantissa limits to precision where e^x differs from 1.0
    override val max_exp = (FORMAT.bias + exponent - 2).U(exponent.W)
    override val min_exp = (FORMAT.bias - mantissa).U(exponent.W)

    // --- Module instantiation ---

    val float_fixed = Module(new FloatTOFixed(FORMAT, f2f_ibits, f2f_fbits)).io
    val cordic_exp = Module(new exp(bw, cordic_fbits, cordic_iters, cordic_latency)).io

    // Connect ready signals backwards
    cordic_exp.out_ready := io.out_ready
    float_fixed.out_ready := cordic_exp.in_ready

    // Propagate ready to input
    io.in_ready := float_fixed.in_ready

    // Pipeline enable for side-path shift registers
    val pipe_enable = cordic_exp.in_ready

    // --- Input saturation ---

    val sign_wire = Wire(UInt(1.W))
    sign_wire := io.in_data(bw - 1)

    val exp_wire = Wire(UInt(exponent.W))
    val frac_wire = Wire(UInt((mantissa + 1).W))
    when(io.in_data(bw - 2, mantissa) > max_exp) {
      exp_wire := max_exp
      frac_wire := 1.U ## max_frac
    }.elsewhen(io.in_data(bw-2,mantissa) < min_exp){
      exp_wire := min_exp
      frac_wire := 1.U ## min_frac
    }.otherwise {
      exp_wire := io.in_data(bw - 2, mantissa)
      frac_wire := 1.U ## io.in_data(mantissa - 1, 0)
    }

    // --- CSD constants for ln(2) and 1/ln(2) ---
    // High-precision constants to support formats up to FP128
    val scale_f = BigDecimal(2).pow(csd_scale_bits)
    val ln2_const = BigDecimal("0.693147180559945309417232121458176568075500134360255254120680009493393621969694715605863326996418687542001481020570685733685520235758130557032670751635075961930727570828371435190307038623891673471123350115364497955239120475172681574932065155524734139525882950453007095326366642654104239157814952043740430385500801953446023295806975610451548712657229174083002445024086242920028700778106122370803036845411583888753703610120930437958500269243404565956307905990388371999379499656975960729908474587863153831040700063")
    val ln2_inv_const = BigDecimal("1.442695040888963407359924681001892137426645954152985934135449406931109219181185079885526622893506344496263849813826284406414528954088289162413781133626771650471937970821478666919247968697936977415927539950809022728569693098397620784813298345703429109413625901172584843992767912461471748830414917975015078402252652753232103805709076397028379855453978657100095038973614126980230767037249434950879229146629948072798814004361117478512757437282834162183834406512327267713419807971748913504971150085600458327399173946223010457746375823991891783379694308000813")
    val ln2 = csdTerms((ln2_const * scale_f).toBigInt)
    val ln2_inv = csdTerms((ln2_inv_const * scale_f).toBigInt)

    // --- Range reduction: x / ln(2) = w + f ---

    val x_inp = sign_wire ## exp_wire ## frac_wire(mantissa-1,0)

    float_fixed.in_valid := io.in_valid
    float_fixed.in_float := x_inp

    // fx is the fixed-point representation of input x in Q(f2f_ibits).(f2f_fbits) format
    val fx = float_fixed.out_fixed

    // fy = fx * (1/ln2) via CSD multiplierless multiplication, then scale correction
    // Result is in the same Q(f2f_ibits).(f2f_fbits) format as fx
    val fy = (ShiftRegister(VecInit(ln2_inv.map(i=> ((if (i._1 == -1) -fx else fx) << i._2).asSInt)).reduceTree(_+&_), csd_multiply_latency, pipe_enable) >> csd_scale_bits)(bw-1,0).asSInt

    // --- Split fy into integer part (w) and fractional part (f) ---
    // fy is Q(f2f_ibits).(f2f_fbits) with fbits = mantissa+1 fractional bits
    // Integer part is bits [bw-1 : mantissa+1], fractional part is bits [mantissa : 0]

    // Floor for negative values: truncation gives ceiling-toward-zero, so we adjust
    val w_fy_ifneg = (fy + -1.S(bw.W))(bw-1, f2f_fbits) + 1.U(1.W)
    val w_fy = Mux(fy(bw-1), w_fy_ifneg, fy(bw-1, f2f_fbits))
    // Delay w_fy to align with CORDIC output: 1 CSD stage (ln2 multiply) + cordic_latency
    val sr_w_fy = ShiftRegister(w_fy, w_fy_delay, pipe_enable)
    // Fractional part: sign bit + lower (mantissa+1) bits, sign-extended to bw
    val f_fy = (fy(bw-1) ## fy(mantissa, 0)).asSInt.pad(bw)

    // --- Back-multiply fractional part by ln(2) to get reduced argument ---
    // fcord_in = f_fy * ln(2), which is the argument for the CORDIC exp engine
    // This value is in [0, ln(2)) ~ [0, 0.693), within hyperbolic CORDIC convergence range
    val fcord_in = (ShiftRegister(VecInit(ln2.map(i=> ((if (i._1 == -1) -f_fy else f_fy) << i._2).asSInt)).reduceTree(_+&_), csd_multiply_latency, pipe_enable) >> csd_scale_bits)(bw-1,0).asSInt

    // Convert fcord_in from Q(f2f_ibits).(f2f_fbits) to the CORDIC's expected Q format
    // CORDIC expects cordic_fbits fractional bits; we have f2f_fbits = mantissa+1 fractional bits
    // Zero-pad LSBs by (cordic_fbits - f2f_fbits) = (exponent - 2) bits to align binary points
    // Slice upper (mantissa+4) bits: sign + 2 guard bits + fractional content
    val cordic_frac_pad = cordic_fbits - f2f_fbits // = exponent - 2
    val cordic_slice_hi = mantissa + 3              // sign + 2 guard bits + mantissa fractional
    val new_fcord_in = (fcord_in(cordic_slice_hi,0) ## 0.U(cordic_frac_pad.W)).asSInt

    cordic_exp.in_valid := ShiftRegister(float_fixed.out_valid, csd_total_stages, pipe_enable)
    cordic_exp.in_z := new_fcord_in

    // --- Output normalization ---
    // CORDIC output is e^(f*ln2) = 2^f in fixed-point with cordic_fbits fractional bits
    // Since f is in [0,1), the result is in [1.0, 2.0), so the integer bit (at position cordic_fbits)
    // should be 1. If it's 0 due to CORDIC gain imprecision, we normalize by shifting left 1 bit.
    val cordic_int_bit = cordic_fbits // = bw - 2, position of the integer '1' in CORDIC output
    val exp_norm = !cordic_exp.out_expz(cordic_int_bit)
    // Extract mantissa bits from CORDIC output, skipping the implicit leading 1
    val final_mant = Mux(exp_norm,
      cordic_exp.out_expz(cordic_int_bit - 2, cordic_int_bit - mantissa - 1),  // normalized: shift left 1
      cordic_exp.out_expz(cordic_int_bit - 1, cordic_int_bit - mantissa))      // standard extraction
    val final_exp = FORMAT.bias.U(exponent.W) + sr_w_fy - exp_norm.asUInt
    val final_sign = 0.U(1.W) // e^x > 0 for all real x

    io.out_valid := cordic_exp.out_valid
    io.out_data := final_sign ## final_exp ## final_mant
  }
}
