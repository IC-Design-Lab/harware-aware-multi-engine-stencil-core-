// `include "../dut/define.v"
// `define ERROR_THRESHOLD 5  //5% error tolerace
// `define INPUT_NO (((`ROW*`COL*`DEPTH)-(2*(`RADIUS+`COL)))/`ST)
// `define OUTPUT_NO ((`DEPTH/2*`RADIUS+1)*(`COL-2*`RADIUS)*(`ROW-2*`RADIUS))
// `define POINTS 7
// `define ROW 16
// `define COL 16
// `define DEPTH 15
// `define ST 2
// `define BW 32
// `define RADIUS 1

// module tb_sc();
// //---------------------------------------------------------
// //--- wire and reg declaration 
// //---------------------------------------------------------
// reg                     clock    ;
// reg                     reset    ;
// reg                    in_ready_d;
// reg  [`ST*`BW-1:0]     in_matrix_d      ;
// reg  [`POINTS*`BW-1:0]     in_weight_d   ;
// wire [`ST*`BW-1:0]     out_data_d     ;
// wire [`ST-1:0]               out_valid_d ;

// reg [`ST*`BW-1:0]     input_ram[0:(`INPUT_NO)-1];
// reg [`ST*`BW-1:0]    output_ram[0:(`OUTPUT_NO)-1];

// reg [`POINTS*`BW-1:0] weights_ram[0:1];

// // -------------------------------------------
// // --Cycle Counter Declarations ---------------
// // -------------------------------------------
// integer cycle_count;
// integer first_input_cycle;
// integer last_output_cycle;
// integer total_cycles;

// initial begin
//  $readmemh("../golden/test_3d_n16_m16_p15_r1_k2_shape_cross/input_ieee754.txt" , input_ram);
//  $readmemh("../golden/test_3d_n16_m16_p15_r1_k2_shape_cross/output_ieee754.txt", output_ram);
//  $readmemh("../golden/test_3d_n16_m16_p15_r1_k2_shape_cross/weights_ieee754.txt", weights_ram);
// end

// `ifdef SC_R1_K2_3D
// SODA_dual_core u_dual (.clock             (clock    ),
//                        .reset             (reset    ),
//                        .io_in_ready_d     (in_ready_d),
//                        .io_in_matrix_d    (in_matrix_d      ),
//                        .io_in_weight_d    (in_weight_d ),
//                        .io_out_data_d     (out_data_d   ),
//                        .io_out_valid_d    (out_valid_d    ));
// `endif
		  
// //---------------------------------------------------------------------
// //------- BFM
// //---------------------------------------------------------------------
// integer i,j, output_index, j_out, start;
// logic [`ST*`BW-1:0] row_data;
// logic [(`ST)*`BW-1:0] row_data_out;
// integer report, report_ieee754;
// always #5 clock = ~clock;

// // -------------------------------------------
// // --Free-Running Cycle Counter ---------------
// // -------------------------------------------
// initial begin
//     cycle_count       = 0;
//     first_input_cycle = 0;
//     last_output_cycle = 0;
// end

// always @(posedge clock) begin
//     cycle_count <= cycle_count + 1;
// end

// initial begin
//   clock           = 1'b0;
//   reset           = 1'b1;
//   in_ready_d       = 1'b0;
//   in_matrix_d             = `BW*`ST'h0;
//   in_weight_d          = `BW*`POINTS'h0;             ;

// repeat (5) @(posedge clock);

//  @(posedge clock);
//   reset = 1'b0;
    
// repeat (10)  @(posedge clock );
//  for(i=0; i<(`INPUT_NO);i=i+1) begin
//  in_ready_d = 1'b1;
//  in_matrix_d = input_ram[i];
//  in_weight_d = weights_ram[0];
//  @(posedge clock);
// end
// @(posedge clock);
//   in_ready_d        = 1'b0;
// end

// // -------------------------------------------
// // --Monitor: Simulation Log -----------------
// // -------------------------------------------

// `include "functions/signal_declare.sv"
// `include "functions/tb_func.sv"

//  reg  [`ST*`BW-1:0] golden_result;
//  reg  [`ST*`BW-1:0] dut_result   ;
// initial begin
//   output_index    = 0;
//   start = 0;
//   wait(~reset);
//   `ifdef SC_R1_K2_3D
//       report=$fopen("./sc_r1_k2_3d/n16_m16_p15/report.log", "w");
//       report_ieee754=$fopen("./sc_r1_k2_3d/n16_m16_p15/report_ieee754.log", "w");
//   `endif

//   // Capture first input cycle
//   @(posedge in_ready_d);
//   first_input_cycle = cycle_count;
//   $display("[CYCLE COUNTER] First input cycle : %0d", first_input_cycle);

//   wait(out_valid_d[0] || out_valid_d[1]);
//   for(output_index=0; output_index<`OUTPUT_NO;) begin
//      @(negedge clock);
//        if(out_valid_d[0] == 1) begin 
//       golden_result=output_ram[output_index][63:32];
//       dut_result   =out_data_d[63:32];
//       `include "functions/error_percent_abs_cal.sv"
//       `include "functions/comp_abs.sv"
//       `include "functions/comp_ieee754.sv"
//     end
//     if(out_valid_d[1] == 1) begin 
//       golden_result=output_ram[output_index][31:0];
//       dut_result   =out_data_d[31:0];
//       `include "functions/error_percent_abs_cal.sv"
//       `include "functions/comp_abs.sv"
//       `include "functions/comp_ieee754.sv"
//       output_index=output_index+1;
//     end

//     // Capture last output cycle
//     if (out_valid_d != 0) begin
//         last_output_cycle = cycle_count;
//     end

//   end

//   total_cycles = last_output_cycle - first_input_cycle + 1;
//   $display("=========================================");
//   $display("[CYCLE COUNTER] First input  cycle : %0d", first_input_cycle);
//   $display("[CYCLE COUNTER] Last  output cycle : %0d", last_output_cycle);
//   $display("[CYCLE COUNTER] Total latency (CC) : %0d", total_cycles);
//   $display("=========================================");

//   `ifdef SC_R1_K2_3D
//       $fclose(report);
//       $fclose(report_ieee754);
//   `endif
// end

// endmodule


`include "../dut/define.v"
`define ERROR_THRESHOLD 5
`define INPUT_NO (((`ROW*`COL*`DEPTH)-(2*(`RADIUS+`COL)))/`ST)
`define OUTPUT_NO ((`DEPTH/2*`RADIUS+1)*(`COL-2*`RADIUS)*(`ROW-2*`RADIUS))
`define POINTS 7
`define ROW 16
`define COL 16
`define DEPTH 16
`define ST 4
`define BW 32
`define RADIUS 1

`define WATCHDOG_CYCLES 200000

module tb_sc();

//---------------------------------------------------------
//--- wire and reg declaration 
//---------------------------------------------------------
reg                     clock;
reg                     reset;
reg                     in_ready_d;
reg  [`ST*`BW-1:0]      in_matrix_d;
reg  [`POINTS*`BW-1:0]  in_weight_d;   // keep this because DUT still has weight port

wire [`ST*`BW-1:0]      out_data_d;
wire [`ST-1:0]          out_valid_d;

reg [`ST*`BW-1:0]       input_ram[0:(`INPUT_NO)-1];
reg [`ST*`BW-1:0]       output_ram[0:(`OUTPUT_NO)-1];

// ------------------------------------------------------------
// Jacobi constant weight bus
// 1/7 in FP32 = 32'h3E124925
// This is only used to drive the DUT weight input.
// Your jacobi_3d PE ignores external weights internally.
// ------------------------------------------------------------
localparam [31:0] JACOBI_WEIGHT = 32'h3E124925;
localparam [`POINTS*`BW-1:0] JACOBI_WEIGHTS = {`POINTS{JACOBI_WEIGHT}};

// -------------------------------------------
// --Cycle Counter Declarations
// -------------------------------------------
integer cycle_count;
integer first_input_cycle;
integer last_output_cycle;
integer total_cycles;
reg     dut_selected;

initial begin
 $readmemh("../golden/test_jacobi_3d_1_n16_m16_p16_r1_k4_t5/input_ieee754.txt" , input_ram);
 $readmemh("../golden/test_jacobi_3d_1_n16_m16_p16_r1_k4_t5/output_ieee754.txt", output_ram);
end

`ifdef SC_R1_K4_Jacobi_3D
SODA_dual_core u_dual (
    .clock          (clock),
    .reset          (reset),
    .io_in_ready_d  (in_ready_d),
    .io_in_matrix_d (in_matrix_d),
    .io_in_weight_d (in_weight_d),
    .io_out_data_d  (out_data_d),
    .io_out_valid_d (out_valid_d)
);
initial dut_selected = 1'b1;
`else
initial begin
  dut_selected = 1'b0;
  $display("[WARNING] SC_R1_K4_Jacobi_3D is not defined at compile time.");
  $display("[WARNING] DUT is NOT instantiated -> out_valid_d will never assert -> sim will hang.");
  $display("[WARNING] Add +define+SC_R1_K4_Jacobi_3D to your simulator invocation.");
end
`endif
		  
//---------------------------------------------------------------------
//------- BFM
//---------------------------------------------------------------------
integer i, j, output_index, j_out, start;
logic [`ST*`BW-1:0] row_data;
logic [`ST*`BW-1:0] row_data_out;
integer report, report_ieee754;

always #5 clock = ~clock;

// -------------------------------------------
// --Free-Running Cycle Counter
// -------------------------------------------
initial begin
    cycle_count       = 0;
    first_input_cycle = 0;
    last_output_cycle = 0;
end

always @(posedge clock) begin
    cycle_count <= cycle_count + 1;
end

initial begin
  clock       = 1'b0;
  reset       = 1'b1;
  in_ready_d  = 1'b0;
  in_matrix_d = {(`ST*`BW){1'b0}};
  in_weight_d = JACOBI_WEIGHTS;

  repeat (5) @(posedge clock);

  @(posedge clock);
  reset = 1'b0;
    
  repeat (10) @(posedge clock);

  for(i = 0; i < `INPUT_NO; i = i + 1) begin
    in_ready_d  = 1'b1;
    in_matrix_d = input_ram[i];
    in_weight_d = JACOBI_WEIGHTS;
    @(posedge clock);
  end

  @(posedge clock);
  in_ready_d = 1'b0;
end

// -------------------------------------------
// --Watchdog process
// -------------------------------------------
initial begin
  repeat (`WATCHDOG_CYCLES) @(posedge clock);
  if (last_output_cycle == 0) begin
    $display("=========================================");
    $display("[TIMEOUT] out_valid_d never asserted within %0d cycles.", `WATCHDOG_CYCLES);
    $display("[TIMEOUT] dut_selected = %0b", dut_selected);
    $display("[TIMEOUT] first_input_cycle = %0d", first_input_cycle);
    $display("=========================================");
    $finish;
  end
end

// -------------------------------------------
// --Monitor: Simulation Log
// -------------------------------------------
`include "functions/signal_declare.sv"
`include "functions/tb_func.sv"

reg [`ST*`BW-1:0] golden_result;
reg [`ST*`BW-1:0] dut_result;

initial begin
  output_index = 0;
  start = 0;

  wait(~reset);

  `ifdef SC_R1_K4_Jacobi_3D
      report        = $fopen("./jacobi_r1_k4_Jacobi_3d/n16_m16_p16/report.log", "w");
      report_ieee754 = $fopen("./jacobi_r1_k4_Jacobi_3d/n16_m16_p16/report_ieee754.log", "w");
  `endif

  @(posedge in_ready_d);
  @(negedge clock);
  first_input_cycle = cycle_count;
  $display("[CYCLE COUNTER] First input cycle : %0d", first_input_cycle);

  wait(out_valid_d[0] || out_valid_d[1]);

  for(output_index = 0; output_index < `OUTPUT_NO;) begin
    @(negedge clock);

    if(out_valid_d[0] == 1) begin 
      golden_result = output_ram[output_index][63:32];
      dut_result    = out_data_d[63:32];
      `include "functions/error_percent_abs_cal.sv"
      `include "functions/comp_abs.sv"
      `include "functions/comp_ieee754.sv"
    end

    if(out_valid_d[1] == 1) begin 
      golden_result = output_ram[output_index][31:0];
      dut_result    = out_data_d[31:0];
      `include "functions/error_percent_abs_cal.sv"
      `include "functions/comp_abs.sv"
      `include "functions/comp_ieee754.sv"
      output_index = output_index + 1;
    end

    if (out_valid_d != 0) begin
      last_output_cycle = cycle_count;
    end
  end

  total_cycles = last_output_cycle - first_input_cycle + 1;

  $display("=========================================");
  $display("[CYCLE COUNTER] First input  cycle : %0d", first_input_cycle);
  $display("[CYCLE COUNTER] Last  output cycle : %0d", last_output_cycle);
  $display("[CYCLE COUNTER] Total latency (CC) : %0d", total_cycles);
  $display("=========================================");

  `ifdef SC_R1_K4_Jacobi_3D
      $fclose(report);
      $fclose(report_ieee754);
  `endif

  $finish;
end

endmodule