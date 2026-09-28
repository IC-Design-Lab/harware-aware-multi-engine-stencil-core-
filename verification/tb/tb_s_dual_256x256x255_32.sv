`include "../dut/define.v"
`define ERROR_THRESHOLD 5

`define POINTS 7
`define ROW 256
`define COL 256
`define DEPTH 255
`define ST 32
`define BW 32
`define RADIUS 1
`define TIME_STEPS 5

// Real scalar input values written by golden model
`define INPUT_VALUES ((`ROW*`COL*`DEPTH) - (2*(`RADIUS + `COL)))

// Packed input packets/lines
`define INPUT_NO (((`INPUT_VALUES) + `ST - 1) / `ST)

// Real scalar outputs after TIME_STEPS shrink
`define OUTPUT_VALUES ((`DEPTH - 2*`TIME_STEPS) * (`ROW - 2*`TIME_STEPS) * (`COL - 2*`TIME_STEPS))

// Packed output packets/lines
`define OUTPUT_NO (((`OUTPUT_VALUES) + `ST - 1) / `ST)

module tb_sc();
//---------------------------------------------------------
//--- wire and reg declaration 
//---------------------------------------------------------
reg                     clock    ;
reg                     reset    ;
reg                    in_ready_d;
reg  [`ST*`BW-1:0]     in_matrix_d      ;
reg  [`POINTS*`BW-1:0]     in_weight_d   ;
wire [`ST*`BW-1:0]     out_data_d     ;
wire [`ST-1:0]               out_valid_d ;

reg [`ST*`BW-1:0]     input_ram[0:(`INPUT_NO)-1];
reg [`ST*`BW-1:0]    output_ram[0:(`OUTPUT_NO)-1];

reg [`POINTS*`BW-1:0] weights_ram[0:1];


initial begin
 $readmemh("../golden/test_3d_n64_m64_p63_r1_k8_shape_cross/input_ieee754.txt" , input_ram);
  $readmemh("../golden/test_3d_n64_m64_p63_r1_k8_shape_cross/output_ieee754.txt", output_ram);
  $readmemh("../golden/test_3d_n64_m64_p63_r1_k8_shape_cross/weights_ieee754.txt", weights_ram);

end

`ifdef SC_R1_K32_3D
SODA_dual_core u_dual (.clock         (clock    ),
                           .reset         (reset    ),
                           .io_in_ready_d     (in_ready_d),
                           .io_in_matrix_d           (in_matrix_d      ),
                           .io_in_weight_d      (in_weight_d ),
                           .io_out_data_d        (out_data_d   ),
                          .io_out_valid_d         (out_valid_d    ));
`endif
		  
//---------------------------------------------------------------------
//------- BFM
//---------------------------------------------------------------------
integer i,j, output_index, j_out, start;
logic [`ST*`BW-1:0] row_data;
logic [(`ST)*`BW-1:0] row_data_out;
integer report, report_ieee754;
always #5 clock = ~clock;
initial begin
  clock           = 1'b0;
  reset           = 1'b1;
  in_ready_d       = 1'b0;
  in_matrix_d             = `BW*`ST'h0;
  in_weight_d          = `BW*`POINTS'h0;             ;

repeat (5) @(posedge clock);

 @(posedge clock);
//  #100
  reset = 1'b0;
//  #16
    
repeat (10)  @(posedge clock );
 for(i=0; i<(`INPUT_NO);i=i+1) begin
// @(posedge clock);
 in_ready_d = 1'b1;
 in_matrix_d = input_ram[i];
 in_weight_d = weights_ram[0];
 @(posedge clock);
//  end
end
@(posedge clock);
  in_ready_d        = 1'b0;
 // repeat(10) @(posedge clock);
end

// -------------------------------------------
// --Monitor: Simulation Log -----------------
// -------------------------------------------

`include "functions/signal_declare.sv"
`include "functions/tb_func.sv"

reg [`BW-1:0] golden_result;
reg [`BW-1:0] dut_result;

integer lane;
integer output_packet;
integer output_scalar_count;

initial begin
  output_packet       = 0;
  output_scalar_count = 0;
  output_index        = 0;
  start               = 0;

  wait(~reset);

  `ifdef SC_R1_K32_3D
      report = $fopen("./sc_r1_k32_3d/n256_m256_p255_t5/report.log", "w");
      report_ieee754 = $fopen("./sc_r1_k32_3d/n256_m256_p255_t5/report_ieee754.log", "w");
  `endif

  if (report == 0) begin
      $display("ERROR: Could not open report.log");
      $finish;
  end

  if (report_ieee754 == 0) begin
      $display("ERROR: Could not open report_ieee754.log");
      $finish;
  end

  // Wait until any output lane becomes valid
  wait(|out_valid_d);

  while (output_scalar_count < `OUTPUT_VALUES) begin
    @(negedge clock);

    if (|out_valid_d) begin

      for (lane = 0; lane < `ST; lane = lane + 1) begin

        if (out_valid_d[lane] && output_scalar_count < `OUTPUT_VALUES) begin

          golden_result = output_ram[output_packet][(`ST-lane)*`BW-1 -: `BW];
          dut_result    = out_data_d[(`ST-lane)*`BW-1 -: `BW];

          `include "functions/error_percent_abs_cal.sv"
          `include "functions/comp_abs.sv"
          `include "functions/comp_ieee754.sv"

          output_scalar_count = output_scalar_count + 1;
        end

      end

      output_packet = output_packet + 1;
      output_index  = output_packet;

    end
  end

  `ifdef SC_R1_K32_3D
      $fclose(report);
      $fclose(report_ieee754);
  `endif
end


endmodule
