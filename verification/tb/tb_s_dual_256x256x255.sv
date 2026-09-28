`include "../dut/define.v"

`define ERROR_THRESHOLD 5 // 5% ERROR TOLERANCE

`define POINTS 7
`define ROW    256
`define COL    256
`define DEPTH  255
`define ST     50
`define BW     32
`define RADIUS 1

// ---------------------------------------------------------
// Input/output counts
// ---------------------------------------------------------
// One input file line has ST = 50 packed FP32 values.
// Therefore each input_ram entry is 50 x 32 = 1600 bits.

`define INPUT_SCALARS ((`ROW*`COL*`DEPTH) - (2*(`RADIUS + `COL)))
`define INPUT_NO      (((`INPUT_SCALARS) + `ST - 1) / `ST)

// For ST = 50, depth is processed in groups of 50.
// DEPTH = 255 gives ceil(255/50) = 6 depth groups.
`define OUTPUT_NO     296529


module tb_sc();


// ---------------------------------------------------------
// Wires and Reg declarations
// ---------------------------------------------------------

reg                     clock;
reg                     reset;
reg                     in_ready_d;

reg  [`ST*`BW-1:0]      in_matrix_d;
reg  [`POINTS*`BW-1:0]  in_weight_d;

wire [`ST*`BW-1:0]      out_data_d;
wire [`ST-1:0]          out_valid_d;


// ---------------------------------------------------------
// Memories
// ---------------------------------------------------------

reg [`ST*`BW-1:0]       input_ram  [0:`INPUT_NO-1];
reg [`POINTS*`BW-1:0]   weights_ram[0:`POINTS-1];
reg [`ST*`BW-1:0]       output_ram [0:`OUTPUT_NO-1];


// ---------------------------------------------------------
// Cycle counter declarations
// ---------------------------------------------------------

integer cycle_count;
integer first_input_cycle;
integer first_output_cycle;
integer last_output_cycle;
integer total_cycles;

reg first_input_seen;
reg first_output_seen;


// ---------------------------------------------------------
// Other TB variables
// ---------------------------------------------------------

integer i;
integer lane;
integer output_index;
integer start;

integer report;
integer report_ieee754;

logic [`ST*`BW-1:0] row_data;
logic [`ST*`BW-1:0] row_data_out;


// ---------------------------------------------------------
// Load golden files
// ---------------------------------------------------------

initial begin

    $display("=========================================");
    $display("TB CONFIGURATION");
    $display("ROW           = %0d", `ROW);
    $display("COL           = %0d", `COL);
    $display("DEPTH         = %0d", `DEPTH);
    $display("ST / k        = %0d", `ST);
    $display("BW            = %0d", `BW);
    $display("INPUT_NO      = %0d packed input lines", `INPUT_NO);
    $display("OUTPUT_NO     = %0d packed output lines", `OUTPUT_NO);
    $display("Input width   = %0d bits", `ST*`BW);
    $display("Output width  = %0d bits", `ST*`BW);
    $display("Valid width   = %0d bits", `ST);
    $display("=========================================");

    $readmemh(
        "../golden/test_3d_n256_m256_p255_r1_k50_shape_cross/input_ieee754.txt",
        input_ram
    );

    $readmemh(
        "../golden/test_3d_n256_m256_p255_r1_k50_shape_cross/weights_ieee754.txt",
        weights_ram
    );

    $readmemh(
        "../golden/test_3d_n256_m256_p255_r1_k50_shape_cross/output_ieee754.txt",
        output_ram
    );

end


// ---------------------------------------------------------
// DUT instantiation
// ---------------------------------------------------------

`ifdef SC_R1_K50_3D

SODA_dual_core u_dual (

    .clock             (clock),
    .reset             (reset),

    .io_in_ready_d     (in_ready_d),
    .io_in_matrix_d    (in_matrix_d),
    .io_in_weight_d    (in_weight_d),

    .io_out_data_d     (out_data_d),
    .io_out_valid_d    (out_valid_d)

);

`endif


// ---------------------------------------------------------
// Clock generation
// ---------------------------------------------------------

initial begin
    clock = 1'b0;
end

always #5 clock = ~clock;


// ---------------------------------------------------------
// Cycle counter
// ---------------------------------------------------------

initial begin
    cycle_count        = 0;
    first_input_cycle  = 0;
    first_output_cycle = 0;
    last_output_cycle  = 0;
    total_cycles       = 0;

    first_input_seen   = 1'b0;
    first_output_seen  = 1'b0;
end


always @(posedge clock) begin

    if (reset) begin

        cycle_count       = 0;
        first_input_seen  = 1'b0;

    end
    else begin

        cycle_count = cycle_count + 1;

        // First real input accepted by DUT
        if (in_ready_d && !first_input_seen) begin

            first_input_cycle = cycle_count;
            first_input_seen  = 1'b1;

            $display("");
            $display("--------------------------------------------------");
            $display("[CYCLE COUNTER] FIRST INPUT ACCEPTED");
            $display("Cycle      = %0d", first_input_cycle);
            $display("Time       = %0t", $time);
            $display("Input word = %h", in_matrix_d);
            $display("--------------------------------------------------");
            $display("");

        end

    end

end


// ---------------------------------------------------------
// Input stimulus
// ---------------------------------------------------------

initial begin

    reset       = 1'b1;
    in_ready_d  = 1'b0;
    in_matrix_d = {(`ST*`BW){1'b0}};
    in_weight_d = {(`POINTS*`BW){1'b0}};

    repeat (5) @(posedge clock);

    // Release reset away from posedge to avoid race
    @(negedge clock);
    reset = 1'b0;

    repeat (10) @(posedge clock);

    // Drive inputs on negedge.
    // DUT samples them on the next posedge.
    for (i = 0; i < `INPUT_NO; i = i + 1) begin

        @(negedge clock);

        in_ready_d  = 1'b1;
        in_matrix_d = input_ram[i];
        in_weight_d = weights_ram[0];

    end

    @(negedge clock);

    in_ready_d  = 1'b0;
    in_matrix_d = {(`ST*`BW){1'b0}};

    $display("");
    $display("[TB] Finished applying %0d packed input lines.", `INPUT_NO);
    $display("");

end


// ---------------------------------------------------------
// Monitor: Simulation log
// ---------------------------------------------------------

`include "functions/signal_declare.sv"
`include "functions/tb_func.sv"


// Compare one FP32 lane at a time
reg [`BW-1:0] golden_result;
reg [`BW-1:0] dut_result;


initial begin

    output_index = 0;
    start        = 0;

    wait (reset == 1'b0);

`ifdef SC_R1_K50_3D

    report = $fopen(
        "../sim/sc_r1_k50_3d/n256_m256_p255/report.log",
        "w"
    );

    report_ieee754 = $fopen(
        "../sim/sc_r1_k50_3d/n256_m256_p255/report_ieee754.log",
        "w"
    );

`endif


    // -----------------------------------------------------
    // Wait for first output vector
    // -----------------------------------------------------

    wait (|out_valid_d);

    // -----------------------------------------------------
    // Output checking loop
    // -----------------------------------------------------

    for (output_index = 0; output_index < `OUTPUT_NO; ) begin

        @(negedge clock);

        if (|out_valid_d) begin

            // First output vector
            if (!first_output_seen) begin

                first_output_seen  = 1'b1;
                first_output_cycle = cycle_count;

                $display("");
                $display("--------------------------------------------------");
                $display("[CYCLE COUNTER] FIRST OUTPUT DETECTED");
                $display("Cycle             = %0d", first_output_cycle);
                $display("Time              = %0t", $time);
                $display("out_valid_d       = %b", out_valid_d);
                $display("First output lat. = %0d CCs",
                         first_output_cycle - first_input_cycle);
                $display("--------------------------------------------------");
                $display("");

            end


            // -------------------------------------------------
            // Check all 50 lanes
            //
            // Mapping follows your old ST=2 TB:
            //
            // lane 0  -> MSB side
            // lane 49 -> LSB side
            // -------------------------------------------------

            for (lane = 0; lane < `ST; lane = lane + 1) begin

                if (out_valid_d[lane] == 1'b1) begin

                    golden_result =
                        output_ram[output_index][(`ST-lane)*`BW-1 -: `BW];

                    dut_result =
                        out_data_d[(`ST-lane)*`BW-1 -: `BW];

                    `include "functions/error_percent_abs_cal.sv"
                    `include "functions/comp_abs.sv"
                    `include "functions/comp_ieee754.sv"

                end

            end


            // -------------------------------------------------
            // Last output vector
            // -------------------------------------------------

            if (output_index == (`OUTPUT_NO - 1)) begin

                last_output_cycle = cycle_count;

                total_cycles =
                    last_output_cycle - first_input_cycle + 1;

                $display("");
                $display("=========================================");
                $display("[CYCLE COUNTER] FINAL RESULTS");
                $display("First input cycle  : %0d", first_input_cycle);
                $display("First output cycle : %0d", first_output_cycle);
                $display("Last output cycle  : %0d", last_output_cycle);
                $display("Total latency CCs  : %0d", total_cycles);
                $display("Output vectors     : %0d", `OUTPUT_NO);
                $display("=========================================");
                $display("");

            end


            // One packed output vector has been processed
            output_index = output_index + 1;

        end

    end


`ifdef SC_R1_K50_3D

    $fclose(report);
    $fclose(report_ieee754);

`endif

    $display("[TB] Completed output checking.");
    $finish;

end


// ---------------------------------------------------------
// Safety timeout
// ---------------------------------------------------------

initial begin

    // This is intentionally large because 256x256x255 is big.
    repeat (20000000) @(posedge clock);

    $display("");
    $display("=========================================");
    $display("[ERROR] TESTBENCH TIMEOUT");
    $display("Current cycle       : %0d", cycle_count);
    $display("Current output_index: %0d / %0d", output_index, `OUTPUT_NO);
    $display("Current out_valid_d : %b", out_valid_d);
    $display("=========================================");
    $display("");

    $finish;

end


endmodule