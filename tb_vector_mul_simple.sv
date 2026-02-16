`timescale 1ns/1ps

module tb_vector_mul_simple;
  // ---------------------------------------------------------------------------
  // Config
  // ---------------------------------------------------------------------------
  localparam time CLK_PERIOD = 10ns;
  localparam int  LATENCY    = 1;      // expected DUT latency in cycles
  localparam int  N_RANDOM   = 2000;

  // ---------------------------------------------------------------------------
  // DUT signals
  // ---------------------------------------------------------------------------
  logic               clk_i;
  logic               rst_ni;

  logic               in_valid_i;
  logic signed [31:0] in_a_i;
  logic signed [31:0] in_b_i;

  logic               out_valid_o;
  logic signed [63:0] out_result_o;

  vector_mul_simple dut (
      .clk_i(clk_i),
      .rst_ni(rst_ni),
      .in_valid_i(in_valid_i),
      .in_a_i(in_a_i),
      .in_b_i(in_b_i),
      .out_valid_o(out_valid_o),
      .out_result_o(out_result_o)
  );

  // ---------------------------------------------------------------------------
  // Clock / Reset
  // ---------------------------------------------------------------------------
  initial clk_i = 1'b0;
  always #(CLK_PERIOD/2) clk_i = ~clk_i;

  task automatic do_reset();
    rst_ni      = 1'b0;
    in_valid_i  = 1'b0;
    in_a_i      = '0;
    in_b_i      = '0;
    repeat (5) @(posedge clk_i);
    rst_ni      = 1'b1;
    repeat (2) @(posedge clk_i);
  endtask

  // ---------------------------------------------------------------------------
  // Scoreboard for expected results (pipeline with configurable latency)
  // ---------------------------------------------------------------------------
  logic               exp_valid_pipe [0:LATENCY];
  logic signed [63:0] exp_data_pipe  [0:LATENCY];
  logic signed [31:0] exp_a_pipe     [0:LATENCY];
  logic signed [31:0] exp_b_pipe     [0:LATENCY];

  function automatic logic signed [63:0] ref_mul(input logic signed [31:0] a,
                                                 input logic signed [31:0] b);
    // Reference model: wide '*' is fine in the TB
    ref_mul = $signed(a) * $signed(b);
  endfunction

  // Shift expected pipeline + check outputs
  int unsigned n_checked;
  int unsigned n_errors;

  always_ff @(posedge clk_i) begin
    if (!rst_ni) begin
      for (int i = 0; i <= LATENCY; i++) begin
        exp_valid_pipe[i] <= 1'b0;
        exp_data_pipe[i]  <= '0;
        exp_a_pipe[i]     <= '0;
        exp_b_pipe[i]     <= '0;
      end
    end else begin
      // Build the *next* expected pipeline state using blocking assignments.
      // We compare DUT outputs against this next-state, so LATENCY=0 works
      // correctly (no extra off-by-one cycle due to nonblocking assignments).
      logic               next_valid_pipe [0:LATENCY];
      logic signed [63:0] next_data_pipe  [0:LATENCY];
      logic signed [31:0] next_a_pipe     [0:LATENCY];
      logic signed [31:0] next_b_pipe     [0:LATENCY];

      for (int i = LATENCY; i > 0; i--) begin
        next_valid_pipe[i] = exp_valid_pipe[i-1];
        next_data_pipe[i]  = exp_data_pipe[i-1];
        next_a_pipe[i]     = exp_a_pipe[i-1];
        next_b_pipe[i]     = exp_b_pipe[i-1];
      end
      next_valid_pipe[0] = in_valid_i;
      next_data_pipe[0]  = ref_mul(in_a_i, in_b_i);
      next_a_pipe[0]     = in_a_i;
      next_b_pipe[0]     = in_b_i;

      // Commit next expected pipeline state
      for (int i = 0; i <= LATENCY; i++) begin
        exp_valid_pipe[i] <= next_valid_pipe[i];
        exp_data_pipe[i]  <= next_data_pipe[i];
        exp_a_pipe[i]     <= next_a_pipe[i];
        exp_b_pipe[i]     <= next_b_pipe[i];
      end
    end
  end

  // Race-free checking: sample after DUT updates (NBA) at the same clock edge.
  always @(posedge clk_i or negedge rst_ni) begin
    if (!rst_ni) begin
      n_checked <= 0;
      n_errors  <= 0;
    end else begin
      int unsigned errors_inc;
      int unsigned checked_inc;
      errors_inc  = 0;
      checked_inc = 0;

      #1step;
      if (out_valid_o !== exp_valid_pipe[LATENCY]) begin
        errors_inc++;
        $display("[%0t] ERROR: out_valid_o=%0b expected=%0b",
                 $time, out_valid_o, exp_valid_pipe[LATENCY]);
      end

      if (out_valid_o) begin
        checked_inc++;
        if (out_result_o !== exp_data_pipe[LATENCY]) begin
          errors_inc++;
          $display("[%0t] ERROR: mismatch a=%0d (0x%08h) b=%0d (0x%08h)",
                   $time, exp_a_pipe[LATENCY], exp_a_pipe[LATENCY],
                   exp_b_pipe[LATENCY], exp_b_pipe[LATENCY]);
          $display("             got=%0d (0x%016h) exp=%0d (0x%016h)",
                   out_result_o, out_result_o, exp_data_pipe[LATENCY], exp_data_pipe[LATENCY]);
        end
      end

      n_checked <= n_checked + checked_inc;
      n_errors  <= n_errors + errors_inc;
    end
  end

  // ---------------------------------------------------------------------------
  // Stimulus helpers
  // ---------------------------------------------------------------------------
  task automatic apply_vec(input logic signed [31:0] a,
                           input logic signed [31:0] b,
                           input logic v);
    // Drive synchronous to rising edge (like signals coming from flops).
    @(posedge clk_i);
    in_a_i     <= a;
    in_b_i     <= b;
    in_valid_i <= v;
  endtask

  function automatic logic signed [31:0] rand_s32();
    logic [31:0] r;
    r = $urandom();
    rand_s32 = $signed(r);
  endfunction

  // ---------------------------------------------------------------------------
  // Test program
  // ---------------------------------------------------------------------------
  initial begin
    // Optional VCD (works in e.g. Verilator/Icarus). Safe to leave enabled.
    $dumpfile("tb_vector_mul_simple.vcd");
    $dumpvars(0, tb_vector_mul_simple);

    do_reset();

    // Directed tests
    apply_vec(32'sd0,  32'sd0,  1'b1);
    apply_vec(32'sd1,  32'sd0,  1'b1);
    apply_vec(32'sd0,  32'sd1,  1'b1);
    apply_vec(32'sd1,  32'sd1,  1'b1);
    apply_vec(-32'sd1, 32'sd1,  1'b1);
    apply_vec(32'sd1,  -32'sd1, 1'b1);
    apply_vec(-32'sd1, -32'sd1, 1'b1);

    apply_vec(32'sh7fffffff, 32'sd2, 1'b1);
    apply_vec(32'sh80000000, 32'sd2, 1'b1);
    apply_vec(32'sh80000000, -32'sd1, 1'b1);
    apply_vec(32'sh7fffffff, 32'sh7fffffff, 1'b1);
    apply_vec(32'sh80000000, 32'sh80000000, 1'b1);

    // Bubble
    apply_vec(32'sd123, 32'sd456, 1'b0);

    // Random tests with occasional bubbles
    for (int k = 0; k < N_RANDOM; k++) begin
      logic v;
      v = ($urandom_range(0, 9) < 7); // ~70% valid
      apply_vec(rand_s32(), rand_s32(), v);
    end

    // Drain pipeline
    for (int d = 0; d < (LATENCY + 5); d++) begin
      apply_vec('0, '0, 1'b0);
    end

    $display("\nChecked outputs: %0d", n_checked);
    if (n_errors == 0) begin
      $display("PASS: no mismatches detected");
    end else begin
      $display("FAIL: %0d errors detected", n_errors);
    end

    $finish;
  end

  // Safety timeout
  initial begin
    #(CLK_PERIOD * (N_RANDOM + 200));
    $display("TIMEOUT");
    $finish;
  end

endmodule
