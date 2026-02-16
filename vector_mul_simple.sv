// Simplified 32x32 signed multiplier with valid in/out.
// Implementation note: no wide '*' operator is used here.
// Instead, the 32x32 product is built from four 17x17 signed multipliers
// (like the original design style) and shift/add recombination.
// Two-cycle latency: one operand pipeline stage before the mul blocks and one
// pipeline stage directly after the mul blocks.

module vector_mul_simple (
    input  logic               clk_i,
    input  logic               rst_ni,

    input  logic               in_valid_i,
    input  logic signed [31:0] in_a_i,
    input  logic signed [31:0] in_b_i,

    output logic               out_valid_o,
    output logic signed [63:0] out_result_o
);

    // Valid pipeline for the 2-stage mul path.
    logic in_valid_q1, in_valid_q2;

    always_ff @(posedge clk_i or negedge rst_ni) begin
        if (!rst_ni) begin
            in_valid_q1 <= 1'b0;
            in_valid_q2 <= 1'b0;
        end else begin
            in_valid_q1 <= in_valid_i;
            in_valid_q2 <= in_valid_q1;
        end
    end

  // Split inputs into 16-bit halves.
  // High halves are signed; low halves are treated as unsigned magnitudes.
  logic signed [16:0] a_hi, b_hi;
  logic signed [16:0] a_lo, b_lo;

  always_comb begin
    a_hi = {in_a_i[31], in_a_i[31:16]};
    b_hi = {in_b_i[31], in_b_i[31:16]};
    a_lo = {1'b0, in_a_i[15:0]};
    b_lo = {1'b0, in_b_i[15:0]};
  end

  // Four 17x17 partial products.
  logic signed [32:0] pp_ll; // a_lo * b_lo
  logic signed [32:0] pp_hl; // a_hi * b_lo
  logic signed [32:0] pp_lh; // a_lo * b_hi
  logic signed [32:0] pp_hh; // a_hi * b_hi

  vproc_mul_block #(
      .BUF_OPS(1'b1),
      .BUF_MUL(1'b0),
      .BUF_RES(1'b1)
  ) u_mul_ll (
      .clk_i(clk_i),
      .async_rst_ni(rst_ni),
      .sync_rst_ni(rst_ni),
      .op1_i(a_lo),
      .op2_i(b_lo),
      .acc_i('0),
      .acc_flag_i(1'b0),
      .acc_sub_i(1'b0),
      .res_o(pp_ll)
  );

  vproc_mul_block #(
      .BUF_OPS(1'b1),
      .BUF_MUL(1'b0),
      .BUF_RES(1'b1)
  ) u_mul_hl (
      .clk_i(clk_i),
      .async_rst_ni(rst_ni),
      .sync_rst_ni(rst_ni),
      .op1_i(a_hi),
      .op2_i(b_lo),
      .acc_i('0),
      .acc_flag_i(1'b0),
      .acc_sub_i(1'b0),
      .res_o(pp_hl)
  );

  vproc_mul_block #(
      .BUF_OPS(1'b1),
      .BUF_MUL(1'b0),
      .BUF_RES(1'b1)
  ) u_mul_lh (
      .clk_i(clk_i),
      .async_rst_ni(rst_ni),
      .sync_rst_ni(rst_ni),
      .op1_i(a_lo),
      .op2_i(b_hi),
      .acc_i('0),
      .acc_flag_i(1'b0),
      .acc_sub_i(1'b0),
      .res_o(pp_lh)
  );

  vproc_mul_block #(
      .BUF_OPS(1'b1),
      .BUF_MUL(1'b0),
      .BUF_RES(1'b1)
  ) u_mul_hh (
      .clk_i(clk_i),
      .async_rst_ni(rst_ni),
      .sync_rst_ni(rst_ni),
      .op1_i(a_hi),
      .op2_i(b_hi),
      .acc_i('0),
      .acc_flag_i(1'b0),
      .acc_sub_i(1'b0),
      .res_o(pp_hh)
  );

  // Recombine into full 64-bit product:
  // (a_hi<<16 + a_lo) * (b_hi<<16 + b_lo)
  //   = pp_ll + ((pp_hl + pp_lh) << 16) + (pp_hh << 32)
    logic signed [63:0] mult_comb;
    logic signed [63:0] pp_ll_64, pp_hl_64, pp_lh_64, pp_hh_64;

    always_comb begin
        pp_ll_64 = {{(64-33){pp_ll[32]}}, pp_ll};
        pp_hl_64 = {{(64-33){pp_hl[32]}}, pp_hl};
        pp_lh_64 = {{(64-33){pp_lh[32]}}, pp_lh};
        pp_hh_64 = {{(64-33){pp_hh[32]}}, pp_hh};
        mult_comb = pp_ll_64 + ((pp_hl_64 + pp_lh_64) <<< 16) + (pp_hh_64 <<< 32);
    end

    assign out_valid_o = in_valid_q2;
    assign out_result_o = in_valid_q2 ? mult_comb : '0;

endmodule
