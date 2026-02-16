// Copyright TU Wien
// Licensed under the Solderpad Hardware License v2.1, see LICENSE.txt for details
// SPDX-License-Identifier: Apache-2.0 WITH SHL-2.1


module vproc_mul_block #(
        parameter bit BUF_OPS  = 1'b0, // buffer operands (op1_i and op2_i)
        parameter bit BUF_MUL  = 1'b0, // buffer multiplication result (and acc_i)
        parameter bit BUF_RES  = 1'b0  // buffer final result (res_o)
    )(
        input  logic        clk_i,
        input  logic        async_rst_ni,
        input  logic        sync_rst_ni,

        input  logic [16:0] op1_i,
        input  logic [16:0] op2_i,

        // note that when operands are buffered, then acc*_i must be delayed by 1 cycle
        input  logic [15:0] acc_i,
        input  logic        acc_flag_i, // use accumulator (otherwise it is replaced with 0)
        input  logic        acc_sub_i,  // subtract multiplication result from accumulator instead of adding

        output logic [32:0] res_o
    );

    logic [16:0] op1_q, op2_q;
    logic [32:0] mul_q, mul_d;
    logic [32:0] res_q, res_d;

    // Accumulation path is currently disabled (res_d = mul_q). Keep these inputs
    // "used" to avoid Verilator UNUSEDSIGNAL warnings when warnings are fatal.
    logic unused_acc_inputs;
    assign unused_acc_inputs = ^{acc_i, acc_flag_i, acc_sub_i};

    generate
        if (BUF_OPS) begin : gen_buf_ops
            always_ff @(posedge clk_i) begin
                op1_q <= op1_i;
                op2_q <= op2_i;
            end
        end else begin : gen_no_buf_ops
            always_comb begin
                op1_q = op1_i;
                op2_q = op2_i;
            end
        end

        if (BUF_MUL) begin : gen_buf_mul
            always_ff @(posedge clk_i) begin
                mul_q     <= mul_d;
            end
        end else begin : gen_no_buf_mul
            always_comb begin
                mul_q     = mul_d;
            end
        end

        if (BUF_RES) begin : gen_buf_res
            always_ff @(posedge clk_i) begin
                res_q <= res_d;
            end
        end else begin : gen_no_buf_res
            always_comb begin
                res_q = res_d;
            end
        end
    endgenerate

    assign mul_d = $signed(op1_q) * $signed(op2_q);
    // assign res_d = acc_sub_i ? {17'b0, acc_q} - mul_q : {17'b0, acc_q} + mul_q;
    assign res_d = mul_q; // we will not accumulate, so removed
    assign res_o = res_q;

endmodule
