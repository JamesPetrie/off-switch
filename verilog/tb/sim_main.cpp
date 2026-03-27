#include "Vtb.h"
#include "verilated.h"
#include "verilated_vcd_c.h"
#include <iostream>

int main(int argc, char** argv) {
    VerilatedContext* ctx = new VerilatedContext;
    ctx->commandArgs(argc, argv);
    ctx->traceEverOn(true);

    Vtb* tb = new Vtb{ctx};

    VerilatedVcdC* vcd = new VerilatedVcdC;
    tb->trace(vcd, 99);
    vcd->open("dump.vcd");

    // initialize rst and clk
    tb->rst_n = 1;
    tb->clk = 0;
    tb->eval();
    vcd->dump(ctx->time());

    // run 1 cycle
    tb->clk = 1; tb->eval(); ctx->timeInc(1); vcd->dump(ctx->time());
    tb->clk = 0; tb->eval(); ctx->timeInc(1); vcd->dump(ctx->time());

    // Assert reset
    tb->rst_n = 0;

    // run 2 cycles
    tb->clk = 1; tb->eval(); ctx->timeInc(1); vcd->dump(ctx->time());
    tb->clk = 0; tb->eval(); ctx->timeInc(1); vcd->dump(ctx->time());
    tb->clk = 1; tb->eval(); ctx->timeInc(1); vcd->dump(ctx->time());
    tb->clk = 0; tb->eval(); ctx->timeInc(1); vcd->dump(ctx->time());

    // Deassert reset
    tb->rst_n = 1;

    // Run the test
    for (int i = 0; i < 10000; i++) {
        tb->clk = 1; tb->eval(); ctx->timeInc(1); vcd->dump(ctx->time());
        tb->clk = 0; tb->eval(); ctx->timeInc(1); vcd->dump(ctx->time());
    }

    vcd->close();
    delete vcd;
    delete tb;
    delete ctx;
    return 0;
}
