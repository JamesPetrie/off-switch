#include "Vtb.h"
#include "verilated.h"
#if VM_TRACE
#include "verilated_fst_c.h"
#endif
#include <iostream>

int main(int argc, char** argv) {
    VerilatedContext* ctx = new VerilatedContext;
    ctx->commandArgs(argc, argv);

#if VM_TRACE
    ctx->traceEverOn(true);
#endif

    Vtb* tb = new Vtb{ctx};

#if VM_TRACE
    VerilatedFstC* fst = new VerilatedFstC;
    tb->trace(fst, 99);
    fst->open("dump.fst");
    #define WAVE_DUMP(t) fst->dump(t)
#else
    #define WAVE_DUMP(t)
#endif

    // initialize rst and clk
    tb->rst_n = 1;
    tb->clk = 0;
    tb->eval();
    WAVE_DUMP(ctx->time());

    // run 1 cycle
    tb->clk = 1; tb->eval(); ctx->timeInc(1); WAVE_DUMP(ctx->time());
    tb->clk = 0; tb->eval(); ctx->timeInc(1); WAVE_DUMP(ctx->time());

    // Assert reset
    tb->rst_n = 0;

    // run 2 cycles
    tb->clk = 1; tb->eval(); ctx->timeInc(1); WAVE_DUMP(ctx->time());
    tb->clk = 0; tb->eval(); ctx->timeInc(1); WAVE_DUMP(ctx->time());
    tb->clk = 1; tb->eval(); ctx->timeInc(1); WAVE_DUMP(ctx->time());
    tb->clk = 0; tb->eval(); ctx->timeInc(1); WAVE_DUMP(ctx->time());

    // Deassert reset
    tb->rst_n = 1;

    // Run the test
    for (int i = 0; i < int(1e8) && !ctx->gotFinish(); i++) {
        tb->clk = 1; tb->eval(); ctx->timeInc(1); WAVE_DUMP(ctx->time());
        tb->clk = 0; tb->eval(); ctx->timeInc(1); WAVE_DUMP(ctx->time());
    }

    if (!ctx->gotFinish()) {
        std::cerr << "ERROR: simulation reached the harness cycle limit without $finish\n";
        delete tb;
        delete ctx;
        return 1;
    }

#if VM_TRACE
    fst->close();
    delete fst;
#endif
    delete tb;
    delete ctx;
    return 0;
}
