#include "Vtop.h"
#include "verilated.h"
#include <iostream>

int main(int argc, char** argv) {
    VerilatedContext* ctx = new VerilatedContext;
    ctx->commandArgs(argc, argv);

    Vtop* top = new Vtop{ctx};

    for (int i = 0; i < 16; i++) {
        top->clk = 0; top->eval();
        top->clk = 1; top->eval();
        std::cout << "count = " << (int)top->count << std::endl;
    }

    delete top;
    delete ctx;
    return 0;
}
