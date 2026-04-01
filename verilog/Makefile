# ---------------------------------------------------------------------------
# Makefile configuration — edit these for your run
# ---------------------------------------------------------------------------

TOOL ?= verilator
TB   ?= top
GUI  ?= 0

# ---------------------------------------------------------------------------
# Constant variables
# ---------------------------------------------------------------------------

RTL_VC     := rtl/design.vc
TB_DIR     := tb
TB_FILE    := $(TB_DIR)/tb_$(TB).sv
BUILD_DIR  := build
TOP_MODULE := tb

# ---------------------------------------------------------------------------
# Simulation
# ---------------------------------------------------------------------------

VFLAGS := -cc --exe --build -j --timing
ifeq ($(GUI),1)
	# using FST instead of VCD as the ECDSA simulation dump is in GBs range
	# FST is 5-20x smaller in general
	VFLAGS += --trace-fst
endif

.PHONY: sim
sim:
ifeq ($(TOOL),verilator)
	@echo "-- VERILATE & BUILD --------"
	verilator $(VFLAGS) -F $(RTL_VC) +incdir+$(TB_DIR) $(TB_FILE) --top-module $(TOP_MODULE) tb/sim_main.cpp --Mdir $(BUILD_DIR)
	@echo "-- RUN ---------------------"
	$(BUILD_DIR)/V$(TOP_MODULE)
	@echo "-- DONE --------------------"
ifeq ($(GUI),1)
	@echo "-- OPENING GUI -------------"
	gtkwave dump.fst
endif
else
	$(error Unknown sim tool '$(TOOL)'. See 'make help' for valid options)
endif

# ---------------------------------------------------------------------------
# Lint
# ---------------------------------------------------------------------------
# ALL_CAPS | CamelCase
VERIBLE_RULES_FLAG := --rules="parameter-name-style=localparam_style_regex:([A-Z][A-Z0-9]*(_[A-Z0-9]+)*|([A-Z][a-z0-9]*)+(_[0-9]+)?)"

.PHONY: lint
lint:
ifeq ($(GUI),1)
	$(error Lint GUI '$(GUI)' not supported. See 'make help' for valid options)
endif
ifeq ($(TOOL),verilator)
	@echo "-- RUN VERILATOR LINT -------"
	verilator --lint-only -Wall -F $(RTL_VC)
	@echo "-- DONE --------------------"
else ifeq ($(TOOL),verible)
	@echo "-- RUN VERIBLE LINT --------"
	cd $(dir $(RTL_VC)) &&                          \
	verible-verilog-lint $(VERIBLE_RULES_FLAG)  $(shell cat $(RTL_VC)) && \
	cd -
	@echo "-- DONE --------------------"
else
	$(error Unknown lint tool '$(TOOL)'. See 'make help' for valid options)
endif



# ---------------------------------------------------------------------------
# Housekeeping
# ---------------------------------------------------------------------------

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR) *.vcd *.fst *.vpd *.wlf *.log simv csrc

.PHONY: help
help:
	@echo ""
	@echo "RTL Makefile — available targets"
	@echo "────────────────────────────────────────────────────────"
	@echo "  sim      Run simulation (TOOL=verilator TB=top|ecdsa|arith GUI=0|1)"
	@echo "  lint     Run linter (TOOL=verilator|verible GUI=0)"
	@echo "  clean    Remove all generated build artefacts"
	@echo "  help     Print this message"

.DEFAULT_GOAL := help
