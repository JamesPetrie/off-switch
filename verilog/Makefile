# ---------------------------------------------------------------------------
# Makefile configuration — edit these for your run
# ---------------------------------------------------------------------------

TOOL        ?= verilator
TB          ?= demo
GUI         ?= 0
CRYPTO_TYPE ?= 0

# ---------------------------------------------------------------------------
# Constant variables
# ---------------------------------------------------------------------------

RTL_VC     := rtl/design.vc
OWN_FILES  := rtl/own_files.vc # list of all files owned by this project, used for linting
TB_DIR     := tb
TB_FILE    := $(TB_DIR)/tb_$(TB).sv
BUILD_DIR  := build
TOP_MODULE := tb
DPI_FILE   := $(TB_DIR)/dpi_sha256.cpp
SVA_FILE   := $(TB_DIR)/rtl_sva.sv # assertions bound into the RTL, simulation only

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
	verilator $(VFLAGS) -F $(RTL_VC) +incdir+$(TB_DIR) $(TB_FILE) $(SVA_FILE) $(DPI_FILE) --top-module $(TOP_MODULE) tb/sim_main.cpp --Mdir $(BUILD_DIR)
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
	@echo "-- RUN VERILATOR LINT (CRYPTO_TYPE=$(CRYPTO_TYPE)) --"
	verilator --lint-only -Wall -F $(RTL_VC) lint/verilator_waivers.vlt --top-module security_block -GCRYPTO_TYPE=$(CRYPTO_TYPE)
	@echo "-- DONE --------------------"
else ifeq ($(TOOL),verible)
	@echo "-- RUN VERIBLE LINT --------"
	cd $(dir $(OWN_FILES)) &&                          \
	verible-verilog-lint $(VERIBLE_RULES_FLAG)  $(shell cat $(OWN_FILES)) && \
	cd -
	@echo "-- DONE --------------------"
else
	$(error Unknown lint tool '$(TOOL)'. See 'make help' for valid options)
endif



# ---------------------------------------------------------------------------
# Synthesis (yosys + slang, area/GE report)
# ---------------------------------------------------------------------------
# Maps the design to standard cells via the Liberty file in $CELL_LIB and
# reports total cell area. Divide by the NAND2 area in that lib for GE count.

YOSYS_SCRIPT  := synth/yosys_synth.tcl
STA_SCRIPT    := synth/opensta_sta.tcl
SYNTH_OUT_DIR := $(BUILD_DIR)/synth_out/crypto_type_$(CRYPTO_TYPE)
AREA_LOG      := $(SYNTH_OUT_DIR)/area_report.log
TIMING_LOG    := $(SYNTH_OUT_DIR)/timing_report.log

.PHONY: synth
synth:
ifndef CELL_LIB
	$(error CELL_LIB is not set. Source ../dotshrc (or export CELL_LIB) first)
endif
	@mkdir -p $(SYNTH_OUT_DIR)
	@echo "-- SYNTH (CRYPTO_TYPE=$(CRYPTO_TYPE)) --"
	yosys -l $(AREA_LOG) -c $(YOSYS_SCRIPT) -- $(RTL_VC) $(CRYPTO_TYPE) $(SYNTH_OUT_DIR)
	@echo "-- STA --"
	cd $(SYNTH_OUT_DIR) && sta -no_init -exit $(abspath $(STA_SCRIPT)) | tee $(notdir $(TIMING_LOG))
	@echo "-- LOGS: $(AREA_LOG)"
	@echo "         $(TIMING_LOG)"

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
	@echo "  sim      Run simulation (TOOL=verilator TB=demo|top_ecdsa|top_hss|ecdsa|arith|hss GUI=0|1)"
	@echo "  lint     Run linter (TOOL=verilator|verible GUI=0)"
	@echo "  synth    Run yosys synthesis and report cell area (CRYPTO_TYPE=0|1). Requires CELL_LIB env var."
	@echo "  clean    Remove all generated build artefacts"
	@echo "  help     Print this message"

.DEFAULT_GOAL := help
