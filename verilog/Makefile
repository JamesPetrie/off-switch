# ---------------------------------------------------------------------------
# Makefile configuration — edit these for your run
# ---------------------------------------------------------------------------

TOOL ?= verilator
TB   ?= mod_add
GUI  ?= 0

# ---------------------------------------------------------------------------
# Constant variables
# ---------------------------------------------------------------------------

RTL_VC     := rtl/design.vc
TB_FILE    := tb/tb_$(TB).sv
BUILD_DIR  := build
TOP_MODULE := tb

# ---------------------------------------------------------------------------
# Simulation
# ---------------------------------------------------------------------------

.PHONY: sim
sim:
ifeq ($(TOOL),verilator)
	@echo "-- VERILATE & BUILD --------"
	verilator -cc --exe --build -j --timing --trace -F $(RTL_VC) $(TB_FILE) --top-module $(TOP_MODULE) tb/sim_main.cpp --Mdir $(BUILD_DIR)
	@echo "-- RUN ---------------------"
	$(BUILD_DIR)/V$(TOP_MODULE)
	@echo "-- DONE --------------------"
ifeq ($(GUI),1)
	@echo "-- OPENING GUI -------------"
	gtkwave dump.vcd
endif
else
	$(error Unknown sim tool '$(TOOL)'. See 'make help' for valid options)
endif

# ---------------------------------------------------------------------------
# Lint
# ---------------------------------------------------------------------------
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
	verible-verilog-lint  $(shell cat $(RTL_VC)) && \
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
	rm -rf $(BUILD_DIR) *.vcd *.vpd *.wlf *.log simv csrc

.PHONY: help
help:
	@echo ""
	@echo "RTL Makefile — available targets"
	@echo "────────────────────────────────────────────────────────"
	@echo "  sim      Run simulation (TOOL=verilator TB=mod_add GUI=0|1)"
	@echo "  lint     Run linter (TOOL=verilator|verible GUI=0)"
	@echo "  clean    Remove all generated build artefacts"
	@echo "  help     Print this message"

.DEFAULT_GOAL := help
