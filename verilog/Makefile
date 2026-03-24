# REVISIT is license below okay to reuse?
# ---------------------------------------------------------------------------
# This file ONLY is placed under the Creative Commons Public Domain
# SPDX-FileCopyrightText: 2020 Wilson Snyder
# SPDX-License-Identifier: CC0-1.0
#
# ---------------------------------------------------------------------------
# Check for sanity to avoid later confusion

ifneq ($(words $(CURDIR)),1)
  $(error Unsupported: GNU Make cannot build in directories containing spaces, build elsewhere: '$(CURDIR)')
endif

# ---------------------------------------------------------------------------
# Makefile configuration — edit these for your run
# ---------------------------------------------------------------------------

TOOL ?= verilator

# ---------------------------------------------------------------------------
# Constant variables
# ---------------------------------------------------------------------------

RTL       := rtl/top.sv
BUILD_DIR := build

# ---------------------------------------------------------------------------
# Simulation
# ---------------------------------------------------------------------------

.PHONY: sim
sim:
	@echo "-- VERILATE & BUILD --------"
	verilator -cc --exe --build -j $(RTL) tb/sim_main.cpp --Mdir $(BUILD_DIR)
	@echo "-- RUN ---------------------"
	$(BUILD_DIR)/Vtop
	@echo "-- DONE --------------------"

# ---------------------------------------------------------------------------
# Lint
# ---------------------------------------------------------------------------
.PHONY: lint
lint:
ifeq ($(TOOL),verilator)
	@echo "-- RUN VERILATOR LINT ----------"
	verilator --lint-only -Wall $(RTL)
	@echo "-- DONE --------------------"
else ifeq ($(TOOL),verible)
	@echo "-- RUN VERIBLE LINT ----------"
	verible-verilog-lint $(RTL)
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
	@echo "  sim      Run simulation"
	@echo "  lint     Run linter (TOOL=verilator|verible)"
	@echo "  clean    Remove all generated build artefacts"
	@echo "  help     Print this message"

.DEFAULT_GOAL := help
