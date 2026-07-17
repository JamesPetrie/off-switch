# ---------------------------------------------------------------------------
# Makefile configuration — edit these for your run
# ---------------------------------------------------------------------------

TOOL        ?= verilator
TB          ?= demo
VERILATOR_JOBS ?= 4
GUI         ?= 0
CRYPTO_TYPE ?= 0
SLH_STREAM_INPUT ?= 0

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
PYNQ_IP_DIR := ../pynq-z2/ip_repo/off_switch_axi_1_0/hdl
PYNQ_TOP    := $(PYNQ_IP_DIR)/off_switch_axi.v
PYNQ_SLAVE  := $(PYNQ_IP_DIR)/off_switch_axi_slave_lite_v1_0_S00_AXI.sv

ifeq ($(TB),pynq_axi_slh)
EXTRA_RTL := $(PYNQ_SLAVE)
endif

# ---------------------------------------------------------------------------
# Simulation
# ---------------------------------------------------------------------------

VFLAGS := -cc --exe --build -j $(VERILATOR_JOBS) --timing
ifeq ($(GUI),1)
	# using FST instead of VCD as the ECDSA simulation dump is in GBs range
	# FST is 5-20x smaller in general
	VFLAGS += --trace-fst
endif

.PHONY: sim
sim:
ifeq ($(TOOL),verilator)
	@echo "-- VERILATE & BUILD --------"
	verilator $(VFLAGS) -F $(RTL_VC) $(EXTRA_RTL) +incdir+$(TB_DIR) $(TB_FILE) $(DPI_FILE) --top-module $(TOP_MODULE) tb/sim_main.cpp --Mdir $(BUILD_DIR)
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
	@echo "-- RUN VERILATOR LINT (CRYPTO_TYPE=$(CRYPTO_TYPE), SLH_STREAM_INPUT=$(SLH_STREAM_INPUT)) --"
	verilator --lint-only -Wall -F $(RTL_VC) lint/verilator_waivers.vlt --top-module security_block -GCRYPTO_TYPE=$(CRYPTO_TYPE) -GSLH_STREAM_INPUT=$(SLH_STREAM_INPUT)
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

.PHONY: pynq-lint pynq-client-test
pynq-lint:
	@echo "-- RUN PYNQ-Z2 WRAPPER LINT (ECDSA) --"
	verilator --lint-only -Wall -Wno-SYNCASYNCNET -F $(RTL_VC) lint/verilator_waivers.vlt $(PYNQ_TOP) $(PYNQ_SLAVE) --top-module off_switch_axi -GCRYPTO_TYPE=0
	@echo "-- RUN PYNQ-Z2 WRAPPER LINT (STREAMED SLH-DSA) --"
	verilator --lint-only -Wall -Wno-SYNCASYNCNET -F $(RTL_VC) lint/verilator_waivers.vlt $(PYNQ_TOP) $(PYNQ_SLAVE) --top-module off_switch_axi -GCRYPTO_TYPE=2
	@echo "-- DONE --------------------"

pynq-client-test:
	python3 tools/test_pynq_slh_client.py



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
	@echo "-- SYNTH (CRYPTO_TYPE=$(CRYPTO_TYPE), 0=ECDSA 1=HSS 2=SLH-DSA) --"
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
	@echo "  sim      Run simulation (TOOL=verilator TB=demo|top_ecdsa|top_hss|top_slh|top_slh_stream|pynq_axi_slh|ecdsa|arith|hss|slh_dsa_verify|slh_fors_pk_from_sig|slh_hash_fh|slh_hash_tl|slh_hmsg|slh_ht_verify|slh_sig_parser|slh_wots_pk_from_sig|slh_xmss_pk_from_sig GUI=0|1)"
	@echo "  lint     Run linter (TOOL=verilator|verible GUI=0)"
	@echo "  pynq-lint        Lint the PYNQ-Z2 AXI wrapper in ECDSA and SLH-DSA modes."
	@echo "  pynq-client-test Test canonical-message and signature-stream byte order."
	@echo "  synth    Run yosys synthesis and report cell area (CRYPTO_TYPE=0|1|2). Requires CELL_LIB env var."
	@echo "  clean    Remove all generated build artefacts"
	@echo "  help     Print this message"

.DEFAULT_GOAL := help
