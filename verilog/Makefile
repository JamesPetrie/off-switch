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

# -- HSS-LMS test signer: Cisco hash-sigs, the RFC 8554 reference (tb/hash-sigs) --
HASH_SIGS_DIR  := $(TB_DIR)/hash-sigs
HASH_SIGS_SRCS := hss.c hss_alloc.c hss_aux.c hss_common.c hss_compute.c hss_generate.c \
                  hss_keygen.c hss_param.c hss_reserve.c hss_sign.c hss_sign_inc.c \
                  hss_thread_single.c hss_verify.c hss_verify_inc.c hss_derive.c hss_zeroize.c \
                  lm_common.c lm_ots_common.c lm_ots_sign.c lm_ots_verify.c lm_verify.c \
                  endian.c hash.c sha256.c
HSS_SIGNER_SRCS := $(TB_DIR)/hss_signer.c $(TB_DIR)/dpi_hss.c
HSS_LIB         := $(BUILD_DIR)/libhss_signer.a
# hash-sigs hashes with OpenSSL's SHA-256
OPENSSL_CFLAGS  := $(shell pkg-config --cflags libcrypto 2>/dev/null || (p=$$(brew --prefix openssl@3 2>/dev/null) && echo "-I$$p/include"))
OPENSSL_LDFLAGS := $(shell pkg-config --libs libcrypto 2>/dev/null || (p=$$(brew --prefix openssl@3 2>/dev/null) && echo "-L$$p/lib -lcrypto"))
VLT_INC         := $(shell verilator --getenv VERILATOR_ROOT 2>/dev/null)/include/vltstd
HSS_CFLAGS      := -O2 -I$(HASH_SIGS_DIR) -I$(VLT_INC) $(OPENSSL_CFLAGS)

$(HSS_LIB): $(addprefix $(HASH_SIGS_DIR)/,$(HASH_SIGS_SRCS)) $(HSS_SIGNER_SRCS) $(TB_DIR)/hss_signer.h
	@test -f $(HASH_SIGS_DIR)/hss.h || { echo "tb/hash-sigs is empty: run 'git submodule update --init'"; exit 1; }
	@mkdir -p $(BUILD_DIR)/hss_signer
	@for f in $(addprefix $(HASH_SIGS_DIR)/,$(HASH_SIGS_SRCS)); do \
	    $(CC) $(HSS_CFLAGS) -w -c $$f -o $(BUILD_DIR)/hss_signer/$$(basename $$f .c).o || exit 1; done
	@for f in $(HSS_SIGNER_SRCS); do \
	    $(CC) $(HSS_CFLAGS) -Wall -Werror -c $$f -o $(BUILD_DIR)/hss_signer/$$(basename $$f .c).o || exit 1; done
	ar rcs $@ $(BUILD_DIR)/hss_signer/*.o

# Only the HSS testbenches sign
ifneq ($(filter hss top_hss,$(TB)),)
SIM_DEPS    := $(HSS_LIB)
SIM_LDFLAGS := -LDFLAGS "$(abspath $(HSS_LIB)) $(OPENSSL_LDFLAGS)"
endif
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
sim: $(SIM_DEPS)
ifeq ($(TOOL),verilator)
	@echo "-- VERILATE & BUILD --------"
	verilator $(VFLAGS) -F $(RTL_VC) +incdir+$(TB_DIR) $(TB_FILE) $(SVA_FILE) --top-module $(TOP_MODULE) tb/sim_main.cpp $(SIM_LDFLAGS) --Mdir $(BUILD_DIR)
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

# Regenerate the test signers' public keys (rtl/hss_keys.svh) from the fixed seeds
.PHONY: hss-keys
hss-keys: $(HSS_LIB)
	$(CC) $(HSS_CFLAGS) -Wall $(TB_DIR)/gen_hss_keys.c $(HSS_LIB) $(OPENSSL_LDFLAGS) -o $(BUILD_DIR)/gen_hss_keys
	$(BUILD_DIR)/gen_hss_keys > rtl/hss_keys.svh
	@echo "wrote rtl/hss_keys.svh"

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
	@echo "  hss-keys Regenerate rtl/hss_keys.svh, the HSS test signers' public keys (hash-sigs)"
	@echo "  clean    Remove all generated build artefacts"
	@echo "  help     Print this message"

.DEFAULT_GOAL := help
