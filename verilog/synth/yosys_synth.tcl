# Synthesis script for security_block — runs yosys + slang, maps to the
# standard-cell library pointed to by $CELL_LIB, and reports area.
#
# Note: using .tcl instead of .ys to allow passing arguments
#
# Usage (the `--` is required so yosys passes args to Tcl):
#     yosys -c synth/yosys_synth.tcl -- <RTL_VC> <CRYPTO_TYPE>

if {$argc != 2} {
    error "Usage: yosys -c path/to/yosys_synth.tcl -- <RTL_VC> <CRYPTO_TYPE>"
}
set RTL_VC      [lindex $argv 0]
set CRYPTO_TYPE [lindex $argv 1]

yosys plugin -i slang

# Read all RTL via slang (handles both Verilog-2005 and SystemVerilog).
yosys read_slang --top security_block -F $RTL_VC -G CRYPTO_TYPE=$CRYPTO_TYPE
yosys synth -top security_block

# Map to standard cells
yosys dfflibmap -liberty $::env(CELL_LIB)
yosys abc -liberty $::env(CELL_LIB)

# Report area
yosys stat -liberty $::env(CELL_LIB)
