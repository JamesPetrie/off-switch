# Synthesis script for security_block — runs yosys + slang, maps to the
# standard-cell library pointed to by $CELL_LIB, and reports area.
# Also emits a gate-mapped Verilog netlist for downstream Static Timing
# Analysis (OpenSTA).
#
# Note: using .tcl instead of .ys to allow passing arguments
#
# Usage (the `--` is required so yosys passes args to Tcl):
#     yosys -c synth/yosys_synth.tcl -- <RTL_VC> <CRYPTO_TYPE> <OUT_DIR>

if {$argc != 3} {
    error "Usage: yosys -c path/to/yosys_synth.tcl -- <RTL_VC> <CRYPTO_TYPE> <OUT_DIR>"
}
set RTL_VC      [lindex $argv 0]
set CRYPTO_TYPE [lindex $argv 1]
set OUT_DIR     [lindex $argv 2]
set TOP_MODULE security_block
if {[info exists ::env(SYNTH_TOP)]} {
    set TOP_MODULE $::env(SYNTH_TOP)
}

yosys plugin -i slang

# Read all RTL via slang (handles both Verilog-2005 and SystemVerilog).
yosys read_slang --top $TOP_MODULE -F $RTL_VC -G CRYPTO_TYPE=$CRYPTO_TYPE
yosys synth -top $TOP_MODULE

# Map to standard cells
yosys dfflibmap -liberty $::env(CELL_LIB)

# Generate ABC constraint file from env. -constr's side-effect is to enable
# ABC's extended script with buffer/upsize/dnsize (needed for delay-driven
# sizing). The driving cell matches our FO4 reference cell.
set constr_path $OUT_DIR/abc.constr
set fh [open $constr_path w]
puts $fh "set_driving_cell $::env(FO4_CELL)"
puts $fh "set_load 5.0"
close $fh

# -D <ps> sets ABC's delay target.
yosys abc -liberty $::env(CELL_LIB) -constr $constr_path -D $::env(TARGET_PERIOD_PS)

# Clean up dangling nets/cells so the emitted netlist is STA-friendly
yosys opt_clean -purge

# Report area
yosys stat -liberty $::env(CELL_LIB)

# Emit gate-mapped netlist for OpenSTA. Fixed filename also used in OpenSTA.
set NETLIST $OUT_DIR/netlist.v
yosys write_verilog -noattr -noexpr $NETLIST
puts "-- NETLIST: $NETLIST"
