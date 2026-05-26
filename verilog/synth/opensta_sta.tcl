# OpenSTA script — reports the worst combinational path of security_block.
#
# Reads the gate-mapped netlist from the cwd. Runs OpenSTA.
#
# Expects $CELL_LIB to be set.

# Clock period matches the synth target (TARGET_PERIOD_PS, in ps) so the
# reported slack reads directly as miss/meet against that target.
set PERIOD_NS [expr {$::env(TARGET_PERIOD_PS) / 1000.0}]

read_liberty $::env(CELL_LIB)
read_verilog netlist.v
link_design security_block

create_clock -name clk -period $PERIOD_NS [get_ports clk]
set_input_delay  0 -clock clk [all_inputs]
set_output_delay 0 -clock clk [all_outputs]

# Worst max-delay (setup) path — full report for visibility
report_checks -path_delay max -group_path_count 1 -fields {slew capacitance}

# Critical path delay ≈ PERIOD − worst_slack (over-estimates by ~1 ns
# of library setup time, which is conservative).
set delay_ns [expr {$PERIOD_NS - [worst_slack -max]}]

puts ""
puts [format "Critical path delay: %.3f ns" $delay_ns]
