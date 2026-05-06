# PYNQ-Z2 build: Zynq PS -> AXI Interconnect -> off_switch_axi -> LD0
# Output: ./build/off_switch.bit + ./build/off_switch.hwh for PYNQ overlay use.

set proj_name off_switch
set proj_dir  ./build
set part      xc7z020clg400-1
set vlnv      futureoflife.org:peter:off_switch_axi:1.0
set bd_name   design_1
set jobs      [exec nproc]

# --- .vc parser - AMD does not support .vc files natively, so vibe-coded a TCL parser
# Resolve a Verilog command (.vc) file into a flat list of absolute source
# paths. Recognizes the `-F <subfile>` include directive used by design.vc.
proc read_vc {path} {
    set out {}
    set fp  [open $path r]
    set dir [file dirname [file normalize $path]]
    while {[gets $fp line] >= 0} {
        set line [string trim $line]
        if {$line eq "" || [string match "//*" $line]} continue
        if {[regexp {^-F\s+(\S+)} $line -> sub]} {
            lappend out {*}[read_vc [file join $dir $sub]]
        } else {
            lappend out [file normalize [file join $dir $line]]
        }
    }
    close $fp
    return $out
}

# Use the official PYNQ project's PS7 config (extracted into pynqz2_ps7.tcl).
# Matches the running PYNQ-Z2 image exactly so loading our overlay doesn't
# disturb DDR/MIO/clocks (and therefore Ethernet/SSH).
source ./pynqz2_ps7.tcl

file delete -force $proj_dir

# --- One-time scaffold: generate AXI-Lite slave template, then patch the .v files.
# Subsequent runs reuse the patched ip_repo/ as-is.
if {![file exists ./ip_repo/off_switch_axi_1_0/component.xml]} {
    create_project ip_gen_tmp $proj_dir/ip_gen_tmp -part $part -force
    create_peripheral futureoflife.org peter off_switch_axi 1.0 -dir ./ip_repo
    add_peripheral_interface S00_AXI -interface_mode slave -axi_type lite \
        [ipx::find_open_core $vlnv]
    generate_peripheral [ipx::find_open_core $vlnv]
    write_peripheral    [ipx::find_open_core $vlnv]
    close_project
    file delete -force $proj_dir/ip_gen_tmp
}

# --- Project ---
create_project $proj_name $proj_dir -part $part -force

# --- Make the IP visible to this project ---
set_property ip_repo_paths ./ip_repo [current_project]
update_ip_catalog

# --- Re-package IP to pick up:
#   - patched slave HDL (LED out, wider AXI address bus, etc)
#   - the security_block + crypto + sha256 sources we now reference
# Idempotent: rerunning a no-op edit is fine.
ipx::edit_ip_in_project -upgrade true -name edit_ip_prj \
    -directory $proj_dir/edit_ip \
    ./ip_repo/off_switch_axi_1_0/component.xml

add_files -norecurse [read_vc ../verilog/rtl/design.vc]

ipx::merge_project_changes ports       [ipx::current_core]
ipx::merge_project_changes file_groups [ipx::current_core]
ipx::create_xgui_files                 [ipx::current_core]
ipx::update_checksums                  [ipx::current_core]
ipx::save_core                         [ipx::current_core]
close_project -delete
update_ip_catalog -rebuild

# --- Block design: PS7 + our AXI slave ---
create_bd_design $bd_name

# Zynq PS configured to match PYNQ-Z2 base overlay exactly (DDR/MIO/clocks).
# The full PCW_* dict comes from PYNQ project's pynqz2.tcl so loading our
# overlay leaves the running PS undisturbed (Ethernet/SSH stay alive).
create_bd_cell -type ip -vlnv xilinx.com:ip:processing_system7 ps7_0
apply_pynqz2_ps7_config [get_bd_cells ps7_0]
# Enable M_AXI_GP0 (preset has it off; the PYNQ base overlay turns it on).
# Drop FCLK0 from 100 MHz to 50 MHz — the ECDSA 256-bit modular arithmetic
# path won't close at 10 ns on this part (WNS ~ -7.5 ns).
set_property -dict [list \
    CONFIG.PCW_USE_M_AXI_GP0           {1}  \
    CONFIG.PCW_FPGA0_PERIPHERAL_FREQMHZ {50} \
] [get_bd_cells ps7_0]
make_bd_intf_pins_external [get_bd_intf_pins ps7_0/FIXED_IO]
make_bd_intf_pins_external [get_bd_intf_pins ps7_0/DDR]

# Our slave
create_bd_cell -type ip -vlnv $vlnv off_switch_axi_0

# Auto-wire PS M_AXI_GP0 -> AXI Interconnect -> off_switch_axi_0/S00_AXI
# (also adds Processor System Reset and ties FCLK0 to all clocks).
apply_bd_automation -rule xilinx.com:bd_rule:axi4 \
    -config { Master {/ps7_0/M_AXI_GP0} Clk {Auto} intc_ip {New AXI SmartConnect}} \
    [get_bd_intf_pins off_switch_axi_0/S00_AXI]

# Externalize the LED pin and rename the port to plain "led".
make_bd_pins_external [get_bd_pins off_switch_axi_0/led]
set_property name led [get_bd_ports led_0]

validate_bd_design
save_bd_design

# --- HDL wrapper around the BD ---
make_wrapper -files [get_files $bd_name.bd] -top
add_files -norecurse \
    [glob $proj_dir/$proj_name.gen/sources_1/bd/$bd_name/hdl/${bd_name}_wrapper.v]
set_property top ${bd_name}_wrapper [current_fileset]
update_compile_order -fileset sources_1

# --- Pin constraint for LD0 on PYNQ-Z2 ---
set xdc_file $proj_dir/pins.xdc
set fp [open $xdc_file w]
puts $fp "set_property PACKAGE_PIN R14 \[get_ports led]"
puts $fp "set_property IOSTANDARD LVCMOS33 \[get_ports led]"
close $fp
add_files -fileset constrs_1 -norecurse $xdc_file

# --- Synth, impl, bitstream ---
launch_runs synth_1 -jobs $jobs
wait_on_run synth_1
launch_runs impl_1 -to_step write_bitstream -jobs $jobs
wait_on_run impl_1

# --- Stage outputs for PYNQ (need matching .bit + .hwh basename) ---
set bit_src  $proj_dir/$proj_name.runs/impl_1/${bd_name}_wrapper.bit
set hwh_src  [glob $proj_dir/$proj_name.gen/sources_1/bd/$bd_name/hw_handoff/${bd_name}.hwh]
file copy -force $bit_src $proj_dir/${proj_name}.bit
file copy -force $hwh_src $proj_dir/${proj_name}.hwh
puts "PYNQ artifacts: $proj_dir/${proj_name}.bit  $proj_dir/${proj_name}.hwh"
