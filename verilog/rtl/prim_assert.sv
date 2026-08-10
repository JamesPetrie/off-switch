// Local stub of OpenTitan/Pavona prim_assert.sv: the vendored SHA-2 core
// includes it for `ASSERT, defined away here so the vendored files stay
// byte-identical. Off-switch sim assertions live in tb/rtl_sva.sv.

`ifndef PRIM_ASSERT_SV
`define PRIM_ASSERT_SV

`define ASSERT(__name, __prop, __clk = clk_i, __rst = rst_ni)

`endif // PRIM_ASSERT_SV
