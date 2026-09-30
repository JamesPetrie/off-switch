// Handshake assertions for the license stream.
//
// Bound into the RTL rather than carried inside it, so the synthesisable
// sources stay free of verification-only code. Pulled in by the simulation
// build; the lint targets do not see it.

module security_block_sva #(
    parameter int unsigned W = 1
) (
    input logic         clk,
    input logic         license_valid,
    input logic         license_ready,
    input logic [W-1:0] license_data,
    input logic         publishing
);
    // Beats are only taken once a verification is under way.
    assert property (@(posedge clk) publishing |-> !license_ready);

    // Strict valid/ready: a presented beat is held, with its data unchanged,
    // until it is accepted. A producer may pause between beats, never
    // withdraw one.
    assert property (@(posedge clk)
        license_valid && !license_ready |=> license_valid && $stable(license_data));
endmodule

module hss_verify_sva #(
    parameter int unsigned DW = 1
) (
    input logic          clk,
    input logic          valid,
    input logic          ready,
    input logic [DW-1:0] data,
    input logic          verify_done
);
    // Completing a verification and asking for another beat are different
    // events; they must never coincide. This belongs here rather than at the
    // top level: for ECDSA the two are deliberately the same signal.
    assert property (@(posedge clk) verify_done |-> !ready);

    // The strict handshake as the engine sees it: security_block passes the
    // stream through unchanged while a verification is in flight, so a beat
    // presented here is likewise held until accepted.
    assert property (@(posedge clk) valid && !ready |=> valid && $stable(data));
endmodule

bind security_block security_block_sva #(.W(LICENSE_BEAT_W)) u_sva (
    .clk           (clk),
    .license_valid (license_valid),
    .license_ready (license_ready),
    .license_data  (license_data),
    .publishing    (state_q == StPublishAndWait)
);

bind hss_verify hss_verify_sva #(.DW(DW)) u_sva (
    .clk         (clk),
    .valid       (valid),
    .ready       (ready),
    .data        (data),
    .verify_done (verify_done)
);
