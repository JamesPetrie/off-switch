#!/usr/bin/env bash
# Formal equivalence of hss_verify between two git refs.
#
# Extracts hss_verify from each ref with yosys + slang, leaving sha2_wrap as a
# black box so that only the verifier's own logic is compared, and proves the
# two netlists equivalent with equiv_simple / equiv_induct. Registers and
# ports are matched by name, so a refactor that keeps them proves quickly;
# renamed state would have to be matched by hand.
#
# usage: tools/equiv_check.sh <gold-ref> [<gate-ref>]      (gate defaults to HEAD)
# needs: yosys with the slang plugin on PATH; compares committed trees only.

set -euo pipefail

gold_ref=${1:?usage: $0 <gold-ref> [<gate-ref>]}
gate_ref=${2:-HEAD}
root=$(git rev-parse --show-toplevel)
work=$(mktemp -d)
keep=0
cleanup() {
    for n in gold gate; do
        git -C "$root" worktree remove --force "$work/$n" 2>/dev/null || true
    done
    if [ "$keep" = 0 ]; then rm -rf "$work"; else echo "logs kept in $work"; fi
}
trap cleanup EXIT

extract() {   # <name> <ref>
    local name=$1 ref=$2
    git -C "$root" worktree add -q "$work/$name" "$ref"
    (cd "$work/$name/verilog" && yosys -q -l "$work/$name.log" -p "
        plugin -i slang
        read_slang --top hss_verify -F rtl/design.vc
        hierarchy -top hss_verify
        blackbox sha2_wrap
        hierarchy -top hss_verify
        proc
        flatten
        opt_clean
        async2sync
        rename hss_verify $name
        write_rtlil $work/$name.il" > /dev/null)
}

extract gold "$gold_ref"
extract gate "$gate_ref"

if yosys -q -l "$work/equiv.log" -p "
        read_rtlil $work/gold.il
        read_rtlil $work/gate.il
        equiv_make gold gate equiv
        hierarchy -top equiv
        equiv_simple -seq 5
        equiv_induct -seq 5
        equiv_status -assert" > /dev/null 2>&1
then
    grep -E "Of those cells" "$work/equiv.log"
    echo "hss_verify @ $gate_ref is equivalent to hss_verify @ $gold_ref"
else
    keep=1
    grep -E "unproven|Unproven|ERROR" "$work/equiv.log" | tail -5
    echo "hss_verify @ $gate_ref: equivalence to $gold_ref NOT proven"
    exit 1
fi
