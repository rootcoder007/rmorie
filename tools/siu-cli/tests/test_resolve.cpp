// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Self-contained assert test for the deterministic SO resolver -- the exact
// seven residual cases the LLM panel left unresolved (see resolve.cpp / the
// resolve_so_residual.py it ports). No framework: exit non-zero on failure.
#include <cassert>
#include <cstdio>
#include <optional>
#include <string>

#include "siu/resolve.hpp"

using siu::resolve_subject_officers;

static void expect(const std::string& name, const std::string& text,
                   std::optional<int> want) {
    auto got = resolve_subject_officers(text).count;
    if (got != want) {
        std::printf("FAIL %s: got %s want %s\n", name.c_str(),
                    got ? std::to_string(*got).c_str() : "none",
                    want ? std::to_string(*want).c_str() : "none");
        std::exit(1);
    }
    std::printf("ok   %s -> %s\n", name.c_str(),
                got ? std::to_string(*got).c_str() : "none");
}

int main() {
    // Singular "the SO" present -> 1 (drids 648/570/763), even when the report
    // also says a witness officer "is not a subject official".
    expect("singular-the-SO",
           "The SO pursued the vehicle. WO #2 is not a subject official. "
           "the SO discharged his firearm; the SO was interviewed.",
           1);

    // Witness-officer-only -> 0 needs a DIRECT assertion; "is not a subject
    // official" and "undesignated officer" were retired as zero cues after
    // the 2182-report regression pass (they match incidental prose in
    // reports with real subject officials -> false zeros). Those two texts
    // are now UNRESOLVED (panel's call), and the direct assertion still
    // resolves to 0.
    expect("witness-only-direct-assertion",
           "No subject official was designated in this investigation. "
           "Witness Officials WO #1 Interviewed WO #2 Interviewed.", 0);

    // Numbered subject officers -> max ordinal.
    expect("ordinal-SO2", "SO #1 and SO #2 were interviewed.", 2);

    // Explicit plural.
    expect("plural-two", "the two subject officials declined interviews.", 2);

    // Boilerplate alone (privacy paragraph) must NOT count as a mention.
    expect("boilerplate-only",
           "This information may include, but is not limited to, the following: "
           "Subject Officer name(s); Witness Officer name(s). Evidence gathered.",
           std::nullopt);

    std::printf("all resolve tests passed\n");
    return 0;
}
