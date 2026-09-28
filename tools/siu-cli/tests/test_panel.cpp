// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Model-free checks on the MoA topology: each mode maps to the right number of
// reader/reviewer models (the auditor is the +1 for modes 2-4).
#include <cassert>
#include <cstdio>

#include "siu/audit.hpp"

using siu::audit::Mode;
using siu::audit::readers_for;

int main() {
    assert(readers_for(Mode::kSingle) == 1);        // 1 model, no auditor
    assert(readers_for(Mode::kOnePlusAudit) == 1);  // 1 reader + auditor
    assert(readers_for(Mode::kTwoPlusAudit) == 2);  // 2 readers + auditor
    assert(readers_for(Mode::kThreePlusAudit) == 3);// 3 readers + auditor
    std::puts("all panel topology tests passed");
    return 0;
}
