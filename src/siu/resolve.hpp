// SPDX-License-Identifier: AGPL-3.0-or-later
//
// resolve.hpp -- deterministic extraction of count-type fields (e.g. the
// subject-officer count) from an SIU director's report, for the residual the
// LLM panel leaves unresolved. Text-only, no model, fully reproducible.
#pragma once
#include <optional>
#include <string>

namespace siu {

// The resolved subject-officer count plus the human-readable evidence for it.
struct SoResolution {
    std::optional<int> count;  // nullopt = needs a human read
    std::string reason;        // why this count (or why unresolved)
};

// Strip the standard SIU privacy boilerplate ("This information may include ...
// Subject Officer name(s) ...") so it never counts as a substantive mention.
std::string strip_boilerplate(const std::string& report_text);

// Resolve the subject-officer count from report text (after strip_boilerplate,
// which also removes the witness-officer glossary note). Rule order:
//   0. the "Subject Officials/Officers" Team block: max(highest "SO #N" ordinal,
//      interview-status entries such as "SO Interviewed")  -> that number
//   1. document-wide highest ordinal, only when an "SO #1" anchor exists
//   2. spelled-out / numeric plural ("the two subject officials") -> that number
//   3. a subject officer is PRESENT (singular "the SO" / "the subject official",
//      no plural)                                        -> 1
//   4. a direct zero assertion ("no subject officials", "did not designate a
//      subject official")                                -> 0
//   5. otherwise                                         -> unresolved (nullopt)
// Rule 3 MUST precede rule 4: a 1-SO report often also says some witness
// officer "is not a subject official".
SoResolution resolve_subject_officials(const std::string& report_text);

}  // namespace siu
