// SPDX-License-Identifier: AGPL-3.0-or-later
// Canonical home of the SIU parse/resolve core. rmorie (src/siu/) and morie
// (morie.siu.native) carry ports of these sources; edit here first, then port.
//
// parse.hpp -- native HTML -> structured-fields parser for Ontario SIU
// director's reports. C++17 port of the morie Python parser
// (src/morie/siu/_parser.py) covering the 16 panel-reviewed schema fields
// (schema.hpp) plus the language tag. Deterministic, offline, no model.
#pragma once
#include <map>
#include <string>

namespace siu {

// Digits from a regex capture, as a small count. std::stoi throws
// std::out_of_range past INT_MAX ("SO #4444444444444444" -- the fuzzer found
// it) and std::invalid_argument on "", and neither is caught by a parser that
// is only counting tags. No count, ordinal, day or year in a report has more
// than six digits, so anything longer is noise and reads as 0, which every
// caller already treats as "no number here".
inline int small_int(const std::string& digits) {
    if (digits.empty() || digits.size() > 6) return 0;
    int v = 0;
    for (const char c : digits) {
        if (c < '0' || c > '9') return 0;
        v = v * 10 + (c - '0');
    }
    return v;
}

// field name -> extracted value ("" when the report does not state it).
// Keys: the 16 schema fields + "_language" ("en" / "fr" / "unknown").
using ParsedFields = std::map<std::string, std::string>;

// Strip tags/scripts/entities from raw report HTML into plain text with
// newlines preserved enough for section slicing. Ends with normalize_text().
std::string html_to_text(const std::string& html);

// The text every extractor runs on: spaces collapsed, newlines trimmed and
// limited to two in a row, no line longer than kMaxLine. Plain-text entry
// points (resolve_subject_officials) apply it before any regex runs, because
// the regex engine recurses once per character a repeated atom consumes.
extern const size_t kMaxLine;
std::string normalize_text(const std::string& text);

// Parse plain report text (from html_to_text) into the schema fields.
ParsedFields parse_report_text(const std::string& text);

// Convenience: html -> fields in one call.
ParsedFields parse_report_html(const std::string& html);

// "January 5, 2023" / "January 5 2023" -> "2023-01-05" ("" if unparseable).
std::string to_iso_date(const std::string& human);

}  // namespace siu
