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
