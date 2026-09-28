// SPDX-License-Identifier: AGPL-3.0-or-later
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
// newlines preserved enough for section slicing.
std::string html_to_text(const std::string& html);

// Parse plain report text (from html_to_text) into the schema fields.
ParsedFields parse_report_text(const std::string& text);

// Convenience: html -> fields in one call.
ParsedFields parse_report_html(const std::string& html);

// "January 5, 2023" / "January 5 2023" -> "2023-01-05" ("" if unparseable).
std::string to_iso_date(const std::string& human);

}  // namespace siu
