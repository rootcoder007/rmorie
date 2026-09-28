// SPDX-License-Identifier: AGPL-3.0-or-later
#include "siu/resolve.hpp"

#include <algorithm>
#include <array>
#include <regex>
#include <unordered_map>

namespace siu {
namespace {

const std::unordered_map<std::string, int> kWordNum = {
    {"one", 1}, {"two", 2}, {"three", 3}, {"four", 4},  {"five", 5},
    {"six", 6}, {"seven", 7}, {"eight", 8}, {"nine", 9}, {"ten", 10}};

int count_matches(const std::string& s, const std::regex& re) {
    return static_cast<int>(
        std::distance(std::sregex_iterator(s.begin(), s.end(), re),
                      std::sregex_iterator()));
}

}  // namespace

std::string strip_boilerplate(const std::string& t) {
    // Reports mix in UTF-8 non-breaking spaces ("SO\u00A0#1"); \s never
    // matches them in byte-mode std::regex, so normalize to plain spaces
    // before any rule runs.
    std::string norm;
    norm.reserve(t.size());
    for (size_t i = 0; i < t.size(); ++i) {
        if (i + 1 < t.size() && static_cast<unsigned char>(t[i]) == 0xC2 &&
            static_cast<unsigned char>(t[i + 1]) == 0xA0) {
            norm += ' ';
            ++i;
        } else {
            norm += t[i];
        }
    }
    // The privacy paragraph runs from "this information may include" to the
    // first "affected person" / "evidence". Remove every occurrence.
    static const std::regex kBoiler(
        R"(this information may include[\s\S]*?(?:affected person|evidence)\.?)",
        std::regex::icase);
    std::string out = std::regex_replace(norm, kBoiler, " ");
    // The witness-officer glossary note ("a witness officer is a police
    // officer who, in the opinion of the SIU Director, is involved in the
    // incident under investigation but is not a subject officer...") appears
    // in most reports and must never feed the zero-SO rule.
    // Two phrasings across report eras, straight or curly apostrophe:
    //   "who, in the opinion of the SIU Director, ... not a subject officer"
    //   "who, in the SIU Director's opinion, ... not a subject officer"
    static const std::regex kGlossary(
        "who,?\\s+in the (?:opinion of the SIU Director|"
        "SIU Director(?:'|\xE2\x80\x99)?s opinion),?"
        "[\\s\\S]{0,120}?not a subject offic(?:er|ial)[^.]*\\.?",
        std::regex::icase);
    return std::regex_replace(out, kGlossary, " ");
}

SoResolution resolve_subject_officers(const std::string& report_text) {
    const std::string body = strip_boilerplate(report_text);

    // Ordinal scanners. "SO" is case-strict with a left word boundary and
    // "#" is REQUIRED: an icase optional-# variant matched "...also 59..."
    // and blew counts up.
    static const std::regex kOrdSo(R"(\bSO\s*#\s*(\d{1,2})\b)");
    static const std::regex kOrdSpelled(
        R"(subject offic(?:er|ial)\s*#\s*(\d{1,2})\b)", std::regex::icase);
    auto max_ordinal = [](const std::string& s) {
        int mo = 0;
        for (const auto* re : {&kOrdSo, &kOrdSpelled}) {
            for (auto it = std::sregex_iterator(s.begin(), s.end(), *re);
                 it != std::sregex_iterator(); ++it) {
                mo = std::max(mo, std::stoi((*it)[1].str()));
            }
        }
        return mo;
    };

    // 0. The Team block under the "Subject Officials"/"Subject Officers"
    // heading is authoritative: one "SO"/"SO #N" entry per official. The
    // narrative can mention other forces' officer shorthands ("SO #7 of
    // YRP"), so the section is scanned FIRST and the whole document is only
    // a fallback.
    static const std::regex kSection(R"(Subject Offic(?:er|ial)s\b)");
    // Terminate the window only at a real heading (line-anchored), never at
    // a word like "Evidence" inside an entry's prose -- that truncated a
    // window before "SO #2" once and undercounted.
    static const std::regex kNextSection(
        R"(\n\s{0,3}(?:Witness Offic(?:er|ial)s|Civilian Witness(?:es)?|Service Employee Witness|Incident Narrative|Materials [Oo]btained|The Scene|Evidence\n|Nature of Injur))");
    std::smatch sec;
    if (std::regex_search(body, sec, kSection)) {
        std::string window = body.substr(
            static_cast<size_t>(sec.position(0)) + sec.length(0), 2500);
        std::smatch nxt;
        if (std::regex_search(window, nxt, kNextSection)) {
            window = window.substr(0, static_cast<size_t>(nxt.position(0)));
        }
        const int sec_ord = max_ordinal(window);
        // Un-numbered entries: one interview-status line per official
        // ("SO Interviewed", "SO Declined interview..."). Prose references
        // ("the SO declined to...") don't match the tag-then-status shape.
        static const std::regex kEntry(
            R"(\bSO\s*(?:#\s*\d{1,2})?\s{0,3}(?:Interviewed|Declined|Did not consent|Not interviewed))");
        const int entries = count_matches(window, kEntry);
        // A report can mislabel two officials with the same ordinal
        // ("SO #1 ... SO #1 ..."), so the entry count can legitimately
        // exceed the highest ordinal -- take the max of the two signals.
        const int sec_n = std::max(sec_ord, entries);
        if (sec_n > 0) {
            return {sec_n, "section: max(ordinal " + std::to_string(sec_ord) +
                               ", entries " + std::to_string(entries) + ")"};
        }
    }

    // 1. Document-wide highest ordinal (older reports without a Team
    // block). A real roster always starts at #1; a lone high ordinal in the
    // narrative ("SO #7 of YRP") is another force's shorthand -- require
    // the #1 anchor before trusting the document-wide maximum.
    static const std::regex kAnchor1(
        R"(\bSO\s*#\s*1\b|subject offic(?:er|ial)\s*#\s*1\b)");
    const int max_ord = max_ordinal(body);
    if (max_ord > 0 && std::regex_search(body, kAnchor1)) {
        return {max_ord, "max ordinal SO #" + std::to_string(max_ord)};
    }

    // 2. Spelled-out / numeric plural: "the two subject officials".
    static const std::regex kPlural(
        R"(\bthe\s+(\d{1,2}|one|two|three|four|five|six|seven|eight|nine|ten)\s+subject offic(?:er|ial)s\b)",
        std::regex::icase);
    std::smatch m;
    if (std::regex_search(body, m, kPlural)) {
        const std::string tok = m[1].str();
        int n = 0;
        std::string low = tok;
        std::transform(low.begin(), low.end(), low.begin(), ::tolower);
        auto wit = kWordNum.find(low);
        n = (wit != kWordNum.end()) ? wit->second : std::stoi(tok);
        return {n, "plural cue '" + m[0].str() + "'"};
    }

    // 3. A subject officer is PRESENT (runs BEFORE the zero-rule).
    // "SO" stays case-strict (icase would match "the so-called"); only the
    // article is sentence-initial-tolerant.
    static const std::regex kTheSo(R"(\b[Tt]he SO\b)");
    static const std::regex kTheSubj(R"(\bthe subject offic(?:er|ial)\b)",
                                     std::regex::icase);
    static const std::regex kAnyPlural(
        R"(\bthe SOs\b|the subject offic(?:er|ial)s\b)", std::regex::icase);
    const int the_so = count_matches(body, kTheSo);
    const int the_subj = count_matches(body, kTheSubj);
    const bool plural = std::regex_search(body, kAnyPlural);
    if ((the_so + the_subj) >= 1 && !plural) {
        return {1, "singular present: 'the SO'x" + std::to_string(the_so) +
                       " 'the subject official'x" + std::to_string(the_subj)};
    }

    // 4. Explicitly ZERO subject officers (witness-officer-only cases).
    static const std::array<std::regex, 2> kZero = {
        std::regex(R"(no subject offic(?:er|ial)s?\b)", std::regex::icase),
        std::regex(
            R"((?:did not|not|never)\s+designate[d]?\s+(?:a\s+|any\s+)?subject offic)",
            std::regex::icase)};
    // NOTE: "undesignated officer" and "is/was not a subject official" were
    // removed as zero cues -- both match incidental prose (bystander
    // officers, one-of-several negations) in reports with real subject
    // officials. Zero needs a direct assertion.
    for (const auto& re : kZero) {
        if (std::regex_search(body, re)) {
            return {0, "zero: witness-officer-only / 'not a subject official'"};
        }
    }

    // 5. Needs a human read.
    return {std::nullopt, "UNRESOLVED: 'the SO'x" + std::to_string(the_so) +
                              " 'the subj off'x" + std::to_string(the_subj)};
}

}  // namespace siu
