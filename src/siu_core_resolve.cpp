// SPDX-License-Identifier: AGPL-3.0-or-later
// Canonical home of the SIU parse/resolve core. rmorie (src/siu/) and morie
// (morie.siu.native) carry ports of these sources; edit here first, then port.
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

static std::string strip_boilerplate_impl(const std::string& t) {
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

// French reports (UES): the subject official is the "agent impliqu\xc3\xa9" ("AI no 1"), the witness
// official the "agent t\xc3\xa9moin" ("AT no 1"). The same rules as the English ones, in that vocabulary.
static SoResolution resolve_fr(const std::string& text) {
    // the definition notes ("[ Remarque : Un agent impliqu\xc3\xa9 est un agent ... ]") are not cues
    // and neither are the privacy paragraph ("le nom d'un agent impliqu\xc3\xa9, d'un agent t\xc3\xa9moin ...",
    // "y compris des t\xc3\xa9moins civils et des agents impliqu\xc3\xa9s"), the definitions ("On entend par
    // \xc2\xab agent impliqu\xc3\xa9 \xc2\xbb ...", "... n'est pas un agent impliqu\xc3\xa9") or the law ("les agents
    // impliqu\xc3\xa9s sont invit\xc3\xa9s \xc3\xa0 participer \xc3\xa0 une entrevue")
    static const std::regex kNote("Remarque\\s*:\\s*Un agent (?:impliqu|t\xc3\xa9moin)[^\\]\\n]*\\]?", std::regex::icase);
    static const std::regex kLegal(
        "[^.;\\n]*(?:sont invit\xc3\xa9s|y compris des|le nom (?:d(?:'|\xe2\x80\x99)un|de tout)|On entend par|"
        "n(?:'|\xe2\x80\x99)est pas un agent impliqu|n(?:'|\xe2\x80\x99)y sont pas)[^.;\\n]*[.;]?",
        std::regex::icase);
    const std::string body = std::regex_replace(std::regex_replace(text, kNote, " "), kLegal, " ");
    // "AI no 1", "AI n o 1", "agent impliqu\xc3\xa9 n o 1", "l'agent(e) impliqu\xc3\xa9(e) n o 1"
    static const std::regex kOrd("(?:\\bAI|agent(?:\\(e\\)|e)?\\s+impliqu\xc3\xa9(?:\\(e\\)|e)?)\\s*(?:#|n\\s?o\\.?|n\xc2[\xb0\xba])\\s*(\\d{1,2})\\b");
    // 0. The "Agent(s) impliqu\xc3\xa9(s)" section lists one entry per official ("AI no 1 A particip\xc3\xa9 \xc3\xa0
    //    une entrevue", or a lone "AI N'a pas consenti ..."): authoritative, as the English roster
    {
        static const std::regex kHead("(?:^|\\n)[ \\t]*Agents? impliqu\xc3\xa9(?:e|s|es)?[ \\t]*(?:\\([ \\t]*AI[ \\t]*\\))?[ \\t]*\\n");
        static const std::regex kNext("\\n[ \\t]*(?:Agents? t\xc3\xa9moins?|T\xc3\xa9moins? civils?|\xc3\x89l\xc3\xa9ments de preuve|Remarque|En vertu)");
        static const std::regex kEntry("(?:^|\\n)[ \\t]*AI\\b(?:\\s*(?:#|n\\s?o\\.?|n\xc2[\xb0\xba])\\s*(\\d{1,2}))?");
        size_t start = std::string::npos;
        for (auto it = std::sregex_iterator(body.begin(), body.end(), kHead); it != std::sregex_iterator(); ++it)
            start = static_cast<size_t>(it->position(0) + it->length(0));
        if (start != std::string::npos) {
            std::string win = body.substr(start, 2500);
            std::smatch nx;
            if (std::regex_search(win, nx, kNext)) win = win.substr(0, static_cast<size_t>(nx.position(0)));
            int entries = 0, ord = 0;
            for (auto it = std::sregex_iterator(win.begin(), win.end(), kEntry); it != std::sregex_iterator(); ++it) {
                ++entries;
                if ((*it)[1].matched) ord = std::max(ord, std::stoi((*it)[1].str()));
            }
            const int n = std::max(entries, ord);
            if (n > 0) return {n, "section: max(ordinal " + std::to_string(ord) + ", entries " + std::to_string(entries) + ")"};
        }
    }
    int mo = 0;
    bool one = false;
    for (auto it = std::sregex_iterator(body.begin(), body.end(), kOrd); it != std::sregex_iterator(); ++it) {
        const int k = std::stoi((*it)[1].str());
        mo = std::max(mo, k);
        one = one || k == 1;
    }
    static const std::unordered_map<std::string, int> kFrNum = {
        {"un", 1}, {"une", 1}, {"deux", 2}, {"trois", 3}, {"quatre", 4}, {"cinq", 5},
        {"six", 6}, {"sept", 7}, {"huit", 8}, {"neuf", 9}, {"dix", 10}};
    auto num = [&](std::string tok) {
        std::transform(tok.begin(), tok.end(), tok.begin(), ::tolower);
        const auto w = kFrNum.find(tok);
        return w != kFrNum.end() ? w->second : std::stoi(tok);
    };
    std::smatch m;
    if (mo > 0 && one) {
        return {mo, "max ordinal AI no " + std::to_string(mo)};
    }
    static const std::regex kPlural(
        "\\b(\\d{1,2}|deux|trois|quatre|cinq|six|sept|huit|neuf|dix)\\s+agent(?:e)?s\\s+impliqu", std::regex::icase);
    if (std::regex_search(body, m, kPlural)) return {num(m[1].str()), "plural cue '" + m[0].str() + "'"};
    static const std::regex kZero("\\baucun(?:e)?\\s+agent(?:e)?\\s+impliqu", std::regex::icase);
    if (std::regex_search(body, kZero)) return {0, "zero: 'aucun agent impliqu\xc3\xa9'"};
    static const std::regex kThe("\\bl(?:'|\xe2\x80\x99)\\s*(?:AI\\b|agent(?:e)?\\s+impliqu)", std::regex::icase);
    static const std::regex kPl("\\bles\\s+AI\\b|\\bagent(?:e)?s\\s+impliqu", std::regex::icase);
    const int the = count_matches(body, kThe);
    if (the >= 1 && !std::regex_search(body, kPl)) {
        return {1, "singular present: 'l'AI / l'agent impliqu\xc3\xa9'x" + std::to_string(the)};
    }
    static const std::regex kAt("\\bAT\\s*(?:#|n\\s?o\\.?|n\xc2[\xb0\xba])\\s*\\d|\\bagent(?:e)?s?\\s+t\xc3\xa9moin", std::regex::icase);
    static const std::regex kAnyAi("\\bAI\\b|agent(?:e)?s?\\s+impliqu", std::regex::icase);
    if (std::regex_search(body, kAt) && !std::regex_search(body, kAnyAi)) {
        return {0, "zero: witness officials only, no agent impliqu\xc3\xa9 named"};
    }
    return {std::nullopt, "UNRESOLVED (fr): 'l'AI'x" + std::to_string(the)};
}

static SoResolution resolve_subject_officials_impl(const std::string& report_text) {
    const std::string body = strip_boilerplate(report_text);
    {
        static const std::regex kUes("\\bUES\\b"), kSiu("\\bSIU\\b");
        static const std::regex kFrRole("agent(?:\\(e\\)|e)?s?\\s+impliqu"), kEnRole("subject offic", std::regex::icase), kEnTag("\\bSOs?\\b");
        if (count_matches(body, kUes) > count_matches(body, kSiu) ||
            (std::regex_search(body, kFrRole) && !std::regex_search(body, kEnRole) && !std::regex_search(body, kEnTag)))
            return resolve_fr(body);
    }

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
    // "SO" stays case-strict (as in the ordinal scan); the spelled-out form is matched in any
    // case -- "Subject Officer #1" never anchored and older reports went unresolved
    static const std::regex kAnchorSo(R"(\bSO\s*#\s*1\b)");
    static const std::regex kAnchorSpelled(R"(subject offic(?:er|ial)\s*#\s*1\b)", std::regex::icase);
    const int max_ord = max_ordinal(body);
    if (max_ord > 0 && (std::regex_search(body, kAnchorSo) || std::regex_search(body, kAnchorSpelled))) {
        return {max_ord, "max ordinal SO #" + std::to_string(max_ord)};
    }

    // 2. Spelled-out / numeric plural: "the two subject officials", "Two subject officials were
    //    designated", "designated two subject officers".
    static const std::regex kPlural(
        R"(\b(?:the\s+)?(\d{1,2}|two|three|four|five|six|seven|eight|nine|ten)\s+subject offic(?:er|ial)s\b)",
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
    static const std::array<std::regex, 3> kZero = {
        std::regex(R"(no subject offic(?:er|ial)s?\b)", std::regex::icase),
        std::regex(
            R"((?:did not|not|never)\s+designate[d]?\s+(?:a\s+|any\s+)?subject offic)",
            std::regex::icase),
        std::regex(R"(\bno (?:police )?(?:official|officer) was (?:a |the )?subject offic)", std::regex::icase)};
    // NOTE: "undesignated officer" and "is/was not a subject official" were
    // removed as zero cues -- both match incidental prose (bystander
    // officers, one-of-several negations) in reports with real subject
    // officials. Zero needs a direct assertion.
    for (const auto& re : kZero) {
        if (std::regex_search(body, re)) {
            return {0, "zero: witness-officer-only / 'not a subject official'"};
        }
    }

    // 4b. Witness officials and no subject official anywhere (the boilerplate is gone): none
    //     was designated -- "WO #1 Interviewed / WO #2 Interviewed" and nothing else
    static const std::regex kWo(R"(\bWO\s*#\s*\d|\bwitness offic(?:er|ial)s?\b)", std::regex::icase);
    static const std::regex kAnySo(R"(\bSOs?\b|subject offic)", std::regex::icase);
    if (std::regex_search(body, kWo) && !std::regex_search(body, kAnySo)) {
        return {0, "zero: witness officials only, no subject official named"};
    }

    // 5. Needs a human read.
    return {std::nullopt, "UNRESOLVED: 'the SO'x" + std::to_string(the_so) +
                              " 'the subj off'x" + std::to_string(the_subj)};
}

// The public entry points answer on any input (see siu_parse.cpp): a regex
// engine that gives up on a pathological line yields "no resolution" rather
// than an exception out of the resolver.
std::string strip_boilerplate(const std::string& report_text) {
    try {
        return strip_boilerplate_impl(report_text);
    } catch (const std::regex_error&) {
        return report_text;
    }
}

SoResolution resolve_subject_officials(const std::string& report_text) {
    try {
        return resolve_subject_officials_impl(report_text);
    } catch (const std::regex_error&) {
        SoResolution r;
        r.reason = "the text defeated the regex engine: no resolution";
        return r;
    }
}

}  // namespace siu
