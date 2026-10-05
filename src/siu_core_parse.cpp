// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Native SIU report parser. Port of morie's src/morie/siu/_parser.py
// extractors for the 16 schema fields. Each extractor mirrors the Python
// original's regex/logic; deviations are commented. All regexes operate on
// the stripped text produced by html_to_text().
#include "siu/parse.hpp"

#include <algorithm>
#include <array>
#include <regex>
#include <tuple>
#include <set>
#include <sstream>

namespace siu {
namespace {

std::string lower(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(), ::tolower);
    return s;
}

std::string trim(const std::string& s) {
    const auto a = s.find_first_not_of(" \t\r\n");
    if (a == std::string::npos) return "";
    const auto z = s.find_last_not_of(" \t\r\n,;:");
    return s.substr(a, z - a + 1);
}

std::string flatten_ws(const std::string& s) {
    return std::regex_replace(s, std::regex(R"(\s+)"), " ");
}

// _section_text: text from a line reading exactly `header` up to the first
// end-marker line (or end of text).
std::string section_text(const std::string& text, const std::string& header,
                         const std::vector<std::string>& ends = {}) {
    const std::regex hpat("(^|\n)[ \t]*" +
                          std::regex_replace(header, std::regex(R"([\^\$\.\|\?\*\+\(\)\[\]\{\}\\])"), R"(\$&)") +
                          "[ \t]*\n");
    // The LAST heading line: report pages open with a table of contents that repeats every
    // section title on its own line, and slicing from there gave each section the TOC text.
    size_t start = std::string::npos;
    for (auto it = std::sregex_iterator(text.begin(), text.end(), hpat); it != std::sregex_iterator(); ++it)
        start = static_cast<size_t>(it->position(0) + it->length(0));
    if (start == std::string::npos) return "";
    size_t end = text.size();
    for (const auto& em : ends) {
        const std::regex epat("(^|\n)[ \t]*" +
                              std::regex_replace(em, std::regex(R"([\^\$\.\|\?\*\+\(\)\[\]\{\}\\])"), R"(\$&)") +
                              "[ \t]*\n");
        std::smatch em_m;
        std::string tail = text.substr(start);
        if (std::regex_search(tail, em_m, epat)) {
            const size_t p = start + em_m.position(0);
            if (p < end) end = p;
        }
    }
    return text.substr(start, end - start);
}

// "Number of <label> assigned: N" (team block).
std::string team_count(const std::string& text, const std::string& label) {
    const std::regex pat("Number of " + label + R"([^0-9\n]{0,30}(\d+))",
                         std::regex::icase);
    std::smatch m;
    return std::regex_search(text, m, pat) ? m[1].str() : "";
}

// Count distinct numbered tags "PFX #n" in a section; bare mention -> 1.
// (Real SIU HTML breaks `SO\n#1` across lines, so flatten first.)
std::string count_tagged(const std::string& section, const std::string& prefix) {
    if (section.empty()) return "";
    const std::string flat = flatten_ws(section);
    const std::regex pat("\\b" + prefix + R"(\s*#?\s*(\d+)\b)");
    int mx = 0;
    for (auto it = std::sregex_iterator(flat.begin(), flat.end(), pat);
         it != std::sregex_iterator(); ++it) {
        mx = std::max(mx, std::stoi((*it)[1].str()));
    }
    if (mx > 0) return std::to_string(mx);
    if (std::regex_search(flat, std::regex("\\b" + prefix + "\\b")))
        return "1";
    return "";
}

// ---- individual field extractors ----------------------------------------

std::string detect_police_service(const std::string& text) {
    // The notification sentence names the force that called the SIU in: prefer it over counting.
    {
        static const std::regex notif(
            R"(((?:[A-Z][A-Za-z'\-]+[ \t]+){1,5}(?:Police Service|Provincial Police|Police|Constabulary))\s*(?:\(\s*[A-Z]{2,6}\s*\)\s*)?(?:notified|contacted)\s+the\s+SIU)");
        std::smatch nm;
        if (std::regex_search(text, nm, notif)) {
            std::string name = trim(nm[1].str());
            static const std::regex lead0(R"(^(?:The|A|An|At|On|In|By)\s+)");
            for (int i = 0; i < 3; ++i) name = std::regex_replace(name, lead0, "");
            if (!name.empty()) return name;
        }
    }
    // DEVIATION from the Python vocabulary list: pattern-based. Capture every
    // "<Proper Name> Police Service|Police|Provincial Police" phrase, count
    // occurrences, return the most frequent (ties -> longer name). Falls back
    // to the big-force abbreviations. Boilerplate-safe enough because the
    // most-frequent rule swamps one-off footer mentions.
    static const std::regex pat(
        R"(((?:[A-Z][A-Za-z'\-]+[ \t]+){1,5}(?:Police Service|Provincial Police|Police|Constabulary))\b(?![ \t]+Services?[ \t]+(?:Act|Board)))");
    std::map<std::string, int> counts;
    for (auto it = std::sregex_iterator(text.begin(), text.end(), pat);
         it != std::sregex_iterator(); ++it) {
        std::string name = trim((*it)[1].str());
        // strip leading connective words captured by the greedy prefix
        static const std::regex lead(
            R"(^(?:The|A|An|Of|And|On|In|By|To|With|From|That|This|Local)\s+)");
        for (int i = 0; i < 3; ++i) name = std::regex_replace(name, lead, "");
        // Incident headlines ("Brampton Collision Between Police") are not
        // service names; no real service name contains these words.
        static const std::regex headline(
            R"(\b(?:Between|Involving|After|During|Following|Collision|Crash|Shooting|Death|Injury|Incident|Arrest)\b)");
        if (std::regex_search(name, headline)) continue;
        if (!name.empty()) counts[name]++;
    }
    std::string best;
    int bestc = 0;
    for (const auto& [k, v] : counts) {
        if (v > bestc || (v == bestc && k.size() > best.size())) {
            best = k;
            bestc = v;
        }
    }
    if (!best.empty()) return best;
    static const std::array<std::pair<const char*, const char*>, 4> kAbbr = {{
        {"OPP", "Ontario Provincial Police"},
        {"TPS", "Toronto Police Service"},
        {"RCMP", "RCMP"},
        {"NRPS", "Niagara Regional Police Service"},
    }};
    for (const auto& [ab, full] : kAbbr) {
        if (std::regex_search(text, std::regex(std::string("\\b") + ab + "\\b")))
            return full;
    }
    return "";
}

std::string detect_incident_date(const std::string& text) {
    // First "On <Month D, Year>" in the narrative NOT near a notification verb.
    for (const char* sec_name : {"Incident Narrative", "The Investigation"}) {
        const std::string sec = section_text(
            text, sec_name,
            {"Nature of Injuries", "Evidence", "The Team",
             "Analysis and Director", "Relevant Legislation"});
        if (sec.empty()) continue;
        static const std::regex pat(R"(\b[Oo]n\s+([A-Z][a-z]+\s+\d{1,2}(?:st|nd|rd|th)?,?\s+\d{4}))");
        for (auto it = std::sregex_iterator(sec.begin(), sec.end(), pat);
             it != std::sregex_iterator(); ++it) {
            const size_t a = it->position(0) > 50 ? it->position(0) - 50 : 0;
            const std::string window =
                lower(sec.substr(a, it->position(0) - a + it->length(0) + 80));
            if (window.find("notified") != std::string::npos ||
                window.find("contacted the siu") != std::string::npos ||
                window.find("notification of the siu") != std::string::npos)
                continue;
            return std::regex_replace((*it)[1].str(), std::regex(","), "");
        }
    }
    return "";
}

std::string detect_siu_notified(const std::string& text) {
    const std::string inv = section_text(text, "The Investigation",
                                         {"The Team", "Incident Narrative"});
    const std::string& hay = inv.empty() ? text : inv;
    std::smatch m;
    // Form A: "On <Date> ... notified/contacted the SIU"
    static const std::regex a(
        R"(\b[Oo]n\s+(?:[A-Z][a-z]+,?\s+)?([A-Z][a-z]+\s+\d{1,2}(?:st|nd|rd|th)?,?\s+\d{4})[^\n]{0,200}?(?:notified|contacted)\s+the\s+SIU)");
    if (std::regex_search(hay, m, a))
        return std::regex_replace(m[1].str(), std::regex(","), "");
    // Form B: "notified/contacted the SIU on <Date>"
    static const std::regex b(
        R"((?:notified|contacted)\s+the\s+SIU[^\n]{0,200}?[Oo]n\s+([A-Z][a-z]+\s+\d{1,2}(?:st|nd|rd|th)?,?\s+\d{4}))");
    if (std::regex_search(hay, m, b))
        return std::regex_replace(m[1].str(), std::regex(","), "");
    // Form C: first "On <Date>" inside "Notification of the SIU"
    const std::string notif = section_text(
        text, "Notification of the SIU", {"The Team", "Incident Narrative", "Evidence"});
    static const std::regex c(R"(On\s+([A-Z][a-z]+\s+\d{1,2}(?:st|nd|rd|th)?,?\s+\d{4}))");
    if (!notif.empty() && std::regex_search(notif, m, c))
        return std::regex_replace(m[1].str(), std::regex(","), "");
    return "";
}

std::string detect_decision_date(const std::string& text) {
    std::smatch m;
    static const std::regex a(R"(Date:\s*([A-Z][a-z]+\s+\d{1,2}(?:st|nd|rd|th)?,?\s+\d{4}))");
    if (std::regex_search(text, m, a))
        return std::regex_replace(m[1].str(), std::regex(","), "");
    static const std::regex b(R"(Date:\s*(\d{4}-\d{2}-\d{2}))");
    if (std::regex_search(text, m, b)) return m[1].str();
    return "";
}

std::string detect_location(const std::string& text) {
    const std::string inv = section_text(text, "The Investigation",
                                         {"The Team", "Incident Narrative"});
    // Flatten first: report HTML wraps lines mid-phrase ("in the City of\n
    // Barrie"), which the space-separated pattern would miss.
    const std::string hay = flatten_ws(inv.empty() ? text : inv);
    static const std::regex pat(
        R"(in the (Township|City|Town|Municipality|Region) of ([A-Z][A-Za-z \-]+?)(?:[\.,]|\s+(?:on|at|when)))");
    std::smatch m;
    if (std::regex_search(hay, m, pat))
        return m[1].str() + " of " + trim(m[2].str());
    return "";
}

std::pair<std::string, std::string> detect_age_sex(const std::string& text) {
    static const std::regex pat(
        R"(\b(\d{1,3})[\s\-]year[\s\-]old\s+(woman|man|female|male|girl|boy|person|individual|youth|child|adult)\b)",
        std::regex::icase);
    std::smatch m;
    if (!std::regex_search(text, m, pat)) return {"", ""};
    return {m[1].str(), lower(m[2].str())};
}

std::string detect_specific_injuries(const std::string& text) {
    static const std::regex pat(
        R"(((?:fractured?|broken|lacerat\w+|gunshot|stab\w+|burns?)[^\n]{1,200}?(?:rib|leg|arm|skull|wrist|ankle|jaw|nose|tooth|finger|spine|vertebra)[^\n]{0,80}))",
        std::regex::icase);
    std::smatch m;
    return std::regex_search(text, m, pat) ? trim(m[1].str()) : "";
}

std::string detect_legislation(const std::string& text) {
    const std::string sec = section_text(text, "Relevant Legislation",
                                         {"Analysis and Director", "News Releases"});
    if (sec.empty()) return "";
    static const std::regex pat(
        R"(Section\s+\d+(?:\.\d+)*(?:\([^)]+\))?,?\s+([A-Z][^\n,]{2,80}?)(?:\s*[-]|\s*$|\n))");
    std::vector<std::string> acts;
    for (auto it = std::sregex_iterator(sec.begin(), sec.end(), pat);
         it != std::sregex_iterator(); ++it) {
        const std::string act = trim((*it)[1].str());
        if (std::find(acts.begin(), acts.end(), act) == acts.end())
            acts.push_back(act);
    }
    std::string out;
    for (const auto& a : acts) out += (out.empty() ? "" : "; ") + a;
    return out;
}

std::string detect_charges(const std::string& text) {
    std::string sec = section_text(text, "Analysis and Director's Decision",
                                   {"Endnotes", "News Release", "Note:"});
    if (sec.empty())
        sec = section_text(text, "Analysis and Director\xE2\x80\x99s Decision",
                           {"Endnotes", "News Release", "Note:"});
    if (sec.empty()) return "";
    const std::string low = lower(sec);
    for (const char* p : {"no charges", "shall issue", "none shall issue",
                          "no basis for charges", "no reasonable grounds",
                          "do not lay", "decline to lay",
                          "lack the necessary grounds"}) {
        if (low.find(p) != std::string::npos) return "FALSE";
    }
    for (const char* p : {"charged with", "criminal charges have been laid",
                          "charges have been laid"}) {
        if (low.find(p) != std::string::npos) return "TRUE";
    }
    return "";
}

std::string detect_directors_name(const std::string& text) {
    // Signature block: "<Name>\n[\n]Director\n[\n]Special Investigations Unit"
    // (real reports put blank lines between the lines) or "<Name>, Director".
    // "Director" must stand alone on its line (or follow the comma): site
    // navigation such as "Public Reports\nDirector's Resource Committee" must
    // never be read as a signature.
    static const std::regex own_line(
        R"(([A-Z][A-Za-z'\-]+(?:[ \t]+[A-Z][A-Za-z.'\-]+){1,3})[ \t]*\n(?:[ \t]*\n)*[ \t]*(?:(?:Interim|Acting)[ \t]+)?(?:Director|Directeur|Directrice)(?:[ \t]+par[ \t]+int\xC3\xA9rim)?[ \t]*(?:\n|$))");
    static const std::regex comma(
        R"(([A-Z][A-Za-z'\-]+(?:[ \t]+[A-Z][A-Za-z.'\-]+){1,3})[ \t]*,[ \t]*(?:(?:Interim|Acting)[ \t]+)?(?:Director|Directeur|Directrice)[ \t]*(?:\n|$))");
    for (const auto* re : {&own_line, &comma}) {
        for (auto it = std::sregex_iterator(text.begin(), text.end(), *re); it != std::sregex_iterator(); ++it) {
            const std::string name = trim((*it)[1].str());
            if (lower(name).find("the") == std::string::npos) return name;
        }
    }
    return "";
}

std::string detect_language(const std::string& text) {
    static const std::array<const char*, 8> en = {
        "The Investigation", "Notification of the SIU", "Mandate engaged",
        "Civilian Witnesses", "Witness Officers", "Subject Officers",
        "Analysis and Director's Decision", "Witness Officials"};
    static const std::array<const char*, 7> fr = {
        "L'enqu\xC3\xAAte", "Exercice du mandat",
        "\xC3\x89l\xC3\xA9ments de preuve",
        "Dispositions l\xC3\xA9gislatives pertinentes",
        "T\xC3\xA9moins civils", "Agents impliqu\xC3\xA9s", "Mandat de l'UES"};
    int e = 0, f = 0;
    for (const char* mk : en) if (text.find(mk) != std::string::npos) ++e;
    for (const char* mk : fr) if (text.find(mk) != std::string::npos) ++f;
    if (e >= 2 && e > f) return "en";
    if (f >= 2 && f > e) return "fr";
    return "unknown";
}

}  // namespace

// shared with rmoriebricklayer's parser: English and French months, ordinals (3rd, 1er)
std::string to_iso_date(const std::string& human) {
    // English and French month names (the SIU publishes both); accents are kept as UTF-8
    // a data table (no code per line), loaded into the map once
    static const char* const kNames[] = {
        "january", "february", "march", "april", "may", "june", "july", "august",
        "september", "october", "november", "december",
        "janvier", "f\xc3\xa9vrier", "fevrier", "mars", "avril", "mai", "juin", "juillet",
        "ao\xc3\xbbt", "aout", "septembre", "octobre", "novembre", "d\xc3\xa9" "cembre", "decembre",
        "jan", "feb", "mar", "apr", "jun", "jul", "aug", "sep", "sept", "oct", "nov", "dec"};
    static const int kNums[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                                1, 2, 2, 3, 4, 5, 6, 7, 8, 8, 9, 10, 11, 12, 12,
                                1, 2, 3, 4, 6, 7, 8, 9, 9, 10, 11, 12};
    static const std::map<std::string, int> kMonths = [] {
        std::map<std::string, int> m;
        for (size_t i = 0; i < sizeof(kNums) / sizeof(kNums[0]); ++i) m[kNames[i]] = kNums[i];
        return m;
    }();
    // "January 5, 2023" / "January 5 2023" (month first) or "5 janvier 2023" / "3 ao\xc3\xbbt 2017" (day first)
    static const std::regex pat(R"(([^\s\d,.]+)\.?\s+(\d{1,2})(?:st|nd|rd|th|er|e)?,?\s+(\d{4}))");
    static const std::regex pat_fr(R"((\d{1,2})(?:er|e|st|nd|rd|th)?\s+(?:of\s+)?([^\s\d,.]+)\.?,?\s+(\d{4}))");
    std::smatch m;
    std::string month, day, year;
    if (std::regex_search(human, m, pat) && kMonths.count(lower(m[1].str()))) {
        month = m[1].str(); day = m[2].str(); year = m[3].str();
    } else if (std::regex_search(human, m, pat_fr) && kMonths.count(lower(m[2].str()))) {
        day = m[1].str(); month = m[2].str(); year = m[3].str();
    } else {
        // already ISO?
        static const std::regex iso(R"(^\d{4}-\d{2}-\d{2}$)");
        return std::regex_match(human, iso) ? human : "";
    }
    const auto it = kMonths.find(lower(month));
    if (it == kMonths.end()) return "";
    // "February 30, 2019" is not a date
    const int y = std::stoi(year), mo = it->second, d = std::stoi(day);
    static const int kDays[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    const bool leap = (y % 4 == 0 && y % 100 != 0) || y % 400 == 0;
    if (d < 1 || d > kDays[mo - 1] + (mo == 2 && leap ? 1 : 0)) return "";
    char buf[16];
    std::snprintf(buf, sizeof buf, "%s-%02d-%02d", year.c_str(), it->second, std::stoi(day));
    return buf;
}

// "&#233;" / "&#xE9;" -> the character as UTF-8 (any code point the named list misses)
static std::string decode_numeric_entities(const std::string& s) {
    static const std::regex ent(R"(&#(x[0-9A-Fa-f]+|[0-9]+);)");
    std::string out;
    std::size_t last = 0;
    for (auto it = std::sregex_iterator(s.begin(), s.end(), ent); it != std::sregex_iterator(); ++it) {
        const std::string g = (*it)[1].str();
        const unsigned long cp = g[0] == 'x' ? std::stoul(g.substr(1), nullptr, 16) : std::stoul(g);
        out.append(s, last, static_cast<std::size_t>(it->position(0)) - last);
        last = static_cast<std::size_t>(it->position(0) + it->length(0));
        if (cp == 0 || cp > 0x10FFFF) continue;
        if (cp < 0x80) {
            out += static_cast<char>(cp);
        } else if (cp < 0x800) {
            out += static_cast<char>(0xC0 | (cp >> 6));
            out += static_cast<char>(0x80 | (cp & 0x3F));
        } else if (cp < 0x10000) {
            out += static_cast<char>(0xE0 | (cp >> 12));
            out += static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
            out += static_cast<char>(0x80 | (cp & 0x3F));
        } else {
            out += static_cast<char>(0xF0 | (cp >> 18));
            out += static_cast<char>(0x80 | ((cp >> 12) & 0x3F));
            out += static_cast<char>(0x80 | ((cp >> 6) & 0x3F));
            out += static_cast<char>(0x80 | (cp & 0x3F));
        }
    }
    out.append(s, last, std::string::npos);
    return out;
}

std::string html_to_text(const std::string& html) {
    // Live SIU pages use CRLF line endings; a stray \r defeats every
    // line-anchored rule (sections, signature, police service), so normalise
    // CRLF / CR to LF before anything else.
    std::string t = std::regex_replace(html, std::regex(R"(\r\n?)"), "\n");
    t = std::regex_replace(
        t, std::regex(R"(<(script|style)[^>]*>[\s\S]*?</\1>)", std::regex::icase),
        " ");
    // Block-level closers become newlines so section headers keep their lines.
    t = std::regex_replace(
        t, std::regex(R"(</(p|div|h[1-6]|tr|li|br)>|<br\s*/?>)", std::regex::icase),
        "\n");
    t = std::regex_replace(t, std::regex(R"(<[^>]+>)"), " ");
    t = std::regex_replace(t, std::regex(R"(&nbsp;)"), " ");
    t = std::regex_replace(t, std::regex(R"(&amp;)"), "&");
    t = std::regex_replace(t, std::regex(R"(&#8217;|&rsquo;|&#x2019;)", std::regex::icase), "'");
    t = std::regex_replace(t, std::regex(R"(&#8216;|&lsquo;|&#x2018;)", std::regex::icase), "'");
    t = std::regex_replace(t, std::regex(R"(&#8220;|&ldquo;|&#8221;|&rdquo;|&#x201c;|&#x201d;)", std::regex::icase), "\"");
    t = std::regex_replace(t, std::regex(R"(&quot;)"), "\"");
    t = std::regex_replace(t, std::regex(R"(&#0?39;|&apos;)"), "'");
    t = std::regex_replace(t, std::regex(R"(&#8211;|&ndash;|&#x2013;)", std::regex::icase), "-");
    t = std::regex_replace(t, std::regex(R"(&#8212;|&mdash;|&#x2014;)", std::regex::icase), "--");
    // Angle brackets last: the markup is already gone, so a decoded "<"
    // cannot be mistaken for a tag by anything downstream.
    // accented named entities of the French pages, as UTF-8
    static const std::array<std::pair<const char*, const char*>, 18> kNamed = {{
        {"&eacute;", "\xc3\xa9"}, {"&egrave;", "\xc3\xa8"}, {"&ecirc;", "\xc3\xaa"}, {"&euml;", "\xc3\xab"},
        {"&agrave;", "\xc3\xa0"}, {"&acirc;", "\xc3\xa2"}, {"&ccedil;", "\xc3\xa7"}, {"&icirc;", "\xc3\xae"},
        {"&iuml;", "\xc3\xaf"}, {"&ocirc;", "\xc3\xb4"}, {"&ouml;", "\xc3\xb6"}, {"&ucirc;", "\xc3\xbb"},
        {"&ugrave;", "\xc3\xb9"}, {"&uuml;", "\xc3\xbc"}, {"&auml;", "\xc3\xa4"}, {"&copy;", "\xc2\xa9"},
        {"&reg;", "\xc2\xae"}, {"&Eacute;", "\xc3\x89"}}};
    for (const auto& [ent, ch] : kNamed) t = std::regex_replace(t, std::regex(ent), ch);
    t = std::regex_replace(t, std::regex(R"(&hellip;|&#8230;)"), "...");
    t = decode_numeric_entities(t);
    t = std::regex_replace(t, std::regex(R"(&lt;)"), "<");
    t = std::regex_replace(t, std::regex(R"(&gt;)"), ">");
    // collapse spaces but keep newlines (section slicing needs them)
    t = std::regex_replace(t, std::regex(R"([ \t]+)"), " ");
    t = std::regex_replace(t, std::regex(R"( ?\n ?)"), "\n");
    t = std::regex_replace(t, std::regex(R"(\n{3,})"), "\n\n");
    return t;
}

// ---- French reports: the SIU publishes every report in both languages, and the French copy
// ---- has its own headings and phrasing ("Le 12 novembre 2022", "a communique ... a l UES")
static const std::string kFrMonths =
    "(?:janvier|f\xc3\xa9vrier|fevrier|mars|avril|mai|juin|juillet|ao\xc3\xbbt|aout|septembre|octobre|novembre|"
    "d\xc3\xa9" "cembre|decembre)";
static const std::string kFrDate = "(\\d{1,2}(?:er)?\\s+" + kFrMonths + "\\s+\\d{4})";
static const std::string kApos = "(?:'|\xe2\x80\x99)";

// the sentence that starts at `pos`: to its first ". " or the end of the line (at most 300 bytes)
static std::string sentence_at(const std::string& s, size_t pos) {
    size_t end = s.find('\n', pos);
    const size_t stop = s.find(". ", pos);
    if (stop != std::string::npos && (end == std::string::npos || stop < end)) end = stop;
    if (end == std::string::npos || end > pos + 300) end = std::min(s.size(), pos + 300);
    return s.substr(pos, end - pos);
}

std::string detect_police_service_fr(const std::string& text) {
    static const std::regex notif("(Service de police[^\\n,.;()]*?|Police provinciale de l" + kApos +
                                  "Ontario)\\s*(?:\\(\\s*[A-Z]{2,6}\\s*\\)\\s*)?(?:a|ont) (?:communiqu|avis|inform)");
    std::smatch m;
    if (std::regex_search(text, m, notif)) return trim(m[1].str());
    static const std::regex pat("(Service de police(?: [a-z\xc3\xa9]+){0,2} (?:de la |de |du |des |d" + kApos +
                                ")[A-Z\xc3][^\\s,.;()]*(?: [A-Z\xc3][^\\s,.;()]*)*|Police provinciale de l" + kApos +
                                "Ontario)");
    std::map<std::string, int> counts;
    for (auto it = std::sregex_iterator(text.begin(), text.end(), pat); it != std::sregex_iterator(); ++it)
        counts[trim((*it)[1].str())]++;
    std::string best;
    int bestc = 0;
    for (const auto& [k, v] : counts) {
        if (v > bestc || (v == bestc && k.size() > best.size())) {
            best = k;
            bestc = v;
        }
    }
    return best;
}

// first "Le <date>" of the investigation that is not the call to the SIU or the team's dispatch
std::string detect_incident_date_fr(const std::string& text) {
    std::string sec = section_text(text, "L\xe2\x80\x99" "enqu\xc3\xaa" "te");
    if (sec.empty()) sec = section_text(text, "L'enqu\xc3\xaa" "te");
    if (sec.empty()) sec = text;
    static const std::regex pat("\\b[Ll]e\\s+" + kFrDate);
    static const std::regex ues("l" + kApos + "\\s*UES\\b");
    for (auto it = std::sregex_iterator(sec.begin(), sec.end(), pat); it != std::sregex_iterator(); ++it) {
        const std::string sent = sentence_at(sec, static_cast<size_t>(it->position(0)));
        const size_t a = it->position(0) > 60 ? static_cast<size_t>(it->position(0)) - 60 : 0;
        const std::string before = lower(sec.substr(a, static_cast<size_t>(it->position(0)) - a));
        if (std::regex_search(sent, ues) || before.find("envoi de l") != std::string::npos ||
            before.find("arriv\xc3\xa9" "e de l") != std::string::npos)
            continue;
        return (*it)[1].str();
    }
    return "";
}

// first date after the "Notification de l'UES" heading
std::string detect_siu_notified_fr(const std::string& text) {
    static const std::regex head("(^|\\n)Notification de l" + kApos + "\\s*UES[^\\n]*\\n");
    size_t start = std::string::npos;
    for (auto it = std::sregex_iterator(text.begin(), text.end(), head); it != std::sregex_iterator(); ++it)
        start = static_cast<size_t>(it->position(0) + it->length(0));
    if (start == std::string::npos) return "";
    const std::string after = text.substr(start, 1500);
    static const std::regex pat(kFrDate);
    std::smatch m;
    return std::regex_search(after, m, pat) ? m[1].str() : "";
}

std::string detect_decision_date_fr(const std::string& text) {
    static const std::regex pat("Date\\s*:\\s*(?:[Ll]e\\s+)?" + kFrDate);
    std::smatch m;
    return std::regex_search(text, m, pat) ? m[1].str() : "";
}

std::pair<std::string, std::string> detect_age_sex_fr(const std::string& text) {
    static const std::regex pat(
        "\\b(femme|homme|fille|gar\xc3\xa7on|adolescente|adolescent|personne)\\s+de\\s+(\\d{1,3})\\s+ans\\b",
        std::regex::icase);
    std::smatch m;
    if (!std::regex_search(text, m, pat)) return {"", ""};
    static const std::map<std::string, std::string> kEn = {
        {"femme", "woman"}, {"homme", "man"}, {"fille", "girl"}, {"gar\xc3\xa7on", "boy"},
        {"adolescente", "youth"}, {"adolescent", "youth"}, {"personne", "person"}};
    const auto it = kEn.find(lower(m[1].str()));
    return {m[2].str(), it == kEn.end() ? lower(m[1].str()) : it->second};
}

static std::string or_else(const std::string& a, const std::string& b) { return a.empty() ? b : a; }

ParsedFields parse_report_text(const std::string& text) {
    ParsedFields f;
    f["_language"] = detect_language(text);

    const bool fr = f["_language"] == "fr";
    f["police_service"] = fr ? or_else(detect_police_service_fr(text), detect_police_service(text))
                             : detect_police_service(text);
    f["date_of_incident_iso"] =
        to_iso_date(fr ? or_else(detect_incident_date_fr(text), detect_incident_date(text)) : detect_incident_date(text));
    f["date_siu_notified_iso"] =
        to_iso_date(fr ? or_else(detect_siu_notified_fr(text), detect_siu_notified(text)) : detect_siu_notified(text));
    f["date_of_director_decision_iso"] = to_iso_date(
        fr ? or_else(detect_decision_date_fr(text), detect_decision_date(text)) : detect_decision_date(text));

    f["siu_investigators"] = team_count(text, "SIU Investigators");
    f["siu_forensics_investigators"] =
        team_count(text, "SIU Forensic Investigators");

    // Officer/witness counts from their sections (both -er and -ial headers;
    // modern reports say "Officials", older say "Officers").
    std::string so = section_text(text, "Subject Officers",
                                  {"Incident Narrative", "Evidence", "Witness Officers"});
    if (so.empty())
        so = section_text(text, "Subject Officials",
                          {"Incident Narrative", "Evidence", "Witness Officials"});
    f["number_of_subject_officials"] = count_tagged(so.empty() ? text : so, "SO");

    std::string wo = section_text(text, "Witness Officers",
                                  {"Incident Narrative", "Evidence", "Subject Officers"});
    if (wo.empty())
        wo = section_text(text, "Witness Officials",
                          {"Incident Narrative", "Evidence", "Subject Officials"});
    if (!wo.empty() &&
        std::regex_search(lower(flatten_ws(wo)),
                          std::regex(R"(no police officers? witness)"))) {
        f["number_of_witness_officials"] = "0";
    } else {
        f["number_of_witness_officials"] = count_tagged(wo, "WO");
    }

    const std::string cw = section_text(text, "Civilian Witnesses",
                                        {"Incident Narrative", "Evidence",
                                         "Witness Officers", "Witness Officials"});
    f["number_of_civilian_witnesses"] = count_tagged(cw, "CW");

    auto [age, sex] = detect_age_sex(text);
    if (fr && age.empty()) std::tie(age, sex) = detect_age_sex_fr(text);
    f["age_affected"] = age;
    f["sex_gender_affected"] = sex;

    f["charges_recommended"] = detect_charges(text);
    f["directors_name"] = detect_directors_name(text);
    f["location_of_call"] = detect_location(text);
    f["specific_injuries"] = detect_specific_injuries(text);
    f["relevant_legislation"] = detect_legislation(text);
    return f;
}

ParsedFields parse_report_html(const std::string& html) {
    return parse_report_text(html_to_text(html));
}

}  // namespace siu
