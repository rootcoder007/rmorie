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
    std::smatch m;
    if (!std::regex_search(text, m, hpat)) return "";
    const size_t start = m.position(0) + m.length(0);
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
    // DEVIATION from the Python vocabulary list: pattern-based. Capture every
    // "<Proper Name> Police Service|Police|Provincial Police" phrase, count
    // occurrences, return the most frequent (ties -> longer name). Falls back
    // to the big-force abbreviations. Boilerplate-safe enough because the
    // most-frequent rule swamps one-off footer mentions.
    static const std::regex pat(
        R"(((?:[A-Z][A-Za-z'\-]+[ \t]+){1,5}(?:Police Service|Provincial Police|Police|Constabulary)))");
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
        static const std::regex pat(R"(\b[Oo]n\s+([A-Z][a-z]+\s+\d{1,2},?\s+\d{4}))");
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
        R"(\b[Oo]n\s+(?:[A-Z][a-z]+,?\s+)?([A-Z][a-z]+\s+\d{1,2},?\s+\d{4})[^\n]{0,200}?(?:notified|contacted)\s+the\s+SIU)");
    if (std::regex_search(hay, m, a))
        return std::regex_replace(m[1].str(), std::regex(","), "");
    // Form B: "notified/contacted the SIU on <Date>"
    static const std::regex b(
        R"((?:notified|contacted)\s+the\s+SIU[^\n]{0,200}?[Oo]n\s+([A-Z][a-z]+\s+\d{1,2},?\s+\d{4}))");
    if (std::regex_search(hay, m, b))
        return std::regex_replace(m[1].str(), std::regex(","), "");
    // Form C: first "On <Date>" inside "Notification of the SIU"
    const std::string notif = section_text(
        text, "Notification of the SIU", {"The Team", "Incident Narrative", "Evidence"});
    static const std::regex c(R"(On\s+([A-Z][a-z]+\s+\d{1,2},?\s+\d{4}))");
    if (!notif.empty() && std::regex_search(notif, m, c))
        return std::regex_replace(m[1].str(), std::regex(","), "");
    return "";
}

std::string detect_decision_date(const std::string& text) {
    std::smatch m;
    static const std::regex a(R"(Date:\s*([A-Z][a-z]+\s+\d{1,2},?\s+\d{4}))");
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

std::string to_iso_date(const std::string& human) {
    static const std::map<std::string, int> kMonths = {
        {"january", 1}, {"february", 2}, {"march", 3},    {"april", 4},
        {"may", 5},     {"june", 6},     {"july", 7},     {"august", 8},
        {"september", 9}, {"october", 10}, {"november", 11}, {"december", 12}};
    static const std::regex pat(R"(([A-Za-z]+)\s+(\d{1,2}),?\s+(\d{4}))");
    std::smatch m;
    if (!std::regex_search(human, m, pat)) {
        // already ISO?
        static const std::regex iso(R"(^\d{4}-\d{2}-\d{2}$)");
        return std::regex_match(human, iso) ? human : "";
    }
    const auto it = kMonths.find(lower(m[1].str()));
    if (it == kMonths.end()) return "";
    char buf[16];
    std::snprintf(buf, sizeof buf, "%s-%02d-%02d", m[3].str().c_str(),
                  it->second, std::stoi(m[2].str()));
    return buf;
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
    t = std::regex_replace(t, std::regex(R"(&#8217;|&rsquo;)"), "'");
    t = std::regex_replace(t, std::regex(R"(&#8216;|&lsquo;)"), "'");
    t = std::regex_replace(t, std::regex(R"(&#8220;|&ldquo;|&#8221;|&rdquo;)"), "\"");
    t = std::regex_replace(t, std::regex(R"(&quot;)"), "\"");
    t = std::regex_replace(t, std::regex(R"(&#0?39;|&apos;)"), "'");
    t = std::regex_replace(t, std::regex(R"(&#8211;|&ndash;)"), "-");
    t = std::regex_replace(t, std::regex(R"(&#8212;|&mdash;)"), "--");
    // Angle brackets last: the markup is already gone, so a decoded "<"
    // cannot be mistaken for a tag by anything downstream.
    t = std::regex_replace(t, std::regex(R"(&lt;)"), "<");
    t = std::regex_replace(t, std::regex(R"(&gt;)"), ">");
    // collapse spaces but keep newlines (section slicing needs them)
    t = std::regex_replace(t, std::regex(R"([ \t]+)"), " ");
    t = std::regex_replace(t, std::regex(R"( ?\n ?)"), "\n");
    t = std::regex_replace(t, std::regex(R"(\n{3,})"), "\n\n");
    return t;
}

ParsedFields parse_report_text(const std::string& text) {
    ParsedFields f;
    f["_language"] = detect_language(text);

    f["police_service"] = detect_police_service(text);
    f["date_of_incident_iso"] = to_iso_date(detect_incident_date(text));
    f["date_siu_notified_iso"] = to_iso_date(detect_siu_notified(text));
    f["date_of_director_decision_iso"] = to_iso_date(detect_decision_date(text));

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
    f["number_of_subject_officers"] = count_tagged(so.empty() ? text : so, "SO");

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

    const auto [age, sex] = detect_age_sex(text);
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
