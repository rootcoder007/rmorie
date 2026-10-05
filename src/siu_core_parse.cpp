// SPDX-License-Identifier: AGPL-3.0-or-later
// Canonical home of the SIU parse/resolve core. rmorie (src/siu/) and morie
// (morie.siu.native) carry ports of these sources; edit here first, then port.
//
// Native SIU report parser. Port of morie's src/morie/siu/_parser.py
// extractors for the 16 schema fields. Each extractor mirrors the Python
// original's regex/logic; deviations are commented. All regexes operate on
// the stripped text produced by html_to_text().
#include "siu/parse.hpp"
#include "siu/resolve.hpp"

#include <algorithm>
#include <array>
#include <map>
#include <regex>
#include <tuple>
#include <vector>
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
    // "SO #1" (English), "AI no 1" / "AI n\xc2\xb0 1" (French)
    // "SO #1" (English), "AI no 1", "AI n o 1" (a superscript o split off) or "AI n\xc2\xb0 1" (French)
    const std::regex pat("\\b" + prefix + R"(\s*(?:#|n\s?o\.?|n\xc2[\xb0\xba])?\s*(\d+)\b)");
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

// The section under a role heading, in any layout: "Witness Officers" (to 2019),
// "Witness Officials ( WO )" and the singular "Civilian Witness ( CW )" (2020 on),
// "Agents t\xc3\xa9moins ( AT )" (French). It runs to the next role heading or the
// evidence / narrative section. The LAST heading line counts (the page opens with a
// table of contents that repeats the titles).
static const char* kRoleHeads[] = {
    "Subject Offic(?:ials?|ers?)", "Witness Offic(?:ials?|ers?)", "Civilian Witness(?:es)?",
    "Agents? impliqu\xc3\xa9(?:e|s|es)?", "Agents? t\xc3\xa9moins?", "T\xc3\xa9moins? civils?",
    "Incident Narrative", "Evidence", "\xc3\x89l\xc3\xa9ments de preuve", "R\xc3\xa9" "cit de l.incident"};

std::string role_section(const std::string& text, const std::string& head) {
    auto line = [](const std::string& h) {
        return std::regex("(^|\n)[ \t]*(?:" + h + ")(?:[ \t]*\\([ \t]*[A-Z]{2}[ \t]*\\))?[ \t]*\n");
    };
    const std::regex hpat = line(head);
    size_t start = std::string::npos;
    for (auto it = std::sregex_iterator(text.begin(), text.end(), hpat); it != std::sregex_iterator(); ++it)
        start = static_cast<size_t>(it->position(0) + it->length(0));
    if (start == std::string::npos) return "";
    size_t end = text.size();
    const std::string tail = text.substr(start);
    for (const char* other : kRoleHeads) {
        if (head == other) continue;
        std::smatch m;
        if (std::regex_search(tail, m, line(other))) end = std::min(end, start + static_cast<size_t>(m.position(0)));
    }
    return text.substr(start, end - start);
}

// Legacy reports have no role sections: the highest "Witness Officer #3" in the text.
// Numbered labels only -- a bare mention there is the definition boilerplate.
std::string count_labelled(const std::string& text, const std::string& label) {
    const std::string flat = flatten_ws(text);
    // "Witness Officer #3"; French "l'agent t\xc3\xa9moin n o 1" (a superscript o split off), "no 1", "n\xc2\xb0 1"
    const std::regex pat("\\b" + label + "s?\\s*(?:#|n\\s?o\\.?|n\xc2[\xb0\xba])\\s*(\\d+)\\b", std::regex::icase);
    int mx = 0;
    for (auto it = std::sregex_iterator(flat.begin(), flat.end(), pat); it != std::sregex_iterator(); ++it)
        mx = std::max(mx, std::stoi((*it)[1].str()));
    return mx > 0 ? std::to_string(mx) : "";
}

// "Three SIU investigators and two forensic investigators ( FIs ) were dispatched", French
// "Cinq enqu\xc3\xaateurs et trois techniciens en identification m\xc3\xa9" "dicol\xc3\xa9gale" (legacy)
std::string team_words(const std::string& text, const std::string& what, bool fr) {
    static const char* kEn[] = {"zero", "one", "two", "three", "four", "five", "six", "seven", "eight",
                                "nine", "ten", "eleven", "twelve"};
    static const char* kFr[] = {"z\xc3\xa9ro", "un", "deux", "trois", "quatre", "cinq", "six", "sept", "huit",
                                "neuf", "dix", "onze", "douze"};
    const char* const* names = fr ? kFr : kEn;
    std::string alt = "\\d+|une";
    for (int i = 0; i <= 12; ++i) alt += std::string("|") + names[i];
    const std::regex pat("(?:^|[^A-Za-z\xc3])(" + alt + ")\\s+" + what + "\\b", std::regex::icase);
    std::smatch m;
    if (!std::regex_search(text, m, pat)) return "";
    const std::string w = lower(m[1].str());
    if (w == "une") return "1";
    for (int i = 0; i <= 12; ++i)
        if (w == names[i]) return std::to_string(i);
    return w;
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
    // a legacy report's header states it: "Incident date: May 9, 2010"
    {
        static const std::regex head(R"(Incident date\s*:\s*(?:[A-Z][a-z]+,?\s+)?([A-Z][a-z]+\.?\s+\d{1,2}(?:st|nd|rd|th)?,?\s+\d{4}))");
        std::smatch hm;
        if (std::regex_search(text, hm, head)) return std::regex_replace(hm[1].str(), std::regex(","), "");
    }
    // First "On <Month D, Year>" (or "... of <Month D, Year>": "Just before 4:00 a.m. of January
    // 7, 2020") in the narrative whose SENTENCE is not the notification or the SIU's own work
    // ("On July 9 ... contacted the SIU to report a serious injury. On July 8, 2019 at about
    // 3:50 p.m., CKLPS were called" dates the incident in the second sentence). Reports whose
    // narrative states no date give it in the Director's analysis ("On December 4, 2020, the
    // Complainant rolled his SUV").
    for (const char* sec_name : {"Incident Narrative", "The Investigation",
                                 "Analysis and Director's Decision",
                                 "Analysis and Director\xe2\x80\x99s Decision"}) {
        const std::string sec = section_text(
            text, sec_name,
            {"Nature of Injuries", "Evidence", "The Team",
             "Analysis and Director", "Relevant Legislation", "Conclusion"});
        if (sec.empty()) continue;
        static const std::regex pat(R"(\b(?:[Oo]n|of)\s+([A-Z][a-z]+\s+\d{1,2}(?:st|nd|rd|th)?,?\s+\d{4}))");
        static const std::regex own(
            R"(notified|contacted the siu|notification of the siu|siu investigators|forensic investigators?|)"
            R"(were assigned|was assigned|designated as|provided (?:a|his|her|their) statement|interviewed|)"
            R"(the siu arrived|director)");
        for (auto it = std::sregex_iterator(sec.begin(), sec.end(), pat);
             it != std::sregex_iterator(); ++it) {
            const size_t pos = static_cast<size_t>(it->position(0));
            size_t a = sec.rfind(". ", pos);
            const size_t nl = sec.rfind('\n', pos);
            a = (a == std::string::npos) ? 0 : a + 2;
            if (nl != std::string::npos && nl + 1 > a) a = nl + 1;
            size_t b = sec.find(". ", pos + static_cast<size_t>(it->length(0)));
            const size_t nl2 = sec.find('\n', pos);
            if (b == std::string::npos || (nl2 != std::string::npos && nl2 < b)) b = nl2;
            if (b == std::string::npos) b = sec.size();
            if (std::regex_search(lower(sec.substr(a, b - a)), own)) continue;
            const std::string month = (*it)[1].str().substr(0, (*it)[1].str().find(' '));
            static const std::regex months("January|February|March|April|May|June|July|August|September|October|November|December");
            if (!std::regex_match(month, months)) continue;
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
    // Form B2: "The SIU was notified of the incident by ... on <Date>"
    static const std::regex b2(
        R"(SIU\s+was\s+(?:notified|contacted)[^\n]{0,200}?\b[Oo]n\s+(?:[A-Z][a-z]+,?\s+)?([A-Z][a-z]+\s+\d{1,2}(?:st|nd|rd|th)?,?\s+\d{4}))");
    if (std::regex_search(hay, m, b2))
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

// The injury as the case narrative states it. The search starts at the investigation
// (the mandate before it quotes the legal definition, "a fracture to the skull, or to a
// limb, rib or vertebra") and passes over any sentence quoting that definition; words
// match whole ("stab" is not "constables", "arm" is not "firearm").
std::string detect_specific_injuries(const std::string& text, bool fr) {
    static const std::regex en(
        R"(\b((?:fracture[sd]?|broken|lacerat\w*|gunshot|stab(?:bed|bing| wounds?)?|burns?)\b[^\n.]{0,200}?\b(?:ribs?|legs?|arms?|skull|wrists?|ankles?|jaw|nose|teeth|tooth|fingers?|spine|vertebrae?|shoulders?|hips?|pelvis|orbit(?:al)?|face|hands?|feet|foot|elbows?|knees?|clavicle|collarbone|femur|humerus|tibia|fibula|sternum|neck|back|head|chest|abdomen)\b[^\n.]{0,80}))",
        std::regex::icase),
        fr_pat("\\b((?:fractures?|fractur\xc3\xa9" "e?s?|lac\xc3\xa9rations?|blessures? par balle|coups? de couteau|br\xc3\xbblures?)"
               "[^\\n.]{0,200}?(?:\\b|(?=\xc3))(?:c\xc3\xb4tes?|jambes?|bras|cr\xc3\xa2ne|poignets?|chevilles?|m\xc3\xa2" "choire|nez|dents?|doigts?|"
               "colonne|vert\xc3\xa8" "bres?|\xc3\xa9paules?|hanches?|bassin|orbite|visage|mains?|pieds?|coudes?|genoux|clavicule|"
               "f\xc3\xa9mur|tibia|p\xc3\xa9ron\xc3\xa9|sternum|cou|dos|t\xc3\xaate|thorax|abdomen)[^\\n.]{0,80})",
               std::regex::icase),
        head_en("(^|\\n)[ \\t]*The Investigation[ \\t]*\\n"), head_fr("(^|\\n)[ \\t]*L(?:'|\xe2\x80\x99)enqu\xc3\xaate[ \\t]*\\n");
    size_t start = 0;
    for (auto it = std::sregex_iterator(text.begin(), text.end(), fr ? head_fr : head_en); it != std::sregex_iterator(); ++it)
        start = static_cast<size_t>(it->position(0) + it->length(0));
    const std::string hay = text.substr(start);
    for (auto it = std::sregex_iterator(hay.begin(), hay.end(), fr ? fr_pat : en); it != std::sregex_iterator(); ++it) {
        const size_t pos = static_cast<size_t>(it->position(0));
        const size_t a = hay.rfind('\n', pos), b = hay.find('\n', pos);
        const std::string line = lower(hay.substr(a == std::string::npos ? 0 : a + 1,
                                                  (b == std::string::npos ? hay.size() : b) - (a == std::string::npos ? 0 : a + 1)));
        if (line.find("serious injury") != std::string::npos || line.find("blessure grave") != std::string::npos) continue;
        return trim((*it)[1].str());
    }
    return "";
}

std::string detect_legislation(const std::string& text, bool fr) {
    // French: "Dispositions l\xc3\xa9gislatives pertinentes", lines such as
    // "Paragraphe 25(1) du Code criminel -- Protection des personnes autoris\xc3\xa9" "es"
    const std::string sec = fr ? section_text(text, "Dispositions l\xc3\xa9gislatives pertinentes",
                                              {"Analyse et d\xc3\xa9" "cision du directeur", "Communiqu\xc3\xa9s de presse"})
                               : section_text(text, "Relevant Legislation",
                                              {"Analysis and Director", "News Releases"});
    if (sec.empty()) return "";
    static const std::regex en_pat(
        R"(Section\s+\d+(?:\.\d+)*(?:\([^)]+\))?,?\s+([A-Z][^\n,]{2,80}?)(?:\s*[-]|\s*$|\n))"),
        fr_pat("(?:Articles?|Paragraphes?|Alin\xc3\xa9" "as?)\\s+\\d+[^\\s]*\\s+(?:du |de la |de l'|de l\xe2\x80\x99|des )"
               "([A-Z\xc3][^\\n,]{2,80}?)(?:\\s*--|\\s+-|\\s*$|\\n)");
    const std::regex& pat = fr ? fr_pat : en_pat;
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

std::string detect_charges(const std::string& text, bool fr) {
    if (fr) {
        std::string sec = section_text(text, "Analyse et d\xc3\xa9" "cision du directeur",
                                       {"Notes de fin", "Communiqu\xc3\xa9", "Remarque :"});
        if (sec.empty()) return "";
        const std::string low = lower(sec);
        for (const char* p : {"aucun motif raisonnable", "pas lieu de porter", "aucune accusation",
                              "ne porterai pas", "ne sera port\xc3\xa9" "e", "dossier est clos"}) {
            if (low.find(p) != std::string::npos) return "FALSE";
        }
        for (const char* p : {"a \xc3\xa9t\xc3\xa9 accus\xc3\xa9", "accusations ont \xc3\xa9t\xc3\xa9 port\xc3\xa9" "es",
                              "accusation a \xc3\xa9t\xc3\xa9 port\xc3\xa9" "e", "inculp\xc3\xa9"}) {
            if (low.find(p) != std::string::npos) return "TRUE";
        }
        return "";
    }
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
    // the legacy (about 2010) English layout: a "Police service:" header and numbered roles
    static const std::array<const char*, 13> en = {
        "The Investigation", "Notification of the SIU", "Mandate engaged",
        "Civilian Witnesses", "Witness Officers", "Subject Officers",
        "Analysis and Director's Decision", "Witness Officials",
        "Police service:", "Incident date:", "Witness Officer #", "Civilian Witness #", "Subject Officer #"};
    static const std::array<const char*, 11> fr = {
        "L'enqu\xC3\xAAte", "L\xE2\x80\x99" "enqu\xC3\xAAte", "Exercice du mandat",
        "\xC3\x89l\xC3\xA9ments de preuve",
        "Dispositions l\xC3\xA9gislatives pertinentes",
        "T\xC3\xA9moins civils", "Agents impliqu\xC3\xA9s", "Mandat de l'UES", "Mandat de l\xE2\x80\x99UES",
        "Service de police :", "Agents t\xC3\xA9moins"};
    int e = 0, f = 0;
    for (const char* mk : en) if (text.find(mk) != std::string::npos) ++e;
    for (const char* mk : fr) if (text.find(mk) != std::string::npos) ++f;
    if (e >= 2 && e > f) return "en";
    if (f >= 2 && f > e) return "fr";
    return "unknown";
}

}  // namespace

// shared with rmoriebricklayer's parser: English and French months, ordinals (3rd, 1er)
std::string to_iso_date(const std::string& human_in) {
    // "1 er septembre 2016" (the ordinal set apart, a space or a no-break space) is "1er"
    static const std::regex sep_er("(\\d)(?:\\s|\xc2\xa0)+er\\b");
    const std::string human = std::regex_replace(human_in, sep_er, "$1er");
    // English and French month names (the SIU publishes both); accents are kept as UTF-8
    // a data table (no code per line), loaded into the map once
    static const char* const kNames[] = {
        "january", "february", "march", "april", "may", "june", "july", "august",
        "september", "october", "november", "december",
        "janvier", "f\xc3\xa9vrier", "fevrier", "mars", "avril", "mai", "juin", "juillet",
        "ao\xc3\xbbt", "aout", "septembre", "octobre", "novembre", "d\xc3\xa9" "cembre", "decembre",
        "jan", "feb", "mar", "apr", "jun", "jul", "aug", "sep", "sept", "oct", "nov", "dec",
        // French abbreviations: "5 janv. 2023", "f\xc3\xa9vr.", "avr.", "juil.", "d\xc3\xa9" "c."
        "janv", "f\xc3\xa9vr", "fevr", "avr", "juil", "d\xc3\xa9" "c"};
    static const int kNums[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                                1, 2, 2, 3, 4, 5, 6, 7, 8, 8, 9, 10, 11, 12, 12,
                                1, 2, 3, 4, 6, 7, 8, 9, 9, 10, 11, 12,
                                1, 2, 2, 4, 7, 12};
    static const std::map<std::string, int> kMonths = [] {
        std::map<std::string, int> m;
        for (size_t i = 0; i < sizeof(kNums) / sizeof(kNums[0]); ++i) m[kNames[i]] = kNums[i];
        return m;
    }();
    // "January 5, 2023" / "January 5 2023" (month first) or "5 janvier 2023" / "3 ao\xc3\xbbt 2017" (day first)
    // "March 3 , 2020" (a space before the comma) as well
    static const std::regex pat(R"(([^\s\d,.]+)\.?\s+(\d{1,2})(?:st|nd|rd|th|er|e)?\s*,?\s+(\d{4}))");
    static const std::regex pat_fr(R"((\d{1,2})(?:er|e|st|nd|rd|th)?\s+(?:of\s+)?([^\s\d,.]+)\.?,?\s+(\d{4}))");
    // "3 AO\xc3\x9bT 2017": lower() folds ASCII only, so fold the accented capitals too
    auto key = [](std::string w) {
        w = lower(w);
        for (size_t i = 0; i + 1 < w.size(); ++i)
            if (static_cast<unsigned char>(w[i]) == 0xc3 && static_cast<unsigned char>(w[i + 1]) >= 0x80 &&
                static_cast<unsigned char>(w[i + 1]) <= 0x9e && static_cast<unsigned char>(w[i + 1]) != 0x97)
                w[i + 1] = static_cast<char>(static_cast<unsigned char>(w[i + 1]) + 0x20);
        return w;
    };
    std::smatch m;
    std::string month, day, year;
    if (std::regex_search(human, m, pat) && kMonths.count(key(m[1].str()))) {
        month = m[1].str(); day = m[2].str(); year = m[3].str();
    } else if (std::regex_search(human, m, pat_fr) && kMonths.count(key(m[2].str()))) {
        day = m[1].str(); month = m[2].str(); year = m[3].str();
    } else {
        // already ISO?
        static const std::regex iso(R"(^\d{4}-\d{2}-\d{2}$)");
        return std::regex_match(human, iso) ? human : "";
    }
    const auto it = kMonths.find(key(month));
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
    static const std::array<std::pair<const char*, const char*>, 30> kNamed = {{
        {"&eacute;", "\xc3\xa9"}, {"&egrave;", "\xc3\xa8"}, {"&ecirc;", "\xc3\xaa"}, {"&euml;", "\xc3\xab"},
        {"&agrave;", "\xc3\xa0"}, {"&acirc;", "\xc3\xa2"}, {"&ccedil;", "\xc3\xa7"}, {"&icirc;", "\xc3\xae"},
        {"&iuml;", "\xc3\xaf"}, {"&ocirc;", "\xc3\xb4"}, {"&ouml;", "\xc3\xb6"}, {"&ucirc;", "\xc3\xbb"},
        {"&ugrave;", "\xc3\xb9"}, {"&uuml;", "\xc3\xbc"}, {"&auml;", "\xc3\xa4"}, {"&copy;", "\xc2\xa9"},
        {"&reg;", "\xc2\xae"}, {"&Eacute;", "\xc3\x89"}, {"&Agrave;", "\xc3\x80"}, {"&Egrave;", "\xc3\x88"},
        {"&Ecirc;", "\xc3\x8a"}, {"&Ccedil;", "\xc3\x87"}, {"&Ocirc;", "\xc3\x94"}, {"&Icirc;", "\xc3\x8e"},
        {"&Acirc;", "\xc3\x82"}, {"&laquo;", "\xc2\xab"}, {"&raquo;", "\xc2\xbb"}, {"&oelig;", "\xc5\x93"},
        {"&OElig;", "\xc5\x92"}, {"&thinsp;", " "}}};
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
static const std::string kFrDate = "(\\d{1,2}(?:(?:\\s|\xc2\xa0)?er)?\\s+" + kFrMonths + "\\s+\\d{4})";
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
    // a legacy (2005-2011) report's header states it: "Date de l'incident : Le 14 janvier 2005"
    {
        static const std::regex head("Date de l" + kApos + "incident\\s*:\\s*(?:[Ll]e\\s+)?" + kFrDate);
        std::smatch hm;
        if (std::regex_search(text, hm, head)) return hm[1].str();
    }
    std::string sec = section_text(text, "L\xe2\x80\x99" "enqu\xc3\xaa" "te");
    if (sec.empty()) sec = section_text(text, "L'enqu\xc3\xaa" "te");
    if (sec.empty()) sec = text;
    // "Le 9 f\xc3\xa9vrier 2022, ..." and "Dans la matin\xc3\xa9e du 9 f\xc3\xa9vrier 2022, ..."
    static const std::regex pat("\\b(?:[Ll]e|du)\\s+" + kFrDate);
    static const std::regex ues("l" + kApos + "\\s*UES\\b");
    // the investigations own dates: interviews, the teams dispatch and arrival
    static const std::regex logistics("entrevue|Date et heure|\xc3\xa9quipe|enqu\xc3\xaateurs", std::regex::icase);
    for (auto it = std::sregex_iterator(sec.begin(), sec.end(), pat); it != std::sregex_iterator(); ++it) {
        const std::string sent = sentence_at(sec, static_cast<size_t>(it->position(0)));
        const size_t a = it->position(0) > 60 ? static_cast<size_t>(it->position(0)) - 60 : 0;
        const std::string before = lower(sec.substr(a, static_cast<size_t>(it->position(0)) - a));
        // the whole line too: "Date et heure d'arriv\xc3\xa9e de l' UES sur les lieux : Le 10 f\xc3\xa9vrier 2022"
        const size_t p0 = static_cast<size_t>(it->position(0));
        const size_t ls = sec.rfind('\n', p0), le = sec.find('\n', p0);
        const std::string line = sec.substr(ls == std::string::npos ? 0 : ls + 1,
                                            (le == std::string::npos ? sec.size() : le) - (ls == std::string::npos ? 0 : ls + 1));
        if (std::regex_search(sent, ues) || std::regex_search(sent, logistics) || std::regex_search(line, logistics) ||
            std::regex_search(line, ues) || before.find("envoi de l") != std::string::npos ||
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
    if (start == std::string::npos) {
        static const std::regex told("UES\\s+a\\s+\xc3\xa9t\xc3\xa9\\s+avis\xc3\xa9" "e[^\\n]{0,200}?\\b(?:le|du)\\s+" + kFrDate);
        std::smatch tm;
        return std::regex_search(text, tm, told) ? tm[1].str() : "";
    }
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

// ---- the subject officials' service -------------------------------------
// police_service is the service whose officers are the subject officials. The force that notified
// the SIU is often a different one (custody, requesting or neighbouring service), so the director's
// analysis decides: the first service named in a sentence that names a subject official ("the SO of
// the BPS", "un agent du SPT"), else the service the analysis names most. The case number's letter
// (T Toronto, P OPP, I First Nations, O any other) rules out services of the wrong kind. Legacy
// reports name the service in a "Police service:" header.

static const std::string kCap = "(?:[A-Z]|\xc3[\x80-\x9d])";
static const std::string kWord = "[^\\s,.;()]*";
static const std::string kEnName =
    "((?:[A-Z][A-Za-z'\\-]+[ \\t]+){1,5}(?:Police Service|Police Department|Provincial Police|Police|Constabulary))"
    "\\b(?![ \\t]+Services?[ \\t]+(?:Act|Board))";
static const std::string kFrName =
    "([Ss]ervice(?: [a-z\xc3\xa9]+){0,2} de (?:la )?police(?: [a-z\xc3\xa9]+){0,2} (?:de la |de |du |des |d" + kApos +
    ")(?:grand )?" + kCap + kWord + "(?: " + kCap + kWord + ")*" +
    "|[Ss]ervice de police (?:Nishnawbe[- ]Aski|Anishinabek|Akwesasne|Wikwemikong|UCCM)" +
    "|[Pp]olice r\xc3\xa9gionale (?:de |du |d" + kApos + ")" + kCap + kWord + "(?: " + kCap + kWord + ")*" +
    "|[Pp]olice [Pp]rovinciale(?: de l" + kApos + "Ontario)?" +
    "|[Pp]olice (?:de |d" + kApos + ")" + kCap + kWord + "(?: " + kCap + kWord + ")*)";

static std::string service_kind(const std::string& n) {
    static const std::regex fn("Nishnawbe|Anishinabek|Treaty|Trait\xc3\xa9|Akwesasne|Wikwemikong|UCCM|Lac Seul|Rama|"
                               "Six Nations|Premi\xc3\xa8res? Nations?|First Nations?");
    static const std::regex prov("Provin\\w*al|[Pp]rovinciale");
    if (std::regex_search(n, fn)) return "I";
    if (std::regex_search(n, prov)) return "P";
    if (n.find("Toronto") != std::string::npos) return "T";
    return "O";
}

static std::string clean_service(std::string n) {
    static const std::regex lead("^(?:The|A|An|Of|And|On|In|By|To|With|From|That|This|Local|While|When|As)\\s+");
    n = trim(n);
    for (int i = 0; i < 3; ++i) n = std::regex_replace(n, lead, "");
    // "la Police provinciale" is a report's back-reference to the OPP, and a legacy header
    // names the detachment ("Police provinciale de l'Ontatio de Sioux Lookout", the source's
    // own typo; "OPP Sioux Lookout"): one name per service
    static const std::regex opp_fr("^[Pp]olice [Pp]rovinciale\\b.*"), opp_en("^(?:OPP|Ontario Provincial Police)\\b.*");
    if (std::regex_match(n, opp_fr)) n = "Police provinciale de l'Ontario";
    else if (std::regex_match(n, opp_en)) n = "Ontario Provincial Police";
    return n;
}

static std::string pick_service(const std::string& sec, const std::string& text, bool fr, const std::string& letter) {
    static const std::regex en_name(kEnName), fr_name(kFrName);
    static const std::regex en_abbr(kEnName + "\\s*\\(\\s*([A-Z]{2,6})\\s*\\)"), fr_abbr(kFrName + "\\s*\\(\\s*([A-Z]{2,6})\\s*\\)");
    static const std::regex not_service("\\b(?:Independent|Review|Office|Between|Involving|After|During|Following|Collision|"
                                        "Crash|Shooting|Death|Injury|Incident|Arrest)\\b");
    static const std::regex so_en("\\bSOs?\\b|[Ss]ubject [Oo]ffic(?:er|ial)s?"), so_fr("\\bAIs?\\b|agente?s? impliqu");
    std::vector<std::pair<std::string, std::string>> abbr =
        fr ? std::vector<std::pair<std::string, std::string>>{
                 {"PPO", "Police provinciale de l'Ontario"}, {"SPT", "Service de police de Toronto"},
                 {"PRY", "Police r\xc3\xa9gionale de York"}, {"PRP", "Police r\xc3\xa9gionale de Peel"},
                 {"SPRP", "Service de police r\xc3\xa9gional de Peel"}, {"SPRD", "Service de police r\xc3\xa9gional de Durham"},
                 {"SPRN", "Service de police r\xc3\xa9gional de Niagara"}, {"SPRH", "Service de police r\xc3\xa9gional de Halton"},
                 {"SPRW", "Service de police r\xc3\xa9gional de Waterloo"}}
           : std::vector<std::pair<std::string, std::string>>{
                 {"OPP", "Ontario Provincial Police"}, {"TPS", "Toronto Police Service"}, {"YRP", "York Regional Police"},
                 {"PRP", "Peel Regional Police"}, {"DRPS", "Durham Regional Police Service"},
                 {"NRPS", "Niagara Regional Police Service"}, {"HRPS", "Halton Regional Police Service"},
                 {"WRPS", "Waterloo Regional Police Service"}};
    // the page's own "<name> ( ABBR )" pairs override the defaults; the first definition wins
    std::set<std::string> seen;
    for (auto it = std::sregex_iterator(text.begin(), text.end(), fr ? fr_abbr : en_abbr); it != std::sregex_iterator(); ++it) {
        const std::string k = (*it)[it->size() - 1].str();
        if (!seen.insert(k).second) continue;
        const std::string v = clean_service((*it)[1].str());
        bool found = false;
        for (auto& [ak, av] : abbr)
            if (ak == k) { av = v; found = true; }
        if (!found) abbr.emplace_back(k, v);
    }
    std::vector<std::pair<size_t, std::string>> ments;
    for (auto it = std::sregex_iterator(sec.begin(), sec.end(), fr ? fr_name : en_name); it != std::sregex_iterator(); ++it) {
        const std::string n = clean_service((*it)[1].str());
        if (!n.empty() && !std::regex_search(n, not_service)) ments.emplace_back(static_cast<size_t>(it->position(0)), n);
    }
    auto word = [](char c) { return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '_'; };
    for (const auto& [k, v] : abbr) {
        if (v.empty() || std::regex_search(v, not_service)) continue;
        for (size_t at = sec.find(k); at != std::string::npos; at = sec.find(k, at + 1))
            if ((at == 0 || !word(sec[at - 1])) && (at + k.size() == sec.size() || !word(sec[at + k.size()])))
                ments.emplace_back(at, v);
    }
    std::sort(ments.begin(), ments.end());
    if (!letter.empty()) {
        std::vector<std::pair<size_t, std::string>> keep;
        for (const auto& m : ments)
            if (service_kind(m.second) == letter) keep.push_back(m);
        if (!keep.empty()) ments = keep;
    }
    if (ments.empty()) return "";
    for (auto& m : ments)
        if (m.second[0] >= 'a' && m.second[0] <= 'z') m.second[0] = static_cast<char>(m.second[0] - 'a' + 'A');
    // a service that notified the SIU ("the OPS contacted the SIU to report that police
    // officers with the VPD ...") is the notifier, not the subject officials' service
    static const std::regex notif_en("^[^.,;]{0,60}?\\b(?:notified|contacted|alerted|advised|called)\\s+the\\s+SIU\\b|"
                                     "^[^.,;]{0,60}?\\breported\\s+(?:to\\s+the\\s+SIU\\b|that\\b)"),
        notif_fr("^[^.,;]{0,60}?\\ba\\s+(?:avis\xc3\xa9|inform\xc3\xa9|signal\xc3\xa9|communiqu\xc3\xa9\\s+avec)\\s+l(?:'|\xe2\x80\x99)\\s?UES");
    auto notifier = [&](size_t pos) {
        const std::string after = sec.substr(pos, 160);
        return std::regex_search(after, fr ? notif_fr : notif_en);
    };
    for (auto it = std::sregex_iterator(sec.begin(), sec.end(), fr ? so_fr : so_en); it != std::sregex_iterator(); ++it) {
        const size_t pos = static_cast<size_t>(it->position(0));
        const size_t dot = pos == 0 ? std::string::npos : sec.rfind('.', pos - 1);
        const size_t a = dot == std::string::npos ? 0 : dot + 1;
        const size_t b = std::min(sec.find('.', pos), sec.size());
        for (const auto& m : ments)
            if (m.first >= a && m.first <= b && !notifier(m.first)) return m.second;
        // "... arrest by NRPS officers. The SIU named the SO ..." -- the sentence before names it
        if (a > 0) {
            const size_t pdot = a >= 2 ? sec.rfind('.', a - 2) : std::string::npos;
            const size_t pa = pdot == std::string::npos ? 0 : pdot + 1;
            for (const auto& m : ments)
                if (m.first >= pa && m.first < a && !notifier(m.first)) return m.second;
        }
    }
    std::map<std::string, int> counts;
    for (const auto& m : ments) counts[m.second]++;
    const std::pair<size_t, std::string>* best = nullptr;
    for (const auto& m : ments)
        if (!best || counts[m.second] > counts[best->second]) best = &m;
    return best->second;
}

std::string detect_subject_service(const std::string& text, bool fr) {
    static const std::regex legacy_en("Police service\\s*:\\s*(.+?)\\s+Incident date"),
        legacy_fr("Service de police\\s*:\\s*(.+?)\\s+Date de l");
    static const std::regex en_name(kEnName), fr_name(kFrName);
    std::smatch m;
    if (std::regex_search(text, m, fr ? legacy_fr : legacy_en)) {
        const std::string h = trim(m[1].str());
        for (auto it = std::sregex_iterator(text.begin(), text.end(), fr ? fr_name : en_name); it != std::sregex_iterator(); ++it) {
            const std::string n = clean_service((*it)[1].str());
            if (lower(n).find(lower(h)) != std::string::npos) return n;
        }
        return clean_service(h);
    }
    static const std::regex case_no("\\b\\d\\d-([TPOI])[A-Z]{2}-\\d{3}\\b");
    const std::string letter = std::regex_search(text, m, case_no) ? m[1].str() : "";
    const std::string head = fr ? "analyse et d\xc3\xa9" "cision du directeur" : "analysis and director";
    const size_t i = lower(text).rfind(head);
    std::string r = i == std::string::npos ? "" : pick_service(text.substr(i), text, fr, letter);
    return r.empty() ? pick_service(text, text, fr, letter) : r;
}

static std::string or_else(const std::string& a, const std::string& b) { return a.empty() ? b : a; }

// days since 1970-01-01 and back (Hinnant's civil-calendar algorithms): no time zones involved
static long days_from_civil(long y, unsigned m, unsigned d) {
    y -= m <= 2;
    const long era = (y >= 0 ? y : y - 399) / 400;
    const unsigned yoe = static_cast<unsigned>(y - era * 400);
    const unsigned doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;
    const unsigned doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    return era * 146097 + static_cast<long>(doe) - 719468;
}
static std::string civil_from_days(long z) {
    z += 719468;
    const long era = (z >= 0 ? z : z - 146096) / 146097;
    const unsigned doe = static_cast<unsigned>(z - era * 146097);
    const unsigned yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
    const unsigned doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    const unsigned mp = (5 * doy + 2) / 153;
    const unsigned d = doy - (153 * mp + 2) / 5 + 1;
    const unsigned m = mp + (mp < 10 ? 3 : -9);
    const long y = static_cast<long>(yoe) + era * 400 + (m <= 2);
    char buf[48];
    std::snprintf(buf, sizeof buf, "%04ld-%02u-%02u", y, m, d);
    return buf;
}

// The incident date a notification states relative to itself: "notified the SIU of the
// injuries sustained ... during his arrest two hours prior" (the same day), "had the day
// before discharged" (the day before); French "deux heures plus t\xc3\xb4t", "la veille".
// Read from the sentences that report the notification; "" when none says.
static std::string relative_incident(const std::string& text, const std::string& notified_iso, bool fr) {
    if (notified_iso.size() != 10) return "";
    static const std::regex notif_sentence_en("\\b(?:notified|contacted|advised|alerted|reported)\\b[^.]{0,40}\\bSIU\\b|\\bSIU\\b[^.]{0,20}\\bnotified\\b"),
        notif_sentence_fr("\\ba\\s+(?:avis\xc3\xa9|inform\xc3\xa9|communiqu\xc3\xa9)[^.]{0,40}UES"),
        prev_en("\\bthe (?:day|night|evening|morning) before\\b|\\bthe previous (?:day|night|evening|morning)\\b", std::regex::icase),
        same_en("\\b(?:\\d+|an?|one|two|three|four|five|six|seven|eight|nine|ten|eleven|twelve|several|a few)\\s+"
                "(?:hours?|minutes?)\\s+(?:prior|earlier|before)\\b|\\bearlier (?:that|the same) (?:day|morning|afternoon|evening|night)\\b|"
                "\\bthe same (?:day|morning|afternoon|evening)\\b", std::regex::icase),
        prev_fr("\\bla veille\\b|\\ble jour pr\xc3\xa9" "c\xc3\xa9" "dent\\b|\\bla nuit pr\xc3\xa9" "c\xc3\xa9" "dente\\b", std::regex::icase),
        same_fr("\\b(?:\\d+|une|deux|trois|quatre|cinq|six|sept|huit|neuf|dix|plusieurs|quelques)\\s+(?:heures?|minutes?)\\s+plus\\s+t\xc3\xb4t|"
                "plus\\s+t\xc3\xb4t\\s+(?:ce jour-l\xc3\xa0|dans la journ\xc3\xa9" "e|ce matin-l\xc3\xa0|ce soir-l\xc3\xa0)|\\ble m\xc3\xaame jour\\b",
                std::regex::icase);
    const std::string flat = flatten_ws(text);
    int shift = 1;  // 1: none found
    size_t start = 0;
    while (start < flat.size()) {
        size_t end = flat.find(". ", start);
        if (end == std::string::npos) end = flat.size();
        const std::string sent = flat.substr(start, end - start);
        start = end + 2;
        if (!std::regex_search(sent, fr ? notif_sentence_fr : notif_sentence_en)) continue;
        if (std::regex_search(sent, fr ? prev_fr : prev_en)) { shift = -1; break; }
        if (std::regex_search(sent, fr ? same_fr : same_en)) { shift = 0; break; }
    }
    if (shift == 1) return "";
    const long y = std::stol(notified_iso.substr(0, 4));
    const unsigned m = static_cast<unsigned>(std::stoul(notified_iso.substr(5, 2)));
    const unsigned d = static_cast<unsigned>(std::stoul(notified_iso.substr(8, 2)));
    return civil_from_days(days_from_civil(y, m, d) + shift);
}

ParsedFields parse_report_text(const std::string& text) {
    ParsedFields f;
    f["_language"] = detect_language(text);

    const bool fr = f["_language"] == "fr";
    f["police_service"] = or_else(detect_subject_service(text, fr),
                                  fr ? or_else(detect_police_service_fr(text), detect_police_service(text))
                                     : detect_police_service(text));
    f["date_of_incident_iso"] =
        to_iso_date(fr ? or_else(detect_incident_date_fr(text), detect_incident_date(text)) : detect_incident_date(text));
    f["date_siu_notified_iso"] =
        to_iso_date(fr ? or_else(detect_siu_notified_fr(text), detect_siu_notified(text)) : detect_siu_notified(text));
    {
        // a notification that dates the incident relative to itself wins over the first dated
        // sentence of the narrative (which may be an earlier, unrelated event)
        const std::string rel = relative_incident(text, f["date_siu_notified_iso"], fr);
        if (!rel.empty()) f["date_of_incident_iso"] = rel;
    }
    f["date_of_director_decision_iso"] = to_iso_date(
        fr ? or_else(detect_decision_date_fr(text), detect_decision_date(text)) : detect_decision_date(text));

    f["siu_investigators"] = team_count(text, "SIU Investigators");
    f["siu_forensics_investigators"] =
        team_count(text, "SIU Forensic Investigators");
    if (fr) {
        // "Nombre d'enqu\xc3\xaateurs de l' UES assign\xc3\xa9s : 3", and the forensic team's line
        static const std::regex inv_fr("Nombre d(?:'|\xe2\x80\x99)enqu\xc3\xaateurs de l[^0-9\n]{0,30}?(\\d+)"),
            for_fr("Nombre d(?:'|\xe2\x80\x99)enqu\xc3\xaateurs sp\xc3\xa9" "cialistes[^0-9\n]{0,90}?(\\d+)");
        std::smatch m;
        if (f["siu_investigators"].empty() && std::regex_search(text, m, inv_fr)) f["siu_investigators"] = m[1].str();
        if (f["siu_forensics_investigators"].empty() && std::regex_search(text, m, for_fr))
            f["siu_forensics_investigators"] = m[1].str();
    }

    // Officer and witness counts from their sections, in every layout (role_section):
    // the English tags SO / WO / CW, the French AI / AT / TC.
    if (f["siu_investigators"].empty())
        f["siu_investigators"] = fr ? team_words(text, "enqu\xc3\xaateurs", true) : team_words(text, "SIU investigators", false);
    if (f["siu_forensics_investigators"].empty())
        f["siu_forensics_investigators"] =
            fr ? team_words(text, "techniciens en identification", true)
               : team_words(text, "(?:SIU )?forensic investigators", false);
    const std::string so = role_section(text, fr ? kRoleHeads[3] : kRoleHeads[0]);
    const std::string so_legacy =
        so.empty() ? count_labelled(text, fr ? "agent impliqu\xc3\xa9" : "Subject Officer") : "";
    // with no subject-official section only a numbered tag counts: a bare "SO" in the text
    // is no evidence of how many (no subject officer may have been identified at all)
    f["number_of_subject_officials"] = !so_legacy.empty() ? so_legacy
                                       : !so.empty()      ? count_tagged(so, fr ? "AI" : "SO")
                                                          : count_labelled(text, fr ? "AI" : "SO");
    if (f["number_of_subject_officials"].empty()) {
        // no section and no numbered officer: the resolver's rules (a plural cue, "the SO",
        // "no subject officer", witness officials only; in French the AI roster, "l'agent(e)
        // impliqu\xc3\xa9(e) n o 3", "aucun agent impliqu\xc3\xa9") -- the same answer resolve_so() gives
        const SoResolution r = resolve_subject_officials(text);
        if (r.count) f["number_of_subject_officials"] = std::to_string(*r.count);
    }

    const std::string wo = role_section(text, fr ? kRoleHeads[4] : kRoleHeads[1]);
    if (!wo.empty() &&
        std::regex_search(lower(flatten_ws(wo)),
                          std::regex(R"(no police officers? witness|aucun agent t\xc3\xa9moin)"))) {
        f["number_of_witness_officials"] = "0";
    } else {
        f["number_of_witness_officials"] = count_tagged(wo, fr ? "AT" : "WO");
        // a section that spells the role out: "Witness Officer #2", "l'agent t\xc3\xa9moin no 2"
        const std::string spelled = count_labelled(wo, fr ? "agent t\xc3\xa9moin" : "Witness Offic(?:er|ial)");
        if (!spelled.empty() && (f["number_of_witness_officials"].empty() ||
                                 std::stoi(spelled) > std::stoi(f["number_of_witness_officials"])))
            f["number_of_witness_officials"] = spelled;
    }
    if (wo.empty()) f["number_of_witness_officials"] = count_labelled(text, fr ? "agent t\xc3\xa9moin" : "Witness Officer");

    const std::string cw = role_section(text, fr ? kRoleHeads[5] : kRoleHeads[2]);
    if (!cw.empty() && std::regex_search(lower(flatten_ws(cw)),
                                         std::regex(R"(no civilian witness|aucun t\xc3\xa9moin civil)"))) {
        f["number_of_civilian_witnesses"] = "0";
    } else {
        f["number_of_civilian_witnesses"] = count_tagged(cw, fr ? "TC" : "CW");
        const std::string spelled = count_labelled(cw, fr ? "t\xc3\xa9moin civil" : "Civilian Witness");
        if (!spelled.empty() && (f["number_of_civilian_witnesses"].empty() ||
                                 std::stoi(spelled) > std::stoi(f["number_of_civilian_witnesses"])))
            f["number_of_civilian_witnesses"] = spelled;
    }
    if (cw.empty()) f["number_of_civilian_witnesses"] = count_labelled(text, fr ? "t\xc3\xa9moin civil" : "Civilian Witness");

    auto [age, sex] = detect_age_sex(text);
    if (fr && age.empty()) std::tie(age, sex) = detect_age_sex_fr(text);
    f["age_affected"] = age;
    f["sex_gender_affected"] = sex;

    f["charges_recommended"] = detect_charges(text, fr);
    f["directors_name"] = detect_directors_name(text);
    f["location_of_call"] = detect_location(text);
    f["specific_injuries"] = detect_specific_injuries(text, fr);
    f["relevant_legislation"] = detect_legislation(text, fr);
    return f;
}

ParsedFields parse_report_html(const std::string& html) {
    return parse_report_text(html_to_text(html));
}

}  // namespace siu
