// SPDX-License-Identifier: AGPL-3.0-or-later
#include "siu/audit.hpp"

#include <condition_variable>
#include <cstdlib>
#include <future>
#include <mutex>
#include <regex>
#include <stdexcept>
#include <string>
#include <vector>

#include "siu/llm.hpp"
#include "siu/nlohmann/json.hpp"
#include "siu/schema.hpp"

using nlohmann::json;

namespace siu::audit {
namespace {

std::string env(const char* name) {
    const char* v = std::getenv(name);
    return v ? std::string(v) : std::string();
}

// C++17 counting semaphore (readers acquire a slot before hitting the server).
class Semaphore {
public:
    explicit Semaphore(int n) : count_(n) {}
    void acquire() {
        std::unique_lock<std::mutex> lk(m_);
        cv_.wait(lk, [&] { return count_ > 0; });
        --count_;
    }
    void release() {
        {
            std::lock_guard<std::mutex> lk(m_);
            ++count_;
        }
        cv_.notify_one();
    }
private:
    std::mutex m_;
    std::condition_variable cv_;
    int count_;
};

json parse_reply(const std::string& reply) {
    std::string s = std::regex_replace(
        reply, std::regex(R"(<think>[\s\S]*?</think>)"), "");
    const auto a = s.find('{');
    const auto z = s.rfind('}');
    if (a == std::string::npos || z == std::string::npos || z <= a) {
        throw std::runtime_error("no JSON object in reply");
    }
    return json::parse(s.substr(a, z - a + 1));
}

const char* kRules =
    "CRITICAL RULES:\n"
    "1. READ THE ENTIRE REPORT before answering. Do not skim -- injuries, "
    "officer designations, charges and the decision often appear only in the "
    "narrative or the Analysis/Decision sections near the end.\n"
    "2. Answer ONLY from what the report actually says, and give the EXACT "
    "supporting quote. If you cannot quote it, you do not know it.\n"
    "3. NEVER answer None/not_stated/unknown out of laziness. None is correct "
    "ONLY after reading the whole report and finding the field genuinely "
    "absent. A lazy None is a serious error.\n"
    "4. COUNT fields (number_of_subject_officers, number_of_witness_officials, "
    "number_of_civilian_witnesses, investigators): give the COUNT of distinct "
    "entities -- counting is NOT inference. Officers are designated explicitly; "
    "a witness-officer-only case has number_of_subject_officers = 0, a REAL "
    "value, never not_stated. Use the canonical key number_of_subject_officers "
    "(never officials/officils).\n"
    "5. Dates in ISO (YYYY-MM-DD) when stated.\n";

std::string reader_prompt(const std::string& parsed, const std::string& text) {
    return std::string(
               "You are a meticulous reviewer extracting structured data from "
               "an Ontario SIU (Special Investigations Unit) director's report. "
               "60+ fields; a single wrong answer is unacceptable.\n\n") +
           kRules +
           "\nOutput ONLY a JSON object: field -> {\"value\": <value>, "
           "\"quote\": \"<exact supporting words, empty only if genuinely "
           "absent>\", \"confidence\": \"high|medium|low\"}.\n\n"
           "PARSED (the parser's guesses -- verify EACH against the report):\n" +
           parsed + "\n\nREPORT:\n" + text;
}

std::string auditor_prompt(const std::string& parsed, const std::string& text,
                           const std::string& reviewers) {
    return std::string(
               "You are the AUDITOR -- the final authority over the reviewers. "
               "You are given the FULL report and, per field, each reviewer's "
               "value + supporting quote + confidence. 60+ fields; a single "
               "wrong answer is unacceptable.\n\n"
               "YOUR JOB:\n"
               "1. READ THE REPORT YOURSELF, in full. Do NOT just take the "
               "reviewers' word -- reviewers skim and lazily answer None.\n"
               "2. For each field, check every reviewer's value AND quote "
               "against the report; accept a value only if the report's exact "
               "words support it.\n"
               "3. Where reviewers disagree, or a quote does not support the "
               "value, or a reviewer said None but the report states it -- "
               "decide by what the REPORT literally says and correct it.\n") +
           kRules +
           "\nOutput ONLY a JSON object: field -> final CLEAN value (the actual "
           "value, never a verdict word like 'agree', never an annotation). Use "
           "the canonical field names exactly as given.\n\n"
           "PARSED:\n" + parsed + "\n\nREVIEWERS (value/quote/confidence per "
           "field):\n" + reviewers + "\n\nREPORT:\n" + text;
}

// Reduce a reader's {field -> {value,quote,confidence}} to {field -> value}
// (used for mode 1, where the reader's answer IS the final record).
json values_only(const json& reader) {
    json out = json::object();
    for (auto it = reader.begin(); it != reader.end(); ++it) {
        out[it.key()] = it.value().is_object() && it.value().contains("value")
                            ? it.value()["value"]
                            : it.value();
    }
    return out;
}

// --- per-field (one-column-at-a-time) prompts -------------------------------
// The whole report is re-read for a SINGLE column, so the model can't skim once
// and hallucinate 60 answers.
std::string per_field_reader_prompt(const Field& f, const std::string& parsed_val,
                                    const std::string& text) {
    return std::string("Read the ENTIRE Ontario SIU director's report below, "
                       "then extract ONLY this one field and nothing else.\n\n"
                       "FIELD: ") + f.name + " -- " + f.desc + "\n\n" + kRules +
           "\nOutput ONLY a JSON object {\"value\": <value>, \"quote\": "
           "\"<exact supporting words, empty only if genuinely absent>\", "
           "\"confidence\": \"high|medium|low\"}.\n\n"
           "PARSER'S GUESS for this field: " + parsed_val + "\n\nREPORT:\n" + text;
}

std::string per_field_auditor_prompt(const Field& f, const std::string& parsed_val,
                                     const std::string& text,
                                     const std::string& reviewers) {
    return std::string("You are the AUDITOR. Read the ENTIRE report yourself, "
                       "then decide the FINAL value for ONLY this one field.\n\n"
                       "FIELD: ") + f.name + " -- " + f.desc + "\n\n" +
           "Check the reviewers' answers against the report; accept only what "
           "the exact words support; reject lazy None; count fields must be "
           "counted (witness-officer-only = 0). " + "\nOutput ONLY a JSON "
           "object {\"" + f.name + "\": <final clean value>}.\n\n"
           "PARSER'S GUESS: " + parsed_val + "\n\nREVIEWERS for this field:\n" +
           reviewers + "\n\nREPORT:\n" + text;
}

std::string get_str(const json& j, const std::string& key) {
    if (!j.contains(key)) return "";
    const auto& v = j[key];
    return v.is_string() ? v.get<std::string>() : v.dump();
}

// Read the report with the chosen granularity -> {field -> {value,quote,conf}}.
json read_report(const llm::Backend& be, const std::string& model,
                 const std::string& report, const json& parsed,
                 Granularity gran) {
    if (gran == Granularity::kAllFields) {
        return parse_reply(llm::chat(be, model,
                                       reader_prompt(parsed.dump(1), report)));
    }
    json out = json::object();
    for (const auto& f : schema()) {
        try {
            const json r = parse_reply(llm::chat(
                be, model,
                per_field_reader_prompt(f, get_str(parsed, f.name), report)));
            out[f.name] = r.contains(f.name) ? r : r;  // {value,quote,conf}
        } catch (...) {
        }
    }
    return out;
}

// Audit with the chosen granularity -> {field -> final value}.
json audit_fields(const llm::Backend& be, const std::string& model,
                  const std::string& report, const json& parsed,
                  const std::string& prior, Granularity gran) {
    if (gran == Granularity::kAllFields) {
        return parse_reply(llm::chat(
            be, model, auditor_prompt(parsed.dump(1), report, prior)));
    }
    json prior_j = json::object();
    try { prior_j = json::parse(prior); } catch (...) {}
    json out = json::object();
    for (const auto& f : schema()) {
        const std::string rev = prior_j.contains(f.name)
                                    ? prior_j[f.name].dump(1)
                                    : prior;
        try {
            const json r = parse_reply(llm::chat(
                be, model,
                per_field_auditor_prompt(f, get_str(parsed, f.name), report, rev)));
            out[f.name] = r.contains(f.name) ? r[f.name] : r;
        } catch (...) {
        }
    }
    return out;
}

}  // namespace

int readers_for(Mode m) {
    switch (m) {
        case Mode::kSingle:        return 1;
        case Mode::kOnePlusAudit:  return 1;
        case Mode::kTwoPlusAudit:  return 2;
        case Mode::kThreePlusAudit:return 3;
    }
    return 1;
}

std::string default_model(const llm::Backend& b) {
    std::string m = env("MORIE_LLM_MODEL");
    if (m.empty()) m = env("OLLAMA_MODEL");
    if (!m.empty()) return m;
    const auto ms = llm::list_models(b);
    return ms.empty() ? std::string() : ms.front();
}

std::vector<std::string> healthy_models(const llm::Backend& b,
                                        const std::vector<std::string>& candidates) {
    std::vector<std::string> ok;
    for (const auto& m : candidates) {
        try {
            if (!llm::chat(b, m, "Reply with the word OK.").empty()) ok.push_back(m);
        } catch (...) {
        }
    }
    return ok;
}

std::string run_panel(const std::string& report_text,
                      const std::string& parsed_json, const PanelConfig& cfg_in) {
    PanelConfig cfg = cfg_in;
    if (!cfg.backend.chat) cfg.backend = llm::resolve(cfg.backend);
    if (cfg.reader_models.empty()) {
        const std::string m = default_model(cfg.backend);
        if (m.empty()) throw std::runtime_error("no model: name one or set MORIE_LLM_MODEL");
        cfg.reader_models.push_back(m);
    }
    if (cfg.auditor_models.empty() && cfg.mode != Mode::kSingle) cfg.auditor_models = cfg.reader_models;

    // Assemble the reader roster (cycle the given models to fill the count).
    const int n = cfg.num_readers > 0 ? cfg.num_readers : readers_for(cfg.mode);
    std::vector<std::string> pool =
        cfg.health_check ? healthy_models(cfg.backend, cfg.reader_models)
                         : cfg.reader_models;
    if (pool.empty()) throw std::runtime_error("no healthy reader model");
    std::vector<std::string> readers;
    for (int i = 0; i < n; ++i) readers.push_back(pool[i % pool.size()]);

    // --- READER TIER: concurrent (bounded), then a hard barrier ----------
    const int slots = cfg.reader_concurrency <= 0
                          ? static_cast<int>(readers.size())
                          : cfg.reader_concurrency;
    Semaphore sem(std::max(1, slots));
    json parsed_j;
    try { parsed_j = json::parse(parsed_json); } catch (...) { parsed_j = json::object(); }
    std::vector<std::future<std::pair<std::string, json>>> futs;
    for (size_t i = 0; i < readers.size(); ++i) {
        const std::string model = readers[i];
        const std::string tag = model + "#" + std::to_string(i + 1);
        auto job = [&, model, tag] {
            sem.acquire();
            std::pair<std::string, json> r{tag, json()};
            try {
                r.second = read_report(cfg.backend, model, report_text, parsed_j,
                                       cfg.reader_granularity);
            } catch (...) {
                r.second = json();  // a flaky reader drops out (>=1 must remain)
            }
            sem.release();
            return r;
        };
        futs.push_back(std::async(cfg.backend.thread_safe ? std::launch::async
                                                          : std::launch::deferred,
                                  job));
    }
    // BARRIER: nothing in the auditor tier runs until every reader has returned.
    json reviews = json::object();
    for (auto& f : futs) {
        auto pr = f.get();
        if (!pr.second.is_null()) reviews[pr.first] = pr.second;
    }
    if (reviews.empty()) throw std::runtime_error("all readers failed");

    // How many auditors? 0 -> mode 1 (no auditor) uses the single reader's
    // answers; otherwise default to the given auditor list (>=1).
    int na = cfg.num_auditors;
    if (na == 0) {
        na = (cfg.mode == Mode::kSingle) ? 0
             : cfg.auditor_models.empty() ? 1
                                          : static_cast<int>(cfg.auditor_models.size());
    }
    if (na <= 0) {
        return values_only(reviews.begin().value()).dump();  // reader-only
    }

    // --- AUDITOR TIER: post-barrier. M auditors form a review CHAIN -- each
    // audits the report AND the previous stage's answers (readers first, then
    // the prior auditor). More auditors = more scrutiny layers. Sequential by
    // design, so the tiers never hit the server at the same time.
    std::vector<std::string> apool =
        cfg.health_check ? healthy_models(cfg.backend, cfg.auditor_models)
                         : cfg.auditor_models;
    if (apool.empty()) throw std::runtime_error("no healthy auditor model");

    std::string prior = reviews.dump(1);
    json final;
    for (int i = 0; i < na; ++i) {
        const std::string model = apool[i % apool.size()];
        final = audit_fields(cfg.backend, model, report_text, parsed_j, prior,
                             cfg.auditor_granularity);
        prior = final.dump(1);  // the next auditor audits this auditor's output
    }
    return final.dump();
}

}  // namespace siu::audit
