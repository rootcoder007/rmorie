// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Rcpp bindings over the SIU C++17 core in src/siu/ (parse, resolve, schema,
// http, llm, audit) -- the same sources the standalone CLI in tools/siu-cli
// builds, so the package and the CLI cannot drift. Every function is a thin
// passthrough; the logic lives in the core.
#include <Rcpp.h>

#include <string>
#include <vector>

#include "siu/audit.hpp"
#include "siu/http.hpp"
#include "siu/llm.hpp"
#include "siu/parse.hpp"
#include "siu/resolve.hpp"
#include "siu/schema.hpp"

namespace {

siu::llm::Backend make_backend(const std::string& api, const std::string& base, const std::string& key,
                               double timeout_s, double temperature) {
    siu::llm::Backend b;
    b.api = api;
    b.base = base;
    b.key = key;
    b.timeout_s = static_cast<long>(timeout_s);
    b.temperature = temperature;
    return b;
}

siu::audit::Granularity granularity(const std::string& g) {
    if (g == "all") return siu::audit::Granularity::kAllFields;
    if (g == "per-field" || g == "per_field") return siu::audit::Granularity::kPerField;
    Rcpp::stop("granularity must be \"all\" or \"per-field\"");
}

}  // namespace

// Every string handed back to R is UTF-8 and marked as such: wrapping a std::string leaves it
// "native", which a C locale reads as bytes ("Rivi\303\250reville" is not "Rivi\u00e8reville").
static Rcpp::String u8(const std::string& s) { return Rcpp::String(s, CE_UTF8); }

// The core polls this inside every whole-document pass; Rcpp turns a pending
// Ctrl-C into its own exception, which the Rcpp export wrapper unwinds cleanly.
static void siu_interrupt_hook() { Rcpp::checkUserInterrupt(); }
static const bool siu_hook_installed = (siu::interrupt_hook() = siu_interrupt_hook, true);

// A line longer than 2000 characters was split before extraction (sentence end
// preferred): say so, as the canonical package does, never silently.
static void siu_split_warning() {
    if (siu::last_split_lines() > 0) {
        Rcpp::warning("%d line(s) longer than 2000 characters were split for extraction; a field spanning a split may be incomplete",
                      static_cast<int>(siu::last_split_lines()));
    }
}

// The caps rmoriebricklayer's own entry points apply (rmbl_siu.cpp): a report
// page is a few hundred KB, and the core's regexes run over text whose lines
// normalize_text() has capped -- libstdc++'s regex executor recurses once per
// character a repeated atom consumes, and 25 KB of whitespace killed R before
// the passes over a whole document became loops.
static const std::string& checked_page(const std::string& s, const char* what) {
    if (s.size() > (2u << 20)) Rcpp::stop("%s is larger than 2 MiB: not a report page", what);
    return s;
}

// [[Rcpp::export(.siu_core_html_to_text)]]
Rcpp::String siu_core_html_to_text(const std::string& html) {
    const std::string out = siu::html_to_text(checked_page(html, "html"));
    siu_split_warning();
    return u8(out);
}

// [[Rcpp::export(.siu_core_parse_html)]]
Rcpp::CharacterVector siu_core_parse_html(const std::string& html) {
    const siu::ParsedFields fields = siu::parse_report_html(checked_page(html, "html"));
    siu_split_warning();
    Rcpp::CharacterVector out(fields.size());
    Rcpp::CharacterVector names(fields.size());
    R_xlen_t i = 0;
    for (const auto& kv : fields) {
        names[i] = u8(kv.first);
        out[i] = u8(kv.second);
        ++i;
    }
    out.attr("names") = names;
    return out;
}

// [[Rcpp::export(.siu_core_to_iso_date)]]
Rcpp::String siu_core_to_iso_date(const std::string& human) {
    // a date string is a few dozen bytes; the date regexes run over the whole input
    if (human.size() > 4096) Rcpp::stop("`x` is longer than 4096 bytes: not a date");
    return u8(siu::to_iso_date(human));
}

// [[Rcpp::export(.siu_core_strip_boilerplate)]]
Rcpp::String siu_core_strip_boilerplate(const std::string& text) {
    return u8(siu::strip_boilerplate(siu::normalize_text(checked_page(text, "text"))));
}

// [[Rcpp::export(.siu_core_resolve_so)]]
Rcpp::List siu_core_resolve_so(const std::string& text) {
    // plain text from the caller gets the same whitespace/line discipline the HTML path has
    const siu::SoResolution res = siu::resolve_subject_officials(siu::normalize_text(checked_page(text, "text")));
    siu_split_warning();
    return Rcpp::List::create(
        Rcpp::Named("count") = res.count.has_value() ? Rcpp::IntegerVector::create(*res.count)
                                                     : Rcpp::IntegerVector::create(NA_INTEGER),
        Rcpp::Named("reason") = u8(res.reason));
}

// [[Rcpp::export(.siu_core_schema)]]
Rcpp::DataFrame siu_core_schema() {
    const auto& fields = siu::schema();
    const R_xlen_t n = static_cast<R_xlen_t>(fields.size());
    Rcpp::CharacterVector name(n), desc(n);
    Rcpp::LogicalVector is_count(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        name[i] = u8(fields[static_cast<size_t>(i)].name);
        is_count[i] = fields[static_cast<size_t>(i)].is_count;
        desc[i] = u8(fields[static_cast<size_t>(i)].desc);
    }
    return Rcpp::DataFrame::create(Rcpp::Named("name") = name, Rcpp::Named("is_count") = is_count,
                                   Rcpp::Named("description") = desc,
                                   Rcpp::Named("stringsAsFactors") = false);
}

// [[Rcpp::export(.siu_core_get)]]
Rcpp::String siu_core_get(const std::string& url, double timeout_s) {
    return u8(siu::http::get(url, static_cast<long>(timeout_s)));
}

// [[Rcpp::export(.siu_core_backend)]]
Rcpp::List siu_core_backend(const std::string& api, const std::string& base, const std::string& key) {
    const siu::llm::Backend b = siu::llm::resolve(make_backend(api, base, key, 300, 0));
    return Rcpp::List::create(Rcpp::Named("api") = b.api, Rcpp::Named("base") = b.base,
                              Rcpp::Named("has_key") = !b.key.empty());
}

// [[Rcpp::export(.siu_core_models)]]
Rcpp::CharacterVector siu_core_models(const std::string& api, const std::string& base, const std::string& key) {
    const auto ms = siu::llm::list_models(make_backend(api, base, key, 300, 0));
    Rcpp::CharacterVector out(ms.size());
    for (size_t i = 0; i < ms.size(); ++i) out[static_cast<R_xlen_t>(i)] = u8(ms[i]);
    return out;
}

// [[Rcpp::export(.siu_core_chat)]]
Rcpp::String siu_core_chat(const std::string& api, const std::string& base, const std::string& key,
                           const std::string& model, const std::string& prompt, double timeout_s,
                           double temperature) {
    return u8(siu::llm::chat(make_backend(api, base, key, timeout_s, temperature), model, prompt));
}

// [[Rcpp::export(.siu_core_default_model)]]
Rcpp::String siu_core_default_model(const std::string& api, const std::string& base, const std::string& key) {
    return u8(siu::audit::default_model(siu::llm::resolve(make_backend(api, base, key, 300, 0))));
}

// [[Rcpp::export(.siu_core_panel)]]
Rcpp::String siu_core_panel(const std::string& report_text, const std::string& parsed_json, int mode,
                           std::vector<std::string> readers, std::vector<std::string> auditors,
                           int num_readers, int num_auditors, int reader_concurrency,
                           bool auditor_sequential, const std::string& reader_granularity,
                           const std::string& auditor_granularity, bool health_check,
                           const std::string& api, const std::string& base, const std::string& key,
                           double timeout_s, double temperature, SEXP chat_fn) {
    if (mode < 1 || mode > 4) Rcpp::stop("mode must be 1, 2, 3 or 4");
    siu::audit::PanelConfig cfg;
    cfg.mode = static_cast<siu::audit::Mode>(mode);
    cfg.reader_models = readers;
    cfg.auditor_models = auditors;
    cfg.num_readers = num_readers;
    cfg.num_auditors = num_auditors;
    cfg.reader_concurrency = reader_concurrency;
    cfg.auditor_sequential = auditor_sequential;
    cfg.reader_granularity = granularity(reader_granularity);
    cfg.auditor_granularity = granularity(auditor_granularity);
    cfg.health_check = health_check;
    cfg.backend = make_backend(api, base, key, timeout_s, temperature);
    if (!Rf_isNull(chat_fn)) {
        // The caller's own model: an R function(model, prompt) -> reply. R is
        // single-threaded, so the panel calls it from this thread only.
        Rcpp::Function f(chat_fn);
        cfg.backend.chat = [f](const std::string& model, const std::string& prompt) {
            return Rcpp::as<std::string>(f(model, prompt));
        };
        cfg.backend.thread_safe = false;
    }
    return u8(siu::audit::run_panel(report_text, parsed_json, cfg));
}
