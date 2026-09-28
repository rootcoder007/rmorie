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

// [[Rcpp::export(.siu_core_html_to_text)]]
std::string siu_core_html_to_text(const std::string& html) { return siu::html_to_text(html); }

// [[Rcpp::export(.siu_core_parse_html)]]
Rcpp::CharacterVector siu_core_parse_html(const std::string& html) {
    const siu::ParsedFields fields = siu::parse_report_html(html);
    Rcpp::CharacterVector out(fields.size());
    Rcpp::CharacterVector names(fields.size());
    R_xlen_t i = 0;
    for (const auto& kv : fields) {
        names[i] = kv.first;
        out[i] = kv.second;
        ++i;
    }
    out.attr("names") = names;
    return out;
}

// [[Rcpp::export(.siu_core_to_iso_date)]]
std::string siu_core_to_iso_date(const std::string& human) { return siu::to_iso_date(human); }

// [[Rcpp::export(.siu_core_strip_boilerplate)]]
std::string siu_core_strip_boilerplate(const std::string& text) { return siu::strip_boilerplate(text); }

// [[Rcpp::export(.siu_core_resolve_so)]]
Rcpp::List siu_core_resolve_so(const std::string& text) {
    const siu::SoResolution res = siu::resolve_subject_officers(text);
    return Rcpp::List::create(
        Rcpp::Named("count") = res.count.has_value() ? Rcpp::IntegerVector::create(*res.count)
                                                     : Rcpp::IntegerVector::create(NA_INTEGER),
        Rcpp::Named("reason") = res.reason);
}

// [[Rcpp::export(.siu_core_schema)]]
Rcpp::DataFrame siu_core_schema() {
    const auto& fields = siu::schema();
    const R_xlen_t n = static_cast<R_xlen_t>(fields.size());
    Rcpp::CharacterVector name(n), desc(n);
    Rcpp::LogicalVector is_count(n);
    for (R_xlen_t i = 0; i < n; ++i) {
        name[i] = fields[static_cast<size_t>(i)].name;
        is_count[i] = fields[static_cast<size_t>(i)].is_count;
        desc[i] = fields[static_cast<size_t>(i)].desc;
    }
    return Rcpp::DataFrame::create(Rcpp::Named("name") = name, Rcpp::Named("is_count") = is_count,
                                   Rcpp::Named("description") = desc,
                                   Rcpp::Named("stringsAsFactors") = false);
}

// [[Rcpp::export(.siu_core_get)]]
std::string siu_core_get(const std::string& url, double timeout_s) {
    return siu::http::get(url, static_cast<long>(timeout_s));
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
    return Rcpp::CharacterVector(ms.begin(), ms.end());
}

// [[Rcpp::export(.siu_core_chat)]]
std::string siu_core_chat(const std::string& api, const std::string& base, const std::string& key,
                          const std::string& model, const std::string& prompt, double timeout_s,
                          double temperature) {
    return siu::llm::chat(make_backend(api, base, key, timeout_s, temperature), model, prompt);
}

// [[Rcpp::export(.siu_core_default_model)]]
std::string siu_core_default_model(const std::string& api, const std::string& base, const std::string& key) {
    return siu::audit::default_model(siu::llm::resolve(make_backend(api, base, key, 300, 0)));
}

// [[Rcpp::export(.siu_core_panel)]]
std::string siu_core_panel(const std::string& report_text, const std::string& parsed_json, int mode,
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
    return siu::audit::run_panel(report_text, parsed_json, cfg);
}
