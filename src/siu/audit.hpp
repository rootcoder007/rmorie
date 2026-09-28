// SPDX-License-Identifier: AGPL-3.0-or-later
//
// audit.hpp -- Mixture-of-Agents panel over ANY model backend (llm.hpp):
// Ollama, any OpenAI-compatible server, or a caller-supplied chat function.
//
// Topology (two tiers, never running at the same time):
//   READERS/REVIEWERS  read the FULL report and answer every field with a
//                      supporting quote + confidence. 1-3 of them, run
//                      concurrently among themselves.
//   AUDITOR            reads the report AND every reviewer's answer+quote+notes,
//                      then issues the FINAL value per field. Runs ONLY after
//                      every reader has finished (a hard barrier) so readers and
//                      the auditor never hit the server simultaneously.
//
// Modes:
//   1  single model reads + answers (no auditor)
//   2  1 reader  + 1 auditor
//   3  2 readers + 1 auditor
//   4  3 readers + 1 auditor
//
// Concurrency is configured PER TIER (Ollama tiers cap concurrency differently):
//   reader_concurrency  max readers in flight at once (0 = all; 1 = sequential)
//   auditor_sequential  when true the auditor is serialized w.r.t. other work
// The reader->auditor barrier is unconditional (the auditor can never fire while
// a reader is still working).
#pragma once
#include <string>
#include <vector>

#include "llm.hpp"

namespace siu::audit {

enum class Mode { kSingle = 1, kOnePlusAudit = 2, kTwoPlusAudit = 3,
                  kThreePlusAudit = 4 };

// How an agent reads the report. kAllFields: one prompt covers every column
// (fast). kPerField: the agent RE-READS the whole report once per column,
// focused on that one field -- far more thorough (no skim-and-hallucinate), at
// the cost of one model call per field. Set independently for readers and for
// auditors so you know exactly how each tier is reading/reviewing.
enum class Granularity { kAllFields = 0, kPerField = 1 };

// Number of reader/reviewer models a mode uses.
int readers_for(Mode m);

struct PanelConfig {
    Mode mode = Mode::kThreePlusAudit;  // preset for the reader/auditor counts
    std::vector<std::string> reader_models;   // reader/reviewer tags (cycled)
    std::vector<std::string> auditor_models;  // auditor tags -- a review CHAIN:
                                              // each audits the previous stage
    // Counts (0 = derive from mode / from the model lists). These let you scale
    // BEYOND the mode presets -- e.g. 5 readers + 2 auditors.
    int num_readers = 0;   // 0 -> readers_for(mode)
    int num_auditors = 0;  // 0 -> auditor_models.size() (>=1 unless mode 1)
    int reader_concurrency = 0;   // 0 = all readers at once; 1 = one at a time; N = cap
    bool auditor_sequential = true;  // serialize the auditor tier (respect tier limits)
    Granularity reader_granularity = Granularity::kAllFields;   // per-tier read mode
    Granularity auditor_granularity = Granularity::kAllFields;
    bool health_check = true;     // drop empty-JSON models before the run
    llm::Backend backend;         // where the models live (llm.hpp; env defaults)
};

// --- model helpers ---------------------------------------------------------
// Default model when none is named: $MORIE_LLM_MODEL, else $OLLAMA_MODEL, else
// the first model the backend lists ("" if none).
std::string default_model(const llm::Backend& b);
// Keep only the models that answer a trivial prompt (drops dead / empty ones).
std::vector<std::string> healthy_models(const llm::Backend& b,
                                        const std::vector<std::string>& candidates);

// --- the panel ------------------------------------------------------------
// Run the MoA panel for ONE report. `parsed_json` is the parser's record (any
// number of fields -- the reader/auditor iterate over whatever is present, so
// 60+ columns work with no special-casing). Returns the FINAL record as a JSON
// string: canonical field names, clean values, no verdict words. Throws when no
// healthy reader (or, for modes >= 2, no healthy auditor) is available. With a
// backend whose chat is not thread-safe (e.g. an R callback) the readers run
// one after another on the calling thread.
std::string run_panel(const std::string& report_text,
                      const std::string& parsed_json, const PanelConfig& cfg);

}  // namespace siu::audit
