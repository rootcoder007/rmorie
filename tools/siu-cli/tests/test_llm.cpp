// SPDX-License-Identifier: AGPL-3.0-or-later
//
// Model-free panel tests: a caller-supplied chat function stands in for ANY
// model, so the whole Mixture-of-Agents pipeline (health check, readers,
// barrier, auditor chain, value extraction) runs with no server. Also checks
// backend resolution from the environment. Non-zero exit on failure.
#include <atomic>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <string>

#include "siu/audit.hpp"
#include "siu/llm.hpp"
#include "siu/nlohmann/json.hpp"

using nlohmann::json;
namespace sa = siu::audit;

int main() {
    std::atomic<int> readers{0}, auditors{0}, health{0};
    siu::llm::Backend fake;
    fake.chat = [&](const std::string& model, const std::string& prompt) -> std::string {
        if (prompt.rfind("Reply with the word OK.", 0) == 0) {
            ++health;
            return model == "dead" ? "" : "OK";
        }
        if (prompt.find("You are the AUDITOR") != std::string::npos) {
            ++auditors;
            return "<think>checking</think>{\"police_service\": \"Barrie Police Service\"}";
        }
        ++readers;
        return "{\"police_service\": {\"value\": \"Barrie\", \"quote\": \"Barrie Police\", \"confidence\": \"high\"}}";
    };
    fake.thread_safe = false;

    sa::PanelConfig c1;
    c1.mode = sa::Mode::kSingle;
    c1.reader_models = {"dead", "m1"};
    c1.backend = fake;
    json r1 = json::parse(sa::run_panel("report", "{}", c1));
    assert(r1["police_service"] == "Barrie");  // mode 1: the reader's value
    assert(readers == 1 && auditors == 0 && health == 2);

    readers = auditors = health = 0;
    sa::PanelConfig c4;
    c4.mode = sa::Mode::kThreePlusAudit;
    c4.reader_models = {"m1", "m2"};
    c4.auditor_models = {"a1", "a2"};
    c4.backend = fake;
    json r4 = json::parse(sa::run_panel("report", "{}", c4));
    assert(r4["police_service"] == "Barrie Police Service");  // auditor chain wins
    assert(readers == 3 && auditors == 2 && health == 4);

    readers = auditors = health = 0;
    c4.health_check = false;
    c4.backend.thread_safe = true;  // concurrent readers through std::async
    c4.num_readers = 5;
    sa::run_panel("report", "{}", c4);
    assert(readers == 5 && health == 0);

    bool threw = false;
    try {
        sa::PanelConfig bad;
        bad.reader_models = {"dead"};
        bad.backend = fake;
        sa::run_panel("report", "{}", bad);
    } catch (const std::runtime_error&) {
        threw = true;
    }
    assert(threw);  // no healthy reader

    setenv("MORIE_LLM_API", "openai", 1);
    unsetenv("MORIE_LLM_BASE");
    unsetenv("OPENAI_BASE_URL");
    setenv("OPENAI_API_KEY", "sk-test", 1);
    unsetenv("MORIE_LLM_KEY");
    siu::llm::Backend e = siu::llm::resolve(siu::llm::Backend{});
    assert(e.api == "openai" && e.base == "http://localhost:8080/v1" && e.key == "sk-test");
    siu::llm::Backend o;
    o.api = "ollama";
    o.base = "gpu-box:11434/";
    setenv("OLLAMA_API_KEY", "k2", 1);
    o = siu::llm::resolve(o);
    assert(o.base == "http://gpu-box:11434" && o.key == "k2");
    siu::llm::Backend r;
    r.api = "openai";
    r.base = "https://openrouter.ai/api";
    assert(siu::llm::resolve(r).base == "https://openrouter.ai/api/v1");
    r.base = "http://localhost:1234/v1/";
    assert(siu::llm::resolve(r).base == "http://localhost:1234/v1");
    unsetenv("OPENAI_API_KEY");
    setenv("LLM_API_BASE_URL", "http://vllm:8000", 1);
    setenv("LLM_API_KEY", "k3", 1);
    siu::llm::Backend g = siu::llm::resolve(siu::llm::Backend{});
    assert(g.base == "http://vllm:8000/v1" && g.key == "k3");
    threw = false;
    try {
        siu::llm::Backend x;
        x.api = "grpc";
        siu::llm::resolve(x);
    } catch (const std::invalid_argument&) {
        threw = true;
    }
    assert(threw);
    std::puts("all llm/panel tests passed");
    return 0;
}
