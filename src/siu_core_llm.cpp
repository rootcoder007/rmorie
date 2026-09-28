// SPDX-License-Identifier: AGPL-3.0-or-later
#include "siu/llm.hpp"

#include <cstdlib>
#include <initializer_list>
#include <stdexcept>

#include "siu/http.hpp"
#include "siu/nlohmann/json.hpp"

using nlohmann::json;

namespace siu::llm {
namespace {

std::string env(const char* name) {
    const char* v = std::getenv(name);
    return v ? std::string(v) : std::string();
}

std::string first_env(std::initializer_list<const char*> names) {
    for (const char* n : names) {
        std::string v = env(n);
        if (!v.empty()) return v;
    }
    return "";
}

}  // namespace

Backend resolve(Backend b) {
    if (b.api.empty()) b.api = env("MORIE_LLM_API");
    if (b.api.empty()) b.api = "ollama";
    if (b.api != "ollama" && b.api != "openai") {
        throw std::invalid_argument("api must be \"ollama\" or \"openai\"");
    }
    if (b.base.empty()) b.base = env("MORIE_LLM_BASE");
    if (b.base.empty()) {
        b.base = b.api == "ollama" ? first_env({"OLLAMA_HOST", "OLLAMA_BASE_URL"})
                                   : first_env({"OPENAI_BASE_URL", "LLM_API_BASE_URL"});
    }
    if (b.base.empty()) {
        b.base = b.api == "ollama" ? "http://localhost:11434" : "http://localhost:8080/v1";
    }
    if (b.base.rfind("http", 0) != 0) b.base = "http://" + b.base;
    while (!b.base.empty() && b.base.back() == '/') b.base.pop_back();
    // OpenAI-style servers are addressed at .../v1 (https://api.openai.com,
    // http://host:8080, https://openrouter.ai/api all become .../v1).
    if (b.api == "openai" && (b.base.size() < 3 || b.base.compare(b.base.size() - 3, 3, "/v1") != 0)) {
        b.base += "/v1";
    }
    if (b.key.empty()) {
        b.key = b.api == "ollama" ? first_env({"MORIE_LLM_KEY", "OLLAMA_API_KEY"})
                                  : first_env({"MORIE_LLM_KEY", "OPENAI_API_KEY", "LLM_API_KEY"});
    }
    return b;
}

std::vector<std::string> list_models(const Backend& b0) {
    std::vector<std::string> out;
    if (b0.chat) return out;
    const Backend b = resolve(b0);
    try {
        if (b.api == "ollama") {
            const json body = json::parse(http::get(b.base + "/api/tags", 10, b.key));
            for (const auto& m : body.value("models", json::array())) {
                if (m.contains("name")) out.push_back(m["name"].get<std::string>());
                else if (m.contains("model")) out.push_back(m["model"].get<std::string>());
            }
        } else {
            const json body = json::parse(http::get(b.base + "/models", 10, b.key));
            for (const auto& m : body.value("data", json::array())) {
                if (m.contains("id")) out.push_back(m["id"].get<std::string>());
            }
        }
    } catch (...) {
    }
    return out;
}

std::string chat(const Backend& b0, const std::string& model, const std::string& prompt) {
    if (b0.chat) return b0.chat(model, prompt);
    const Backend b = resolve(b0);
    const json messages = json::array({{{"role", "user"}, {"content", prompt}}});
    if (b.api == "ollama") {
        const json req = {{"model", model},
                          {"stream", false},
                          {"options", {{"temperature", b.temperature}}},
                          {"messages", messages}};
        const json body = json::parse(
            http::post_json(b.base + "/api/chat", req.dump(), b.key, b.timeout_s));
        return body.value("message", json::object()).value("content", std::string());
    }
    const json req = {{"model", model},
                      {"stream", false},
                      {"temperature", b.temperature},
                      {"messages", messages}};
    const json body = json::parse(
        http::post_json(b.base + "/chat/completions", req.dump(), b.key, b.timeout_s));
    const json& choices = body.contains("choices") ? body["choices"] : json::array();
    if (!choices.is_array() || choices.empty()) return "";
    const json& msg = choices[0].contains("message") ? choices[0]["message"] : json::object();
    return msg.contains("content") && msg["content"].is_string() ? msg["content"].get<std::string>()
                                                                   : std::string();
}

}  // namespace siu::llm
