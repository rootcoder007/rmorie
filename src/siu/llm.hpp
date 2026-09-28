// SPDX-License-Identifier: AGPL-3.0-or-later
//
// llm.hpp -- bring-your-own language model for the SIU panel.
//
// Nothing is hardcoded: no host, no model, no key. A Backend points at ANY
// server that speaks either protocol below, or at a caller-supplied function:
//
//   api = "ollama"   POST {base}/api/chat, GET {base}/api/tags
//                    (Ollama, local or remote, or a tunnelled gateway)
//   api = "openai"   POST {base}/chat/completions, GET {base}/models, where
//                    base ends in /v1 (llama.cpp server, vLLM, LM Studio,
//                    LocalAI, text-generation-webui, TGI, Jan, OpenRouter,
//                    or any other OpenAI-compatible endpoint)
//   chat set         the caller's own function(model, prompt) -> reply, e.g.
//                    an R or Python function wrapping a home-made model
//
// Configuration comes from the Backend fields, else the environment:
//   MORIE_LLM_API   (ollama | openai)      default: ollama
//   MORIE_LLM_BASE  base URL               else OLLAMA_HOST / OLLAMA_BASE_URL
//                                          (ollama) or OPENAI_BASE_URL /
//                                          LLM_API_BASE_URL (openai), else
//                                          http://localhost:11434 (ollama) or
//                                          http://localhost:8080/v1 (openai);
//                                          an openai base gets /v1 appended
//                                          unless it already ends in /v1
//   MORIE_LLM_KEY   bearer token           else OLLAMA_API_KEY (ollama) or
//                                          OPENAI_API_KEY / LLM_API_KEY (openai)
#pragma once
#include <functional>
#include <string>
#include <vector>

namespace siu::llm {

using ChatFn = std::function<std::string(const std::string& model, const std::string& prompt)>;

struct Backend {
    std::string api;          // "ollama" or "openai" ("" -> $MORIE_LLM_API or ollama)
    std::string base;         // base URL ("" -> from the environment)
    std::string key;          // bearer token ("" -> from the environment; may stay empty)
    long timeout_s = 300;     // per request
    double temperature = 0.0; // deterministic by default
    ChatFn chat;              // if set, used instead of HTTP
    bool thread_safe = true;  // false when `chat` must run on the calling thread
};

// Fill empty api/base/key from the environment and normalise the base URL.
Backend resolve(Backend b);

// Model names the server offers (empty on failure or for a custom chat).
std::vector<std::string> list_models(const Backend& b);

// One user-turn chat; returns the assistant text. Throws on transport errors.
std::string chat(const Backend& b, const std::string& model, const std::string& prompt);

}  // namespace siu::llm
