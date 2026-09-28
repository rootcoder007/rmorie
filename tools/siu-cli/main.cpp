// SPDX-License-Identifier: AGPL-3.0-or-later
//
// siu -- command-line front end for the SIU corpus + audit pipeline, built from
// the same C++ core as the rmorie / morie R packages (src/siu/).
//
//   siu version
//   siu models  [backend]                 list the models your server offers
//   siu chat    [backend] --model M TEXT  one prompt to one model
//   siu fetch   <drid>                    fetch + strip a report to stdout
//   siu parse   <report.html|.txt>        native parser -> schema-field JSON
//   siu resolve <report.txt>              deterministic subject-officer count
//   siu audit   <parsed.json> <report.txt> [backend] [panel options]
//
// Backend (any model you run or choose; nothing is hardcoded):
//   --api ollama|openai   protocol (default $MORIE_LLM_API, else ollama)
//   --base URL            server  (default $MORIE_LLM_BASE / $OLLAMA_HOST /
//                         $OPENAI_BASE_URL; localhost otherwise)
//   --key TOKEN           bearer token (default $MORIE_LLM_KEY / $OLLAMA_API_KEY /
//                         $OPENAI_API_KEY)
//   --timeout S, --temperature T
#include <cstdio>
#include <fstream>
#include <regex>
#include <sstream>
#include <string>
#include <vector>

#include "siu/audit.hpp"
#include "siu/http.hpp"
#include "siu/llm.hpp"
#include "siu/nlohmann/json.hpp"
#include "siu/parse.hpp"
#include "siu/resolve.hpp"

namespace {

constexpr const char* kVersion = "siu (morie) 1.0.0";

std::string slurp(const std::string& path) {
    std::ifstream f(path);
    if (!f) throw std::runtime_error("cannot open " + path);
    std::stringstream ss;
    ss << f.rdbuf();
    return ss.str();
}

int usage() {
    std::fputs(
        "usage: siu <command> [options]\n"
        "  version\n"
        "  models  [backend]\n"
        "  chat    [backend] --model <m> <prompt>\n"
        "  fetch   <drid>\n"
        "  parse   <report.html|.txt>   native parser -> schema-field JSON\n"
        "  resolve <report.txt>\n"
        "  audit   <parsed.json> <report.txt> [backend] [options]\n"
        "backend (bring your own model; nothing hardcoded):\n"
        "     --api <ollama|openai>    Ollama /api/chat or any OpenAI-compatible /v1 server\n"
        "                              (llama.cpp, vLLM, LM Studio, LocalAI, TGI, ...)\n"
        "     --base <url>             server URL (else $MORIE_LLM_BASE, $OLLAMA_HOST, $OPENAI_BASE_URL)\n"
        "     --key <token>            bearer token (else $MORIE_LLM_KEY / provider key)\n"
        "     --timeout <s>  --temperature <t>\n"
        "audit options:\n"
        "     --mode <1..4>            1 single | 2 1+auditor | 3 2+auditor | 4 3+auditor (default 4)\n"
        "     --readers <m1,m2,...>    reader/reviewer models (cycled; default $MORIE_LLM_MODEL,\n"
        "                              $OLLAMA_MODEL, else the first model the server lists)\n"
        "     --auditors <m1,m2,...>   auditor models -- a review CHAIN (default: the readers)\n"
        "     --num-readers <N>  --num-auditors <M>\n"
        "     --reader-concurrency <N> readers in flight (0=all, 1=sequential)\n"
        "     --auditor-sequential <0|1>\n"
        "     --reader-granularity <all|per-field>  --auditor-granularity <all|per-field>\n"
        "     --no-health-check        skip the empty-reply model pre-flight\n"
        "  The reader->auditor barrier is unconditional.\n",
        stderr);
    return 2;
}

std::vector<std::string> split_csv(const std::string& s) {
    std::vector<std::string> out;
    std::stringstream ss(s);
    std::string tok;
    while (std::getline(ss, tok, ',')) {
        if (!tok.empty()) out.push_back(tok);
    }
    return out;
}

std::string flag(int argc, char** argv, int start, const std::string& name, const std::string& dflt) {
    for (int i = start; i + 1 < argc; ++i) {
        if (name == argv[i]) return argv[i + 1];
    }
    return dflt;
}

bool has(int argc, char** argv, int start, const std::string& name) {
    for (int i = start; i < argc; ++i) {
        if (name == argv[i]) return true;
    }
    return false;
}

siu::llm::Backend backend(int argc, char** argv, int start) {
    siu::llm::Backend b;
    b.api = flag(argc, argv, start, "--api", "");
    b.base = flag(argc, argv, start, "--base", "");
    b.key = flag(argc, argv, start, "--key", "");
    b.timeout_s = std::stol(flag(argc, argv, start, "--timeout", "300"));
    b.temperature = std::stod(flag(argc, argv, start, "--temperature", "0"));
    return siu::llm::resolve(b);
}

std::string strip_html(const std::string& h) {
    std::string t = siu::html_to_text(h);
    return std::regex_replace(t, std::regex(R"([ \t]+)"), " ");
}

}  // namespace

int main(int argc, char** argv) {
    if (argc < 2) return usage();
    const std::string cmd = argv[1];
    try {
        if (cmd == "version") {
            std::puts(kVersion);
            return 0;
        }
        if (cmd == "models") {
            const auto b = backend(argc, argv, 2);
            const auto models = siu::llm::list_models(b);
            std::printf("server: %s [%s] (%zu models)\n", b.base.c_str(), b.api.c_str(), models.size());
            for (const auto& m : models) std::printf("  %s\n", m.c_str());
            return models.empty() ? 1 : 0;
        }
        if (cmd == "chat" && argc >= 3) {
            const auto b = backend(argc, argv, 2);
            std::string model = flag(argc, argv, 2, "--model", "");
            if (model.empty()) model = siu::audit::default_model(b);
            if (model.empty()) throw std::runtime_error("no model: pass --model or set MORIE_LLM_MODEL");
            std::puts(siu::llm::chat(b, model, argv[argc - 1]).c_str());
            return 0;
        }
        if (cmd == "fetch" && argc == 3) {
            const std::string url =
                "https://www.siu.on.ca/en/directors_report_details.php?drid=" + std::string(argv[2]);
            std::puts(strip_html(siu::http::get(url)).c_str());
            return 0;
        }
        if (cmd == "parse" && argc == 3) {
            const std::string raw = slurp(argv[2]);
            const bool is_html = raw.find('<') != std::string::npos;
            const auto fields = is_html ? siu::parse_report_html(raw) : siu::parse_report_text(raw);
            nlohmann::json j;
            for (const auto& [k, v] : fields) j[k] = v;
            std::puts(j.dump(1).c_str());
            return 0;
        }
        if (cmd == "resolve" && argc == 3) {
            const auto r = siu::resolve_subject_officers(slurp(argv[2]));
            std::printf("subject_officers=%s  (%s)\n", r.count ? std::to_string(*r.count).c_str() : "UNRESOLVED",
                        r.reason.c_str());
            return r.count ? 0 : 1;
        }
        if (cmd == "audit" && argc >= 4) {
            siu::audit::PanelConfig cfg;
            const int mode = std::stoi(flag(argc, argv, 4, "--mode", "4"));
            if (mode < 1 || mode > 4) return usage();
            cfg.mode = static_cast<siu::audit::Mode>(mode);
            cfg.reader_models = split_csv(flag(argc, argv, 4, "--readers", ""));
            cfg.auditor_models = split_csv(flag(argc, argv, 4, "--auditors", flag(argc, argv, 4, "--auditor", "")));
            cfg.num_readers = std::stoi(flag(argc, argv, 4, "--num-readers", "0"));
            cfg.num_auditors = std::stoi(flag(argc, argv, 4, "--num-auditors", "0"));
            cfg.reader_concurrency = std::stoi(flag(argc, argv, 4, "--reader-concurrency", "0"));
            cfg.auditor_sequential = flag(argc, argv, 4, "--auditor-sequential", "1") != "0";
            using G = siu::audit::Granularity;
            cfg.reader_granularity =
                flag(argc, argv, 4, "--reader-granularity", "all") == "per-field" ? G::kPerField : G::kAllFields;
            cfg.auditor_granularity =
                flag(argc, argv, 4, "--auditor-granularity", "all") == "per-field" ? G::kPerField : G::kAllFields;
            cfg.health_check = !has(argc, argv, 4, "--no-health-check");
            cfg.backend = backend(argc, argv, 4);
            std::puts(siu::audit::run_panel(slurp(argv[3]), slurp(argv[2]), cfg).c_str());
            return 0;
        }
        return usage();
    } catch (const std::exception& e) {
        std::fprintf(stderr, "siu: %s\n", e.what());
        return 1;
    }
}
