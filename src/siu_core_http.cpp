// SPDX-License-Identifier: AGPL-3.0-or-later
#include "siu/http.hpp"

#include <curl/curl.h>

#include <chrono>
#include <string>
#include <stdexcept>
#include <thread>

namespace siu::http {
namespace {

// Cloud tiers cap concurrent requests and return HTTP 429 when exceeded; local
// servers can 503 while a model loads. Retry those with linear backoff.
constexpr int kMaxRetries = 6;

bool retryable(long status) { return status == 429 || status == 503; }

size_t write_cb(char* ptr, size_t size, size_t nmemb, void* userdata) {
    auto* out = static_cast<std::string*>(userdata);
    out->append(ptr, size * nmemb);
    return size * nmemb;
}

// One RAII CURL handle per call (the easy interface is not thread-safe on a
// shared handle); curl_global_init is reference-counted, so coexisting with
// other libcurl users in the same process is safe.
struct GlobalInit {
    GlobalInit() { curl_global_init(CURL_GLOBAL_DEFAULT); }
    ~GlobalInit() { curl_global_cleanup(); }
};
const GlobalInit kGlobalInit;

std::string perform(CURL* curl, const std::string& url) {
    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_cb);
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "morie-siu/1.0");
    for (int attempt = 0; attempt <= kMaxRetries; ++attempt) {
        std::string body;
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &body);
        const CURLcode rc = curl_easy_perform(curl);
        if (rc != CURLE_OK) {
            throw std::runtime_error(std::string("curl: ") + curl_easy_strerror(rc));
        }
        long status = 0;
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &status);
        if (retryable(status) && attempt < kMaxRetries) {
            std::this_thread::sleep_for(std::chrono::seconds(2 * (attempt + 1)));
            continue;
        }
        if (status >= 400) {
            throw std::runtime_error("http status " + std::to_string(status) + " for " + url);
        }
        return body;
    }
    throw std::runtime_error("http retries exhausted for " + url);
}

struct Handle {
    CURL* curl = curl_easy_init();
    curl_slist* headers = nullptr;
    ~Handle() {
        if (headers) curl_slist_free_all(headers);
        if (curl) curl_easy_cleanup(curl);
    }
    void bearer(const std::string& token) {
        if (!token.empty()) {
            const std::string h = "Authorization: Bearer " + token;
            headers = curl_slist_append(headers, h.c_str());
        }
    }
};

}  // namespace

std::string get(const std::string& url, long timeout_s, const std::string& bearer) {
    Handle h;
    if (!h.curl) throw std::runtime_error("curl_easy_init failed");
    h.bearer(bearer);
    if (h.headers) curl_easy_setopt(h.curl, CURLOPT_HTTPHEADER, h.headers);
    curl_easy_setopt(h.curl, CURLOPT_TIMEOUT, timeout_s);
    curl_easy_setopt(h.curl, CURLOPT_CONNECTTIMEOUT, timeout_s);
    return perform(h.curl, url);
}

std::string post_json(const std::string& url, const std::string& json_body, const std::string& bearer,
                      long timeout_s) {
    Handle h;
    if (!h.curl) throw std::runtime_error("curl_easy_init failed");
    h.headers = curl_slist_append(h.headers, "Content-Type: application/json");
    h.bearer(bearer);
    curl_easy_setopt(h.curl, CURLOPT_HTTPHEADER, h.headers);
    curl_easy_setopt(h.curl, CURLOPT_POSTFIELDS, json_body.c_str());
    curl_easy_setopt(h.curl, CURLOPT_POSTFIELDSIZE, static_cast<long>(json_body.size()));
    curl_easy_setopt(h.curl, CURLOPT_TIMEOUT, timeout_s);
    return perform(h.curl, url);
}

}  // namespace siu::http
