// SPDX-License-Identifier: AGPL-3.0-or-later
//
// morie_http.cpp -- shared C++ HTTP(S) primitives via libcurl.
// See morie_http.h for the public surface.
//
// 3VV: promoted out of siu_parser.cpp so dataset loaders (Socrata
// SODA2/SODA3, OData, CKAN, ArcGIS Hub, raw CSV) can share the same
// libcurl-backed transport instead of going through R-side httr2.

#include "morie_http.h"

#include <Rcpp.h>
#include <curl/curl.h>

#include <cstddef>
#include <string>
#include <vector>

namespace morie {
namespace http {

const char* kDefaultUserAgent =
  "morie-R/0.9.5.5 (+https://github.com/hadesllm/morie) "
  "libcurl";

namespace {

// One-time libcurl global initialisation. Multiple translation
// units (this file + siu_parser.cpp) each declare their own
// instance; libcurl's global init/cleanup are reference-counted so
// the double-init is benign.
struct CurlGlobal {
  CurlGlobal()  { curl_global_init(CURL_GLOBAL_DEFAULT); }
  ~CurlGlobal() { curl_global_cleanup(); }
};
const CurlGlobal kCurlGlobal;

// libcurl write callback: append received bytes to a std::string
// passed via userdata.
size_t write_cb(char* ptr, size_t size, size_t nmemb, void* userdata) {
  std::string* buf = static_cast<std::string*>(userdata);
  const size_t n = size * nmemb;
  buf->append(ptr, n);
  return n;
}

// Binary-safe write callback: append received bytes to a
// std::vector<uint8_t> passed via userdata. No NUL truncation.
size_t write_cb_bytes(char* ptr, size_t size, size_t nmemb, void* userdata) {
  std::vector<uint8_t>* buf = static_cast<std::vector<uint8_t>*>(userdata);
  const size_t n = size * nmemb;
  const uint8_t* p = reinterpret_cast<const uint8_t*>(ptr);
  buf->insert(buf->end(), p, p + n);
  return n;
}

// Shared libcurl handle configuration applied to both text and
// byte GETs. Keeps the curl_easy_setopt block in one place.
void configure_handle(CURL* h,
                       const std::string& url,
                       int timeout_s,
                       const std::vector<std::string>& headers,
                       const std::string& user_agent,
                       bool follow_redirects,
                       struct curl_slist** out_hdrs) {
  curl_easy_setopt(h, CURLOPT_URL, url.c_str());
  curl_easy_setopt(h, CURLOPT_FOLLOWLOCATION,
                    follow_redirects ? 1L : 0L);
  curl_easy_setopt(h, CURLOPT_TIMEOUT,
                    static_cast<long>(timeout_s));
  curl_easy_setopt(h, CURLOPT_CONNECTTIMEOUT, 30L);
  curl_easy_setopt(h, CURLOPT_ACCEPT_ENCODING, "");  // all supported
  curl_easy_setopt(h, CURLOPT_NOSIGNAL, 1L);
  const char* ua = !user_agent.empty()
                    ? user_agent.c_str()
                    : kDefaultUserAgent;
  curl_easy_setopt(h, CURLOPT_USERAGENT, ua);
  struct curl_slist* hdrs = nullptr;
  for (const auto& hh : headers) {
    if (hh.empty()) continue;
    hdrs = curl_slist_append(hdrs, hh.c_str());
  }
  if (hdrs != nullptr) {
    curl_easy_setopt(h, CURLOPT_HTTPHEADER, hdrs);
  }
  *out_hdrs = hdrs;
}

}  // namespace

std::string get(const std::string& url,
                int timeout_s,
                const std::vector<std::string>& headers,
                const std::string& user_agent,
                bool follow_redirects) {
  std::string buf;
  CURL* h = curl_easy_init();
  if (h == nullptr) return buf;

  struct curl_slist* hdrs = nullptr;
  configure_handle(h, url, timeout_s, headers, user_agent,
                    follow_redirects, &hdrs);
  curl_easy_setopt(h, CURLOPT_WRITEFUNCTION, write_cb);
  curl_easy_setopt(h, CURLOPT_WRITEDATA, &buf);

  const CURLcode rc = curl_easy_perform(h);
  curl_easy_cleanup(h);
  if (hdrs != nullptr) curl_slist_free_all(hdrs);

  if (rc != CURLE_OK) {
    // Mirror siu_http_get's "empty string on failure" contract.
    return std::string();
  }
  return buf;
}

std::vector<uint8_t> get_bytes(const std::string& url,
                                int timeout_s,
                                const std::vector<std::string>& headers,
                                const std::string& user_agent,
                                bool follow_redirects) {
  std::vector<uint8_t> buf;
  CURL* h = curl_easy_init();
  if (h == nullptr) return buf;

  struct curl_slist* hdrs = nullptr;
  configure_handle(h, url, timeout_s, headers, user_agent,
                    follow_redirects, &hdrs);
  curl_easy_setopt(h, CURLOPT_WRITEFUNCTION, write_cb_bytes);
  curl_easy_setopt(h, CURLOPT_WRITEDATA, &buf);

  const CURLcode rc = curl_easy_perform(h);
  curl_easy_cleanup(h);
  if (hdrs != nullptr) curl_slist_free_all(hdrs);

  if (rc != CURLE_OK) {
    buf.clear();
  }
  return buf;
}

std::string post(const std::string& url,
                  const std::string& body,
                  const std::string& content_type,
                  int timeout_s,
                  const std::vector<std::string>& headers,
                  const std::string& user_agent,
                  bool follow_redirects) {
  std::string buf;
  CURL* h = curl_easy_init();
  if (h == nullptr) return buf;

  // Build the effective headers: caller's list + Content-Type if
  // not already there.
  std::vector<std::string> all_headers = headers;
  if (!content_type.empty()) {
    all_headers.emplace_back("Content-Type: " + content_type);
  }

  struct curl_slist* hdrs = nullptr;
  configure_handle(h, url, timeout_s, all_headers, user_agent,
                    follow_redirects, &hdrs);
  curl_easy_setopt(h, CURLOPT_WRITEFUNCTION, write_cb);
  curl_easy_setopt(h, CURLOPT_WRITEDATA, &buf);
  curl_easy_setopt(h, CURLOPT_POST, 1L);
  curl_easy_setopt(h, CURLOPT_POSTFIELDS, body.c_str());
  curl_easy_setopt(h, CURLOPT_POSTFIELDSIZE_LARGE,
                    static_cast<curl_off_t>(body.size()));

  const CURLcode rc = curl_easy_perform(h);
  curl_easy_cleanup(h);
  if (hdrs != nullptr) curl_slist_free_all(hdrs);

  if (rc != CURLE_OK) return std::string();
  return buf;
}

Response get_with_status(const std::string& url,
                          int timeout_s,
                          const std::vector<std::string>& headers,
                          const std::string& user_agent,
                          bool follow_redirects) {
  Response out{std::string(), 0L};
  CURL* h = curl_easy_init();
  if (h == nullptr) return out;

  struct curl_slist* hdrs = nullptr;
  configure_handle(h, url, timeout_s, headers, user_agent,
                    follow_redirects, &hdrs);
  curl_easy_setopt(h, CURLOPT_WRITEFUNCTION, write_cb);
  curl_easy_setopt(h, CURLOPT_WRITEDATA, &out.body);

  const CURLcode rc = curl_easy_perform(h);
  if (rc == CURLE_OK) {
    curl_easy_getinfo(h, CURLINFO_RESPONSE_CODE, &out.status_code);
  } else {
    out.body.clear();
    out.status_code = 0L;
  }
  curl_easy_cleanup(h);
  if (hdrs != nullptr) curl_slist_free_all(hdrs);
  return out;
}

Response post_with_status(const std::string& url,
                           const std::string& body,
                           const std::string& content_type,
                           int timeout_s,
                           const std::vector<std::string>& headers,
                           const std::string& user_agent,
                           bool follow_redirects) {
  Response out{std::string(), 0L};
  CURL* h = curl_easy_init();
  if (h == nullptr) return out;

  std::vector<std::string> all_headers = headers;
  if (!content_type.empty()) {
    all_headers.emplace_back("Content-Type: " + content_type);
  }
  struct curl_slist* hdrs = nullptr;
  configure_handle(h, url, timeout_s, all_headers, user_agent,
                    follow_redirects, &hdrs);
  curl_easy_setopt(h, CURLOPT_WRITEFUNCTION, write_cb);
  curl_easy_setopt(h, CURLOPT_WRITEDATA, &out.body);
  curl_easy_setopt(h, CURLOPT_POST, 1L);
  curl_easy_setopt(h, CURLOPT_POSTFIELDS, body.c_str());
  curl_easy_setopt(h, CURLOPT_POSTFIELDSIZE_LARGE,
                    static_cast<curl_off_t>(body.size()));

  const CURLcode rc = curl_easy_perform(h);
  if (rc == CURLE_OK) {
    curl_easy_getinfo(h, CURLINFO_RESPONSE_CODE, &out.status_code);
  } else {
    out.body.clear();
    out.status_code = 0L;
  }
  curl_easy_cleanup(h);
  if (hdrs != nullptr) curl_slist_free_all(hdrs);
  return out;
}

std::string curl_version() {
  return std::string(curl_version_info(CURLVERSION_NOW)->version);
}

}  // namespace http
}  // namespace morie

// ---------------------------------------------------------------------------
// Rcpp surface -- exposed to R as .morie_http_get and .morie_http_curl_version
// ---------------------------------------------------------------------------

//' Synchronous HTTP(S) GET via the shared libcurl backend (C++).
//'
//' Phase-3VV promoted helper. Returns the response body as a length-1
//' character vector. On any libcurl-level failure returns the empty
//' string (parity with the SIU parser's transport contract).
//'
//' @param url Fully-formed URL.
//' @param timeout_s Total request timeout in seconds.
//' @param headers Character vector of `"Key: Value"` HTTP headers.
//' @param user_agent Optional User-Agent override.
//' @param follow_redirects Logical; default `TRUE`.
//' @return Length-1 character vector with the response body.
//' @keywords internal
// [[Rcpp::export(.morie_http_get)]]
std::string morie_http_get_(std::string url,
                            int timeout_s = 60,
                            Rcpp::CharacterVector headers = Rcpp::CharacterVector::create(),
                            std::string user_agent = "",
                            bool follow_redirects = true) {
  std::vector<std::string> hdrs;
  hdrs.reserve(headers.size());
  for (int i = 0; i < headers.size(); ++i) {
    hdrs.emplace_back(Rcpp::as<std::string>(headers[i]));
  }
  return morie::http::get(url, timeout_s, hdrs,
                           user_agent, follow_redirects);
}

//' Binary-safe HTTP(S) GET via libcurl.
//'
//' Phase-3XX get_bytes wrapper. Returns the response body as an R
//' raw vector (no NUL truncation), suitable for shapefiles, FGDB
//' zips, PDFs, KMZ, and any other binary payload the std::string
//' get() interface would corrupt.
//'
//' @inheritParams .morie_http_get
//' @return Raw vector with the response body. Empty raw vector on
//'   any libcurl-level transport failure (parity with .morie_http_get's
//'   empty-string-on-failure contract).
//' @keywords internal
// [[Rcpp::export(.morie_http_get_bytes)]]
Rcpp::RawVector morie_http_get_bytes_(std::string url,
                                       int timeout_s = 60,
                                       Rcpp::CharacterVector headers = Rcpp::CharacterVector::create(),
                                       std::string user_agent = "",
                                       bool follow_redirects = true) {
  std::vector<std::string> hdrs;
  hdrs.reserve(headers.size());
  for (int i = 0; i < headers.size(); ++i) {
    hdrs.emplace_back(Rcpp::as<std::string>(headers[i]));
  }
  std::vector<uint8_t> bytes = morie::http::get_bytes(
    url, timeout_s, hdrs, user_agent, follow_redirects);
  Rcpp::RawVector out(bytes.size());
  if (!bytes.empty()) {
    std::copy(bytes.begin(), bytes.end(), out.begin());
  }
  return out;
}

//' Synchronous HTTP(S) POST via the shared libcurl backend (C++).
//'
//' Phase-3YY helper. Body is sent verbatim; for JSON payloads call
//' `jsonlite::toJSON(...)` before passing in. Default content_type
//' is "application/json"; pass "" to skip the Content-Type header
//' (caller can set it explicitly via `headers`).
//'
//' @inheritParams .morie_http_get
//' @param body Length-1 character vector containing the request body.
//' @param content_type Content-Type header value (default
//'   `"application/json"`). Empty string skips the header.
//' @return Length-1 character vector with the response body. Empty
//'   string on libcurl-level transport failure.
//' @keywords internal
// [[Rcpp::export(.morie_http_post)]]
std::string morie_http_post_(std::string url,
                             std::string body,
                             std::string content_type = "application/json",
                             int timeout_s = 60,
                             Rcpp::CharacterVector headers = Rcpp::CharacterVector::create(),
                             std::string user_agent = "",
                             bool follow_redirects = true) {
  std::vector<std::string> hdrs;
  hdrs.reserve(headers.size());
  for (int i = 0; i < headers.size(); ++i) {
    hdrs.emplace_back(Rcpp::as<std::string>(headers[i]));
  }
  return morie::http::post(url, body, content_type, timeout_s,
                            hdrs, user_agent, follow_redirects);
}

//' Status-aware HTTP(S) GET via the libcurl backend (C++).
//'
//' Phase-3ZZ helper for callers that need HTTP status-code
//' inspection (401/403/4xx error handling). Returns a length-2
//' list: `body` (character) + `status_code` (integer, 0 on
//' transport failure).
//'
//' @inheritParams .morie_http_get
//' @return Named list with elements `body` (length-1 character
//'   vector with the response body, possibly empty on 4xx) and
//'   `status_code` (length-1 integer HTTP status, 0 on libcurl-
//'   level transport failure).
//' @keywords internal
// [[Rcpp::export(.morie_http_get_with_status)]]
Rcpp::List morie_http_get_with_status_(std::string url,
                                        int timeout_s = 60,
                                        Rcpp::CharacterVector headers = Rcpp::CharacterVector::create(),
                                        std::string user_agent = "",
                                        bool follow_redirects = true) {
  std::vector<std::string> hdrs;
  hdrs.reserve(headers.size());
  for (int i = 0; i < headers.size(); ++i) {
    hdrs.emplace_back(Rcpp::as<std::string>(headers[i]));
  }
  morie::http::Response r = morie::http::get_with_status(
    url, timeout_s, hdrs, user_agent, follow_redirects);
  return Rcpp::List::create(
    Rcpp::Named("body")        = r.body,
    Rcpp::Named("status_code") = static_cast<int>(r.status_code));
}

//' Status-aware HTTP(S) POST via the libcurl backend (C++).
//'
//' Phase-3ZZ helper. Same status-code-return contract as
//' .morie_http_get_with_status, but for POST bodies.
//'
//' @inheritParams .morie_http_post
//' @return Named list with elements `body` + `status_code`.
//' @keywords internal
// [[Rcpp::export(.morie_http_post_with_status)]]
Rcpp::List morie_http_post_with_status_(std::string url,
                                         std::string body,
                                         std::string content_type = "application/json",
                                         int timeout_s = 60,
                                         Rcpp::CharacterVector headers = Rcpp::CharacterVector::create(),
                                         std::string user_agent = "",
                                         bool follow_redirects = true) {
  std::vector<std::string> hdrs;
  hdrs.reserve(headers.size());
  for (int i = 0; i < headers.size(); ++i) {
    hdrs.emplace_back(Rcpp::as<std::string>(headers[i]));
  }
  morie::http::Response r = morie::http::post_with_status(
    url, body, content_type, timeout_s, hdrs, user_agent, follow_redirects);
  return Rcpp::List::create(
    Rcpp::Named("body")        = r.body,
    Rcpp::Named("status_code") = static_cast<int>(r.status_code));
}

//' libcurl version string the morie C++ backend was built against.
//' @return Length-1 character vector.
//' @keywords internal
// [[Rcpp::export(.morie_http_curl_version)]]
std::string morie_http_curl_version_() {
  return morie::http::curl_version();
}
