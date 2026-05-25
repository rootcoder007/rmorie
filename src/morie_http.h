// SPDX-License-Identifier: AGPL-3.0-or-later
//
// morie_http.h -- shared C++ HTTP(S) primitives via libcurl.
//
// Promoted in phase 3VV out of src/siu_parser.cpp (which originally
// inlined its own libcurl handle). This header is the single source
// of truth for synchronous HTTP fetches across morie's C++ backend:
//
//   * siu_parser.cpp (Ontario SIU corpus fetcher)
//   * R/datasets.R   (Socrata / CKAN / ArcGIS / OData loaders via
//                       the .morie_http_get Rcpp export)
//
// The Rcpp surface lives in morie_http.cpp; consumers should call
// morie::http::get(...) from C++ and `.morie_http_get(...)` from R.
//
// Build: requires libcurl. Linker flag -lcurl is set in src/Makevars
// (with cflags+libs filled in by ./configure via curl-config so the
// committed Makefile stays portable).

#ifndef MORIE_HTTP_H
#define MORIE_HTTP_H

#include <cstdint>
#include <string>
#include <vector>

namespace morie {
namespace http {

// Default User-Agent used when the caller doesn't supply one.
// Intentionally identifiable so server-side analytics can tell
// morie-issued requests from generic curl traffic.
extern const char* kDefaultUserAgent;

// Synchronous HTTP(S) GET. Returns the response body as a UTF-8
// string. On any transport-level failure, returns an empty string
// (parity with siu_parser's original siu_http_get behaviour).
//
//   url         -- fully-formed URL (caller is responsible for
//                  query-string assembly + URL encoding).
//   timeout_s   -- total request timeout in seconds. Default 60.
//   headers     -- vector of "Key: Value" header strings to send
//                  (e.g. {"X-App-Token: abc", "Accept: application/json"}).
//                  Empty vector = no extra headers.
//   user_agent  -- override User-Agent string. Empty = use the
//                  morie default.
//   follow_redirects -- if true (default), libcurl follows 3xx
//                  redirects. Set false to inspect them yourself.
std::string get(const std::string& url,
                int timeout_s = 60,
                const std::vector<std::string>& headers = {},
                const std::string& user_agent = std::string(),
                bool follow_redirects = true);

// Binary-safe sibling of get() for raw byte payloads -- shapefiles,
// FGDB zips, PDFs, KMZ, anything the std::string interface would
// truncate at the first embedded NUL. Same contract: returns the
// response body, empty vector on transport failure.
std::vector<uint8_t> get_bytes(const std::string& url,
                                int timeout_s = 60,
                                const std::vector<std::string>& headers = {},
                                const std::string& user_agent = std::string(),
                                bool follow_redirects = true);

// Synchronous HTTP(S) POST. Body is sent verbatim (caller is
// responsible for serialisation, e.g. jsonlite::toJSON for JSON
// bodies). content_type defaults to "application/json"; pass ""
// to skip the Content-Type header (libcurl will use whatever the
// caller put in `headers`). Returns the response body as a
// std::string; empty string on transport failure.
std::string post(const std::string& url,
                  const std::string& body,
                  const std::string& content_type = "application/json",
                  int timeout_s = 60,
                  const std::vector<std::string>& headers = {},
                  const std::string& user_agent = std::string(),
                  bool follow_redirects = true);

// Status-aware variants of get() + post() for callers that need
// to inspect the HTTP status code (401/403/4xx handling).
// status_code is the numeric HTTP status (200, 401, 404, etc.);
// body is the response body (still populated on 4xx so the caller
// can extract error details). status_code is 0 if libcurl itself
// failed before getting a response.
struct Response {
  std::string body;
  long status_code;
};

Response get_with_status(const std::string& url,
                          int timeout_s = 60,
                          const std::vector<std::string>& headers = {},
                          const std::string& user_agent = std::string(),
                          bool follow_redirects = true);

Response post_with_status(const std::string& url,
                           const std::string& body,
                           const std::string& content_type = "application/json",
                           int timeout_s = 60,
                           const std::vector<std::string>& headers = {},
                           const std::string& user_agent = std::string(),
                           bool follow_redirects = true);

// libcurl version string morie was built against. For diagnostics
// + version-skew investigations.
std::string curl_version();

}  // namespace http
}  // namespace morie

#endif  // MORIE_HTTP_H
