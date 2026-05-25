// SPDX-License-Identifier: AGPL-3.0-or-later
//
// siu_parser.cpp -- C/C++ parser for the Ontario Special Investigations
// Unit (SIU) corpus: director's reports and news releases.
//
// HTTP(S) transport is libcurl (linked via src/Makevars). This file is
// the foundation; the concurrent fetcher and the 64-field HTML parser
// are layered on top in later commits.

#include <Rcpp.h>
#include <curl/curl.h>
#include <algorithm>
#include <chrono>
#include <cstring>
#include <map>
#include <regex>
#include <string>
#include <vector>

// parser_version stamped into every emitted row.
#define MORIE_SIU_PARSER_VERSION "0.9.5.12"

namespace {

// One-time libcurl global initialisation (libcurl requires this before
// any handle is created when the program is multi-threaded).
struct CurlGlobal {
  CurlGlobal()  { curl_global_init(CURL_GLOBAL_DEFAULT); }
  // # nocov start
  // -- the destructor runs only at process teardown (static-storage
  // -- object); gcov does not reliably attribute exit-time execution.
  ~CurlGlobal() { curl_global_cleanup(); }
  // # nocov end
};
const CurlGlobal kCurlGlobal;

const char* kUserAgent =
  "morie/0.9.5 (+https://github.com/hadesllm/morie)";

// libcurl write callback: append received bytes to a std::string.
size_t write_cb(char* ptr, size_t size, size_t nmemb, void* userdata) {
  std::string* buf = static_cast<std::string*>(userdata);
  const size_t n = size * nmemb;
  buf->append(ptr, n);
  return n;
}

}  // namespace

//' Fetch a single URL over HTTP(S) via libcurl
//'
//' Internal building block of the SIU parser. Returns the response
//' body, or an empty string on any transport-level failure.
//'
//' @param url URL to fetch.
//' @param timeout_s Request timeout in seconds.
//' @return The response body as a length-1 character vector.
//' @keywords internal
// [[Rcpp::export(.siu_http_get)]]
std::string siu_http_get(std::string url, int timeout_s = 60) {
  CURL* h = curl_easy_init();
  if (h == nullptr) return std::string();
  std::string buf;
  curl_easy_setopt(h, CURLOPT_URL, url.c_str());
  curl_easy_setopt(h, CURLOPT_WRITEFUNCTION, write_cb);
  curl_easy_setopt(h, CURLOPT_WRITEDATA, &buf);
  curl_easy_setopt(h, CURLOPT_FOLLOWLOCATION, 1L);
  curl_easy_setopt(h, CURLOPT_TIMEOUT, static_cast<long>(timeout_s));
  curl_easy_setopt(h, CURLOPT_CONNECTTIMEOUT, 30L);
  curl_easy_setopt(h, CURLOPT_USERAGENT, kUserAgent);
  curl_easy_setopt(h, CURLOPT_ACCEPT_ENCODING, "");  // all supported
  curl_easy_setopt(h, CURLOPT_NOSIGNAL, 1L);
  const CURLcode rc = curl_easy_perform(h);
  curl_easy_cleanup(h);
  if (rc != CURLE_OK) return std::string();
  return buf;
}

//' libcurl version string morie was built against
//' @return A length-1 character vector.
//' @keywords internal
// [[Rcpp::export(.siu_curl_version)]]
std::string siu_curl_version() {
  return std::string(curl_version());
}

namespace {

// One in-flight request. The body buffer accumulates over retries
// (cleared on each new attempt). `attempts` counts how many times the
// URL has been dispatched (1 on first send; retries bump it).
struct Req {
  int idx;
  std::string url;
  std::string body;
  long http_code;     // last observed HTTP status (0 on transport failure)
  int attempts;
  long earliest_ns;   // wall-clock ns before which the next attempt may start
};

// Configure a fresh easy handle for one SIU page fetch.
void setup_handle(CURL* e, const char* url, Req* r, long timeout_s) {
  curl_easy_setopt(e, CURLOPT_URL, url);
  curl_easy_setopt(e, CURLOPT_WRITEFUNCTION, write_cb);
  curl_easy_setopt(e, CURLOPT_WRITEDATA, &r->body);
  curl_easy_setopt(e, CURLOPT_PRIVATE, r);
  curl_easy_setopt(e, CURLOPT_FOLLOWLOCATION, 1L);
  curl_easy_setopt(e, CURLOPT_TIMEOUT, timeout_s);
  curl_easy_setopt(e, CURLOPT_CONNECTTIMEOUT, 30L);
  curl_easy_setopt(e, CURLOPT_USERAGENT, kUserAgent);
  curl_easy_setopt(e, CURLOPT_ACCEPT_ENCODING, "");
  curl_easy_setopt(e, CURLOPT_NOSIGNAL, 1L);
}

// Current monotonic clock in nanoseconds.
long now_ns() {
  using namespace std::chrono;
  return duration_cast<nanoseconds>(
    steady_clock::now().time_since_epoch()).count();
}

// Core throttled multi-fetch. Returns the per-URL results in `reqs`
// (parallel to `urls`). All work is bounded by `rate_rps` request
// starts per second across the whole pool. Retries 429/503 up to
// `max_retries` with exponential backoff (250ms * 2^attempt).
void fetch_many_throttled(const std::vector<std::string>& urls,
                          int concurrency, long timeout_s,
                          double rate_rps, int max_retries,
                          std::vector<Req*>* reqs_out) {
  const int n = static_cast<int>(urls.size());
  std::vector<Req*>& reqs = *reqs_out;
  reqs.assign(n, nullptr);
  for (int i = 0; i < n; ++i) {
    reqs[i] = new Req{i, urls[i], std::string(), 0L, 0, 0L};
  }
  if (n == 0) return;
  if (concurrency < 1) concurrency = 1;
  if (concurrency > n) concurrency = n;
  if (rate_rps <= 0.0) rate_rps = 1e9;  // effectively unthrottled
  if (rate_rps > 1000.0) rate_rps = 1000.0;
  if (max_retries < 0) max_retries = 0;

  // Minimum gap between request starts (in ns).
  const long gap_ns = static_cast<long>(1.0e9 / rate_rps);
  long next_allowed = now_ns();

  CURLM* multi = curl_multi_init();

  // Queue of req indices ready to dispatch (FIFO for first attempt,
  // then 429/503 retries get appended back at the tail with an
  // earliest_ns gate). When the queue is empty and nothing is in
  // flight, we're done.
  std::vector<int> queue;
  queue.reserve(n);
  for (int i = 0; i < n; ++i) queue.push_back(i);

  std::map<CURL*, Req*> in_flight;

  auto dispatch_one = [&](int idx) {
    Req* r = reqs[idx];
    r->body.clear();
    r->attempts += 1;
    CURL* e = curl_easy_init();
    setup_handle(e, r->url.c_str(), r, timeout_s);
    curl_multi_add_handle(multi, e);
    in_flight[e] = r;
  };

  while (!queue.empty() || !in_flight.empty()) {
    // Top up the in-flight set, respecting both concurrency and rate.
    while (!queue.empty() && static_cast<int>(in_flight.size()) < concurrency) {
      const int idx = queue.front();
      Req* r = reqs[idx];
      const long t = now_ns();
      // Per-request backoff gate (set when this req was a 429/503).
      if (r->earliest_ns > t) break;
      // Global rate-limit gate.
      if (next_allowed > t) break;
      queue.erase(queue.begin());
      dispatch_one(idx);
      next_allowed = std::max(t, next_allowed) + gap_ns;
    }

    int still_running = 0;
    curl_multi_perform(multi, &still_running);
    int numfds = 0;
    // Poll up to 250ms so the rate-limit gate has a chance to open.
    curl_multi_poll(multi, nullptr, 0, 250, &numfds);

    CURLMsg* msg = nullptr;
    int msgs_left = 0;
    while ((msg = curl_multi_info_read(multi, &msgs_left)) != nullptr) {
      if (msg->msg != CURLMSG_DONE) continue;
      CURL* e = msg->easy_handle;
      Req* r = in_flight[e];
      long code = 0;
      curl_easy_getinfo(e, CURLINFO_RESPONSE_CODE, &code);
      r->http_code = code;
      const CURLcode rc = msg->data.result;
      curl_multi_remove_handle(multi, e);
      curl_easy_cleanup(e);
      in_flight.erase(e);

      const bool retryable = (rc != CURLE_OK) ||
        code == 429 || code == 502 || code == 503 || code == 504;
      if (retryable && r->attempts <= max_retries) {
        // Exponential backoff: 250ms, 500ms, 1000ms, 2000ms, ...
        const long backoff_ns =
          250L * 1000000L * (1L << std::min(r->attempts - 1, 6));
        r->earliest_ns = now_ns() + backoff_ns;
        queue.push_back(r->idx);  // re-queue at tail
      }
      // Otherwise: this slot is final. r->body and r->http_code keep
      // whatever the last attempt produced.
    }
    Rcpp::checkUserInterrupt();
  }

  curl_multi_cleanup(multi);
}

}  // namespace

//' Fetch many URLs concurrently via libcurl, with rate-limiting + retry
//'
//' Drives up to \code{concurrency} simultaneous transfers, but with a
//' global token-bucket limit of \code{rate_rps} request starts per
//' second across the whole pool. HTTP 429/502/503/504 and transport
//' errors are retried up to \code{max_retries} times with exponential
//' backoff (250ms * 2^attempt). Final failures yield an empty string
//' at their slot.
//'
//' Throttling is the safe default for SIU and similar small-gov
//' endpoints: hammering them with 16-24 concurrent requests triggers
//' WAF/Cloudflare-style bot-protection that returns short
//' interstitial pages, which look like data but aren't.
//'
//' @param urls Character vector of URLs.
//' @param concurrency Maximum simultaneous transfers.
//' @param timeout_s Per-request timeout in seconds.
//' @param rate_rps Maximum request starts per second across the pool.
//'   Default \code{4.0} is a polite scrape rate that stays well under
//'   any common WAF threshold. Set very large (e.g. \code{1e9}) to
//'   effectively disable throttling.
//' @param max_retries Maximum retry attempts per URL on 429/5xx /
//'   transport failure.
//' @return A character vector of response bodies, parallel to \code{urls}.
//' @keywords internal
// [[Rcpp::export(.siu_http_get_many)]]
Rcpp::CharacterVector siu_http_get_many(Rcpp::CharacterVector urls,
                                        int concurrency = 4,
                                        int timeout_s = 60,
                                        double rate_rps = 4.0,
                                        int max_retries = 3) {
  const int n = urls.size();
  Rcpp::CharacterVector out(n);
  for (int i = 0; i < n; ++i) out[i] = "";
  if (n == 0) return out;

  std::vector<std::string> u(n);
  for (int i = 0; i < n; ++i) u[i] = std::string(urls[i]);

  std::vector<Req*> reqs;
  fetch_many_throttled(u, concurrency, static_cast<long>(timeout_s),
                       rate_rps, max_retries, &reqs);

  for (Req* r : reqs) {
    out[r->idx] = r->body;
    delete r;
  }
  return out;
}

//' Fetch many URLs and return body + http_code + attempts
//'
//' Same throttle/retry behaviour as \code{.siu_http_get_many} but the
//' return value preserves the HTTP status code and attempt count for
//' each URL, so callers can distinguish a healthy 200 with a small
//' body from a 429/503/short interstitial. Used by the DRID manifest
//' builder (\code{morie_siu_refresh_manifest}).
//'
//' @inheritParams siu_http_get_many
//' @return A list with three parallel slots: \code{body} (character),
//'   \code{http_code} (integer), \code{attempts} (integer).
//' @keywords internal
// [[Rcpp::export(.siu_http_get_many_with_status)]]
Rcpp::List siu_http_get_many_with_status(Rcpp::CharacterVector urls,
                                         int concurrency = 4,
                                         int timeout_s = 60,
                                         double rate_rps = 4.0,
                                         int max_retries = 3) {
  const int n = urls.size();
  Rcpp::CharacterVector body(n);
  Rcpp::IntegerVector code(n);
  Rcpp::IntegerVector attempts(n);
  for (int i = 0; i < n; ++i) {
    body[i] = "";
    code[i] = NA_INTEGER;
    attempts[i] = 0;
  }
  if (n == 0) {
    return Rcpp::List::create(Rcpp::Named("body") = body,
                              Rcpp::Named("http_code") = code,
                              Rcpp::Named("attempts") = attempts);
  }

  std::vector<std::string> u(n);
  for (int i = 0; i < n; ++i) u[i] = std::string(urls[i]);

  std::vector<Req*> reqs;
  fetch_many_throttled(u, concurrency, static_cast<long>(timeout_s),
                       rate_rps, max_retries, &reqs);

  for (Req* r : reqs) {
    body[r->idx] = r->body;
    code[r->idx] = static_cast<int>(r->http_code);
    attempts[r->idx] = r->attempts;
    delete r;
  }
  return Rcpp::List::create(Rcpp::Named("body") = body,
                            Rcpp::Named("http_code") = code,
                            Rcpp::Named("attempts") = attempts);
}

// ===========================================================================
// HTML parsing -- SIU director's-report pages -> the 64-column schema.
// ===========================================================================

namespace {

// Canonical 64-column order of the SIU dataset.
const std::vector<std::string> kSiuCols = {
  "case_number", "drid", "nrid", "source_url_report", "source_url_news",
  "scraped_at_utc", "parser_version", "date_of_incident_iso",
  "date_of_incident_raw", "time_of_incident_raw", "date_of_injury_iso",
  "date_of_injury_raw", "incident_to_injury_raw", "date_siu_notified_iso",
  "date_siu_notified_raw", "time_of_notification_raw", "notifying_party",
  "notifying_party_other_text", "date_of_director_decision_iso",
  "date_of_director_decision_raw", "time_of_director_decision_raw",
  "siu_investigators", "siu_forensics_investigators", "police_service",
  "number_of_officers_involved", "location_of_call",
  "type_of_building_or_scene", "reason_for_interaction", "injuries_sustained",
  "injuries_other_text", "specific_injuries", "location_of_treatment",
  "number_of_affected_persons", "sex_gender_affected", "age_affected",
  "affected_interviewed", "date_of_affected_interview_iso",
  "date_of_affected_interview_raw", "number_of_civilian_witnesses",
  "date_of_witness_interview_raw", "number_of_subject_officials",
  "subject_official_interviewed_or_notes", "date_of_subject_interview_raw",
  "number_of_witness_officials", "date_of_witness_official_interview_raw",
  "evidence_types", "evidence_other_text", "evidence_features",
  "narrative_summary", "relevant_legislation", "legislation_other_text",
  "weapons_or_force_used", "weapons_other_text", "charges_recommended",
  "directors_decision_reasonable", "supplemental_materials",
  "news_links_extra", "mental_health_or_race_indications", "_language",
  "news_release_title", "news_release_date_iso", "news_release_date_raw",
  "news_release_summary", "directors_name"
};

// Decode the HTML entities that occur in SIU pages.
std::string decode_entities(std::string s) {
  struct E { const char* k; const char* v; };
  static const E named[] = {
    {"&amp;", "&"}, {"&lt;", "<"}, {"&gt;", ">"}, {"&quot;", "\""},
    {"&apos;", "'"}, {"&nbsp;", " "}, {"&rsquo;", "'"}, {"&lsquo;", "'"},
    {"&ldquo;", "\""}, {"&rdquo;", "\""}, {"&ndash;", "-"}, {"&mdash;", "-"},
    {"&hellip;", "..."}, {"&eacute;", "\xC3\xA9"}, {"&egrave;", "\xC3\xA8"},
    {"&agrave;", "\xC3\xA0"}, {"&ccedil;", "\xC3\xA7"}, {"&acirc;", "\xC3\xA2"},
    {"&ecirc;", "\xC3\xAA"}, {"&icirc;", "\xC3\xAE"}, {"&ocirc;", "\xC3\xB4"},
    {"&ucirc;", "\xC3\xBB"}, {"&iuml;", "\xC3\xAF"}, {"&euml;", "\xC3\xAB"},
    {"&ugrave;", "\xC3\xB9"}, {"&igrave;", "\xC3\xAC"}, {"&Eacute;", "\xC3\x89"},
    {"&oelig;", "\xC5\x93"}, {"&laquo;", "\xC2\xAB"}, {"&raquo;", "\xC2\xBB"},
    {"&#39;", "'"}
  };
  for (const E& e : named) {
    std::string::size_type p = 0;
    while ((p = s.find(e.k, p)) != std::string::npos) {
      s.replace(p, std::string(e.k).size(), e.v);
      p += std::string(e.v).size();
    }
  }
  // Numeric entities &#NN; / &#xNN; -- decode the ASCII range; map a few
  // common Unicode punctuation points to their ASCII equivalents.
  static const std::regex num_re("&#(x?[0-9A-Fa-f]+);");
  std::string out;
  out.reserve(s.size());
  auto begin = std::sregex_iterator(s.begin(), s.end(), num_re);
  auto end = std::sregex_iterator();
  std::string::size_type last = 0;
  for (auto it = begin; it != end; ++it) {
    out.append(s, last, it->position() - last);
    std::string tok = (*it)[1].str();
    long cp = (tok[0] == 'x') ? std::strtol(tok.c_str() + 1, nullptr, 16)
                              : std::strtol(tok.c_str(), nullptr, 10);
    if (cp == 8217 || cp == 8216 || cp == 8242) out.push_back('\'');
    else if (cp == 8220 || cp == 8221) out.push_back('"');
    else if (cp == 8211 || cp == 8212) out.push_back('-');
    else if (cp >= 32 && cp < 127) out.push_back(static_cast<char>(cp));
    else out.push_back(' ');
    last = it->position() + it->length();
  }
  out.append(s, last, std::string::npos);
  return out;
}

// Collapse runs of whitespace to single spaces and trim.
std::string squeeze(const std::string& s) {
  std::string out;
  out.reserve(s.size());
  bool sp = false;
  for (char c : s) {
    const bool ws = (c == ' ' || c == '\t' || c == '\n' || c == '\r' ||
                     c == '\f' || c == '\v');
    if (ws) { sp = true; continue; }
    if (sp && !out.empty()) out.push_back(' ');
    sp = false;
    out.push_back(c);
  }
  return out;
}

// Case-insensitive substring search starting at pos. Returns npos if
// not found. Used by the linear HTML stripper below.
std::string::size_type ifind(const std::string& s, const char* needle,
                             std::string::size_type pos) {
  const std::size_t n = std::strlen(needle);
  if (n == 0) return pos;
  for (std::string::size_type i = pos; i + n <= s.size(); ++i) {
    std::string::size_type k = 0;
    for (; k < n; ++k) {
      const char a = s[i + k];
      const char b = needle[k];
      const char la = (a >= 'A' && a <= 'Z') ? (a + ('a' - 'A')) : a;
      const char lb = (b >= 'A' && b <= 'Z') ? (b + ('a' - 'A')) : b;
      if (la != lb) break;
    }
    if (k == n) return i;
  }
  return std::string::npos;
}

// Strip all HTML markup from a fragment and return decoded plain text.
//
// Linear state-machine implementation (one pass, no recursion). The
// previous std::regex_replace("<script[^>]*>.*?</script>") form blew
// the C stack via catastrophic backtracking on at least one drid in
// the 1..6000 sweep, killing the whole manifest job. We can't catch a
// stack overflow in C++, so the only safe fix is to remove the
// recursive matcher entirely.
//
// Inputs are also defensively bounded to 4 MB before scanning. SIU
// report pages run ~50-100 kB; anything larger is malformed or a
// runaway server response, and parsing it accomplishes nothing.
std::string html_to_text(std::string h) {
  static const std::size_t kMaxBytes = 4u * 1024u * 1024u;  // 4 MB
  if (h.size() > kMaxBytes) h.resize(kMaxBytes);

  std::string out;
  out.reserve(h.size());
  std::string::size_type i = 0;
  const std::string::size_type n = h.size();

  while (i < n) {
    if (h[i] != '<') { out.push_back(h[i]); ++i; continue; }

    // Look for <script[^>]*> or <style[^>]*>: must skip past the
    // matching close tag, replacing the whole span with a space.
    const bool is_script = (i + 7 <= n) &&
      (ifind(h.substr(i, 7), "<script", 0) == 0);
    const bool is_style  = (i + 6 <= n) &&
      (ifind(h.substr(i, 6), "<style", 0) == 0);
    if (is_script || is_style) {
      const char* close = is_script ? "</script>" : "</style>";
      std::string::size_type end = ifind(h, close, i);
      if (end == std::string::npos) {
        // Unclosed -- treat rest of input as junk and bail.
        out.push_back(' ');
        break;
      }
      end += std::strlen(close);
      out.push_back(' ');
      i = end;
      continue;
    }

    // Any other tag: skip to '>' and emit a space.
    std::string::size_type gt = h.find('>', i + 1);
    if (gt == std::string::npos) {
      // Unterminated tag -- treat rest as text.
      out.append(h, i, std::string::npos);
      break;
    }
    out.push_back(' ');
    i = gt + 1;
  }

  return squeeze(decode_entities(out));
}

// Lowercase-fold an ASCII string AND normalise common Unicode
// punctuation to ASCII equivalents so substring-matching against a
// C-string keyword works. SIU h2 headings use typographic
// apostrophes (U+2019 'Director's report') and curly quotes; without
// this normalisation, a keyword like "director's report" with an
// ASCII apostrophe would silently fail to match. Cheap, no allocs.
std::string lower_ascii(std::string s) {
  // U+2019 = E2 80 99, U+2018 = E2 80 98, U+201C = E2 80 9C,
  // U+201D = E2 80 9D, U+2013 = E2 80 93 (en-dash),
  // U+2014 = E2 80 94 (em-dash). Replace each 3-byte UTF-8 sequence
  // with a single ASCII char, in-place by overwrite + erase.
  std::string out;
  out.reserve(s.size());
  for (std::string::size_type i = 0; i < s.size(); ) {
    const unsigned char c = static_cast<unsigned char>(s[i]);
    if (c == 0xE2 && i + 2 < s.size()) {
      const unsigned char c1 = static_cast<unsigned char>(s[i + 1]);
      const unsigned char c2 = static_cast<unsigned char>(s[i + 2]);
      if (c1 == 0x80) {
        char repl = 0;
        if (c2 == 0x98 || c2 == 0x99) repl = '\'';  // curly apostrophes
        else if (c2 == 0x9C || c2 == 0x9D) repl = '"';  // curly quotes
        else if (c2 == 0x93 || c2 == 0x94) repl = '-';  // en/em dashes
        if (repl != 0) { out.push_back(repl); i += 3; continue; }
      }
    }
    out.push_back((c >= 'A' && c <= 'Z') ? c + ('a' - 'A') : s[i]);
    ++i;
  }
  return out;
}

// Find the bounds of the section whose <h2> heading text contains
// `title_keyword` (case-insensitive substring). The SIU site has
// two template families that flip the section_5/section_6 ordering
// between "Evidence" and "Incident Narrative", so looking up by
// title is robust where hard-coded numbers are not.
//
// Returns {start, end} where start is just past the `<h2 ...>...</h2>`
// closing tag and end is at the next <h2/footer/aside/nav boundary.
// Both are npos if the title isn't found.
std::pair<std::string::size_type, std::string::size_type>
section_bounds_by_title(const std::string& html,
                        const char* title_keyword) {
  const std::string kw = lower_ascii(title_keyword);
  std::string::size_type pos = 0;
  while (true) {
    const std::string::size_type h2 = html.find("<h2", pos);
    if (h2 == std::string::npos) break;
    const std::string::size_type gt = html.find('>', h2);
    if (gt == std::string::npos) break;
    const std::string::size_type close = html.find("</h2>", gt);
    if (close == std::string::npos) break;
    const std::string heading_text =
      lower_ascii(html.substr(gt + 1, close - gt - 1));
    if (heading_text.find(kw) != std::string::npos) {
      const std::string::size_type body = close + 5;  // past </h2>
      std::string::size_type end = html.size();
      for (const char* terminator : {"<h2", "<footer", "<aside", "<nav"}) {
        const std::string::size_type t = html.find(terminator, body);
        if (t != std::string::npos && t < end) end = t;
      }
      return {body, end};
    }
    pos = close + 5;
  }
  return {std::string::npos, std::string::npos};
}

// Plain text of the section whose <h2> heading matches `title_keyword`.
// Falls back to an empty string when no matching section exists --
// e.g. some short reports omit the legislation section entirely.
std::string section_text_by_title(const std::string& html,
                                  const char* title_keyword) {
  auto bounds = section_bounds_by_title(html, title_keyword);
  if (bounds.first == std::string::npos) return std::string();
  return html_to_text(html.substr(bounds.first,
                                  bounds.second - bounds.first));
}

// Plain text of the report section whose <h2> carries id="section_<n>".
// Stops at the NEXT <h2> (the next section heading) OR at the first
// page-chrome boundary (<footer, <aside, <nav) -- whichever comes
// first. Without the chrome cutoff, the LAST section on a page (no
// further <h2> follows it) silently includes the site footer, which
// leaks left-nav phrases like "First Nations, Inuit and Metis
// Liaison Program" into every report's narrative_summary,
// supplemental_materials, and mental_health_or_race_indications.
std::string section_text(const std::string& html, int n) {
  const std::string anchor = "id=\"section_" + std::to_string(n) + "\"";
  std::string::size_type a = html.find(anchor);
  if (a == std::string::npos) return std::string();
  std::string::size_type body = html.find('>', a);
  if (body == std::string::npos) return std::string();
  ++body;
  std::string::size_type b = html.size();
  for (const char* terminator : {"<h2", "<footer", "<aside", "<nav"}) {
    const std::string::size_type t = html.find(terminator, body);
    if (t != std::string::npos && t < b) b = t;
  }
  return html_to_text(html.substr(body, b - body));
}

// First capture group of `pat` in `text`, or "" when there is no match.
std::string rx1(const std::string& text, const std::string& pat) {
  try {
    std::smatch m;
    const std::regex re(pat, std::regex::icase);
    if (std::regex_search(text, m, re) && m.size() > 1) return m[1].str();
  } catch (...) {}
  return std::string();
}

// Highest N among "<label> #N" tokens; 1 if the bare label occurs; else "".
std::string label_count(const std::string& text, const std::string& label) {
  int hi = 0;
  try {
    const std::regex re(label + "\\s*#\\s*(\\d+)");
    auto begin = std::sregex_iterator(text.begin(), text.end(), re);
    for (auto it = begin; it != std::sregex_iterator(); ++it) {
      const int v = std::atoi((*it)[1].str().c_str());
      if (v > hi) hi = v;
    }
  } catch (...) {}
  if (hi > 0) return std::to_string(hi);
  try {
    if (std::regex_search(text, std::regex("\\b" + label + "\\b")))
      return "1";
  } catch (...) {}
  return std::string();
}

const char* kMonths[] = {"january", "february", "march", "april", "may",
                         "june", "july", "august", "september", "october",
                         "november", "december"};

// "May 8, 2026" or "8 May, 2026" -> "2026-05-08"; "" when unparseable.
std::string to_iso(const std::string& raw) {
  std::smatch m;
  std::string mon, day, year;
  if (std::regex_search(raw, m,
        std::regex("([A-Za-z]+)\\s+(\\d{1,2}),?\\s+(\\d{4})"))) {
    mon = m[1].str(); day = m[2].str(); year = m[3].str();
  } else if (std::regex_search(raw, m,
        std::regex("(\\d{1,2})\\s+([A-Za-z]+),?\\s+(\\d{4})"))) {
    day = m[1].str(); mon = m[2].str(); year = m[3].str();
  } else {
    return std::string();
  }
  std::transform(mon.begin(), mon.end(), mon.begin(), ::tolower);
  int mi = 0;
  for (int i = 0; i < 12; ++i) if (mon == kMonths[i]) { mi = i + 1; break; }
  if (mi == 0) return std::string();
  char buf[16];
  std::snprintf(buf, sizeof(buf), "%s-%02d-%02d", year.c_str(), mi,
                std::atoi(day.c_str()));
  return std::string(buf);
}

// First "Month D, YYYY" date appearing in `text`.
std::string first_date(const std::string& text) {
  return rx1(text, "([A-Za-z]+\\s+\\d{1,2},?\\s+\\d{4})");
}

// Every "Month D, YYYY" date in `text`, in order of appearance.
std::vector<std::string> all_dates(const std::string& text) {
  std::vector<std::string> v;
  try {
    const std::regex re("[A-Za-z]+\\s+\\d{1,2},?\\s+\\d{4}");
    for (auto it = std::sregex_iterator(text.begin(), text.end(), re);
         it != std::sregex_iterator(); ++it)
      v.push_back(it->str());
  } catch (...) {}
  return v;
}

// Modal police-service name in `text`: the most-mentioned "X Police"
// / "X Police Service" (ties broken toward the longer, more complete
// name), with SIU self-references and a leading "The " dropped. Far
// more robust than the first regex hit, which is often truncated.
// SIU's standard police-service abbreviations. Many reports refer to
// the service almost exclusively by acronym after introducing it
// once (or never spelling it out at all -- see drid 1723 etc.).
// modal_service() falls back to counting these acronyms when no
// "X Police Service" pattern is found in the section.
struct AcronymMap { const char* acronym; const char* canonical; };
const AcronymMap kSiuPoliceAcronyms[] = {
  // English acronyms (modern SIU reports)
  {"OPP",   "Ontario Provincial Police"},
  {"TPS",   "Toronto Police Service"},
  {"HPS",   "Hamilton Police Service"},
  {"HRPS",  "Halton Regional Police Service"},
  {"NRPS",  "Niagara Regional Police Service"},
  {"PRP",   "Peel Regional Police"},
  {"YRP",   "York Regional Police"},
  {"DRPS",  "Durham Regional Police Service"},
  {"WRPS",  "Waterloo Regional Police Service"},
  {"OPS",   "Ottawa Police Service"},
  {"LPS",   "London Police Service"},
  {"WPS",   "Windsor Police Service"},
  {"GPS",   "Guelph Police Service"},
  {"KPS",   "Kingston Police"},
  {"BPS",   "Belleville Police Service"},
  {"BPPS",  "Brockville Police Service"},
  {"CKPS",  "Chatham-Kent Police Service"},
  {"PRPS",  "Peterborough Police Service"},
  {"GSPS",  "Greater Sudbury Police Service"},
  {"SSMPS", "Sault Ste. Marie Police Service"},
  {"SLPS",  "South Simcoe Police Service"},
  {"SPS",   "Stratford Police Service"},
  {"TBPS",  "Thunder Bay Police Service"},
  {"BPSB",  "Brantford Police Service"},
  // French acronyms (Service de Police de X / Police provinciale).
  // SIU publishes a French version of every report; for cases with
  // no English drid (12-TFD-104 in our manifest) we still need to
  // identify the police service. Mapped to the canonical English
  // name so downstream analyses don't have to split on language.
  {"SPT",   "Toronto Police Service"},           // Service de Police de Toronto
  {"PPO",   "Ontario Provincial Police"},        // Police provinciale de l'Ontario
  {"SPRH",  "Halton Regional Police Service"},   // SP régional de Halton
  {"SPRY",  "York Regional Police"},             // SP régional de York
  {"SPRP",  "Peel Regional Police"},             // SP régional de Peel
  {"SPRD",  "Durham Regional Police Service"},   // SP régional de Durham
  {"SPRN",  "Niagara Regional Police Service"},  // SP régional de Niagara
  {"SPRW",  "Waterloo Regional Police Service"}, // SP régional de Waterloo
  {"SPO",   "Ottawa Police Service"},            // Service de Police d'Ottawa
  {"SPL",   "London Police Service"},            // Service de Police de London
  {"SPH",   "Hamilton Police Service"},          // Service de Police de Hamilton
  {"SPW",   "Windsor Police Service"},           // Service de Police de Windsor
  {"SPG",   "Guelph Police Service"},            // Service de Police de Guelph
  {"SPK",   "Kingston Police"}                   // Service de Police de Kingston
};

std::string modal_service(const std::string& text) {
  std::map<std::string, int> counts;
  // Pass 1: full "X Police Service" patterns.
  try {
    const std::regex re("[A-Z][A-Za-z.'\\-]+(?: [A-Z][A-Za-z.'\\-]+){0,4} "
                        "Police(?: Service)?");
    for (auto it = std::sregex_iterator(text.begin(), text.end(), re);
         it != std::sregex_iterator(); ++it) {
      std::string s = it->str();
      if (s.find("SIU") != std::string::npos) continue;
      if (s.rfind("The ", 0) == 0) s = s.substr(4);
      // Reject section-heading matches that don't name a service.
      if (s == "Materials Obtained from Police Service") continue;
      if (s == "Notifying Police Service") continue;
      if (s == "Police Service") continue;
      ++counts[s];
    }
  } catch (...) {}

  // Pass 2: acronym lookup. Counts each acronym occurrence and maps
  // to the canonical full name. Acronyms must be whole-word matches
  // (preceded by space/start and followed by space/punctuation) to
  // avoid matching within larger words.
  for (const AcronymMap& m : kSiuPoliceAcronyms) {
    const std::size_t alen = std::strlen(m.acronym);
    int n = 0;
    std::string::size_type pos = 0;
    while ((pos = text.find(m.acronym, pos)) != std::string::npos) {
      const bool boundary_left =
        (pos == 0) || !((text[pos - 1] >= 'A' && text[pos - 1] <= 'Z') ||
                        (text[pos - 1] >= 'a' && text[pos - 1] <= 'z'));
      const bool boundary_right =
        (pos + alen == text.size()) ||
        !((text[pos + alen] >= 'A' && text[pos + alen] <= 'Z') ||
          (text[pos + alen] >= 'a' && text[pos + alen] <= 'z'));
      if (boundary_left && boundary_right) ++n;
      pos += alen;
    }
    if (n > 0) counts[m.canonical] += n;
  }

  std::string best;
  int best_n = 0;
  for (const auto& kv : counts) {
    if (kv.second > best_n ||
        (kv.second == best_n && kv.first.size() > best.size())) {
      best_n = kv.second;
      best = kv.first;
    }
  }
  return best;
}

}  // namespace

//' Parse one SIU director's-report HTML page into the 64-column schema
//'
//' @param html The report page HTML.
//' @param drid The director's-report id.
//' @param url The source URL of the report page.
//' @return A named character vector with the 64 SIU dataset columns;
//'   report-derived fields are populated, news fields left empty.
//' @keywords internal
// [[Rcpp::export(.siu_parse_report)]]
Rcpp::CharacterVector siu_parse_report(std::string html, int drid,
                                       std::string url) {
  std::map<std::string, std::string> f;
  f["drid"] = std::to_string(drid);
  f["source_url_report"] = url;
  f["parser_version"] = MORIE_SIU_PARSER_VERSION;
  {
    std::time_t t = std::time(nullptr);
    std::tm gm;
#ifdef _WIN32
    gmtime_s(&gm, &t);
#else
    gmtime_r(&t, &gm);
#endif
    char buf[32];
    std::strftime(buf, sizeof(buf), "%Y-%m-%dT%H:%M:%SZ", &gm);
    f["scraped_at_utc"] = buf;
  }

  // Language: English vs French boilerplate header.
  // Language detection. SIU publishes every report in both English
  // and French; we always fetch via the /en/ URL, so the page IS
  // English unless it self-identifies as French via one of these
  // explicit FR markers. The previous detector returned "unknown"
  // for 313 of 4743 cached pages whose h2 headings happened to use
  // phrases not in the EN-marker list ("Mandate engaged" instead of
  // "Mandate of the SIU", etc.). Those are all real English
  // reports; FR-by-explicit-match-else-EN gets every page right.
  const bool is_fr =
    html.find("Mandat de l'UES") != std::string::npos ||
    html.find("Mandat de l\xE2\x80\x99UES") != std::string::npos ||
    html.find("Notification de l'UES") != std::string::npos ||
    html.find("Notification de l\xE2\x80\x99UES")
      != std::string::npos ||
    html.find("Rapport du directeur") != std::string::npos ||
    // L'enqu* (matches "L'enquête" without spelling out the
    // accented "ête"). The `\x99` hex escape gets terminated by
    // adjacent-string concatenation so the trailing `e` of "enqu"
    // isn't slurped into the hex sequence.
    html.find("L\xE2\x80\x99" "enqu") != std::string::npos ||
    html.find("Note explicative") != std::string::npos;
  f["_language"] = is_fr ? "fr" : "en";

  const std::string full = html_to_text(html);
  f["case_number"] = rx1(full, "Case #\\s*([0-9A-Za-z]+-[0-9A-Za-z]+-[0-9]+)");

  // Section lookup by TITLE rather than fixed number: SIU has
  // (a) two template families that flip section_5/section_6 between
  //     "Evidence" and "Incident Narrative"
  // (b) some 2014 English reports that use "Overview" instead of
  //     "The Investigation" as the section_4 heading
  // (c) some French-only reports whose section headings are in
  //     French ("L'enquête", "Aperçu", etc.).
  // Title-based lookup with multiple fallbacks works for all three.
  std::string investigation = section_text_by_title(html, "investigation");
  if (investigation.empty()) {
    investigation = section_text_by_title(html, "overview");
  }
  if (investigation.empty()) {
    // French "L'enquête" (= the investigation). Match the unaccented
    // stem "enqu" so this hits whether or not the input was
    // accent-stripped earlier.
    investigation = section_text_by_title(html, "enqu");
  }
  if (investigation.empty()) {
    // French "Aperçu" (= overview).
    investigation = section_text_by_title(html, "aper");
  }
  std::string narrative = section_text_by_title(html, "narrative");
  if (narrative.empty()) {
    // Some 2015-2018 reports title the narrative section
    // "Description of the Incident" instead.
    narrative = section_text_by_title(html, "description of the incident");
  }
  if (narrative.empty()) {
    // Pre-2015 SIU reports use an entirely different template:
    //   section_1 = "Explanatory note"      (boilerplate)
    //   section_2 = "Director's report"     (table of contents wrapper)
    //   section_3 = "Notification of the SIU"
    //   section_4 = "The investigation"     <- case narrative lives here
    //   section_5 = "Director's decision under s. 113(7) ..."
    // There's no separate "Narrative" section on these old reports;
    // the case story is woven into "The Investigation". Use that as
    // the narrative for backward compatibility.
    narrative = investigation;
  }
  const std::string evidence = section_text_by_title(html, "evidence");
  const std::string legislation =
    section_text_by_title(html, "legislation");
  std::string analysis = section_text_by_title(html, "analysis");
  if (analysis.empty()) analysis = section_text_by_title(html, "decision");

  f["narrative_summary"] = narrative;
  if (!legislation.empty()) f["relevant_legislation"] = legislation;

  // SIU notification: first date in "The Investigation"; the notifying
  // service is the police service named before "contacted the SIU".
  const std::string notif_date = first_date(investigation);
  if (!notif_date.empty()) {
    f["date_siu_notified_raw"] = notif_date;
    f["date_siu_notified_iso"] = to_iso(notif_date);
  }
  std::string service = modal_service(investigation);
  if (service.empty()) service = modal_service(full);
  if (!service.empty()) {
    f["police_service"] = service;
    f["notifying_party"] = service;
  }

  // Incident date: in "The Investigation" the first date is the SIU
  // notification and the second is the incident itself; fall back to
  // the Incident Narrative, then the Analysis section.
  std::string inc_date;
  {
    const std::vector<std::string> idates = all_dates(investigation);
    if (idates.size() >= 2) inc_date = idates[1];
    if (inc_date.empty()) inc_date = first_date(narrative);
    if (inc_date.empty()) inc_date = first_date(analysis);
    if (inc_date.empty() && !idates.empty()) inc_date = idates[0];
  }
  if (!inc_date.empty()) {
    f["date_of_incident_raw"] = inc_date;
    f["date_of_incident_iso"] = to_iso(inc_date);
  }

  // Director's decision date + name from the signature block.
  const std::string dec_date = rx1(
    full, "Date:\\s*([A-Za-z]+\\s+\\d{1,2},?\\s+\\d{4})");
  if (!dec_date.empty()) {
    f["date_of_director_decision_raw"] = dec_date;
    f["date_of_director_decision_iso"] = to_iso(dec_date);
  }
  std::string dname = rx1(
    full, "(?:approved by|signed by)\\s+([A-Z][A-Za-z.'\\- ]+?)\\s+Director");
  if (dname.empty())
    dname = rx1(full, "([A-Z][a-z]+(?: [A-Z]\\.?)? [A-Z][a-z]+),?\\s+"
                      "Director\\s+Special Investigations Unit");
  if (dname.empty())
    dname = rx1(full, "Director,?\\s+Special Investigations Unit\\s+"
                      "([A-Z][a-z]+(?: [A-Z]\\.?)? [A-Z][a-z]+)");
  f["directors_name"] = dname;

  // Official / witness counts. Scan investigation + evidence +
  // narrative -- the affected-person and officer rosters can land
  // in either of the first two sections depending on the template.
  const std::string roster = investigation + " " + evidence + " " +
                             narrative;
  const std::string so = label_count(roster, "SO");
  const std::string wo = label_count(roster, "WO");
  const std::string cw = label_count(roster, "CW");
  f["number_of_subject_officials"] = so;
  f["number_of_witness_officials"] = wo;
  f["number_of_civilian_witnesses"] = cw;
  // Compound officer-count format that matches the SIU's own
  // notation ("N SO M WO"), not a single sum. This is how the
  // canonical Qualtrics extraction template records it -- the
  // single-integer sum hides the subject/witness split which
  // matters for downstream analysis.
  if (!so.empty() || !wo.empty()) {
    std::string compound;
    if (!so.empty()) {
      compound = so + " SO";
      if (!wo.empty()) compound += " " + wo + " WO";
    } else if (!wo.empty()) {
      compound = wo + " WO";
    }
    f["number_of_officers_involved"] = compound;
  }

  // Affected-person attributes from the narrative.
  const std::string age = rx1(full, "(\\d{1,3})[ -]year[ -]old");
  if (!age.empty()) f["age_affected"] = age;
  {
    const std::string n = narrative + " " + analysis;
    auto count = [&](const std::string& w) {
      int c = 0;
      try {
        const std::regex re("\\b" + w + "\\b", std::regex::icase);
        for (auto it = std::sregex_iterator(n.begin(), n.end(), re);
             it != std::sregex_iterator(); ++it) ++c;
      } catch (...) {}
      return c;
    };
    const int male = count("he") + count("his") + count("him") +
                     count("man") + count("boy") + count("male");
    const int female = count("she") + count("her") + count("woman") +
                       count("girl") + count("female");
    const std::string lc = full;  // a non-binary signal anywhere counts
    const bool nonbinary =
      lc.find("non-binary") != std::string::npos ||
      lc.find("nonbinary") != std::string::npos ||
      lc.find("transgender") != std::string::npos ||
      lc.find("two-spirit") != std::string::npos;
    // Sex/gender is not binary: an explicit non-binary / transgender /
    // two-spirit signal wins; otherwise the dominant gendered-term
    // count decides; an indeterminate page is left blank.
    if (nonbinary)
      f["sex_gender_affected"] = "Non-binary";
    else if (male + female >= 3)
      f["sex_gender_affected"] = (male >= female) ? "Male" : "Female";
  }
  // Location of the call -- try several anchor phrases in order.
  // SIU reports state the incident location in the Investigation
  // or Narrative section's first paragraph. Scan only those (avoid
  // page chrome). Each pattern's capture group is tightened to stop
  // at common boundary punctuation so we don't trail off into the
  // next clause ("...Burlington, regarding...").
  {
    const std::string loc_scope = investigation + " " + narrative;
    std::string loc = rx1(loc_scope,
      "in the area of ([^.,;]{5,110},\\s*[A-Z][a-z]+(?:\\s+[A-Z][a-z]+)?)");
    if (loc.empty()) {
      loc = rx1(loc_scope,
        "at ([0-9]+[^.,;]{5,100},\\s*[A-Z][a-z]+(?:\\s+[A-Z][a-z]+)?)");
    }
    if (loc.empty()) {
      loc = rx1(loc_scope,
        "on ([A-Z][A-Za-z. -]{2,50} (?:Street|Road|Avenue|Drive|Boulevard|"
        "Lane|Place|Way|Highway))");
    }
    if (loc.empty()) {
      loc = rx1(loc_scope,
        "in the City of ([A-Z][a-zA-Z. -]{2,40})");
    }
    f["location_of_call"] = squeeze(loc);
  }

  // Director's decision and charges-recommended outcome.
  //
  // SIU verdict language has shifted over time. Reports from 2019+
  // use standardized phrasing ("no reasonable grounds to believe...");
  // older reports (2015-2018) used more literary language
  // ("commendable in the circumstances", "appropriate use of force",
  // "no criminal liability"). We match both vocabularies.
  //
  // When regex disagreement matters for downstream analysis,
  // morie_siu_llm_extract() / morie_siu_anomaly_check() can
  // re-extract these fields from the cached HTML with a model
  // that handles natural-language variance.
  {
    const std::string a_lower = lower_ascii(analysis);
    // No-charges signals (modern + legacy)
    const bool no_modern =
      a_lower.find("no reasonable grounds") != std::string::npos ||
      a_lower.find("no charges will be laid") != std::string::npos ||
      a_lower.find("no charges will issue") != std::string::npos ||
      a_lower.find("no charges are warranted") != std::string::npos ||
      a_lower.find("not warrant") != std::string::npos;
    const bool no_legacy =
      a_lower.find("did not commit") != std::string::npos ||
      a_lower.find("no criminal offence") != std::string::npos ||
      a_lower.find("no criminal liability") != std::string::npos ||
      a_lower.find("no grounds for charges") != std::string::npos ||
      a_lower.find("commendable") != std::string::npos ||
      a_lower.find("appropriate use of force") != std::string::npos ||
      a_lower.find("officer did nothing wrong") != std::string::npos;
    // Charges-laid signals (modern + legacy)
    const bool yes_modern =
      a_lower.find("reasonable grounds to believe") != std::string::npos ||
      a_lower.find("charges will be laid") != std::string::npos ||
      a_lower.find("charges have been laid") != std::string::npos ||
      a_lower.find("the charge of") != std::string::npos ||
      a_lower.find("charged with") != std::string::npos;
    const bool yes_legacy =
      a_lower.find("committed a criminal offence") != std::string::npos;

    if (no_modern || no_legacy) {
      f["directors_decision_reasonable"] = "No";
      f["charges_recommended"] = "No";
    } else if (yes_modern || yes_legacy) {
      f["directors_decision_reasonable"] = "Yes";
      f["charges_recommended"] = "Yes";
    }
  }

  // Mental-health / race indications mentioned in the case content.
  //
  // CRITICAL: scope this search to the case-content sections only
  // (investigation + affected-person + narrative + analysis). The
  // whole-page html_to_text leaks the site's left-nav chrome,
  // which contains "First Nations, Inuit and Metis Liaison
  // Program" on every report and would falsely tag every case as
  // "First Nation". Section 5 ("Affected Person") is where race
  // and mental-health context typically lives on SIU reports.
  {
    // Search the case-content sections only. Both templates have
    // "Investigation" + "Evidence" + "Narrative" + "Analysis";
    // race/mental-health context can land in any of them depending
    // on the report's writing style and template era.
    const std::string n = investigation + " " + evidence + " " +
                          narrative + " " + analysis;
    std::string tags;
    const char* kw[] = {
      "mental health", "Black", "Indigenous", "First Nation",
      "Inuit", "racializ", "racial", "in crisis",
      "suicidal", "psychotic", "self-harm", "self harm",
      "emotionally disturbed", "EDP", "Mental Health Act"
    };
    for (const char* k : kw) {
      if (n.find(k) != std::string::npos) {
        if (!tags.empty()) tags += "; ";
        tags += k;
      }
    }
    f["mental_health_or_race_indications"] = tags;
  }

  // News release linked from the report page: capture both the title
  // and the nrid, so the news page can be fetched and joined later.
  {
    std::smatch m;
    if (std::regex_search(html, m, std::regex(
          "News Releases for this Case:.*?<a[^>]*href=\"([^\"]+)\"[^>]*>"
          "(.*?)</a>", std::regex::icase))) {
      f["news_release_title"] = squeeze(html_to_text(m[2].str()));
      const std::string nrid = rx1(m[1].str(), "nrid=(\\d+)");
      if (!nrid.empty()) {
        f["nrid"] = nrid;
        f["source_url_news"] =
          "https://www.siu.on.ca/en/news_template.php?nrid=" + nrid;
      }
    }
  }

  Rcpp::CharacterVector out(kSiuCols.size());
  Rcpp::CharacterVector nm(kSiuCols.size());
  for (std::size_t i = 0; i < kSiuCols.size(); ++i) {
    nm[i] = kSiuCols[i];
    const auto it = f.find(kSiuCols[i]);
    out[i] = (it == f.end()) ? "" : it->second;
  }
  out.attr("names") = nm;
  return out;
}

//' Parse one SIU news-release HTML page
//'
//' @param html The news-release page HTML.
//' @param nrid The news-release id.
//' @param url The source URL of the news-release page.
//' @return A named character vector: nrid, source_url_news,
//'   news_release_title, news_release_date_iso, news_release_date_raw,
//'   news_release_summary.
//' @keywords internal
// [[Rcpp::export(.siu_parse_news)]]
Rcpp::CharacterVector siu_parse_news(std::string html, int nrid,
                                     std::string url) {
  std::string title;
  try {
    const std::regex hre("<h[1-4][^>]*>(.*?)</h[1-4]>",
                         std::regex::icase);
    for (auto it = std::sregex_iterator(html.begin(), html.end(), hre);
         it != std::sregex_iterator(); ++it) {
      const std::string t = squeeze(html_to_text((*it)[1].str()));
      if (t.size() > title.size() &&
          t.find("News Release") == std::string::npos &&
          t != "The Unit")
        title = t;
    }
  } catch (...) {}

  // Dateline: "<strong>City, ON</strong> ( DD Month, YYYY ) ---".
  std::string draw = rx1(html, "</strong>\\s*\\(([^)]{6,32})\\)\\s*-");
  if (draw.empty())
    draw = rx1(html_to_text(html),
               "\\(([0-9]{1,2} [A-Za-z]+,? [0-9]{4})\\)");

  const std::string txt = html_to_text(html);
  std::string summary = rx1(
    txt, "\\)\\s*-+\\s*(.{20,700}?)(?:Full Director|If you or someone)");
  if (summary.empty())
    summary = rx1(txt, "\\)\\s*-+\\s*(.{20,400})");

  std::map<std::string, std::string> f;
  f["nrid"] = std::to_string(nrid);
  f["source_url_news"] = url;
  f["news_release_title"] = title;
  f["news_release_date_raw"] = squeeze(draw);
  f["news_release_date_iso"] = to_iso(draw);
  f["news_release_summary"] = squeeze(summary);

  static const char* cols[] = {
    "nrid", "source_url_news", "news_release_title",
    "news_release_date_iso", "news_release_date_raw",
    "news_release_summary"};
  Rcpp::CharacterVector out(6);
  Rcpp::CharacterVector nm(6);
  for (int i = 0; i < 6; ++i) { nm[i] = cols[i]; out[i] = f[cols[i]]; }
  out.attr("names") = nm;
  return out;
}
