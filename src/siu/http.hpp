// SPDX-License-Identifier: AGPL-3.0-or-later
//
// http.hpp -- minimal libcurl wrapper: GET a report page or a model list; POST
// JSON to a model server. No host is hardcoded and no key is bundled; callers
// pass the base URL and an optional bearer token (see llm.hpp).
#pragma once
#include <string>

namespace siu::http {

// GET url, returning the body. Throws std::runtime_error on transport failure
// or a >= 400 status. `timeout_s` bounds the whole request; `bearer`, when
// non-empty, is sent as an Authorization: Bearer header.
std::string get(const std::string& url, long timeout_s = 30, const std::string& bearer = "");

// POST `json_body` (Content-Type: application/json) to url, returning the body.
// HTTP 429/503 are retried with linear backoff (cloud tier limits, model loads).
std::string post_json(const std::string& url, const std::string& json_body,
                      const std::string& bearer = "", long timeout_s = 300);

}  // namespace siu::http
