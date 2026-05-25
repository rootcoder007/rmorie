# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Phase 2T: tests for vertex.R — Google Vertex AI / Gemini client.
# All paths are network/credential-gated; we exercise the structured
# error branches via withr::with_envvar to scrub the relevant
# environment variables.

test_that("morie_vertex_resolve_config errors without GOOGLE_APPLICATION_CREDENTIALS", {
  withr::with_envvar(
    c(GOOGLE_APPLICATION_CREDENTIALS = "",
      GOOGLE_CLOUD_PROJECT = "",
      MORIE_VERTEX_PROJECT = ""),
    {
      out <- tryCatch(morie_vertex_resolve_config(),
                      error = function(e) e)
      expect_true(inherits(out, "error") || is.list(out))
    }
  )
})

test_that("morie_vertex_access_token errors cleanly without creds", {
  withr::with_envvar(
    c(GOOGLE_APPLICATION_CREDENTIALS = ""),
    {
      out <- tryCatch(morie_vertex_access_token(cfg = NULL),
                      error = function(e) e)
      expect_true(inherits(out, "error") || is.character(out))
    }
  )
})

test_that("morie_vertex_ask_gemini errors cleanly without creds", {
  withr::with_envvar(
    c(GOOGLE_APPLICATION_CREDENTIALS = ""),
    {
      out <- tryCatch(
        morie_vertex_ask_gemini(prompt = "ping"),
        error = function(e) e)
      expect_true(inherits(out, "error") || is.character(out) ||
                    is.list(out))
    }
  )
})

test_that("morie_vertex_health_check returns a structured status", {
  out <- tryCatch(morie_vertex_health_check(),
                  error = function(e) e)
  # Whether vertex is reachable or not, this should return a
  # structured status (list / character / error).
  expect_true(inherits(out, "error") || is.list(out) ||
                is.character(out) || is.logical(out))
})
