# SPDX-License-Identifier: AGPL-3.0-or-later
#
# The help pages for the language-model settings (morie_llm_config / `rmorie
# config`), which are rmoriebricklayer's: its own tests cover the settings and the
# routes, and test-llm-byok.R covers how rmorie hands them over. The settings live
# in a temporary XDG_CONFIG_HOME, so nothing touches the network or $HOME.

.pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"

.cfg_sandbox <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  vars <- c("MORIE_LLM_ROUTE", "MORIE_LLM_BASE_URL", "MORIE_LLM_API_KEY", "MORIE_LLM_MODEL",
            "OLLAMA_HOST", "OLLAMA_BASE_URL", "OLLAMA_MODEL", "OLLAMA_API_KEY",
            "MORIE_HOSTED_BASE_URL", "MORIE_HOSTED_MODEL", "MORIE_HOSTED_KEY", "MORIE_HOSTED_AUTH_URL",
            "LLM_API_BASE_URL", "LLM_API_KEY", "MORIE_API_MODEL",
            "GEMINI_API_KEY", "GOOGLE_API_KEY", "OPENAI_API_KEY")
  withr::local_envvar(c(XDG_CONFIG_HOME = dir, stats::setNames(rep(NA_character_, length(vars)), vars)),
                      .local_envir = env)
  # probes are mocked below, so they may run even under R CMD check
  withr::local_options(morie.llm.allow_net_probe = TRUE, .local_envir = env)
  .morie_llm_reset_probes()
  withr::defer(.morie_llm_reset_probes(), envir = env)
  dir
}

.cap <- function(...) {
  txt <- character()
  st <- morie_cli(c(...), out = function(s) txt <<- c(txt, s))
  list(status = st, text = paste(txt, collapse = ""))
}

test_that("help has a guide per topic and every verb, config included", {
  .cfg_sandbox()
  v <- .cap("help")
  expect_match(v$text, "config \\[show \\| help")
  expect_match(v$text, "rmorie help llm")
  expect_match(.cap("help", "start")$text, "Getting started")
  # the language-model pages are rmoriebricklayer's, under the rmorie name
  expect_match(.cap("help", "llm")$text, "rmorie config set route anthropic")
  expect_match(.cap("help", "r")$text, "bricklayer_llm_config\\(route = \"anthropic\"")
  expect_match(.cap("help", "ask")$text, "usage: rmorie ask \\[--model NAME\\] \\[--route")
  expect_match(.cap("ask", "--help")$text, "--route")
  expect_equal(.cap("help", "no-such-topic")$status, 2L)
})
