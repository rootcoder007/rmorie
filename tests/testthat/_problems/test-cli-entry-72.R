# Extracted from test-cli-entry.R:72

# prequel ----------------------------------------------------------------------
.pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.capture <- function(...) {
  buf <- character()
  status <- morie_cli(c(...), out = function(s) buf <<- c(buf, s))
  list(text = paste(buf, collapse = ""), status = status)
}

# test -------------------------------------------------------------------------
withr::local_envvar(XDG_CONFIG_HOME = withr::local_tempdir(), GEMINI_API_KEY = NA,
                      LLM_API_BASE_URL = NA, LLM_API_KEY = NA, OPENAI_API_KEY = NA)
.morie_llm_cache$ollama_cached <- FALSE
.morie_llm_cache$hosted_cached <- NULL
withr::defer({ .morie_llm_cache$ollama_cached <- NULL; .morie_llm_cache$hosted_cached <- NULL })
d <- .capture("doctor")
expect_equal(d$status, 0L)
expect_match(d$text, "Ollama \\(local\\) +not reachable")
expect_match(d$text, "not logged in")
expect_match(d$text, "active provider: local")
testthat::local_mocked_bindings(.package = .pkg, morie_llm_ask = function(prompt, ...) paste("echo:", prompt))
a <- .capture("ask", "what", "is", "MORIE")
expect_equal(a$text, "echo: what is MORIE\n")
expect_match(.capture("ask", "--help")$text, "usage: rmorie ask")
