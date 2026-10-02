# Extracted from test-cli-entry.R:102

# prequel ----------------------------------------------------------------------
.pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.capture <- function(...) {
  buf <- character()
  status <- morie_cli(c(...), out = function(s) buf <<- c(buf, s))
  list(text = paste(buf, collapse = ""), status = status)
}

# test -------------------------------------------------------------------------
withr::local_envvar(XDG_CONFIG_HOME = withr::local_tempdir())
.morie_llm_cache$ollama_cached <- FALSE
.morie_llm_cache$hosted_cached <- NULL
withr::defer({ .morie_llm_cache$ollama_cached <- NULL; .morie_llm_cache$hosted_cached <- NULL
                 .morie_llm_cache$hosted_models <- NULL })
expect_match(.capture("models")$text, "Hosted LLM: not logged in")
testthat::local_mocked_bindings(.package = .pkg,
    .morie_llm_hosted_key = function() "sk-test",
    morie_llm_probe_hosted = function(...) { .morie_llm_cache$hosted_models <- c("a:cloud", "b:cloud"); TRUE },
    .morie_llm_hosted_model = function() "b:cloud")
m <- .capture("models")
expect_equal(m$status, 0L)
expect_match(m$text, "Hosted LLM \\(https://llm.rmorie.com\\); default marked \\*:")
expect_match(m$text, "\n    a:cloud\n  \\* b:cloud\n")
expect_match(m$text, "Local Ollama: not reachable")
hm <- morie_llm_hosted_models()
expect_equal(as.character(hm), c("a:cloud", "b:cloud"))
expect_equal(attr(hm, "default"), "b:cloud")
expect_match(.capture("doctor")$text, "models: a:cloud, b:cloud \\(default b:cloud\\)")
seen <- NULL
testthat::local_mocked_bindings(.package = .pkg,
    morie_llm_ask = function(prompt, model = NULL, ...) { seen <<- model; paste("echo:", prompt) })
expect_equal(.capture("ask", "--model", "a:cloud", "hi", "there")$text, "echo: hi there\n")
expect_equal(seen, "a:cloud")
expect_equal(.capture("ask", "hi")$text, "echo: hi\n")
expect_null(seen)
expect_match(.capture("ask", "--help")$text, "usage: rmorie ask \\[--model NAME\\]")
