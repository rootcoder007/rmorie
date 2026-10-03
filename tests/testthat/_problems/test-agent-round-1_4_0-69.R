# Extracted from test-agent-round-1_4_0.R:69

# prequel ----------------------------------------------------------------------
.pkg <- utils::packageName(environment(morie_cli))
.cap <- function(...) {
  buf <- character()
  status <- withCallingHandlers(
    morie_cli(c(...), out = function(x) buf <<- c(buf, x)),
    message = function(m) invokeRestart("muffleMessage")
  )
  list(status = status, text = paste(buf, collapse = ""))
}

# test -------------------------------------------------------------------------
r <- .cap("list-datasets")
expect_equal(r$status, 0L)
expect_match(r$text, "naps-no2-on-2023 .*ECCC NAPS")
expect_match(r$text, "cchs22 .*Statistics Canada")
