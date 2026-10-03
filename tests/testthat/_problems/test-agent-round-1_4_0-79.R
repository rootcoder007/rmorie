# Extracted from test-agent-round-1_4_0.R:79

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
for (verb in c("inspect", "verify", "pull")) {
    r <- .cap(verb)
    expect_equal(r$status, 2L, info = verb)
    expect_match(r$text, paste0("usage: rmorie ", verb))
  }
