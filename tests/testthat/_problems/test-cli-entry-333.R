# Extracted from test-cli-entry.R:333

# prequel ----------------------------------------------------------------------
.pkg <- if (isNamespaceLoaded("rmorie")) "rmorie" else "morie"
.capture <- function(...) {
  buf <- character()
  status <- morie_cli(c(...), out = function(s) buf <<- c(buf, s))
  list(text = paste(buf, collapse = ""), status = status)
}

# test -------------------------------------------------------------------------
r <- .capture("verify-pollution", "--pollutant", "no2", "--demo")
expect_equal(r$status, 0L)
expect_match(r$text, "STATUS: ok")
expect_match(r$text, "source:   Atkinson")
