# Extracted from test-agent-round-1_4_0.R:74

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
expect_match(r$text, "mapq .*own file: .*datasets/vsr/TKARONTOMAPQ.xlsx")
expect_match(r$text, "siumanifest .*rmoriedata \\(CRAN\\)")
expect_match(r$text, "hibsa .*health-infobase.canada.ca \\(or data.rmorie.com\\)")
expect_match(r$text, "71 keys: 70 download from their portal, rmoriedata or data.rmorie.com on first use; 1 is your own research file")
expect_match(r$text, "Curated tables at data.rmorie.com")
