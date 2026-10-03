# Extracted from test-agent-round-1_4_0.R:15

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
cat_df <- morie_dataset_catalog()
expect_equal(nrow(cat_df), 71L)
