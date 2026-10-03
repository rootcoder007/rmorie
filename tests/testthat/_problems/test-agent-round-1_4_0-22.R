# Extracted from test-agent-round-1_4_0.R:22

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
expect_true(all(c("naps-no2-on-2023", "naps-pm25-ca-2023", "naps-co-on-2023", "cchs22") %in% cat_df$key))
naps <- cat_df[cat_df$source == "naps", ]
expect_equal(nrow(naps), 24L)
expect_true(all(naps$fetcher == "morie_fetch_naps"))
args <- .morie_parse_fetcher_args(naps$fetcher_args[naps$key == "naps-no2-on-2023"])
expect_equal(args, list(pollutant = "no2", year = 2023L, province = "ON"))
expect_equal(nrow(morie_list_datasets()), 71L + sum(morie_list_datasets()$type == "hosted"))
