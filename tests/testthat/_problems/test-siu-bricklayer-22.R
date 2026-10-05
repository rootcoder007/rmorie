# Extracted from test-siu-bricklayer.R:22

# test -------------------------------------------------------------------------
skip_if_not_installed("rmoriedata")
df <- morie_siu_reports()
expect_gt(nrow(df), 2000L)
expect_equal(ncol(df), 65L)
