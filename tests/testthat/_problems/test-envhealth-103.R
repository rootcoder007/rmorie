# Extracted from test-envhealth.R:103

# test -------------------------------------------------------------------------
ok <- morie_verify_pollution("no2", exposure_mean = 25, exposure_prevalence = 0.9)
expect_equal(ok$status, "ok")
expect_equal(attr(ok, "exit_status"), 0L)
expect_equal(ok$pipeline$crf$rr, exp(0.039 * (25 - 5.8) / 10))
expect_equal(ok$pipeline$paf, morie_envhealth_attributable_fraction(ok$pipeline$crf$rr, 0.9))
expect_equal(ok$pipeline$displaced$deaths_displaced, 500 / 1e5 * 1e6 * (1 - 1 / ok$pipeline$crf$rr))
