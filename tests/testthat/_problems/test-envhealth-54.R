# Extracted from test-envhealth.R:54

# test -------------------------------------------------------------------------
b <- morie_envhealth_burden(25, 1, 0.008, 1e6, pollutant = "NO2")
rr <- exp(0.039 * 1.5)
expect_equal(b$extra$rr, rr)
expect_equal(b$paf, (rr - 1) / rr)
