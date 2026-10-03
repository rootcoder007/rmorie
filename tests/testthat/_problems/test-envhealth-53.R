# Extracted from test-envhealth.R:53

# test -------------------------------------------------------------------------
b <- morie_envhealth_burden(25, 1, 0.008, 1e6, pollutant = "NO2")
rr <- exp(0.039 * 1.5)
expect_equal(b$extra$rr, rr)
