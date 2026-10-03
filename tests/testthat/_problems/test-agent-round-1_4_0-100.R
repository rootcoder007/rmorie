# Extracted from test-agent-round-1_4_0.R:100

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
r <- morie_verify_pollution(pollutant = "no2", exposure_mean = 20, exposure_prevalence = 0.5,
                              reference = 5.8, baseline_rate = 500, population = 1e6)
rr <- exp(0.039 * (20 - 5.8) / 10)
paf <- 0.5 * (rr - 1) / (0.5 * (rr - 1) + 1)
expect_equal(r$burden$reference_conc, 5.8)
expect_equal(r$burden$attributable_cases, paf * 500 / 1e5 * 1e6, tolerance = 1e-9)
expect_equal(r$displaced$deaths_displaced, 500 / 1e5 * 1e6 * 0.5 * (1 - 1 / rr), tolerance = 1e-9)
