# A missing case number used to page the WHOLE live SIU index at the
# rate limit -- about ninety requests, 209 of test-siu.R's 215 seconds,
# for the same result either way. It also meant R CMD check reached
# external services, which CRAN does not allow. These assertions pin
# the four states of the switch so that cost cannot come back quietly.

test_that("a live index sweep is refused under R CMD check only", {
  allowed <- rmorie:::.siu_live_fetch_allowed

  withr::with_envvar(
    c("_R_CHECK_PACKAGE_NAME_" = NA, RMORIE_NETWORK_TESTS = NA),
    withr::with_options(list(morie.siu.allow_fetch = NULL), {
      # an ordinary user session is untouched: the live fallback is the
      # documented behaviour there
      expect_true(allowed())
    })
  )

  withr::with_envvar(
    c("_R_CHECK_PACKAGE_NAME_" = "rmorie", RMORIE_NETWORK_TESTS = NA),
    withr::with_options(list(morie.siu.allow_fetch = NULL), {
      expect_false(allowed())
    })
  )

  # both opt-ins still reach the network, so the deliberate network
  # tests are unaffected
  withr::with_envvar(
    c("_R_CHECK_PACKAGE_NAME_" = "rmorie", RMORIE_NETWORK_TESTS = "1"),
    withr::with_options(list(morie.siu.allow_fetch = NULL), {
      expect_true(allowed())
    })
  )

  withr::with_envvar(
    c("_R_CHECK_PACKAGE_NAME_" = "rmorie", RMORIE_NETWORK_TESTS = NA),
    withr::with_options(list(morie.siu.allow_fetch = TRUE), {
      expect_true(allowed())
    })
  )
})

test_that("resolving an absent case number costs nothing under check", {
  # The contract that matters: the miss is reported, not swept for.
  withr::with_envvar(
    c("_R_CHECK_PACKAGE_NAME_" = "rmorie", RMORIE_NETWORK_TESTS = NA),
    withr::with_options(list(morie.siu.allow_fetch = NULL), {
      t0 <- proc.time()[["elapsed"]]
      got <- rmorie:::.siu_resolve_drid("99-XXX-999")
      elapsed <- proc.time()[["elapsed"]] - t0
      expect_true(is.na(got) || is.numeric(got))
      # ninety rate-limited requests took 209 seconds; a refusal is
      # immediate. Ten seconds is loose enough for a slow machine and
      # still fails if the sweep returns.
      expect_lt(elapsed, 10)
    })
  )
})
