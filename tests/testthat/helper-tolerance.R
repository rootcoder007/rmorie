# Absolute-tolerance comparison, matching Python's pytest.approx(abs=...).
#
# testthat 3e's `tolerance` is RELATIVE whenever the expected value is
# non-zero, while pytest.approx(abs=) is absolute.  A parity pair written
# with the same number in both arms therefore does NOT test the same
# thing: on a p-value of 0.0074, tolerance = 1e-5 admits an absolute
# difference of only 7.4e-8, so an agreement of 1.6e-7 -- far better than
# the 1e-5 the author intended -- is reported as a failure.
#
# Use expect_close() wherever the intent is "within this many units", and
# plain expect_equal(tolerance = ) where the intent is "within this
# fraction".  Comparing against exactly 0 is already absolute in
# testthat, so those need no change.
expect_close <- function(object, expected, tol = 1e-9, info = NULL) {
  act <- as.numeric(object)
  exp <- as.numeric(expected)
  testthat::expect_equal(length(act), length(exp), info = info)
  gap <- max(abs(act - exp))
  testthat::expect_true(
    gap <= tol,
    info = paste0(if (!is.null(info)) paste0(info, ": "),
                  "max absolute difference ", format(gap, digits = 6),
                  " exceeds ", format(tol, digits = 6)))
  invisible(object)
}
