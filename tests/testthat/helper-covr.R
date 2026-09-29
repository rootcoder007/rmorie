# covr replaces every namespace binding with a traced copy after the
# package's function registries (lists of closures built at load) exist,
# so identical() between a registry element and the binding holds
# everywhere except under covr; there the pieces are compared instead.
expect_same_function <- function(a, b) {
  if (nzchar(Sys.getenv("R_COVR"))) {
    testthat::expect_true(is.function(a) && is.function(b))
    testthat::expect_identical(formals(a), formals(b))
    testthat::expect_identical(environment(a), environment(b))
  } else {
    testthat::expect_identical(a, b)
  }
}
