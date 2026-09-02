# morie/_fast.R -- R-side wrappers for the Rcpp hot kernels.
#
# This file shadows morie.fast in the Python side: users get the same
# numerical results from `rmorie:::morie_normal_pdf(x, 0, 1)` as they do
# from `morie.fast.normal_pdf(x, 0, 1)` in Python.
#
# When the package is built WITHOUT the C++ toolchain (rare on CRAN
# builders; common on minimal-Linux containers without g++ / clang),
# the .Call dispatcher falls back to the pure-R implementations
# defined below.  Users get the right numbers either way.

#' Fast normal PDF (Rcpp accelerated when toolchain available)
#'
#' Drop-in for \code{dnorm(x, mean, sd)} with a single-pass C++ kernel.
#' Numerically identical; faster on hot paths because the per-call
#' R-S4-dispatch overhead is bypassed.
#'
#' @param x numeric vector
#' @param mean numeric scalar
#' @param sd numeric scalar (> 0)
#' @return numeric vector of densities
#' @keywords internal
#' @examples
#' rmorie:::morie_normal_pdf(0, 0, 1)
morie_normal_pdf <- function(x, mean = 0, sd = 1) {
  if (.cpp_available()) {
    morie_normal_pdf_cpp(as.numeric(x), as.numeric(mean), as.numeric(sd))
  } else {
    dnorm(x, mean = mean, sd = sd)
  }
}

#' Fast mean
#'
#' Single-pass C++ kernel.  Equivalent to \code{mean(x)}.
#' @param x See Usage.
#' @keywords internal
#' @return A numeric scalar: the mean of \code{x}.
#' @examples
#' rmorie:::morie_mean(1:10)
morie_mean <- function(x) {
  if (.cpp_available()) {
    morie_mean_cpp(as.numeric(x))
  } else {
    mean(x)
  }
}

#' Fast variance
#'
#' Single-pass two-step C++ kernel with optional ddof correction.
#' @param x numeric vector
#' @param ddof integer; default 1 (sample variance)
#' @keywords internal
#' @return A numeric scalar: the variance of \code{x}.
#' @examples
#' abs(rmorie:::morie_var(c(1, 2, 3, 4, 5), ddof = 1) - 2.5) < 1e-9
morie_var <- function(x, ddof = 1) {
  if (.cpp_available()) {
    morie_var_cpp(as.numeric(x), as.integer(ddof))
  } else {
    n <- length(x)
    if (n - ddof <= 0) {
      return(NA_real_)
    }
    sum((x - mean(x))^2) / (n - ddof)
  }
}

#' Fast Pearson correlation
#'
#' Single-pass C++ kernel.  Equivalent to \code{cor(x, y)} when both
#' vectors are equal-length and complete (no NA handling).
#' @param x See Usage.
#' @param y See Usage.
#' @keywords internal
#' @return A numeric scalar: the Pearson correlation between \code{x} and \code{y}.
#' @examples
#' rmorie:::morie_cor_pearson(1:10, 1:10)
morie_cor_pearson <- function(x, y) {
  if (.cpp_available()) {
    morie_cor_pearson_cpp(as.numeric(x), as.numeric(y))
  } else {
    suppressWarnings(cor(x, y))
  }
}

# Internal: detect whether the Rcpp .so was successfully built.
#' Internal helper: Cpp Available
#' @noRd
.cpp_available <- function() {
  tryCatch(
    {
      exists("morie_normal_pdf_cpp", mode = "function") &&
        is.function(get("morie_normal_pdf_cpp"))
    },
    error = function(e) FALSE
  )
}

#' Is the R-side JIT acceleration active?
#'
#' Mirrors \code{morie.fast.is_jit_available()} on the Python side.
#' Returns TRUE when the Rcpp .so was built and loaded; FALSE when
#' falling back to base-R implementations.
#'
#' @return A logical scalar: \code{TRUE} when the compiled Rcpp backend was
#'   built and loaded, \code{FALSE} when falling back to base-R kernels.
#' @examples
#' morie_fast_available()
#' @export
morie_fast_available <- function() {
  .cpp_available()
}
