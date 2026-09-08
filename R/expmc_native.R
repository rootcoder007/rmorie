# Differential-privacy mechanisms exposed under their wave3 module
# names: exponential mechanism (expmc), Laplace mechanism (laplc),
# local DP by randomized response (locdp).
#
# Each of these already ships as a NATIVE implementation in this
# package (morie_dp_exponential_mechanism, morie_dp_laplace_mechanism,
# morie_randomized_response_dp).  Nothing here wraps an external
# package -- these are the module-name entry points onto morie's own
# native code, exactly mirroring the Python side, where expmc.py /
# laplc.py / locdp.py bind the same three native functions rather
# than adding a second implementation of each.
#
# Sources:
#   Laplace mechanism -- Dwork, C., McSherry, F., Nissim, K. and
#     Smith, A. (2006), Calibrating noise to sensitivity in private
#     data analysis, TCC 2006, LNCS 3876, 265-284; Dwork & Roth
#     (2014), FnT-TCS 9(3-4), Definition 3.3 and Theorem 3.6.
#   Exponential mechanism -- McSherry, F. and Talwar, K. (2007),
#     Mechanism design via differential privacy, FOCS 2007, 94-103;
#     Dwork & Roth (2014), Definition 3.4.
#   Local model / randomized response -- Warner, S. L. (1965), JASA
#     60(309), 63-69; Kasiviswanathan, S. P. et al. (2011), SIAM J.
#     Comput. 40(3), 793-826; Dwork & Roth (2014), Sec. 3.2.
#   Local copy: fetched-wave3/
#   dwork-roth-2014-algorithmic-foundations-differential-privacy.pdf

#' Exponential mechanism (module-name entry point)
#'
#' Selects an output with probability proportional to
#' exp(epsilon u(D, r) / (2 Delta u)) (McSherry & Talwar 2007; Dwork
#' & Roth 2014, Definition 3.4).  Entry point onto this package's
#' native \code{morie_dp_exponential_mechanism}.
#'
#' @param ... Passed to \code{morie_dp_exponential_mechanism}.
#' @return The list returned by
#'   \code{morie_dp_exponential_mechanism}.
#' @references McSherry, F. and Talwar, K. (2007). Mechanism design
#'   via differential privacy. FOCS 2007, 94-103.
#' @export
morie_expmc <- function(...) morie_dp_exponential_mechanism(...)

#' Laplace mechanism (module-name entry point)
#'
#' Releases f(D) + Lap(sensitivity/epsilon) (Dwork et al. 2006;
#' Dwork & Roth 2014, Definition 3.3, Theorem 3.6).  Entry point onto
#' this package's native \code{morie_dp_laplace_mechanism}.
#'
#' @param ... Passed to \code{morie_dp_laplace_mechanism}.
#' @return The list returned by \code{morie_dp_laplace_mechanism}.
#' @references Dwork, C., McSherry, F., Nissim, K. and Smith, A.
#'   (2006). Calibrating noise to sensitivity in private data
#'   analysis. TCC 2006, LNCS 3876, 265-284.
#' @export
