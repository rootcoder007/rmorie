% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_strmkr_strauss_process}
\alias{morie_strmkr_strauss_process}
\alias{morie_strmkr}
\title{The Strauss inhibition process fitted by maximum}
\usage{
morie_strmkr_strauss_process(coords, r, gamma = NULL, window = NULL, nx = 12L, ny = 12L, max_iter = 100L, tol = 1e-11)
}
\description{
R arm of strmkr -- the Strauss inhibition process fitted by maximum pseudolikelihood through the Baddeley-Turner quadrature device.
}
\value{
A list with components \code{n}, \code{i}, \code{estimate}, \code{beta}, \code{gamma}, \code{log_beta}, \code{log_gamma}, \code{se_log_beta}, \code{se_log_gamma}, \code{gamma_ci_lower}, \code{gamma_ci_upper}, \code{n_points}, \code{n_close_pairs}, \code{radius}, \code{area}, \code{window}, \code{window_source}, \code{n_quadrature}, \code{n_dummy}, \code{log_pseudolikelihood}, \code{log_pseudolikelihood_poisson}, \code{iterations}, \code{converged}, \code{valid_density}, \code{interaction}.
}
\references{
Strauss, D. J. (1975) Biometrika 62(2), 467-475; Besag, J. (1977) Bull.
ISI 47(2), 77-92; Baddeley, A. & Turner, R. (2000) ANZJS 42(3), 283-322.
}
\keyword{internal}
