% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Svmkkt}
\alias{Svmkkt}
\title{KKT conditions of the support vector classifier (MVSML eqs. 9.38-9.43)}
\usage{
Svmkkt(X, y, beta0, beta, alpha, delta, zeta, T)
}
\description{
(9.38) L = (1/2)||beta||^2 + T sum_i zeta_i - sum_i alpha_i [ y_i(beta_0 + x_i beta) - 1 + zeta_i ] - sum_i delta_i zeta_i, with residuals (9.39) beta - sum alpha_i y_i x_i, (9.40) sum alpha_i y_i, (9.41) alpha_i + delta_i - T, (9.42) alpha_i [ y_i(beta_0 + x_i beta) - 1 + zeta_i ] and (9.43) delta_i zeta_i. The printed sign of the delta term on p.356 is inconsistent with the book\'s own (9.41), which states dL/dzeta_i = T - alpha_i - delta_i; that requires the term to enter with a minus, and it does so here.
}
\value{
A list with \code{L}, \code{stationarity_beta}, \code{balance}, \code{multiplier_sum}, \code{complementary_alpha}, \code{complementary_delta}, \code{max_residual}, \code{kkt_satisfied}.
}
\keyword{internal}
