% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.lggvls_logistic_fit}
\alias{.lggvls_logistic_fit}
\title{IRLS logistic regression (no package dependencies)}
\usage{
.lggvls_logistic_fit(X, y, max_iter = 25L, tol = 1e-08)
}
\arguments{
\item{X}{A matrix; passed to \code{nrow}.}
\item{y}{Numeric; combined arithmetically in the body.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{25L}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-08}.}
}
\description{
A step of the lggvls_native implementation. Called by \code{.lggvls_ip_weights}. See the file header for the source the module follows.
}
\value{
A list with \code{beta}, \code{p_hat}, \code{converged}.
}
\references{
----------
Robins, J. (1986) "A new approach to causal inference in mortality
studies with a sustained exposure period -- application to control of
the healthy worker survivor effect", Mathematical Modelling 7(9-12),
1393-1512, doi:10.1016/0270-0255(86)90088-6.

Hernan, M. A. & Robins, J. M. (2020) Causal Inference: What If, Boca
Raton: Chapman & Hall/CRC, Sec. 21.2 for the product-over-time weights
and Ch. 20 for why conditioning fails where weighting works.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.lggvls_logistic_fit(V, V)
}
