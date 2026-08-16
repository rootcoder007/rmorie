% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tipsne_draw_beta}
\alias{.tipsne_draw_beta}
\title{Beta* ~ N(betahat, sigma2 (X\\\'X)^-1) via the Cholesky of the}
\usage{
.tipsne_draw_beta(e, beta, xtx_inv, sigma2_draw)
}
\arguments{
\item{e}{See Usage.}
\item{beta}{See Usage.}
\item{xtx_inv}{See Usage.}
\item{sigma2_draw}{See Usage.}
}
\description{
covariance The draw is coordinate by coordinate so the stream position matches the Python arm term for term.
}
\value{
A vector, from \code{vapply}.
}
\references{
  Rubin, D.B. (1987) "Multiple Imputation for Nonresponse in Surveys."
    Wiley. Chapter 3: the combining rules.
  Barnard, J. and Rubin, D.B. (1999) "Small-sample degrees of freedom
    with multiple imputation." Biometrika 86(4), 948-955.
  Yan, X., Lee, S., Ling, N. and Lin, J. (2021) tipping-point
    sensitivity analysis for MNAR departures in clinical trials; the
    delta-adjustment procedure here follows the now-standard regulatory
    form (impute under MAR, shift imputed values by delta per arm,
    re-analyse, locate the crossing).
}
\keyword{internal}
