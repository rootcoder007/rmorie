% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_bnsadt}
\alias{morie_bnsadt}
\title{Publication-bias correction with its adversarial bound}
\usage{
morie_bnsadt(y, D, family = "symmetric_step", grid = NULL, target = NULL, target_se = NULL, fit = TRUE, iters = 600L)
}
\arguments{
\item{y}{Published point estimates, one per study.}
\item{D}{Their standard errors. Required and positive: the model is   identified BY the variation in sigma across studies, so a constant   or absent sigma is not a smaller version of this problem, it is a   different one.}
\item{family}{A name of the selection family.}
\item{grid}{Publication probabilities to sweep for the adversarial   bound. The default runs from 1 down to 0.05, spanning "nothing was   suppressed" to "insignificant work was almost never published".}
\item{target}{The study to correct; the largest absolute z by   default, which is the one selection distorts most and so the one   the bound is most informative about.}
\item{target_se}{Its standard error; taken from D when target is   defaulted.}
\item{fit}{Estimate the selection parameters by maximum likelihood as   well as sweeping the family.}
\item{iters}{Nelder-Mead iterations.}
}
\description{
Publication-bias correction with its adversarial bound
}
\value{
A list with the fit, the corrected estimate under it, and the   adversarial interval over the family with the beta attaining each   end.
}
\keyword{internal}
