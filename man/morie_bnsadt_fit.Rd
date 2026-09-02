% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_bnsadt_fit}
\alias{morie_bnsadt_fit}
\title{Maximum likelihood over the selection model, by Nelder-Mead}
\usage{
morie_bnsadt_fit(x, sigma, family = "symmetric_step", mu0 = NULL, tau0 = NULL, 
    beta0 = 0.5, iters = 600L) 
}
\arguments{
\item{x}{Published point estimates.}
\item{sigma}{Their standard errors.}
\item{family}{A name of the selection family.}
\item{mu0}{Starting mean; the sample mean by default.}
\item{tau0}{Starting heterogeneity; a method-of-moments value by   default.}
\item{beta0}{Starting publication probability.}
\item{iters}{Nelder-Mead iterations.}
}
\description{
tau and every beta are optimised on the log scale, so the simplex cannot step to a negative variance or a negative probability and the run needs no penalty term to stay inside the parameter space. Betas are squashed by the logistic, which bounds them by 1 -- a publication probability above the normalisation would be the likelihood saying the significant region is SUPPRESSED, which the family does not describe. Two guards, both statements about identification and not numerical tape: a group with no observed studies is HELD at 1 and reported in `unidentified`, because its beta is bounded only from above; and tau is floored at 1e-6 times the mean standard error, below which the between-study spread is far under the within-study noise and the likelihood is flat in it. The floor is reported in `tau_at_floor` rather than hidden.
}
\value{
A list with mu, tau, the betas and the log-likelihood.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_bnsadt_fit(M, V)
}
