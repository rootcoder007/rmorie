% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ctmle}
\alias{ctmle}
\title{ctmle}
\usage{
ctmle(A, Y, Q1, Q0, W, g_models, V = 5L, seed = 0L, penalty = TRUE) 
}
\arguments{
\item{A}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Q1}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Q0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{W}{Passed to \code{candidate_sequence}.}
\item{g_models}{Passed to \code{candidate_sequence}.}
\item{V}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{5L}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0L}.}
\item{penalty}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the tlctmle_native implementation. Called by \code{morie_tlctmle}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{psi}, \code{selected}, \code{selected_covariates}, \code{cv_risks}, \code{cv_losses}, \code{variance_penalties}, \code{penalized}, \code{epsilon}, \code{se}, \code{ci}, \code{mean_eic}, \code{solves_eic}, \code{max_clever_covariate}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
n <- 100
W <- matrix(rnorm(n * 3), n, 3)
A <- rbinom(n, 1, plogis(W[, 1]))
Y <- rbinom(n, 1, plogis(0.5 * A + W[, 2]))
Q1 <- plogis(0.5 + W[, 2])
Q0 <- plogis(W[, 2])
g_models <- list(1, c(1, 2), c(1, 2, 3))
ctmle(A, Y, Q1, Q0, W, g_models, V = 3L)
}
