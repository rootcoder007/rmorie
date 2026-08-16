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
\item{A}{See Usage.}
\item{Y}{See Usage.}
\item{Q1}{See Usage.}
\item{Q0}{See Usage.}
\item{W}{See Usage.}
\item{g_models}{See Usage.}
\item{V}{Defaults to \code{5L}.}
\item{seed}{Defaults to \code{0L}.}
\item{penalty}{Defaults to \code{TRUE}.}
}
\description{
Part of the tlctmle_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{psi}, \code{selected}, \code{selected_covariates}, \code{cv_risks}, \code{cv_losses}, \code{variance_penalties}, \code{penalized}, \code{epsilon}, \code{se}, \code{ci}, \code{mean_eic}, \code{solves_eic}, \code{max_clever_covariate}, \code{method}, \code{note}.
}
\keyword{internal}
