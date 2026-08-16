% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_pql_score}
\alias{.schab_pql_score}
\title{.schab_pql_score}
\usage{
.schab_pql_score(z, X, beta, S, Sigma_S, family, link_kind, sigma2 = 1, R = NULL)
}
\arguments{
\item{z}{See Usage.}
\item{X}{See Usage.}
\item{beta}{See Usage.}
\item{S}{See Usage.}
\item{Sigma_S}{See Usage.}
\item{family}{See Usage.}
\item{link_kind}{See Usage.}
\item{sigma2}{Defaults to \code{1}.}
\item{R}{Defaults to \code{NULL}.}
}
\description{
Part of the schab_glmm_shared implementation; see the file header for the source it follows.
}
\value{
A list with \code{score_beta}, \code{score_S}.
}
\keyword{internal}
