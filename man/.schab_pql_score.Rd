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
\item{z}{Numeric; combined arithmetically in the body.}
\item{X}{A matrix; passed to \code{t}.}
\item{beta}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{S}{A matrix; passed to \code{solve}.}
\item{Sigma_S}{A matrix; passed to \code{as.matrix}.}
\item{family}{Passed to \code{.schab_data_covariance}.}
\item{link_kind}{Passed to \code{.schab_link}.}
\item{sigma2}{Passed to \code{.schab_data_covariance}. Defaults to \code{1}.}
\item{R}{Passed to \code{.schab_data_covariance}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{sppql}. See the file header for the source the module follows.
}
\value{
A list with \code{score_beta}, \code{score_S}.
}
\keyword{internal}
