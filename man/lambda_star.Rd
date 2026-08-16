% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{lambda_star}
\alias{lambda_star}
\title{lambda_star}
\usage{
lambda_star(dist, hi = 20, tol = 1e-14, max_iter = 300)
}
\arguments{
\item{dist}{A vector; indexed elementwise.}
\item{hi}{Numeric; combined arithmetically in the body. Defaults to \code{20}.}
\item{tol}{Defaults to \code{1e-14}.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{300}.}
}
\description{
A step of the blstn_native implementation. Called by \code{morie_karlin_altschul}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
