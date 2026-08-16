% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_fg_newton}
\alias{.morie_fg_newton}
\title{.morie_fg_newton}
\usage{
.morie_fg_newton(t, e, X, competing, Gfun, Gi, max_iter = 50L, tol = 1e-09)
}
\arguments{
\item{t}{A vector; indexed elementwise.}
\item{e}{Passed to \code{==}.}
\item{X}{A matrix; indexed by row and column.}
\item{competing}{Passed to \code{&}.}
\item{Gfun}{Accepted by the signature and not used anywhere in the body.}
\item{Gi}{Numeric; combined arithmetically in the body.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{50L}.}
\item{tol}{Passed to \code{<}. Defaults to \code{1e-09}.}
}
\description{
Part of the competing_risks_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{beta}, \code{loglik}, \code{I}, \code{U}.
}
\keyword{internal}
