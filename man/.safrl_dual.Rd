% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.safrl_dual}
\alias{.safrl_dual}
\title{.safrl_dual}
\usage{
.safrl_dual(q, r, S, c, delta, m, tol, max_iter)
}
\arguments{
\item{q}{See Usage.}
\item{r}{A vector; indexed elementwise.}
\item{S}{A matrix; indexed by row and column.}
\item{c}{A vector; indexed elementwise.}
\item{delta}{Numeric; combined arithmetically in the body.}
\item{m}{A count; the body uses it as \code{seq_len(...)}.}
\item{tol}{Passed to \code{<}.}
\item{max_iter}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the safrl_native implementation. Called by \code{morie_safrl}. See the file header for the source the module follows.
}
\value{
A list with \code{lam}, \code{nu}.
}
\keyword{internal}
