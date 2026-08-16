% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_tmle}
\alias{.s4_tmle}
\title{# One TMLE pass for a binary point treatment. W carries its intercept}
\usage{
.s4_tmle(y, D, W, gbound = 0.025)
}
\arguments{
\item{y}{A vector; its length is taken.}
\item{D}{Numeric; combined arithmetically in the body.}
\item{W}{A matrix; passed to \code{\%*\%}.}
\item{gbound}{Numeric; combined arithmetically in the body. Defaults to \code{0.025}.}
}
\description{
A step of the s04core implementation. Called by \code{Tmlfed}, \code{Tmlnte}, \code{Tmlper} and 2 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{psi}, \code{se}, \code{eps}, \code{g}, \code{H}, \code{Q1}, \code{Q0}, \code{ic}, \code{n}.
}
\keyword{internal}
