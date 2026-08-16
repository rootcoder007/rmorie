% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tmlspl_frac_treated}
\alias{.tmlspl_frac_treated}
\title{SPDX-License-Identifier: AGPL-3.0-or-later}
\usage{
.tmlspl_frac_treated(D, A)
}
\arguments{
\item{D}{A vector; its length is taken and its elements indexed.}
\item{A}{A matrix; indexed by row and column.}
}
\description{
# Default exposure summary: fraction of neighbours treated.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.tmlspl_frac_treated(D, M)
}
