% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_qr_mgs}
\alias{.s4_qr_mgs}
\title{# Thin QR by modified Gram-Schmidt. R diagonal is non-negative by}
\usage{
.s4_qr_mgs(A)
}
\arguments{
\item{A}{A matrix; passed to \code{nrow}.}
}
\description{
# construction, so Q is unique and there is no sign convention left for # the two arms to disagree about (LAPACK and LINPACK differ here).
}
\value{
A list with \code{Q}, \code{R}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.s4_qr_mgs(V)
}
