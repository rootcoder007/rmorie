% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.frwil_lstsq}
\alias{.frwil_lstsq}
\title{.frwil_lstsq}
\usage{
.frwil_lstsq(M, y, ridge = 0)
}
\arguments{
\item{M}{A matrix; indexed by row and column.}
\item{y}{A vector; indexed elementwise.}
\item{ridge}{Defaults to \code{0}.}
}
\description{
A step of the frwil_native implementation. Called by \code{.frwil_free_wilson}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\references{
----------
Free, S. M. & Wilson, J. W. (1964) "A mathematical contribution to
structure-activity studies", Journal of Medicinal Chemistry 7(4),
395-399, doi:10.1021/jm00334a001. The additive model above, the
indicator design over substituent positions, least-squares fitting,
and the singularity that forces a constraint.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.frwil_lstsq(D, V)
}
