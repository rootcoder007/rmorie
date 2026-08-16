% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.frwil_prep}
\alias{.frwil_prep}
\title{.frwil_prep}
\usage{
.frwil_prep(compounds, activity)
}
\arguments{
\item{compounds}{Iterated over elementwise, with \code{lapply}.}
\item{activity}{See Usage.}
}
\description{
A step of the frwil_native implementation. Called by \code{.frwil_free_wilson}. See the file header for the source the module follows.
}
\value{
A list with \code{C}, \code{y}, \code{k}.
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
.frwil_prep(V, V)
}
