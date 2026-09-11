% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_groebn}
\alias{morie_groebn}
\title{morie_groebn}
\usage{
morie_groebn(polys, order = "lex", prune = TRUE, reduced = TRUE) 
}
\arguments{
\item{polys}{Iterated over elementwise, with \code{lapply}.}
\item{order}{Passed to \code{.groebn_buchberger}. Defaults to \code{"lex"}.}
\item{prune}{Passed to \code{.groebn_buchberger}. Defaults to \code{TRUE}.}
\item{reduced}{Passed to \code{.groebn_buchberger}. Defaults to \code{TRUE}.}
}
\description{
Entry point ---------------------------------------------------------------------------
}
\value{
The value of \code{.groebn_buchberger}.
}
\keyword{internal}
\examples{
# <x^3 - 2xy, x^2 y - 2y^2 + x>; a name "i_j" is the monomial x^i y^j
f1 <- .groebn_poly(list("3_0" = 1, "1_1" = -2))
f2 <- .groebn_poly(list("2_1" = 1, "0_2" = -2, "1_0" = 1))
res <- morie_groebn(list(f1, f2), order = "grlex")
res$size
# the published basis for this ideal is {x^2, xy, y^2 - x/2}
lapply(res$basis, names)
}
