% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ghc_minimize_nm}
\alias{.ghc_minimize_nm}
\title{Internal: Nelder-Mead wrapper. The Python arm uses}
\usage{
.ghc_minimize_nm(objective, x0, maxit = NULL)
}
\arguments{
\item{objective}{See Usage.}
\item{x0}{A vector; its length is taken.}
\item{maxit}{Optional; may be \code{NULL}. Carried through into a list the body builds.}
}
\description{
_sci_core.minimize with method="Nelder-Mead"; optim(method= "Nelder-Mead") in base R runs the same algorithm and gives numerically equivalent results on smooth objectives like this one. The Python arm\\'s six-inner-iteration structure is preserved by the outer loop in morie_survvae(), which calls this helper up to six times per restart, each time from the current point -- exactly mirroring the "for _ in range(6)" loop in fit().
}
\value{
A list with \code{x}, \code{value}.
}
\keyword{internal}
