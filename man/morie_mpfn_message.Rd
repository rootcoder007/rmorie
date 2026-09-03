% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mpfn_message}
\alias{morie_mpfn_message}
\title{Eq. (1)\'s M_t}
\usage{
morie_mpfn_message(h_v, h_w, e_vw, A = NULL) 
}
\arguments{
\item{h_v}{Source node state (unused; kept for API symmetry).}
\item{h_w}{Neighbour state.}
\item{e_vw}{Edge feature (scalar when A is NULL).}
\item{A}{Optional edge network: a function of e_vw returning a matrix.}
}
\description{
Eq. (1)\'s M_t
}
\value{
A message vector.
}
\keyword{internal}
\examples{
morie_mpfn_message(h_v = c(1, 2, 3, 4, 5, 6, 7, 8), h_w = c(1, 2, 3, 4, 5, 6, 7, 8),
  e_vw = c(1, 2, 3, 4, 5, 6, 7, 8))
}
