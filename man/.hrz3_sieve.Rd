% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hrz3_sieve}
\alias{.hrz3_sieve}
\title{Series basis on [0, 1], eq. (5.79).  "cos" is the orthonormal cosine}
\usage{
.hrz3_sieve(z, J, kind = "poly")
}
\arguments{
\item{z}{See Usage.}
\item{J}{See Usage.}
\item{kind}{Defaults to \code{"poly"}.}
}
\description{
basis {1, sqrt(2) cos(pi k v)}, for which the coefficients in (5.79) are literally the inner products beta_j = <g, psi_j> as the text notes.  "poly" is the monomial basis, which spans the same spaces but is not orthonormal.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
