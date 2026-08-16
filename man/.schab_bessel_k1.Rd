% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_bessel_k1}
\alias{.schab_bessel_k1}
\title{K_1(z) = integral_0^inf exp{-z cosh u} cosh u du}
\usage{
.schab_bessel_k1(z, upper = 40, n_quad = 400L)
}
\arguments{
\item{z}{See Usage.}
\item{upper}{Defaults to \code{40}.}
\item{n_quad}{Defaults to \code{400L}.}
}
\description{
Part of the schab_st_shared implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
