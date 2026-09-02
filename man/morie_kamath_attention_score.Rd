% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_attention_score}
\alias{morie_kamath_attention_score}
\title{Kamath Ch 2 attention chain (Eq 2.7-2.12, 2.15-2.16, 2.19)}
\usage{
morie_kamath_attention_score(q, k_i, alpha = "scaled_dot") 
}
\arguments{
\item{q,k_i}{Query and key.}
\item{alpha}{Score family or function.}
}
\description{
Kamath Ch 2 attention chain (Eq 2.7-2.12, 2.15-2.16, 2.19)
}
\value{
A list with \code{estimate}, \code{alpha}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_kamath_attention_score(V, V)
}
