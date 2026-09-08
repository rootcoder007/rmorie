% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{DiffEnt}
\alias{DiffEnt}
\title{Eq (3.6): H = - integral p log2(p) d eta, in bits.  p log p -> 0 as}
\usage{
DiffEnt(pdf = NULL, x = NULL, lower = -Inf, upper = Inf) 
}
\arguments{
\item{pdf}{Passed to \code{.morie_rg_pdfint}.}
\item{x}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{lower}{Passed to \code{.morie_rg_pdfint}. Defaults to \code{-Inf}.}
\item{upper}{Passed to \code{.morie_rg_pdfint}. Defaults to \code{Inf}.}
}
\description{
p -> 0, so zero-density points contribute nothing.  This is a density inside the log, so unlike eq (3.11) the result may be negative.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
xg <- seq(-6, 6, by = 0.01)
DiffEnt(x = xg, p = dnorm(xg))
}
