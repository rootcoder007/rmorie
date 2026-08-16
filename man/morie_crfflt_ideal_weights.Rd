% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_crfflt_ideal_weights}
\alias{morie_crfflt_ideal_weights}
\title{Ideal band pass weights}
\usage{
morie_crfflt_ideal_weights(p_low, p_high, n)
}
\arguments{
\item{p_low}{Numeric, lower period bound.}
\item{p_high}{Numeric, upper period bound.}
\item{n}{Integer, number of non-zero lag weights to return.}
}
\description{
Equation (1.3) of Christiano & Fitzgerald (2003): \code{B_0 = (b-a)/pi} and \code{B_j = (sin(jb) - sin(ja)) / (pi j)} for \code{j >= 1}, with \code{a = 2 pi / p_u} and \code{b = 2 pi / p_l}.
}
\value{
A list with \code{B}, \code{a}, \code{b}, \code{p_low},   \code{p_high}, \code{B0} and a note.
}
\keyword{internal}
