% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ht_prof}
\alias{.ht_prof}
\title{Objective at b with a, mu and sigma all profiled out.  For fixed b}
\usage{
.ht_prof(xv, yv, b)
}
\arguments{
\item{xv}{See Usage.}
\item{yv}{See Usage.}
\item{b}{See Usage.}
}
\description{
the residual variance is a quadratic in a, so the minimising a is the ordinary-least-squares slope of u = y/x^b on v = x^(1-b) about their means.  Profiling a out ANALYTICALLY is what makes the two language arms agree: the likelihood is flat along the (a, b) ridge, and a numerical search there decides its steps on the last bits, which walked the arms 1e-8 apart.  The sums are accumulated in loops for the same reason -- R sums in long double, Python in double.
}
\value{
A list with \code{f}, \code{a}, \code{mu}, \code{sd}.
}
\keyword{internal}
