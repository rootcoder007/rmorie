% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_fauzi_agamma}
\alias{.morie_fauzi_agamma}
\title{`A_h(v)` of Eq. (1.9): the sample mean of the}
\usage{
.morie_fauzi_agamma(x, v, h)
}
\description{
Gamma(shape = `h^(-1/2)`, scale = `v sqrt(h) + h`) density. NOT Chen\'s gamma kernel, which takes shape `v/h + 1` and scale `h`. The book fixes the SHAPE at `h^(-1/2)` and moves the scale; that single change buys the smaller variance orders of Remark 1.2 and costs the bias its rate, taking it from `O(h)` up to `O(sqrt(h))` in (1.10) -- which the geometric extrapolation (1.14) then buys back.
}
\value{
A vector.
}
\keyword{internal}
