% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.baygsl_expo}
\alias{.baygsl_expo}
\title{Exp(1) on the log scale without underflow. Mirrors baygsl._expo: keep}
\usage{
.baygsl_expo(e)
}
\arguments{
\item{e}{See Usage.}
}
\description{
drawing uniforms until we get one strictly positive, then take -log(u). R\\\'s qexp would consume the rng from somewhere else, so the loop matches the Python arm draw-for-draw.
}
\value{
A numeric value.
}
\keyword{internal}
