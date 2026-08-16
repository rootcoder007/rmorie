% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_encoder_state}
\alias{morie_kamath_encoder_state}
\title{Kamath Ch 2 encoder-decoder scaffolding (Eq 2.1-2.6)}
\usage{
morie_kamath_encoder_state(h_t_1, x_t, f = NULL)
}
\arguments{
\item{h_t_1,x_t}{Previous state and input.}
\item{f}{Optional cell.}
}
\description{
Kamath Ch 2 encoder-decoder scaffolding (Eq 2.1-2.6)
}
\value{
A list with \code{h}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
