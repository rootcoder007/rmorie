% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_kamath_positional_sin}
\alias{morie_kamath_positional_sin}
\title{Positional encodings, FFN, layer norm (Eq 2.13-2.14, 2.17-2.18)}
\usage{
morie_kamath_positional_sin(i, j, d)
}
\arguments{
\item{i}{Position.}
\item{j}{Frequency index.}
\item{d}{Model width.}
}
\description{
Positional encodings, FFN, layer norm (Eq 2.13-2.14, 2.17-2.18)
}
\value{
A list with \code{estimate}, \code{wavelength}, \code{n}, \code{method}.
}
\keyword{internal}
