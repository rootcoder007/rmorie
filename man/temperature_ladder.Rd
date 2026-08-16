% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{temperature_ladder}
\alias{temperature_ladder}
\title{temperature_ladder}
\usage{
temperature_ladder(n_steps, kind = "geometric", power = 1)
}
\arguments{
\item{n_steps}{See Usage.}
\item{kind}{One of \code{"geometric"}, \code{"power"}, \code{"prior"}. Defaults to \code{"geometric"}.}
\item{power}{Defaults to \code{1}.}
}
\description{
A step of the smcsam_native implementation. Called by \code{smcsam}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
\examples{
temperature_ladder(n_steps = 5L)
}
