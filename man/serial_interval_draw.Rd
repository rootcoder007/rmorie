% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{serial_interval_draw}
\alias{serial_interval_draw}
\title{serial_interval_draw}
\usage{
serial_interval_draw(mean, sd, e, allow_presymptomatic = TRUE) 
}
\arguments{
\item{mean}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{sd}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{e}{Passed to \code{.ghc_norm}.}
\item{allow_presymptomatic}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the ttrace_native implementation. Called by \code{simulate_outbreak}. See the file header for the source the module follows.
}
\value{
The value of \code{v}, as built in the body.
}
\keyword{internal}
\examples{
e <- rmorie:::.ghc_rng(1)
replicate(3, serial_interval_draw(mean = 5, sd = 1.9, e))
}
