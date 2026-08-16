% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.forecast}
\alias{.forecast}
\title{Iterate the state equations with epsilon = 0. Future innovations}
\usage{
.forecast(spec, theta, x0, z, h, long_run_b = 0)
}
\arguments{
\item{spec}{A list; the body reads \code{$harmonics}, \code{$p}, \code{$periods}, \code{$q}, \code{$use_trend} from it.}
\item{theta}{Passed to \code{.unpack}.}
\item{x0}{See Usage.}
\item{z}{See Usage.}
\item{h}{A count; the body uses it as \code{seq_len(...)}.}
\item{long_run_b}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
have mean zero, so the point forecast just runs equations 3b-3f forward from the final state. The MA terms die after q steps; the AR recursion on d continues.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.forecast(spec = c(1, 2, 3, 4, 5, 6, 7, 8), theta = 0.5, x0 = c(1, 2, 3, 4, 5, 6, 7, 8), z = c(1, 2, 3, 4, 5, 6, 7, 8), h = 0.5)
}
