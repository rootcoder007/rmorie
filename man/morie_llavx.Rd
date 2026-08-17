% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_llavx}
\alias{morie_llavx}
\title{morie_llavx}
\usage{
morie_llavx(op, ...)
}
\arguments{
\item{op}{A vector; its length is taken.}
\item{...}{Passed through.}
}
\description{
A step of the llavx_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{switch}.
}
\keyword{internal}
\examples{
r <- morie_llavx("training_stage", 2)
r$stage == 2L
}
