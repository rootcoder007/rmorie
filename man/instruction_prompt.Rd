% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{instruction_prompt}
\alias{instruction_prompt}
\title{instruction_prompt}
\usage{
instruction_prompt(symbolic, kind = "conversation")
}
\arguments{
\item{symbolic}{See Usage.}
\item{kind}{Defaults to \code{"conversation"}.}
}
\description{
Part of the llavx_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{prompt}, \code{kind}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
instruction_prompt(D)
}
