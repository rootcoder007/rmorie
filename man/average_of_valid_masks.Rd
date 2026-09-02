% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{average_of_valid_masks}
\alias{average_of_valid_masks}
\title{average_of_valid_masks}
\usage{
average_of_valid_masks(masks) 
}
\arguments{
\item{masks}{A vector; its length is taken.}
}
\description{
A step of the sammkr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{mask}, \code{ambiguous_fraction}, \code{n_averaged}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
average_of_valid_masks(V)
}
