% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{whole_part_subpart}
\alias{whole_part_subpart}
\title{whole_part_subpart}
\usage{
whole_part_subpart(masks, target_hierarchy = NULL)
}
\arguments{
\item{masks}{A vector; its length is taken and its elements indexed.}
\item{target_hierarchy}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the sammkr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{assignment}, \code{sizes}, \code{nested}, \code{note}.
}
\keyword{internal}
\examples{
whole_part_subpart(list(c(1, 1, 1, 0), c(1, 1, 0, 0), c(1, 0, 0, 0)))
}
