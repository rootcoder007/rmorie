% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tqipb_blocks}
\alias{.tqipb_blocks}
\title{Split d coordinates into n_blocks as evenly as possible; the first}
\usage{
.tqipb_blocks(d, n_blocks)
}
\arguments{
\item{d}{Numeric; combined arithmetically in the body.}
\item{n_blocks}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
d \%\% n_blocks blocks get one extra, which is what a packer does when the dimension does not divide.
}
\value{
The value of \code{ifelse}.
}
\keyword{internal}
