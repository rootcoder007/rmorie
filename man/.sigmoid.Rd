% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sigmoid}
\alias{.sigmoid}
\title{.sigmoid}
\usage{
.sigmoid(z)
}
\arguments{
\item{z}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the mambss_native implementation. Called by \code{gated_rnn_equivalent}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.sigmoid(V)
}
