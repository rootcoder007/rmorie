% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4d_wp_split}
\alias{.morie_w4d_wp_split}
\title{.morie_w4d_wp_split}
\usage{
.morie_w4d_wp_split(word)
}
\arguments{
\item{word}{Character; passed to \code{strsplit}.}
}
\description{
A step of the geron_w4d_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
S <- c("a", "b", "c")
.morie_w4d_wp_split(S)
}
