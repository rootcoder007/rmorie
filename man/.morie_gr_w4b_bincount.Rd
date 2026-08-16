% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_w4b_bincount}
\alias{.morie_gr_w4b_bincount}
\title{.morie_gr_w4b_bincount}
\usage{
.morie_gr_w4b_bincount(x, w, minlength)
}
\arguments{
\item{x}{A vector; its length is taken.}
\item{w}{See Usage.}
\item{minlength}{A count; the body uses it as \code{numeric(...)}.}
}
\description{
A step of the geron_w4b_native implementation. Called by \code{.morie_gr_w4b_hgb_best_split}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
.morie_gr_w4b_bincount(x = c(1, 2, 3, 4, 5, 6, 7, 8), w = c(1, 2, 3, 4, 5, 6, 7, 8), minlength = 5L)
}
