% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sv_dl_insert}
\alias{.sv_dl_insert}
\title{Outer distance: left-most start to right-most end}
\usage{
.sv_dl_insert(p)
}
\arguments{
\item{p}{A list; the body reads \code{$len2}, \code{$pos1}, \code{$pos2} from it.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{.sv_dl_size}, \code{morie_sv_dl_classify_pair}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.sv_dl_insert(D)
}
