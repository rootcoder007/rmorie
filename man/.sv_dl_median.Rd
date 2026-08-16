% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sv_dl_median}
\alias{.sv_dl_median}
\title{.sv_dl_median}
\usage{
.sv_dl_median(v)
}
\arguments{
\item{v}{Numeric; passed to \code{sort}.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_insert_size_stats}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.sv_dl_median(V)
}
