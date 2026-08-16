% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tps_coords}
\alias{.tps_coords}
\title{Internal helper: Tps Coords}
\usage{
.tps_coords(df, lat_col, lon_col)
}
\description{
Internal helper: Tps Coords
}
\value{
A matrix, from \code{as.matrix}.
}
\keyword{internal}
\examples{
.tps_coords(df = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), lat_col = c(1, 2, 3, 4, 5, 6, 7, 8), lon_col = c(1, 2, 3, 4, 5, 6, 7, 8))
}
