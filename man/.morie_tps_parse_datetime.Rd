% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_tps_parse_datetime}
\alias{.morie_tps_parse_datetime}
\title{Internal helper: parse TPS datetime strings (ISO or US m/d/Y AM/PM)}
\usage{
.morie_tps_parse_datetime(x)
}
\description{
Live PSDP ArcGIS exports use ISO 8601 (\code{2017-05-20T19:05:58.000Z}); the bundled sample CSVs use US-style \code{4/15/2014 4:00:00 AM}. CRAN checks run against the samples, so both must parse.
}
\value{
The value of \code{dt}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_tps_parse_datetime(V)
}
