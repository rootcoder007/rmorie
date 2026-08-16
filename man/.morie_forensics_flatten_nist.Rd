% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_forensics_flatten_nist}
\alias{.morie_forensics_flatten_nist}
\title{Internal helper: Morie Forensics Flatten Nist}
\usage{
.morie_forensics_flatten_nist(rec)
}
\description{
Internal helper: Morie Forensics Flatten Nist
}
\value{
A list with \code{dataset_id}, \code{title}, \code{description}, \code{publisher}, \code{issued}, \code{modified}, \code{keyword}, \code{landing_page}, \code{size_bytes}, \code{license}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.morie_forensics_flatten_nist(D)
}
