% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.otis_overlay_wrap}
\alias{.otis_overlay_wrap}
\title{Internal helper: Otis Overlay Wrap}
\usage{
.otis_overlay_wrap(title, summary_lines, tables = list(), interpretation = "", warnings = character(0), payload = NULL)
}
\description{
Internal helper: Otis Overlay Wrap
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.otis_overlay_wrap(V, V)
}
