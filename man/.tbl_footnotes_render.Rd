% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.tbl_footnotes_render}
\alias{.tbl_footnotes_render}
\title{Internal helper: Tbl Footnotes Render}
\usage{
.tbl_footnotes_render(reg, fmt = "text")
}
\description{
Internal helper: Tbl Footnotes Render
}
\value{
A character value.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.tbl_footnotes_render(D)
}
