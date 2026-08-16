% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_xml_find_all}
\alias{.morie_xml_find_all}
\title{Internal helper: find elements by tag in a native XML tree}
\usage{
.morie_xml_find_all(node, tag)
}
\description{
Internal helper: find elements by tag in a native XML tree
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.morie_xml_find_all(D, V)
}
