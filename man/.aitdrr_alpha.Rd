% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.aitdrr_alpha}
\alias{.aitdrr_alpha}
\title{.aitdrr_alpha}
\usage{
.aitdrr_alpha(Xm, B)
}
\description{
.aitdrr_alpha
}
\value{
The value of \code{A}, as built in the body.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
.aitdrr_alpha(M, D)
}
