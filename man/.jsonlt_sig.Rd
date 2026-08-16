% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.jsonlt_sig}
\alias{.jsonlt_sig}
\title{I(n) as a digits argument means n SIGNIFICANT digits, matching}
\usage{
.jsonlt_sig(digits)
}
\arguments{
\item{digits}{See Usage.}
}
\description{
jsonlite\\'s digits = I(n). Anything else is decimal places.
}
\value{
The value of \code{inherits}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.jsonlt_sig(V)
}
