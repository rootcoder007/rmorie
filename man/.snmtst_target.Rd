% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.snmtst_target}
\alias{.snmtst_target}
\title{.snmtst_target}
\usage{
.snmtst_target(post, l_vec)
}
\arguments{
\item{post}{A vector; its length is taken.}
\item{l_vec}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the snmtst_native implementation. Called by \code{identified_set}. See the file header for the source the module follows.
}
\value{
The value of \code{lv}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.snmtst_target(V, V)
}
