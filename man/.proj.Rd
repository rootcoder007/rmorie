% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.proj}
\alias{.proj}
\title{.proj}
\usage{
.proj(row, W, b = NULL)
}
\arguments{
\item{row}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{W}{A matrix; passed to \code{as.matrix}.}
\item{b}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the berte_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
