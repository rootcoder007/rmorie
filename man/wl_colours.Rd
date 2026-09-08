% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{wl_colours}
\alias{wl_colours}
\title{wl_colours}
\usage{
wl_colours(adj, n, rounds = 2, initial = NULL) 
}
\arguments{
\item{adj}{A vector; indexed elementwise.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{rounds}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.}
\item{initial}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the sortP_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{c}, as built in the body.
}
\keyword{internal}
\examples{
wl_colours(adj = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)), n = 5L)
}
