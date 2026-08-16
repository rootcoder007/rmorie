% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Lstm}
\alias{Lstm}
\title{Lstm}
\usage{
Lstm(sequences, labels = NULL, hidden = 8, ridge = 1e-06, seed = 1, weights = NULL)
}
\arguments{
\item{sequences}{A vector; its length is taken.}
\item{labels}{Optional; may be \code{NULL}. Passed to \code{.morie_bx_vec}.}
\item{hidden}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{8}.}
\item{ridge}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1e-06}.}
\item{seed}{Passed to \code{.morie_bx_rng}. Defaults to \code{1}.}
\item{weights}{Optional; may be \code{NULL}. A list; the body reads \code{$bias} from it.}
}
\description{
A step of the rangayyan_class2 implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{hidden}, \code{cell}, \code{predictions}, \code{accuracy}, \code{readout}, \code{classes}, \code{method}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
Lstm(D)
}
