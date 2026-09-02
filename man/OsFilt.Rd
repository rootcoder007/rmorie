% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{OsFilt}
\alias{OsFilt}
\title{OsFilt}
\usage{
OsFilt(x, window, kind = "median", alpha = 0, weights = NULL, order = NULL) 
}
\arguments{
\item{x}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{window}{Coerced to integer by the body, with \code{as.integer}.}
\item{kind}{One of \code{"l"}, \code{"order"}, \code{"trimmed"}. Defaults to \code{"median"}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
\item{weights}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{order}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the rangayyan_filt implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{y}, \code{n}, \code{window}, \code{kind}, \code{alpha}, \code{trimmed_each_end}, \code{order}, \code{nonlinear}, \code{no_frequency_response}, \code{edges}, \code{method}.
}
\keyword{internal}
\examples{
OsFilt(x = c(1, 2, 3, 4, 5, 6, 7, 8), window = 5L)
}
