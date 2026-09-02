% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_unclr_lr_impute}
\alias{morie_unclr_lr_impute}
\title{morie_unclr_lr_impute}
\usage{
morie_unclr_lr_impute(X, dl, n_iter, draw = NULL) 
}
\arguments{
\item{X}{A matrix; passed to \code{as.matrix}.}
\item{dl}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_iter}{Coerced to integer by the body, with \code{as.integer}.}
\item{draw}{Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.}
}
\description{
A step of the unclr implementation. Called by \code{Lrda}, \code{Lrem}. See the file header for the source the module follows.
}
\value{
A list with \code{X}, \code{n}, \code{n_parts}, \code{n_iter}, \code{n_censored}.
}
\keyword{internal}
\examples{
morie_unclr_lr_impute(X = c(1, 2, 3, 4, 5, 6, 7, 8), dl = 5L, n_iter = c(1, 2, 3, 4, 5, 6, 7, 8))
}
