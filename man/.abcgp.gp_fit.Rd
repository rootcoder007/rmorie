% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcgp.gp_fit}
\alias{.abcgp.gp_fit}
\title{.abcgp.gp_fit}
\usage{
.abcgp.gp_fit(design, values, nugget = NULL, lengthscale = NULL, kernel = "sqexp", tau2 = NULL)
}
\arguments{
\item{design}{A matrix; passed to \code{as.matrix}.}
\item{values}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{nugget}{Passed to \code{.abcgp.mle_lengthscale}.}
\item{lengthscale}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{kernel}{Passed to \code{.abcgp.mle_lengthscale}. Defaults to \code{"sqexp"}.}
\item{tau2}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the abcgp_native implementation. Called by \code{.abcgp.history_match}. See the file header for the source the module follows.
}
\value{
A list with \code{design}, \code{values}, \code{beta}, \code{tau2}, \code{lengthscale}, \code{kernel}, \code{nugget}, \code{chol}, \code{Ainv_r}, \code{Ainv_H}, \code{H}, \code{HtAinvH_chol}, \code{n}, \code{q}, \code{dim}.
}
\keyword{internal}
