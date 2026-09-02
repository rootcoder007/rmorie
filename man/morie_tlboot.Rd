% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_tlboot}
\alias{morie_tlboot}
\title{morie_tlboot}
\usage{
morie_tlboot(data, estimator, B = 200L, seed = 0L, method = c("naive", "targeted", 
    "multiplier"), ic = NULL) 
}
\arguments{
\item{data}{Coerced to list by the body, with \code{as.list}.}
\item{estimator}{Accepted by the signature and not used anywhere in the body.}
\item{B}{A vector; its length is taken. Defaults to \code{200L}.}
\item{seed}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0L}.}
\item{method}{One of \code{"naive"}, \code{"targeted"}.}
\item{ic}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the tlboot_native implementation. Called by \code{multiplier_bootstrap}, \code{naive_bootstrap}, \code{targeted_bootstrap}. See the file header for the source the module follows.
}
\value{
A list with \code{replicates}, \code{mean}, \code{se}, \code{influence_curve_se}, \code{ratio}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(1)
d <- rnorm(50, 1)
r <- morie_tlboot(d, estimator = function(s) mean(unlist(s)),
                  B = 200L, method = "naive")
str(r, max.level = 1)
}
