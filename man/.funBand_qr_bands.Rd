% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.funBand_qr_bands}
\alias{.funBand_qr_bands}
\title{Banded Q (n x n-2) and R (n-2 x n-2) of the natural cubic spline}
\usage{
.funBand_qr_bands(x)
}
\arguments{
\item{x}{A vector; its length is taken.}
}
\description{
A step of the funBand_native implementation. Called by \code{.funBand_roughness}. See the file header for the source the module follows.
}
\value{
A list with \code{Q}, \code{R}.
}
\references{
Wahba, G. (1983) "Bayesian \'confidence intervals\' for the cross-validated
smoothing spline", Journal of the Royal Statistical Society Series B
(Methodological) 45(1), 133-150, doi:10.1111/j.2517-6161.1983.tb01239.x.
Craven, P. and Wahba, G. (1979) "Smoothing noisy data with spline functions",
Numerische Mathematik 31(4), 377-403, doi:10.1007/BF01404567.
Green, P. J. and Silverman, B. W. (1994) Nonparametric Regression and
Generalized Linear Models, Chapman and Hall, ISBN 978-0-412-30040-0, Sec 2.1.2.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.funBand_qr_bands(V)
}
