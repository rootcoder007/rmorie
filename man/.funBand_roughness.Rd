% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.funBand_roughness}
\alias{.funBand_roughness}
\title{K = Q R^-1 Q\\', symmetric PSD with a two-dimensional null space}
\usage{
.funBand_roughness(x)
}
\arguments{
\item{x}{Passed to \code{.funBand_qr_bands}.}
}
\description{
A step of the funBand_native implementation. Called by \code{morie_funBand_influence_matrix}. See the file header for the source the module follows.
}
\value{
A numeric value.
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
