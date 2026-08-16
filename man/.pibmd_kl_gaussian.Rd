% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.pibmd_kl_gaussian}
\alias{.pibmd_kl_gaussian}
\title{KL(q || p) for two normals -- exact when both are normal}
\usage{
.pibmd_kl_gaussian(mq, sq2, mp, sp2)
}
\arguments{
\item{mq}{Numeric; combined arithmetically in the body.}
\item{sq2}{Numeric; passed to \code{max}.}
\item{mp}{Numeric; combined arithmetically in the body.}
\item{sp2}{Numeric; passed to \code{max}.}
}
\description{
A step of the pibmd_native implementation. Called by \code{morie_pibmd_prior_informativeness_bias_diagnostic}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
.pibmd_kl_gaussian(mq = c(1, 2, 3, 4, 5, 6, 7, 8), sq2 = c(1, 2, 3, 4, 5, 6, 7, 8), mp = c(1, 2, 3, 4, 5, 6, 7, 8), sp2 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
