% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bartlett_se}
\alias{bartlett_se}
\title{bartlett_se}
\usage{
bartlett_se(rho, n) 
}
\arguments{
\item{rho}{A list; the body reads \code{$1} from it.}
\item{n}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the sarima_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{variance}, \code{se}, \code{white_noise_se}.
}
\keyword{internal}
\examples{
set.seed(1)
rho <- sample_acf(rnorm(150), c(1, 11, 12, 13))
se <- bartlett_se(rho, 150)
is.numeric(se) || is.list(se)
}
