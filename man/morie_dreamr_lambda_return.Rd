% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dreamr_lambda_return}
\alias{morie_dreamr_lambda_return}
\title{morie_dreamr_lambda_return}
\usage{
morie_dreamr_lambda_return(rewards, values, gamma = 0.99, lam = 0.95, estimator = "lambda", 
    k = 1) 
}
\arguments{
\item{rewards}{Passed to \code{.dreamr_vec}.}
\item{values}{Passed to \code{.dreamr_vec}.}
\item{gamma}{Numeric; combined arithmetically in the body. Defaults to \code{0.99}.}
\item{lam}{Numeric; combined arithmetically in the body. Defaults to \code{0.95}.}
\item{estimator}{One of \code{"k-step"}, \code{"lambda"}, \code{"reward"}. Defaults to \code{"lambda"}.}
\item{k}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.}
}
\description{
A step of the dreamr_native implementation. Called by \code{morie_dreamr}. See the file header for the source the module follows.
}
\value{
The value of \code{.dreamr_pack}.
}
\keyword{internal}
\examples{
r <- morie_dreamr_lambda_return(rewards = c(1, 0.8, 0.6, 0.4),
                                values = c(2, 1.6, 1.2, 0.8, 0.4))
str(r, max.level = 1)
}
