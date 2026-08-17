% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dreamr}
\alias{morie_dreamr}
\title{morie_dreamr}
\usage{
morie_dreamr(state, action_model, transition, reward_model, value_model, horizon = 15, gamma = 0.99, lam = 0.95, estimator = "lambda", k = 1)
}
\arguments{
\item{state}{Passed to \code{morie_dreamr_imagine}.}
\item{action_model}{Passed to \code{morie_dreamr_imagine}.}
\item{transition}{Passed to \code{morie_dreamr_imagine}.}
\item{reward_model}{Passed to \code{morie_dreamr_imagine}.}
\item{value_model}{Passed to \code{morie_dreamr_imagine}.}
\item{horizon}{Passed to \code{morie_dreamr_imagine}. Defaults to \code{15}.}
\item{gamma}{Passed to \code{morie_dreamr_lambda_return}. Defaults to \code{0.99}.}
\item{lam}{Passed to \code{morie_dreamr_lambda_return}. Defaults to \code{0.95}.}
\item{estimator}{Passed to \code{morie_dreamr_lambda_return}. Defaults to \code{"lambda"}.}
\item{k}{Passed to \code{morie_dreamr_lambda_return}. Defaults to \code{1}.}
}
\description{
A step of the dreamr_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{returns}, \code{objective}, \code{value_loss}, \code{residual}, \code{states}, \code{actions}, \code{rewards}, \code{values}, \code{horizon}, \code{gamma}, \code{lam}, \code{estimator}, \code{method}.
}
\keyword{internal}
\examples{
am <- function(s) 1
tr <- function(s, a) s * 0.9 + 0.1
rm <- function(s) sum(s)
vm <- function(s) sum(s) * 2
r <- morie_dreamr(c(1, 0.5), am, tr, rm, vm, horizon = 5)
str(r, max.level = 1)
}
