% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_aft_common}
\alias{.morie_aft_common}
\title{.morie_aft_common}
\usage{
.morie_aft_common(time, event, X, family, title, method, ...)
}
\arguments{
\item{time}{Passed to \code{.morie_cox_prepare}.}
\item{event}{Passed to \code{.morie_cox_prepare}.}
\item{X}{Passed to \code{.morie_cox_prepare}.}
\item{family}{Passed to \code{.morie_aft_fit}.}
\item{title}{Passed to \code{.morie_aft_result}.}
\item{method}{Passed to \code{.morie_aft_result}.}
\item{...}{Passed through.}
}
\description{
A step of the aft_native implementation. Called by \code{morie_aft_generalized_gamma}, \code{morie_aft_log_logistic}, \code{morie_aft_weibull}. See the file header for the source the module follows.
}
\value{
The value of \code{.morie_aft_result}.
}
\keyword{internal}
