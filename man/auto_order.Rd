% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{auto_order}
\alias{auto_order}
\title{auto_order}
\usage{
auto_order(y, X = NULL, d = 0, D = 0, s = 1, method = "css", max_steps = 20) 
}
\arguments{
\item{y}{Passed to \code{.try_fit}.}
\item{X}{Passed to \code{.try_fit}.}
\item{d}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{D}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
\item{s}{Passed to \code{.try_fit}. Defaults to \code{1}.}
\item{method}{Passed to \code{.try_fit}. Defaults to \code{"css"}.}
\item{max_steps}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{20}.}
}
\description{
A step of the sarimax_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{aic}, \code{fit}, \code{order}, \code{seasonal_order}, \code{constant}, \code{steps}, \code{n_models_tried}, \code{tried}, \code{s}, \code{search_method}, \code{differencing_note}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
aic(V, V)
auto_order(y = V)
}
