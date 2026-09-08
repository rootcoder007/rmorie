% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_prphet_fit}
\alias{morie_prphet_fit}
\title{morie_prphet_fit}
\usage{
morie_prphet_fit(t, y, n_changepoints = 10L, changepoint_range = 0.8, changepoints = NULL, 
    seasonalities = NULL, holidays = NULL, holiday_window = c(0, 0), changepoint_prior = 0.05, 
    ridge = 1e-08) 
}
\arguments{
\item{t}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{y}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n_changepoints}{Passed to \code{.changepoints}. Defaults to \code{10L}.}
\item{changepoint_range}{Passed to \code{.changepoints}. Defaults to \code{0.8}.}
\item{changepoints}{Passed to \code{.changepoints}.}
\item{seasonalities}{Optional; may be \code{NULL}. Passed to \code{morie_prphet_design}.}
\item{holidays}{Passed to \code{morie_prphet_design}.}
\item{holiday_window}{Passed to \code{morie_prphet_design}. Defaults to \code{c(0, 0)}.}
\item{changepoint_prior}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.05}.}
\item{ridge}{Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.}
}
\description{
A step of the prphet_native implementation. Called by \code{.prnFil_changepoint_path}, \code{.prnFil_select_changepoints}, \code{prophe_additive_components}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{fitted}, \code{residual}, \code{coef}, \code{beta}, \code{columns}, \code{changepoints}, \code{deltas}, \code{k}, \code{m}, \code{trend}, \code{holiday.names}, \code{t}, \code{n}, \code{changepoint_prior}, \code{n.active.changepoints}, \code{sigma}, \code{seasonalities}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_prphet_fit(V, V)
}
