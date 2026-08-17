% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{prophe_additive_components}
\alias{prophe_additive_components}
\title{prophe_additive_components}
\usage{
prophe_additive_components(t, y, seasonalities = NULL, holidays = NULL, holiday_window = c(0, 0), ...)
}
\arguments{
\item{t}{Passed to \code{morie_prphet_fit}.}
\item{y}{Passed to \code{morie_prphet_fit}.}
\item{seasonalities}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{holidays}{Optional; may be \code{NULL}. A vector; its length is taken.}
\item{holiday_window}{A vector; indexed elementwise. Defaults to \code{c(0, 0)}.}
\item{...}{Passed through.}
}
\description{
A step of the prophe_native implementation. Called by \code{morie_prophe}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{components}, \code{total}, \code{fitted}, \code{residual}, \code{reconstruction_error}, \code{reconstructs}, \code{coef}, \code{changepoints}, \code{sigma}, \code{n}, \code{component_names}, \code{method}.
}
\keyword{internal}
\examples{
set.seed(1)
t <- 1:60
y <- sin(2 * pi * t / 12) + 0.05 * t + rnorm(60) * 0.1
prophe_additive_components(t, y)
}
