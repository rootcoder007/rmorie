% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_novelty_detection}
\alias{morie_geron_novelty_detection}
\title{Novelty detection by density ratio against a clean training set (Geron Ch 8, hmnov)}
\usage{
morie_geron_novelty_detection(model, X_new, reference = NULL)
}
\arguments{
\item{model}{Callable log-density, list with `log_density`/`reference`, or clean training matrix.}
\item{X_new}{Points to test.}
\item{reference}{Optional log reference density override.}
}
\description{
Novelty detection by density ratio against a clean training set (Geron Ch 8, hmnov)
}
\value{
A list with \code{ratio}, \code{log_ratio}, \code{is_novel}, \code{log_density}, \code{reference}, \code{novel_fraction}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
