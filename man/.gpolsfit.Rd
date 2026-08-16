% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.gpolsfit}
\alias{.gpolsfit}
\title{Chapter 3: least squares, eq (3.1)}
\usage{
.gpolsfit(X, y, add_intercept = FALSE)
}
\arguments{
\item{X}{Passed to \code{.gpmat}.}
\item{y}{A matrix; passed to \code{\%*\%}.}
\item{add_intercept}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
----------------------------------- Part of the helpers_gp_core implementation; see the file header for the source it follows.
}
\value{
A list with \code{beta}, \code{fitted}, \code{residuals}, \code{rss}, \code{sigma2}, \code{sigma2_ml}, \code{var_beta}, \code{se_beta}.
}
\keyword{internal}
