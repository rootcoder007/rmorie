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
\item{y}{See Usage.}
\item{X}{Defaults to \code{NULL}.}
\item{d}{Defaults to \code{0}.}
\item{D}{Defaults to \code{0}.}
\item{s}{Defaults to \code{1}.}
\item{method}{Defaults to \code{"css"}.}
\item{max_steps}{Defaults to \code{20}.}
}
\description{
Part of the sarimax_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{aic}, \code{fit}, \code{order}, \code{seasonal_order}, \code{constant}, \code{steps}, \code{n_models_tried}, \code{tried}, \code{s}, \code{search_method}, \code{differencing_note}, \code{method}.
}
\keyword{internal}
