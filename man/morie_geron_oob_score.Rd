% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_oob_score}
\alias{morie_geron_oob_score}
\title{Out-of-bag evaluation over per-estimator bag masks (Geron Ch 6, hmoob)}
\usage{
morie_geron_oob_score(X, y, models, task = "auto")
}
\arguments{
\item{X,y}{Data and targets.}
\item{models}{List of `(predict, in_bag)` pairs or lists with those names.}
\item{task}{"auto", "classification" or "regression".}
}
\description{
Out-of-bag evaluation over per-estimator bag masks (Geron Ch 6, hmoob)
}
\value{
A list with \code{oob_score}, \code{oob_predictions}, \code{covered}, \code{votes}, \code{mean_oob_votes}, \code{task}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
