% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.esl_mtry}
\alias{.esl_mtry}
\title{The book\\'s defaults for the number of variables sampled at each}
\usage{
.esl_mtry(p, task = "regression")
}
\arguments{
\item{p}{Numeric; passed to \code{sqrt}.}
\item{task}{The body requires: task must be 'regression' or 'classification'. Defaults to \code{"regression"}.}
}
\description{
split. They cross at p = 9, where both give 3: below it the classification rule is larger, above it the regression rule is, by a widening margin (33 against 10 at p = 100). Both are tuning parameters, not laws.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
