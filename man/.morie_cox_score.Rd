% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_cox_score}
\alias{.morie_cox_score}
\title{Score and information at a FIXED beta. Shared by the fitter and by}
\usage{
.morie_cox_score(ts, es, Xs, beta, offs, ties)
}
\arguments{
\item{ts}{A vector; indexed elementwise.}
\item{es}{See Usage.}
\item{Xs}{A matrix; indexed by row and column.}
\item{beta}{A matrix; passed to \code{\%*\%}.}
\item{offs}{Numeric; combined arithmetically in the body.}
\item{ties}{See Usage.}
}
\description{
the stratified model, which needs the pieces without taking a step.
}
\value{
A list with \code{loglik}, \code{U}, \code{I}.
}
\keyword{internal}
