% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.flxipt_fit}
\alias{.flxipt_fit}
\title{Train one candidate on `rows`; return predictor over all rows}
\usage{
.flxipt_fit(y, W, spec, rows, binary, ridge)
}
\arguments{
\item{y}{A vector; indexed elementwise.}
\item{W}{Passed to \code{.flxipt_expand}.}
\item{spec}{A list; the body reads \code{$penalty} from it.}
\item{rows}{See Usage.}
\item{binary}{A flag; the body branches on it.}
\item{ridge}{Numeric; passed to \code{max}.}
}
\description{
A step of the flxipt_native implementation. Called by \code{super_learner}. See the file header for the source the module follows.
}
\value{
A list with \code{pred}, \code{b}.
}
\keyword{internal}
