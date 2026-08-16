% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfImpEeg}
\alias{MfImpEeg}
\title{Eq (4.56), identical to eq (4.49).  Section 4.6.2 adds that because h}
\usage{
MfImpEeg(x, t0 = NULL, gain = 1, dt = 1)
}
\arguments{
\item{x}{See Usage.}
\item{t0}{Defaults to \code{NULL}.}
\item{gain}{Defaults to \code{1}.}
\item{dt}{Defaults to \code{1}.}
}
\description{
is a reversed reference, the filtering is equivalent to correlation.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
