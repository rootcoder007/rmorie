% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.joint}
\alias{.joint}
\title{The joint density in (theta, r) up to a constant: log p(theta) +}
\usage{
.joint(logp, theta, r)
}
\arguments{
\item{logp}{See Usage.}
\item{theta}{See Usage.}
\item{r}{Numeric; combined arithmetically in the body.}
}
\description{
log p(r), with p(r) the standard normal so log p(r) = -r.r/2.
}
\value{
A numeric value.
}
\keyword{internal}
