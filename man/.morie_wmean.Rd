% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_wmean}
\alias{.morie_wmean}
\title{Normalised weighted mean: sum(y * w) / sum(w). Huber\\\'s Section 3}
\usage{
.morie_wmean(y, w)
}
\arguments{
\item{y}{See Usage.}
\item{w}{See Usage.}
}
\description{
normalises so the weights within each treatment state add to unity, following Imbens (2004) and Busso, DiNardo and McCrary (2009).
}
\value{
A numeric value.
}
\keyword{internal}
