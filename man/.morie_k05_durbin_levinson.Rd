% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_k05_durbin_levinson}
\alias{.morie_k05_durbin_levinson}
\title{R is indexed from 1 for lag 0, so r[k + 1] is lag k}
\usage{
.morie_k05_durbin_levinson(r, max_lag)
}
\arguments{
\item{r}{A vector; indexed elementwise.}
\item{max_lag}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the k05_tranche1 implementation. Called by \code{morie_sample_pacf}. See the file header for the source the module follows.
}
\value{
The value of \code{phi}, as built in the body.
}
\keyword{internal}
