% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_wrd2v_subsample_probability}
\alias{morie_wrd2v_subsample_probability}
\title{Discard probability 1 - sqrt(t / f(w)) (2013b eq. 5). Clamped at 0}
\usage{
morie_wrd2v_subsample_probability(counts, t = 1e-05)
}
\arguments{
\item{counts}{See Usage.}
\item{t}{Defaults to \code{1e-05}.}
}
\description{
Part of the wrd2v_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
