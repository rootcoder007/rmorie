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
\item{counts}{A vector; indexed elementwise.}
\item{t}{Numeric; combined arithmetically in the body. Defaults to \code{1e-05}.}
}
\description{
A step of the wrd2v_native implementation. Called by \code{morie_wrd2v_wrd2v}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_wrd2v_subsample_probability(V)
}
