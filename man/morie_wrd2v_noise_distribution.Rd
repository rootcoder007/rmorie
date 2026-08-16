% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_wrd2v_noise_distribution}
\alias{morie_wrd2v_noise_distribution}
\title{Pn(w) = U(w)^{3/4} / Z of Mikolov et al. (2013b) 2.2}
\usage{
morie_wrd2v_noise_distribution(counts, power = 0.75)
}
\arguments{
\item{counts}{A vector; indexed elementwise.}
\item{power}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.75}.}
}
\description{
A step of the wrd2v_native implementation. Called by \code{morie_wrd2v_wrd2v}. See the file header for the source the module follows.
}
\value{
The value of \code{stats::setNames}.
}
\keyword{internal}
