% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfesf_fit_multinomial}
\alias{.alfesf_fit_multinomial}
\title{Multinomial logistic head by full-batch gradient ascent.}
\usage{
.alfesf_fit_multinomial(X, y, n_bins, l2, iters, lr)
}
\arguments{
\item{X}{See Usage.}
\item{y}{See Usage.}
\item{n_bins}{See Usage.}
\item{l2}{See Usage.}
\item{iters}{See Usage.}
\item{lr}{See Usage.}
}
\description{
Deterministic: zero init, fixed step, fixed iteration count, so both language arms walk the same path. No line search -- a search that branches on a floating-point comparison is exactly what makes two arms disagree.
}
\value{
A list with \code{W}, \code{b}.
}
\keyword{internal}
