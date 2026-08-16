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
\item{X}{A matrix; indexed by row and column.}
\item{y}{A vector; indexed elementwise.}
\item{n_bins}{A count; the body uses it as \code{seq_len(...)}.}
\item{l2}{Numeric; combined arithmetically in the body.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}.}
\item{lr}{Numeric; combined arithmetically in the body.}
}
\description{
Deterministic: zero init, fixed step, fixed iteration count, so both language arms walk the same path. No line search -- a search that branches on a floating-point comparison is exactly what makes two arms disagree.
}
\value{
A list with \code{W}, \code{b}.
}
\keyword{internal}
