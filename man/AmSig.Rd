% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{AmSig}
\alias{AmSig}
\title{Section 5.5.1: y(t) = x(t) cos(wc t) -- double-sideband SUPPRESSED}
\usage{
AmSig(x, fc, fs, conventional = FALSE, depth = 1)
}
\arguments{
\item{x}{See Usage.}
\item{fc}{See Usage.}
\item{fs}{See Usage.}
\item{conventional}{A flag; the body branches on it. Defaults to \code{FALSE}.}
\item{depth}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
carrier -- with synchronous demodulation x_d = y cos(wc t) = 0.5 x + 0.5 x cos(2 wc t).  The placeholder stated the conventional large-carrier form A(1 + m) cos(wc t), which is a different model; it is available under conventional=TRUE rather than substituted.
}
\value{
A list with \code{y}, \code{carrier}, \code{demodulated}, \code{fc}, \code{fs}, \code{suppressed_carrier}, \code{baseband_gain}, \code{image_frequency}, \code{method}.
}
\keyword{internal}
