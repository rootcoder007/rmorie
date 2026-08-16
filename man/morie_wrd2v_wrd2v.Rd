% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_wrd2v_wrd2v}
\alias{morie_wrd2v_wrd2v}
\title{morie_wrd2v_wrd2v}
\usage{
morie_wrd2v_wrd2v(corpus, size = 16, window = 5, architecture = "skip-gram", lr = 0.05, epochs = 20, min_count = 1, dynamic_window = TRUE, loss = "softmax", negative = 5, noise_power = 0.75, subsample = NULL, seed = 0)
}
\arguments{
\item{corpus}{See Usage.}
\item{size}{Defaults to \code{16}.}
\item{window}{Defaults to \code{5}.}
\item{architecture}{Defaults to \code{"skip-gram"}.}
\item{lr}{Defaults to \code{0.05}.}
\item{epochs}{Defaults to \code{20}.}
\item{min_count}{Defaults to \code{1}.}
\item{dynamic_window}{Defaults to \code{TRUE}.}
\item{loss}{Defaults to \code{"softmax"}.}
\item{negative}{Defaults to \code{5}.}
\item{noise_power}{Defaults to \code{0.75}.}
\item{subsample}{Defaults to \code{NULL}.}
\item{seed}{Defaults to \code{0}.}
}
\description{
Part of the wrd2v_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{vectors}, \code{output_vectors}, \code{vocab}, \code{loss_curve}, \code{final_loss}, \code{similarity}, \code{most_similar}, \code{size}, \code{window}, \code{architecture}, \code{loss}, \code{negative}, \code{noise}, \code{method}.
}
\keyword{internal}
