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
\item{size}{A count; the body uses it as \code{matrix(...)}. Defaults to \code{16}.}
\item{window}{Numeric; combined arithmetically in the body. Defaults to \code{5}.}
\item{architecture}{One of \code{"cbow"}, \code{"skip-gram"}. Defaults to \code{"skip-gram"}.}
\item{lr}{Passed to \code{.wrd2v_cbow_step}. Defaults to \code{0.05}.}
\item{epochs}{Defaults to \code{20}.}
\item{min_count}{Defaults to \code{1}.}
\item{dynamic_window}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{loss}{One of \code{"neg"}, \code{"softmax"}. Defaults to \code{"softmax"}.}
\item{negative}{Passed to \code{.wrd2v_neg_step}. Defaults to \code{5}.}
\item{noise_power}{Passed to \code{morie_wrd2v_noise_distribution}. Defaults to \code{0.75}.}
\item{subsample}{Optional; may be \code{NULL}. Passed to \code{morie_wrd2v_subsample_probability}.}
\item{seed}{Numeric; combined arithmetically in the body. Defaults to \code{0}.}
}
\description{
A step of the wrd2v_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{vectors}, \code{output_vectors}, \code{vocab}, \code{loss_curve}, \code{final_loss}, \code{similarity}, \code{most_similar}, \code{size}, \code{window}, \code{architecture}, \code{loss}, \code{negative}, \code{noise}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_wrd2v_wrd2v(V)
}
