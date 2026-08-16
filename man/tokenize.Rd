% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{tokenize}
\alias{tokenize}
\title{tokenize}
\usage{
tokenize(x, bins, context = NULL, add_eos = TRUE, pad_to = NULL)
}
\arguments{
\item{x}{See Usage.}
\item{bins}{A list; the body reads \code{$n_bins} from it.}
\item{context}{Defaults to \code{NULL}.}
\item{add_eos}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{pad_to}{Defaults to \code{NULL}.}
}
\description{
A step of the chronos_native implementation. Called by \code{morie_chronos}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{tokens}, \code{scale}, \code{n_clipped}, \code{clipped_fraction}, \code{vocab_size}, \code{method}, \code{ignores}.
}
\keyword{internal}
