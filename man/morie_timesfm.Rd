% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_timesfm}
\alias{morie_timesfm}
\title{morie_timesfm}
\usage{
morie_timesfm(history, predictor, horizon, input_patch_len, output_patch_len)
}
\arguments{
\item{history}{See Usage.}
\item{predictor}{See Usage.}
\item{horizon}{See Usage.}
\item{input_patch_len}{See Usage.}
\item{output_patch_len}{See Usage.}
}
\description{
Part of the timesfm_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{forecast}, \code{steps}, \code{horizon}, \code{input_patch_len}, \code{output_patch_len}, \code{context_grew_to}, \code{method}.
}
\keyword{internal}
