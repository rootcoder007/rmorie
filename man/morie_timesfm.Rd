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
\item{history}{Passed to \code{unlist}.}
\item{predictor}{Accepted by the signature and not used anywhere in the body.}
\item{horizon}{Passed to \code{morie_timesfm_rollout_steps}.}
\item{input_patch_len}{Coerced to integer by the body, with \code{as.integer}.}
\item{output_patch_len}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the timesfm_native implementation. Called by \code{morie_timesf}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{forecast}, \code{steps}, \code{horizon}, \code{input_patch_len}, \code{output_patch_len}, \code{context_grew_to}, \code{method}.
}
\keyword{internal}
