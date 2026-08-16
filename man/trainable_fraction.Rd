% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{trainable_fraction}
\alias{trainable_fraction}
\title{trainable_fraction}
\usage{
trainable_fraction(qformer_params, frozen_vision_params, frozen_llm_params)
}
\arguments{
\item{qformer_params}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{frozen_vision_params}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{frozen_llm_params}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the blip2v_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{trainable}, \code{total}, \code{fraction}, \code{frozen_fraction}, \code{note}.
}
\keyword{internal}
