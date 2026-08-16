% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_predict_glm}
\alias{.schab_predict_glm}
\title{Eq (6.90) with its own MSPE (6.91), kept apart from the inverse-link}
\usage{
.schab_predict_glm(nu0_hat, sigma2_nu0, mu0_hat, link_kind)
}
\arguments{
\item{nu0_hat}{See Usage.}
\item{sigma2_nu0}{See Usage.}
\item{mu0_hat}{See Usage.}
\item{link_kind}{See Usage.}
}
\description{
predictor (6.87), whose delta-method variance (6.88) the text says belongs to a different predictor.
}
\value{
A list with \code{prediction}, \code{mspe}, \code{prediction_error}, \code{inverse_link_prediction}, \code{pseudo_scale_prediction}, \code{pseudo_scale_mspe}, \code{mspe_is_for}.
}
\keyword{internal}
