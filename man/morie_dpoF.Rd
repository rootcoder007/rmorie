% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dpoF}
\alias{morie_dpoF}
\title{morie_dpoF}
\usage{
morie_dpoF(logp_w = NULL, logp_l = NULL, logp_ref_w = NULL, logp_ref_l = NULL, beta = 0.1, model = "bradley-terry", logp = NULL, logp_ref = NULL, label_smoothing = 0)
}
\arguments{
\item{logp_w}{Defaults to \code{NULL}.}
\item{logp_l}{Defaults to \code{NULL}.}
\item{logp_ref_w}{Defaults to \code{NULL}.}
\item{logp_ref_l}{Defaults to \code{NULL}.}
\item{beta}{Defaults to \code{0.1}.}
\item{model}{Defaults to \code{"bradley-terry"}.}
\item{logp}{Defaults to \code{NULL}.}
\item{logp_ref}{Defaults to \code{NULL}.}
\item{label_smoothing}{Defaults to \code{0}.}
}
\description{
Part of the dpoF_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{loss}, \code{losses}, \code{reward_w}, \code{reward_l}, \code{margin}, \code{grad_weight}, \code{accuracy}, \code{beta}, \code{n}, \code{model}, \code{method}.
}
\keyword{internal}
