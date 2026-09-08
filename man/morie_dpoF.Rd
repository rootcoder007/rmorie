% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dpoF}
\alias{morie_dpoF}
\title{morie_dpoF}
\usage{
morie_dpoF(logp_w = NULL, logp_l = NULL, logp_ref_w = NULL, logp_ref_l = NULL, 
    beta = 0.1, model = "bradley-terry", logp = NULL, logp_ref = NULL, 
    label_smoothing = 0) 
}
\arguments{
\item{logp_w}{Passed to \code{.dpoF_vec}.}
\item{logp_l}{Passed to \code{.dpoF_vec}.}
\item{logp_ref_w}{Passed to \code{.dpoF_vec}.}
\item{logp_ref_l}{Passed to \code{.dpoF_vec}.}
\item{beta}{Numeric; combined arithmetically in the body. Defaults to \code{0.1}.}
\item{model}{Compared against \code{"plackett-luce"}. Defaults to \code{"bradley-terry"}.}
\item{logp}{Passed to \code{.dpoF_plackett_luce}.}
\item{logp_ref}{Passed to \code{.dpoF_plackett_luce}.}
\item{label_smoothing}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0}.}
}
\description{
A step of the dpoF_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{loss}, \code{losses}, \code{reward_w}, \code{reward_l}, \code{margin}, \code{grad_weight}, \code{accuracy}, \code{beta}, \code{n}, \code{model}, \code{method}.
}
\keyword{internal}
\examples{
morie_dpoF(logp_w = c(-1, -2), logp_l = c(-3, -4),
           logp_ref_w = c(-1.5, -2.5), logp_ref_l = c(-2.5, -3.5))
}
