% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.mor_ros_signed}
\alias{.mor_ros_signed}
\title{Wilcoxon signed-rank sensitivity bound at one Gamma (Python cnsRos)}
\usage{
.mor_ros_signed(pairs, Gamma = 1)
}
\arguments{
\item{pairs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{Gamma}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the rosenb_native implementation. Called by \code{morie_rosenb}. See the file header for the source the module follows.
}
\value{
A list with \code{p_upper}, \code{p_lower}, \code{W}, \code{mu_plus}, \code{sigma_plus}, \code{z_upper}, \code{n_pairs}, \code{Gamma}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.mor_ros_signed(V)
}
