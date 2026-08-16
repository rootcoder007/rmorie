% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_check_mdp}
\alias{.morie_gr_check_mdp}
\title{.morie_gr_check_mdp}
\usage{
.morie_gr_check_mdp(policy, transitions, rewards, gamma)
}
\arguments{
\item{policy}{See Usage.}
\item{transitions}{See Usage.}
\item{rewards}{See Usage.}
\item{gamma}{See Usage.}
}
\description{
A step of the geron_train_native implementation. Called by \code{.morie_gr_policy_evaluation}. See the file header for the source the module follows.
}
\value{
A list with \code{pi}, \code{P}, \code{R}, \code{gamma}, \code{S}, \code{A}.
}
\keyword{internal}
