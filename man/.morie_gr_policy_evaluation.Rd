% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_policy_evaluation}
\alias{.morie_gr_policy_evaluation}
\title{.morie_gr_policy_evaluation}
\usage{
.morie_gr_policy_evaluation(policy, transitions, rewards, gamma)
}
\arguments{
\item{policy}{Passed to \code{.morie_gr_check_mdp}.}
\item{transitions}{Passed to \code{.morie_gr_check_mdp}.}
\item{rewards}{Passed to \code{.morie_gr_check_mdp}.}
\item{gamma}{Passed to \code{.morie_gr_check_mdp}.}
}
\description{
A step of the geron_train_native implementation. Called by \code{morie_geron_action_value_function}, \code{morie_geron_state_value_function}. See the file header for the source the module follows.
}
\value{
A list with \code{V}, \code{pi}, \code{P}, \code{R}, \code{r_sa}, \code{gamma}, \code{S}, \code{A}.
}
\keyword{internal}
