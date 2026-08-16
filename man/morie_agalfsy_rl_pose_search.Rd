% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_agalfsy_rl_pose_search}
\alias{morie_agalfsy_rl_pose_search}
\title{morie_agalfsy_rl_pose_search}
\usage{
morie_agalfsy_rl_pose_search(receptor, ligand, site = NULL, policy = NULL, critic = NULL, max_steps = 600L, min_steps = 300L, window = 50L, tol = 0.3, box = 18, seed = 2)
}
\arguments{
\item{receptor}{Passed to \code{.agalfsy_coords}.}
\item{ligand}{Passed to \code{.agalfsy_coords}.}
\item{site}{Optional; may be \code{NULL}. Passed to \code{.agalfsy_coords}.}
\item{policy}{Defaults to \code{NULL}.}
\item{critic}{Defaults to \code{NULL}.}
\item{max_steps}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{600L}.}
\item{min_steps}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{300L}.}
\item{window}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{50L}.}
\item{tol}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.3}.}
\item{box}{Numeric; combined arithmetically in the body. Defaults to \code{18}.}
\item{seed}{Accepted by the signature and not used anywhere in the body. Defaults to \code{2}.}
}
\description{
A step of the agalfsy_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{pose}, \code{rmsd}, \code{rmsd_start}, \code{dcc}, \code{success}, \code{improved}, \code{steps}, \code{stop_reason}, \code{reward_total}, \code{trajectory}, \code{policy_kind}, \code{n_actions}, \code{translation_step}, \code{rotation_step_deg}, \code{box}, \code{method}, \code{note}.
}
\keyword{internal}
