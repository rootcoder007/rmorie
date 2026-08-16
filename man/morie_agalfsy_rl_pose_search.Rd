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
\item{receptor}{See Usage.}
\item{ligand}{See Usage.}
\item{site}{Defaults to \code{NULL}.}
\item{policy}{Defaults to \code{NULL}.}
\item{critic}{Defaults to \code{NULL}.}
\item{max_steps}{Defaults to \code{600L}.}
\item{min_steps}{Defaults to \code{300L}.}
\item{window}{Defaults to \code{50L}.}
\item{tol}{Defaults to \code{0.3}.}
\item{box}{Defaults to \code{18}.}
\item{seed}{Defaults to \code{2}.}
}
\description{
Part of the agalfsy_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{estimate}, \code{pose}, \code{rmsd}, \code{rmsd_start}, \code{dcc}, \code{success}, \code{improved}, \code{steps}, \code{stop_reason}, \code{reward_total}, \code{trajectory}, \code{policy_kind}, \code{n_actions}, \code{translation_step}, \code{rotation_step_deg}, \code{box}, \code{method}, \code{note}.
}
\keyword{internal}
