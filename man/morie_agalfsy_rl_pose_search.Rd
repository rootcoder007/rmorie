% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{morie_agalfsy_rl_pose_search}
\alias{morie_agalfsy_rl_pose_search}
\alias{morie_agalfsy}
\title{Wang, C., Chen, Y., Zhang, Y., Li, K., Lin, M., Pan, F., Wu, W. and}
\usage{
morie_agalfsy_rl_pose_search(receptor, ligand, site = NULL, policy = NULL, critic = NULL, max_steps = 600L, min_steps = 300L, window = 50L, tol = 0.3, box = 18, seed = 2)
}
\description{
Wang, C., Chen, Y., Zhang, Y., Li, K., Lin, M., Pan, F., Wu, W. and binding pose prediction. BMC Bioinformatics 23, 368,
}
\value{
A list with components \code{estimate}, \code{pose}, \code{rmsd}, \code{rmsd_start}, \code{dcc}, \code{success}, \code{improved}, \code{steps}, \code{stop_reason}, \code{reward_total}, \code{trajectory}, \code{policy_kind}, \code{n_actions}, \code{translation_step}, \code{rotation_step_deg}, \code{box}, \code{method}, \code{note}.
}
\references{
R arm of agalfsy -- the docking MDP of Wang et al. (2022).
Zhang, J. (2022) A reinforcement learning approach for protein-ligand
doi:10.1186/s12859-022-04912-7.
Scardino, Di Filippo and Cavasotto (2022) iScience 26(1), 105920,
doi:10.1016/j.isci.2022.105920 -- the AlphaFold receptor caveat.
Jumper et al. (2021) Nature 596, 583-589 -- the predicted receptor.
}
\keyword{internal}
