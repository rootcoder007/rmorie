% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gail}
\alias{gail}
\title{Fit the GAIL discriminator and emit the per-step cost for the}
\usage{
gail(expert_states, expert_actions, policy_states, policy_actions, 
    features = NULL, lr = 0.1, epochs = 200, l2 = 0, lam = 0, policy_entropy = 0, 
    clip = 1e-09) 
}
\arguments{
\item{expert_states,expert_actions}{Lists of equal length: the   expert demonstrations tau_E.}
\item{policy_states,policy_actions}{Lists of equal length:   trajectories tau_i sampled from the current policy.}
\item{features}{Optional callable \code{features(s, a) -> numeric};   defaults to the one-hot indicator over the observed (s, a) pairs   plus a bias column.}
\item{lr}{Learning rate for the full-batch ascent on eq. 17.}
\item{epochs}{Number of ascent steps (at least one).}
\item{l2}{Optional ridge penalty on the weights.}
\item{lam}{lambda, the causal-entropy weight of eq. 16. Only enters   the reported objective; the entropy of your policy is yours to   supply.}
\item{policy_entropy}{H(pi), if you have it, for the eq. 16 value.}
\item{clip}{Floor on D and 1-D inside the logarithms.}
}
\description{
policy Full-batch gradient ascent on eq. 17 (the gradient of E_pi[log D] + E_piE[log(1 - D)]) with optional ridge. D is a logistic model on features of (s, a); by default the features are a one-hot indicator over the observed (s, a) pairs, which makes D fully non-parametric and is the setting in which eq. 16\'s optimum is exactly rho_pi / (rho_pi + rho_piE).
}
\value{
A named list whose names match the Python RichResult   payload keys: \code{estimate} and \code{cost} are log D(s, a) for   each policy sample (the per-step cost a TRPO-style optimiser   consumes); also \code{Q} (eq. 18\'s per-pair average over the   policy samples), \code{D_policy}, \code{D_expert}, \code{objective}   (eq. 16), \code{accuracy}, \code{weights}, \code{occupancy_policy},   \code{occupancy_expert}, \code{n_policy}, \code{n_expert},   \code{method}.
}
\keyword{internal}
\examples{
gail(expert_states = c(1, 2, 3, 4, 5, 6, 7, 8), expert_actions = c(1, 2, 3, 4, 5, 6, 7, 8),
  policy_states = c(1, 2, 3, 4, 5, 6, 7, 8), policy_actions = c(1, 2, 3, 4, 5, 6, 7, 8))
}
