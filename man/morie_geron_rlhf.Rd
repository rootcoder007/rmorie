% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_rlhf}
\alias{morie_geron_rlhf}
\title{Reinforcement learning from human feedback: reward maximised under a KL penalty (Geron Ch 15, hmrlhf)}
\usage{
morie_geron_rlhf(policy, reward_model, prompts = NULL, beta = 0.1, lr = 0.5, epochs = 500)
}
\arguments{
\item{policy}{Initial (n_prompts, n_responses) logits.}
\item{reward_model}{Function or matrix.}
\item{prompts}{Optional prompt identifiers.}
\item{beta,lr,epochs}{Controls.}
}
\description{
Reinforcement learning from human feedback: reward maximised under a KL penalty (Geron Ch 15, hmrlhf)
}
\value{
A list with \code{policy}, \code{reference_policy}, \code{optimal_policy}, \code{max_deviation}, \code{objective}, \code{objective_history}, \code{mean_reward}, \code{kl}, \code{beta}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_geron_rlhf(M, M)
}
