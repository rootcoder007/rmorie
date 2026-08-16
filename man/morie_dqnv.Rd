% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_dqnv}
\alias{morie_dqnv}
\title{morie_dqnv}
\usage{
morie_dqnv(P, R, n_states, n_actions, gamma = 0.99, alpha = 0.1, steps = 20000, C = 100, buffer_size = 1000, batch = 16, seed = 0, use_replay = TRUE, use_target = TRUE)
}
\arguments{
\item{P}{A vector; indexed elementwise.}
\item{R}{A vector; indexed elementwise.}
\item{n_states}{See Usage.}
\item{n_actions}{See Usage.}
\item{gamma}{Passed to \code{.dqnv_td_target}. Defaults to \code{0.99}.}
\item{alpha}{Defaults to \code{0.1}.}
\item{steps}{Defaults to \code{20000}.}
\item{C}{Defaults to \code{100}.}
\item{buffer_size}{Passed to \code{.dqnv_replay_buffer_new}. Defaults to \code{1000}.}
\item{batch}{Passed to \code{.dqnv_replay_buffer_sample}. Defaults to \code{16}.}
\item{seed}{Passed to \code{.ghc_rng}. Defaults to \code{0}.}
\item{use_replay}{A flag; the body branches on it. Defaults to \code{TRUE}.}
\item{use_target}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the dqnv_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{Q}, \code{residual_history}, \code{final_residual}, \code{greedy_policy}, \code{used_replay}, \code{used_target_network}, \code{C}, \code{method}.
}
\references{
----------
Mnih, V., Kavukcuoglu, K., Silver, D., Rusu, A. A., Veness, J.,
Bellemare, M. G., Graves, A., Riedmiller, M., Fidjeland, A. K.,
Ostrovski, G., Petersen, S., Beattie, C., Sadik, A., Antonoglou, I.,
King, H., Kumaran, D., Wierstra, D., Legg, S. & Hassabis, D. (2015)
"Human-level control through deep reinforcement learning", *Nature*
518(7540), 529-533, doi:10.1038/nature14236. The two key ideas:
experience replay, which randomises over the data to remove
correlations in the observation sequence and smooth over changes in
the data distribution; and an iterative update towards target values
that are only periodically updated, with theta^- held fixed between
updates and refreshed every C steps. The Methods section gives the
reward clipping to [-1, 1].

Watkins, C. J. C. H. & Dayan, P. (1992) "Q-learning", *Machine
Learning* 8, 279-292, doi:10.1007/BF00992698. The learning rule
itself.

Lin, L.-J. (1992) "Self-improving reactive agents based on
reinforcement learning, planning and teaching", *Machine Learning* 8,
293-321, doi:10.1007/BF00992699. Experience replay.
}
\keyword{internal}
