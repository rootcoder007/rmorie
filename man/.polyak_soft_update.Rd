% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.polyak_soft_update}
\alias{.polyak_soft_update}
\title{.polyak_soft_update}
\usage{
.polyak_soft_update(target, online, tau = 0.001)
}
\arguments{
\item{target}{See Usage.}
\item{online}{See Usage.}
\item{tau}{Defaults to \code{0.001}.}
}
\description{
Part of the polyak_native implementation; see the file header for the source it follows.
}
\value{
A numeric value.
}
\references{
----------
Polyak, B. T. & Juditsky, A. B. (1992) "Acceleration of Stochastic
Approximation by Averaging", SIAM Journal on Control and
Optimization 30(4), 838-855, doi:10.1137/0330046. Averaging the
iterates of a slowly-decaying stochastic approximation attains the
asymptotically optimal rate.

Ruppert, D. (1988) Efficient Estimations from a Slowly Convergent
Robbins-Monro Process, Technical Report 781, School of Operations
Research and Industrial Engineering, Cornell University. The same
averaging idea, independently.

Lillicrap, T. P., Hunt, J. J., Pritzel, A., Heess, N., Erez, T.,
Tassa, Y., Silver, D. & Wierstra, D. (2016) "Continuous control with
deep reinforcement learning", International Conference on Learning
Representations (ICLR 2016), arXiv:1509.02971. Sec. 3: the Q update
is prone to divergence because the network being updated also computes
the target; "soft" target updates theta\\' <- tau theta + (1 - tau)
theta\\' with tau << 1 replace DQN\\'s periodic copy, constraining target
values to change slowly and moving the problem closer to supervised
learning; the supplementary details give tau = 0.001.

Mnih, V., Kavukcuoglu, K., Silver, D. et al. (2015) "Human-level
control through deep reinforcement learning", Nature 518, 529-533,
doi:10.1038/nature14236. The periodic-copy target network;
implemented in dqnv.
}
\keyword{internal}
