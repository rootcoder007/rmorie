% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dqnv_bellman_residual}
\alias{.dqnv_bellman_residual}
\title{.dqnv_bellman_residual}
\usage{
.dqnv_bellman_residual(Q, P, R, gamma = 0.99)
}
\arguments{
\item{Q}{A vector; its length is taken and its elements indexed.}
\item{P}{A vector; indexed elementwise.}
\item{R}{A vector; indexed elementwise.}
\item{gamma}{Defaults to \code{0.99}.}
}
\description{
A step of the dqnv_native implementation. Called by \code{morie_dqnv}. See the file header for the source the module follows.
}
\value{
The value of \code{worst}, as built in the body.
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
