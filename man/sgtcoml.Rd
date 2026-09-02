% Generated from the module source by ledger/gen_rd.R -- roxygen2 hangs
% on this package. Usage comes from args(), so it tracks the formals.
\name{sgtcoml}
\alias{sgtcoml}
\alias{morie_sgtcoml}
\title{SPDX-License-Identifier: AGPL-3.0-or-later}
\usage{
sgtcoml(A) 

morie_sgtcoml(A) 
}
\description{
SPDX-License-Identifier: AGPL-3.0-or-later ' One Louvain local-moving phase ' ' Each node starts alone and is repeatedly moved to the neighbouring ' community with the largest positive modularity gain ' dQ = k_i_in/m - Sigma_tot k_i / (2 m^2).  Nodes are visited in index order ' and ties go to the smallest community label, so the pass is deterministic. ' P10008, section 2. ' ' @param A symmetric adjacency (or weight) matrix. ' @return list: estimate, communities, n_communities, modularity_before, '   sweeps, n, method. ' @keywords internal ' @examples ' sgtcoml(matrix(c(0,1,1,0), 2, 2))$n_communities ' @export
}
\arguments{
\item{A}{Argument \code{A}; see Usage.}
}
\value{
A list with components \code{comm}, \code{sweeps}, \code{estimate}, \code{communities}, \code{n_communities}, \code{modularity_before}, \code{n}, \code{method}.
}
\references{
' Source consulted: Blondel, Guillaume, Lambiotte and Lefebvre (2008), JSTAT
}
\keyword{internal}
\examples{
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
sgtcoml(M)
}
