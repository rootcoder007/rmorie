% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfqud}
\alias{morie_alfqud}
\title{Search for a machine program that sorts the given inputs}
\usage{
morie_alfqud(target, action_space = NULL, reward_fn = NULL, n_reg = 2L, max_len = 3L, latency_weight = 0, search = "mcts", n_sim = 400L, c_puct = 1.25, seed = 0)
}
\arguments{
\item{target}{The test inputs. What each one should become is its own   values in ascending order -- that is the specification of a sort,   and making it derived rather than supplied means the two cannot   drift apart.}
\item{action_space}{The instructions the search may use, or NULL for   every legal instruction over the memory and registers.}
\item{reward_fn}{A function of program, inputs, targets and n_reg   returning the correctness, or NULL for the count of correctly   placed elements.}
\item{n_reg}{How many registers the machine has.}
\item{max_len}{The longest program the search will consider.}
\item{latency_weight}{Instructions charged against correctness. Zero   searches for correctness alone.}
\item{search}{Either mcts or bfs; see the file header on why both.}
\item{n_sim}{Simulations for the tree search.}
\item{c_puct}{The PUCT exploration constant.}
\item{seed}{Unused by this search, kept so the signature matches the   Python arm.}
}
\description{
Search for a machine program that sorts the given inputs
}
\value{
A list with the best program, its score, its correctness, and   how much of the space was looked at.
}
\references{
  Mankowitz, D.J. et al. (2023) "Faster sorting algorithms discovered
    using deep reinforcement learning." Nature 618, 257-263.
    doi:10.1038/s41586-023-06004-9.
  Silver, D. et al. (2017) "Mastering the game of Go without human
    knowledge." Nature 550, 354-359. The PUCT selection rule.
  Rosin, C.D. (2011) "Multi-armed bandits with episode context."
    Annals of Mathematics and Artificial Intelligence 61(3), 203-230.
}
\keyword{internal}
