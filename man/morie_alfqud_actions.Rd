% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfqud_actions}
\alias{morie_alfqud_actions}
\title{Every legal instruction over the given memory and registers}
\usage{
morie_alfqud_actions(n_mem, n_reg)
}
\arguments{
\item{n_mem}{How many memory slots.}
\item{n_reg}{How many registers.}
}
\description{
Ordered so the enumeration is reproducible: by operation in the order they are defined, then by source, then by destination. A compare of a location with itself is dropped -- its flag is always zero, so it can only waste an instruction.
}
\value{
A list of instructions.
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
\examples{
morie_alfqud_actions(n_mem = c(1, 2, 3, 4, 5, 6, 7, 8), n_reg = 5L)
}
