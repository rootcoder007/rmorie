% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfqud_run}
\alias{morie_alfqud_run}
\title{Run a program on one input vector}
\usage{
morie_alfqud_run(program, x, n_reg)
}
\arguments{
\item{program}{A list of instructions.}
\item{x}{The input vector.}
\item{n_reg}{How many registers.}
}
\description{
Memory starts holding the input, the registers start at zero and the flag starts cleared, so a program's behaviour depends on nothing but the instructions it contains.
}
\value{
The memory the program leaves behind.
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
