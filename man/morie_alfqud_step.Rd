% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_alfqud_step}
\alias{morie_alfqud_step}
\title{Execute one instruction}
\usage{
morie_alfqud_step(st, instr)
}
\arguments{
\item{st}{A list with mem, reg and flag.}
\item{instr}{A list of the operation and its two locations.}
}
\description{
Deterministic, and the whole semantics. \code{mov src dst} gives dst the value of src; \code{cmp a b} sets the flag to minus one, zero or one as a is below, equal to or above b and moves nothing; \code{cmovl} and \code{cmovg} move only when the flag is negative or positive respectively. The conditional moves are the reason this instruction set is worth searching at all: they let a program reorder two values without a branch, which is what the discovered routines exploit.
}
\value{
The state after the instruction.
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
