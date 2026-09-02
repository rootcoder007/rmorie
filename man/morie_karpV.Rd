% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_karpV}
\alias{morie_karpV}
\title{Genetic programming}
\usage{
morie_karpV(fitness = NULL, ops = NULL, gens = 20L, cases = NULL, terminals = "x", 
    functions = NULL, erc = c(-5, 5), pop_size = 100L, max_depth_init = 6L, 
    max_depth = 17L, p_crossover = 0.9, p_mutation = 0, internal_bias = 0.9, 
    seed = 1L, elitism = 0L) 
}
\arguments{
\item{fitness}{function of a tree returning raw fitness, or NULL to   use the sum of absolute errors over cases.}
\item{ops}{alias for functions, for the older call shape.}
\item{gens}{generations after the initial one.}
\item{cases}{list of list(inputs, target).}
\item{terminals}{variable names, matched positionally to inputs.}
\item{functions}{list of name and arity pairs; defaults to plus,   minus, times and protected division.}
\item{erc}{length-two range for ephemeral constants, or NULL.}
\item{pop_size}{population size.}
\item{max_depth_init}{deepest initial tree.}
\item{max_depth}{cap on offspring depth; a child that exceeds it is   replaced by its first parent.}
\item{p_crossover}{crossover probability.}
\item{p_mutation}{mutation probability.}
\item{internal_bias}{probability of an internal crossover point.}
\item{seed}{seeds the module's own generator, so a run reproduces in   both language arms.}
\item{elitism}{how many best individuals to carry over unchanged.}
}
\description{
Genetic programming
}
\value{
a list with best, best_string, best_raw, best_adjusted,   best_size, best_depth, generation_found, history, evaluations,   generations, pop_size, seed and method.
}
\keyword{internal}
