% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{dit_block}
\alias{dit_block}
\title{One DiT block: adaLN-zero, attention, adaLN-zero, MLP.}
\usage{
dit_block(hidden, cond, attn_fn, mlp_fn, W_scale, W_shift, W_alpha, W_scale2, W_shift2, W_alpha2)
}
\arguments{
\item{hidden}{   numeric hidden vector.}
\item{cond}{     numeric conditioning vector.}
\item{attn_fn}{  function(mod) -> numeric, attention over modulated.}
\item{mlp_fn}{   function(mod) -> numeric, MLP over modulated.}
\item{W_scale}{  d x |cond| matrix, first adaLN gamma.}
\item{W_shift}{  d x |cond| matrix, first adaLN beta.}
\item{W_alpha}{  d x |cond| matrix, first adaLN alpha (zero init).}
\item{W_scale2}{ d x |cond| matrix, second adaLN gamma.}
\item{W_shift2}{ d x |cond| matrix, second adaLN beta.}
\item{W_alpha2}{ d x |cond| matrix, second adaLN alpha (zero init).}
}
\description{
Each sub-layer is gated by a learned alpha (initialised to zero) so the whole block starts as the identity.
}
\value{
list with output, identity_at_init.
}
\keyword{internal}
