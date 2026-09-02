% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mistr_mistral_block}
\alias{morie_mistr_mistral_block}
\title{One Mistral decoder block}
\usage{
morie_mistr_mistral_block(X, Wq, Wk, Wv, Wo, W1, W2, W3, n_heads, n_kv_heads, window, norm1 = NULL, 
    norm2 = NULL, base = 10000) 
}
\arguments{
\item{X}{L x d input matrix.}
\item{Wq,Wk,Wv,Wo}{Attention projections.}
\item{W1,W2,W3}{SwiGLU projections.}
\item{n_heads,n_kv_heads}{GQA.}
\item{window}{Sliding window.}
\item{norm1,norm2}{Optional RMSNorm gains.}
\item{base}{RoPE base.}
}
\description{
One Mistral decoder block
}
\value{
A list with output, attention_mask, and bookkeeping.
}
\keyword{internal}
\examples{
set.seed(3)
L <- 4; d <- 8
X <- matrix(rnorm(L * d, 0, 0.5), L, d)
W <- function(o) matrix(rnorm(d * o, 0, 0.3), d, o)
r <- morie_mistr_mistral_block(X, Wq = W(8), Wk = W(4), Wv = W(4),
                               Wo = matrix(rnorm(8 * d, 0, 0.3), 8, d),
                               W1 = W(16),
                               W2 = matrix(rnorm(16 * d, 0, 0.3), 16, d),
                               W3 = W(16),
                               n_heads = 2, n_kv_heads = 1, window = 3)
str(r, max.level = 1)
}
