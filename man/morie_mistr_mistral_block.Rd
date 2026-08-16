% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mistr_mistral_block}
\alias{morie_mistr_mistral_block}
\title{One Mistral decoder block}
\usage{
morie_mistr_mistral_block(X, Wq, Wk, Wv, Wo, W1, W2, W3, n_heads, n_kv_heads, window, norm1 = NULL, norm2 = NULL, base = 10000)
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
