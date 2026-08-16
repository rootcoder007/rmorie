% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{strec_stamp_scores}
\alias{strec_stamp_scores}
\title{strec_stamp_scores}
\usage{
strec_stamp_scores(embeddings, item_table, Ws, Wt, bs = NULL, bt = NULL, attention = NULL)
}
\arguments{
\item{embeddings}{Passed to \code{.strec_as_rows}.}
\item{item_table}{Passed to \code{.strec_as_rows}.}
\item{Ws}{Passed to \code{strec_mlp_cell}.}
\item{Wt}{Passed to \code{strec_mlp_cell}.}
\item{bs}{Passed to \code{strec_mlp_cell}.}
\item{bt}{Passed to \code{strec_mlp_cell}.}
\item{attention}{Optional; may be \code{NULL}. A list; the body reads \code{$m_a} from it.}
}
\description{
A step of the strec_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{ranking}, \code{probability}, \code{score}, \code{h_s}, \code{h_t}, \code{attention_used}, \code{model}, \code{method}, \code{note}.
}
\references{
----------
Liu, Q., Zeng, Y., Mokhosi, R. & Zhang, H. (2018) "STAMP: Short-Term
Attention/Memory Priority Model for Session-based Recommendation",
*Proceedings of the 24th ACM SIGKDD International Conference on
Knowledge Discovery & Data Mining (KDD \'18)*, 1831-1839,
doi:10.1145/3219819.3219950. [PDF supplied by Vee.] The abstract and
Sec. 1 (that predicting from a session prefix without allowing for
users\' interests drifting with time is problematic, the digital-camera
example, and the proposal of a short-term attention/memory priority
model capturing general interests from the long-term memory of the
session context while taking the current interests from the short-term
memory of the last click); Sec. 3.1 (the trilinear product
<a,b,c> = sum a_i b_i c_i = a^T (b (*) c)); Sec. 3.2 (the STMP model
with m_s the average of the external memory, m_t = x_t the last click,
two identically structured MLP cells with independent parameters, the
score z_i = sigma(<h_s, h_t, x_i>), the softmax over candidates and
the cross-entropy loss); and Sec. 3.3 (that treating each item in the
prefix as equally important is problematic for interest drift in long
sessions, and the attention net alpha_i = W_0 sigma(W_1 x_i + W_2 x_t
+ W_3 m_s + b_a) with m_a the attention-weighted sum replacing m_s).

Li, J., Ren, P., Chen, Z., Ren, Z., Lian, T. & Ma, J. (2017) "Neural
Attentive Session-based Recommendation", *CIKM 2017*, 1419-1428,
arXiv:1711.04725. NARM, which the paper distinguishes itself from --
it combines main purpose and sequential behaviour as equally
important, where STAMP explicitly privileges the last click;
implemented in :mod:`narm`.
}
\keyword{internal}
\examples{
strec_stamp_scores(embeddings = 5L, item_table = c(1, 2, 3, 4, 5, 6, 7, 8), Ws = c(1, 2, 3, 4, 5, 6, 7, 8), Wt = c(1, 2, 3, 4, 5, 6, 7, 8))
}
