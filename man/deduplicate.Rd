% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{deduplicate}
\alias{deduplicate}
\title{Deduplicate by cosine similarity}
\usage{
deduplicate(embeddings, threshold = 0.999)
}
\arguments{
\item{embeddings}{Numeric matrix (n x d), or list of row vectors.}
\item{threshold}{Cosine-similarity threshold for "near-duplicate".}
}
\description{
Walks the rows of an embedding matrix and drops any whose cosine similarity to an already-kept row is at least the threshold.
}
\value{
List with \code{keep}, \code{dropped}, \code{n_before},   \code{n_after}, \code{note}.
}
\references{
Oquab, M., Darcet, T., Moutakanni, T., Vo, H., Szafraniec, M.,
Khalidov, V., Fernandez, P., Haziza, D., Massa, F., El-Nouby, A. et
al. (2024) "DINOv2: Learning Robust Visual Features without
Supervision", TMLR 01/2024, arXiv:2304.07193.
Caron, M., Touvron, H., Misra, I., Jegou, H., Mairal, J., Bojanowski,
P. & Joulin, A. (2021) "Emerging Properties in Self-Supervised Vision
Transformers", ICCV 2021, arXiv:2104.14294. DINO.
Zhou, J., Wei, C., Wang, H., Shen, W., Xie, C., Yuille, A. & Kong, T.
(2022) "iBOT: Image BERT Pre-Training with Online Tokenizer", ICLR
2022, arXiv:2111.07832.
Sablayrolles, A., Douze, M., Schmid, C. & Jegou, H. (2019)
"Spreading vectors for similarity search", ICLR 2019,
arXiv:1806.03198.
}
\keyword{internal}
