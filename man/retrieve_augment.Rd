% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{retrieve_augment}
\alias{retrieve_augment}
\title{Retrieve uncurated nearest neighbours for each curated query}
\usage{
retrieve_augment(curated, uncurated, per_query = 2L, min_similarity = 0)
}
\arguments{
\item{curated}{Numeric matrix (n_c x d).}
\item{uncurated}{Numeric matrix (n_u x d).}
\item{per_query}{Number of neighbours requested per curated row.}
\item{min_similarity}{Floor on cosine similarity to keep a pick.}
}
\description{
Retrieve uncurated nearest neighbours for each curated query
}
\value{
List with \code{retrieved}, \code{per_query},   \code{duplication}, \code{n_added}, \code{max_times_retrieved},   \code{note}.
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
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
retrieve_augment(V, V)
}
