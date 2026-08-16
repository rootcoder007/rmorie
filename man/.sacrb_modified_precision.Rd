% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sacrb_modified_precision}
\alias{.sacrb_modified_precision}
\title{.sacrb_modified_precision}
\usage{
.sacrb_modified_precision(cand_tokens, refs_tokens, n)
}
\arguments{
\item{cand_tokens}{Passed to \code{.sacrb_ngram_counts}.}
\item{refs_tokens}{See Usage.}
\item{n}{Passed to \code{.sacrb_ngram_counts}.}
}
\description{
A step of the sacrb_native implementation. Called by \code{morie_sacrb_bleu}. See the file header for the source the module follows.
}
\value{
A list with \code{numerator}, \code{denominator}, \code{precision}.
}
\references{
----------
Papineni, K., Roukos, S., Ward, T. & Zhu, W.-J. (2002) "BLEU: a
Method for Automatic Evaluation of Machine Translation", *Proceedings
of the 40th Annual Meeting of the Association for Computational
Linguistics (ACL 2002)*, 311-318, doi:10.3115/1073083.1073135.
Sec. 2.1 (modified n-gram precision with clipping), Sec. 2.2.2 (the
sentence brevity penalty, the "best match length" as the closest
reference length, and the argument for computing the penalty over the
corpus rather than per sentence), and Sec. 2.3 (the geometric mean
and the BP formula reproduced above).

Post, M. (2018) "A Call for Clarity in Reporting BLEU Scores",
*Proceedings of the Third Conference on Machine Translation (WMT18)*,
186-191, doi:10.18653/v1/W18-6319, arXiv:1804.08771. That BLEU depends on tokenisation and
preprocessing, that these are usually unreported, and that scores are
therefore not comparable across papers; the proposal to score
detokenised output with a single internal tokenisation and to publish
a version signature.
"""
}
\keyword{internal}
