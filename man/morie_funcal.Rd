% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_funcal}
\alias{morie_funcal}
\title{morie_funcal}
\usage{
morie_funcal(hits, groups, annotations, taxa = NULL, target_taxa = NULL, target_types = NULL, sources = NULL, evalue = 0.001, score = 60, query_cov = 0.2, target_cov = 0.2, min_support = 1, searcher = "diamond")
}
\arguments{
\item{hits}{Passed to \code{morie_funcal_seed_orthologs}.}
\item{groups}{Passed to \code{morie_funcal_assign_orthologs}.}
\item{annotations}{Passed to \code{morie_funcal_transfer_terms}.}
\item{taxa}{Passed to \code{morie_funcal_assign_orthologs}.}
\item{target_taxa}{Optional; may be \code{NULL}. Passed to \code{morie_funcal_assign_orthologs}.}
\item{target_types}{Optional; may be \code{NULL}. Passed to \code{morie_funcal_assign_orthologs}.}
\item{sources}{Passed to \code{morie_funcal_transfer_terms}.}
\item{evalue}{Passed to \code{morie_funcal_seed_orthologs}. Defaults to \code{0.001}.}
\item{score}{Passed to \code{morie_funcal_seed_orthologs}. Defaults to \code{60}.}
\item{query_cov}{Passed to \code{morie_funcal_seed_orthologs}. Defaults to \code{0.2}.}
\item{target_cov}{Passed to \code{morie_funcal_seed_orthologs}. Defaults to \code{0.2}.}
\item{min_support}{Passed to \code{morie_funcal_transfer_terms}. Defaults to \code{1}.}
\item{searcher}{Passed to \code{morie_funcal_seed_orthologs}. Defaults to \code{"diamond"}.}
}
\description{
A step of the funcal_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{annotations}, \code{seeds}, \code{orthologs}, \code{n_queries}, \code{n_with_seed}, \code{n_annotated}, \code{searcher}, \code{target_taxa}, \code{target_types}, \code{min_support}, \code{method}, \code{note}.
}
\keyword{internal}
