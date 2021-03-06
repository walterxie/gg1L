% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg1LBase.R
\name{ggOptFacetGrid}
\alias{ggOptFacetGrid}
\title{Facet grid}
\usage{
ggOptFacetGrid(p, col.names, x.facet.id = NULL, y.facet.id = NULL,
  scales = "fixed", space = "fixed", shrink = TRUE, drop = TRUE,
  verbose = TRUE)
}
\arguments{
\item{col.names}{Colum names of data, which are only used to validate
if *.id exsits in the data.
which creates facets (a formula) in \code{\link{facet_grid}}.}

\item{x.facet.id, y.facet.id}{The string of column names in \code{df},}

\item{scales, space, shrink, drop}{The parameters refer to \code{\link{facet_grid}}.}
}
\description{
It passes arguments to \code{\link{facet_grid}}.
}
\examples{
col.names <- colnames(df)
p <- ggOptFacetGrid(p, col.names, x.facet.id="gene")

}

