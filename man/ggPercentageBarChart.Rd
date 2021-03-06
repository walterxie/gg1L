% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GraphExtendedBarChart.R
\name{ggPercentageBarChart}
\alias{ggPercentageBarChart}
\title{Percentage Bar}
\usage{
ggPercentageBarChart(df.to.melt, melt.id, title = "Percentage Bar Chart",
  x.lab = "", y.lab = "", palette = NULL, colour.levels = c(),
  x.levels = c(), x.meta.data = NULL, x.text.angle = 90,
  autoWidth = TRUE, ...)
}
\arguments{
\item{df.to.melt}{A data frame required to \code{\link{melt}} before making a percent bar chart,
where Phyla have to be in row.names, otherwise it will be melted into values in y-axis to cause
"Error: Discrete value supplied to continuous scale".}

\item{melt.id}{A column name to \code{\link{melt}} and used to assign the colours.}

\item{title, x.lab, y.lab, palette, colour.levels, x.meta.data, x.text.angle, ...}{Refer to \code{\link{ggBarChart}}.}

\item{x.levels}{The level to order x axis.}

\item{autoWidth}{If TRUE, then use number of bars and legend columns
to estimate pdf width automatically. Default to TRUE.
#' @param title Graph title}
}
\description{
Percentage bar chart coloured by groups, which is extended from \code{\link{ggBarChart}}.
}
\examples{
data(reads.phyla)
bar.chart <- ggPercentageBarChart(reads.phyla, melt.id="TaxaGroup")
bar.chart$gg.plot
}
\keyword{bar}
\keyword{chart}

