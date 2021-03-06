% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GraphUtils.R
\name{gLegend}
\alias{gLegend}
\title{Extract legend in \pkg{ggplot2}}
\source{
\url{http://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2}
}
\usage{
gLegend(a.gplot)
}
\arguments{
\item{a.gplot}{The \code{\link{ggplot}} object.}
}
\value{
The legend.
}
\description{
Extract legend in \pkg{ggplot2}
}
\examples{
library(ggplot2)
my_hist<-ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()
legend <- gLegend(my_hist)
library(grid)
grid.draw(legend)
}
\keyword{utils}

