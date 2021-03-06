% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Graph.R
\name{ggPlot}
\alias{ggBarChart}
\alias{ggBoxWhiskersPlot}
\alias{ggDensityEstimate}
\alias{ggHeatmap}
\alias{ggHistogram}
\alias{ggLineWithPoints}
\alias{ggPlot}
\alias{ggScatterPlot}
\title{One-line function to make \pkg{ggplot}}
\usage{
ggBarChart(df, x.id, y.id, fill.id = NULL, bar.pos = "dodge",
  bar.stat = "identity", x.interval = 0, x.trans = "identity",
  x.scale = "discrete", auto.scale.x = FALSE, y.trans = "identity",
  y.scale = "continuous", auto.scale.y = FALSE, x.facet.id = NULL,
  y.facet.id = NULL, facet.scales = "fixed", facet.space = "fixed",
  facet.shrink = TRUE, facet.drop = TRUE, xintercept = NULL,
  yintercept = NULL, line.type = 2, x.lim.cart = NULL,
  y.lim.cart = NULL, palette = NULL, legend.title = NULL,
  legend.col = 1, legend.row = 0, title = "Bar Chart",
  title.hjust = 0.5, title.size = 10, x.lab = NULL, y.lab = NULL,
  no.legend = NULL, legend.position = "right",
  legend.direction = "vertical", coord.flip = FALSE, x.text.angle = 0,
  x.text = TRUE, y.text = TRUE, plot.margin.cm = NULL,
  no.panel.border = FALSE, verbose = TRUE)

ggHistogram(df, x.id, fill.id = NULL, bar.pos = "stack", bins = NULL,
  binwidth = NULL, x.interval = 0, x.trans = "identity",
  x.scale = "continuous", auto.scale.x = FALSE, y.trans = "identity",
  x.facet.id = NULL, y.facet.id = NULL, coord.flip = FALSE,
  xintercept = NULL, yintercept = NULL, line.type = 2,
  x.lim.cart = NULL, y.lim.cart = NULL, palette = NULL,
  legend.title = NULL, legend.col = 1, legend.row = 0,
  title = "Histogram", title.hjust = 0.5, title.size = 10, x.lab = NULL,
  y.lab = NULL, no.legend = NULL, legend.position = "right",
  legend.direction = "vertical", x.text.angle = 0, x.text = TRUE,
  y.text = TRUE, plot.margin.cm = NULL, no.panel.border = FALSE,
  verbose = TRUE)

ggBoxWhiskersPlot(df, x.id, y.id, fill.id = NULL, colour.id = NULL,
  outlier.colour = alpha("black", 0.3), dodge.width = 0.8,
  x.facet.id = NULL, y.facet.id = NULL, facet.scales = "fixed",
  facet.space = "fixed", facet.shrink = TRUE, facet.drop = TRUE,
  y.trans = "identity", auto.scale.y = FALSE, x.lim.cart = NULL,
  y.lim.cart = NULL, lwd = 1, fatten = 1, palette = NULL,
  scale.to = "fill", scale.type = NULL, legend.title.fill = NULL,
  legend.title.colour = NULL, no.legend = NULL, legend.col = 1,
  legend.row = 0, legend.position = "right",
  legend.direction = "vertical", title = "Box Whiskers Plot",
  title.hjust = 0.5, title.size = 10, x.lab = NULL, y.lab = NULL,
  coord.flip = FALSE, x.text.angle = 0, x.text = TRUE, y.text = TRUE,
  no.panel.border = FALSE, verbose = TRUE)

ggScatterPlot(df, x.id, y.id, colour.id = NULL, text.colour.id = NULL,
  shape.id = NULL, shapes = NULL, link.id = NULL, ellipsed.id = NULL,
  text.id = NULL, text.or.point = 2, point.size = 3, point.alpha = 1,
  text.data = NULL, text.size = 3, text.alpha = 0.5, text.hjust = -0.1,
  text.vjust = -0.2, text.avoid.overlap = FALSE, text.repel = FALSE,
  box.padding = unit(0.25, "lines"), point.padding = unit(1e-06, "lines"),
  arrow = NULL, force = 1, x.trans = "identity", x.scale = "continuous",
  auto.scale.x = FALSE, y.trans = "identity", y.scale = "continuous",
  auto.scale.y = FALSE, x.facet.id = NULL, y.facet.id = NULL,
  facet.scales = "fixed", facet.space = "fixed", facet.shrink = TRUE,
  facet.drop = TRUE, xintercept = NULL, yintercept = NULL,
  line.type = 2, x.lim.cart = NULL, y.lim.cart = NULL, palette = NULL,
  scale.limits.min = NULL, legend.title.colour = NULL,
  legend.title.shape = NULL, legend.title.size = NULL, legend.col = 1,
  legend.row = 0, no.legend = NULL, legend.position = "right",
  legend.direction = "vertical", title = "Scatter Plot", title.size = 10,
  title.hjust = 0.5, x.lab = NULL, y.lab = NULL, x.text = TRUE,
  y.text = TRUE, x.text.angle = 0, coord.flip = FALSE,
  plot.margin.cm = NULL, no.panel.border = FALSE, verbose = TRUE)

ggLineWithPoints(df, x.id, y.id, group.id = NULL, colour.id = NULL,
  shape.id = NULL, shapes = NULL, text.id = NULL, text.colour.id = NULL,
  line.or.point = 3, line.size = 0.5, line.type = 1, line.alpha = 1,
  point.size = 3, point.alpha = 1, point.data = NULL, dodge.width = 0,
  text.or.point = 2, text.data = NULL, text.size = 3, text.alpha = 0.5,
  text.hjust = -0.1, text.vjust = -0.2, text.avoid.overlap = FALSE,
  text.repel = FALSE, box.padding = unit(0.25, "lines"),
  point.padding = unit(1e-06, "lines"), arrow = NULL, force = 1,
  x.facet.id = NULL, y.facet.id = NULL, facet.scales = "fixed",
  facet.space = "fixed", facet.shrink = TRUE, facet.drop = TRUE,
  x.trans = "identity", x.scale = "continuous", auto.scale.x = FALSE,
  y.trans = "identity", y.scale = "continuous", auto.scale.y = FALSE,
  x.lim.cart = NULL, y.lim.cart = NULL, palette = NULL,
  legend.title.group = NULL, legend.title.colour = NULL,
  legend.title.shape = NULL, legend.col = 1, legend.row = 0, title = "",
  title.hjust = 0.5, title.size = 10, x.lab = NULL, y.lab = NULL,
  no.legend = NULL, legend.position = "right",
  legend.direction = "vertical", coord.flip = FALSE, x.text.angle = 0,
  x.text = TRUE, y.text = TRUE, no.panel.border = FALSE, verbose = TRUE)

ggDensityEstimate(df, x.id, y.id = NULL, fill.id = NULL, colour.id = NULL,
  density.pos = "identity", density.alpha = 0.1, x.facet.id = NULL,
  y.facet.id = NULL, x.lim.cart = NULL, y.lim.cart = NULL,
  x.trans = "identity", x.scale = "continuous", auto.scale.x = FALSE,
  y.trans = "identity", y.scale = "continuous", auto.scale.y = FALSE,
  fill.palette = NULL, colour.palette = NULL, coord.flip = FALSE,
  legend.title.colour = NULL, legend.title.fill = NULL, legend.col = 1,
  legend.row = 0, title = "Kernel Density Estimate", title.hjust = 0.5,
  title.size = 10, x.lab = NULL, y.lab = NULL, no.legend = NULL,
  legend.position = "right", legend.direction = "vertical",
  x.text.angle = 0, x.text = TRUE, y.text = TRUE,
  no.panel.border = FALSE, verbose = TRUE)

ggHeatmap(df.to.melt, melt.id, low = "white", high = "steelblue",
  mid = "white", midpoint = NULL, colours = NULL, values = NULL,
  na.value = "transparent", limits = NULL, breaks = waiver(),
  add.label = T, label.digits = 1, auto.breaks.length = 5,
  breaks.digits = 0, title = "Heatmap", title.hjust = 0.5,
  title.size = 10, x.lab = "", y.lab = "", log.scale.colour = FALSE,
  legend.title = "Counts", x.lim.cart = NULL, y.lim.cart = NULL,
  coord.flip = FALSE, guide = "colourbar", legend.position = "right",
  legend.direction = "vertical", x.text.angle = 90, x.text = TRUE,
  y.text = TRUE, x.levels = c(), y.levels = c(),
  no.panel.border = FALSE, verbose = TRUE)
}
\arguments{
\item{df}{A data frame used for plot.}

\item{x.id, y.id, fill.id, group.id}{The string of column names in \code{df} or
\code{df.to.melt}, which use for \code{x, y, fill, group} in \code{\link{aes}}.}

\item{bar.pos}{Position adjustment for bars, either as a string,
or the result of a call to a position adjustment function.
Default to "dodge" in \code{ggBarChart}, "stack" in \code{ggHistogram}.
Use \code{fill.id} to generate group percentage bars.}

\item{bar.stat}{Determine what is mapped to bar height in \code{ggBarChart}.
Default to "identity",
which defines the heights of the bars to represent values in the data.
Refer to \code{\link{geom_bar}}.}

\item{x.interval}{The interval to display x values in axis.
Assume x values are discrete for each bar. Default to 0 to do nothing.}

\item{x.trans, y.trans}{The string defines the data scale used in either x-axis or y-axis,
which can be "identity" standing for normal, or "per" standing for percentage,
moreover either the name of a transformation object for \code{\link{scale_x_continuous}}
or \code{\link{scale_y_continuous}} (e.g. \code{trans="log"}), or the object itself.
Built-in transformations include "asn", "atanh", "boxcox", "exp", "identity",
"log", "log10", "log1p", "log2", "logit", "probability", "probit", "reciprocal",
"reverse" and "sqrt". Default to "identity".}

\item{x.scale, y.scale, auto.scale.x, auto.scale.y}{Which data type to scale, use either
"continuous" or "discrete". If set \code{auto.scale.?} to TRUE, then try to rescale
according to the maximun value. Default to FALSE to use original plot.}

\item{x.facet.id, y.facet.id}{The string of column names in \code{df},
which creates facets (a formula) in \code{\link{facet_grid}}.}

\item{facet.scales, facet.space, facet.shrink, facet.drop}{The parameters refer to \code{\link{facet_grid}}.}

\item{xintercept, yintercept}{Add horizontal or vertical line.
Refer to \code{\link{geom_hline}} or \code{\link{geom_vline}}.}

\item{line.type, line.size, line.alpha}{The feature of lines for \code{\link{geom_line}}.
Default line.type to 1, size to 0.5, alpha to 1.}

\item{x.lim.cart, y.lim.cart}{Setting limits on the coordinate system will zoom the plot,
and will not change the underlying data like setting limits on a scale will.
Refer to \code{\link{coord_cartesian}}.
Set lower bound only to y-axis using y.lim.cart=c(1000,NA). Default NULL.}

\item{palette}{The colour palette for bar, box, scatter plot, etc.
If length == 1, then use \code{\link{scale_colour_brewer}}
(\url{http://www.datavis.ca/sasmac/brewerpal.html}), such as "Set1" (max 8 colours).
If 1 != length <= 3, then use \code{\link{scale_colour_gradientn}},
such as c("blue", "orange").
Otherwise use \code{\link{scale_fill_manual}} for a vector of customized colours.
Set palette=c("red","darkgreen","darkgrey",""), if \code{\link{scale_fill_manual}}
has to be used when the actual length(palette) <=3.
Default NULL to use \code{\link{ggplot}} default colours.}

\item{legend.title, legend.title.fill, legend.title.colour, legend.title.shape, legend.title.group, legend.title.size, }{The title of legend created by fill, colour, shape, group, or size.
Set legend.title.*="" to remove legend.}

\item{legend.col, legend.row}{Customize the number of columns or rows for legend in bar chart.
They cannot be used at the same time. Default not to use them, legend.col=1, legend.row=0.}

\item{title}{Graph title, set title="" to remove it from the plot.}

\item{title.hjust}{Numeric, adjust title position, default to 0.5 (middle),
0 is left and 1 right.}

\item{title.size}{Graph title font size.}

\item{x.lab, y.lab}{The label of x-axis or y-axis, such as plot names.
Set x.lab="" to remove x-axis label from the plot. Default to NULL to do nothing.}

\item{no.legend}{Turning off some legends, such as, fill, shape, colour.
Default to NULL, which is to do nothing.}

\item{legend.position}{Legend position, default to "right".}

\item{legend.direction}{The direction to display legend, default to "vertical".}

\item{coord.flip}{If TRUE, then flip cartesian coordinates so that horizontal
becomes vertical, and vertical becomes horizontal. Default to FALSE. Refer to
\code{\link{coord_flip}}.}

\item{x.text.angle}{The angle to rotate tick labels along x-axis.}

\item{x.text, y.text}{If FALSE, hide tick labels along x-axis or y-axis in plot.
Default to TRUE.}

\item{plot.margin.cm}{margin around entire plot in a 4-element vector
(the sizes of the top, right, bottom, and left margins in cm),
such as \code{c(1,2,3,4)}.}

\item{no.panel.border}{Add panel border or not. Default to FALSE.}

\item{verbose}{More messages. Default to TRUE.}

\item{bins}{Number of bins for \code{ggHistogram}. Overridden by \code{binwidth}.
Defaults to 30.
or the result of a call to a position adjustment function. Default to "stack".
Use \code{fill.id} to generate group percentage bars.}

\item{binwidth}{The width of the bins for \code{ggHistogram}.
The default is to use \code{bins} that cover the range of the data.
You should always override this value, exploring multiple widths to find
the best to illustrate the stories in your data.
The bin width of a date variable is the number of days in each time;
the bin width of a time variable is the number of seconds.
Refer to \code{\link{geom_histogram}}.}

\item{colour.id, shape.id, link.id}{The column name in \code{df} to
define how the data points are coloured, shaped, or linked according their values.}

\item{outlier.colour}{The colour of outliers in box whiskers plot
used for \code{outlier.colour} in \code{\link{geom_boxplot}}.
Default to alpha("black", 0.3).}

\item{dodge.width}{Dodging width, when different to the width of
the individual elements in box Whiskers plot.
This is useful when you want to align narrow geoms with wider geoms.
Refer to \code{\link{position_dodge}}.}

\item{lwd}{change the line thinkness.}

\item{fatten}{make the median line thinner relative to the other lines.}

\item{scale.to, scale.type}{The scale of palette, where \code{scale.to = c("colour", "fill")},
\code{scale.type = c("brewer", "gradientn", "manual")}.}

\item{text.colour.id}{The column name in \code{df} to define
the group of texts and assign colours.}

\item{shapes}{Manually define the shapes of points.
Refer to \code{\link{scale_shape_manual}},
and \url{http://www.cookbook-r.com/Graphs/Shapes_and_line_types/}.}

\item{ellipsed.id}{The column name in \code{df} to define
how to draw ellipse over data points, which is normally same as
\code{colour.id} to show clusters.
The ellipses are drawn by \code{\link{stat_ellipse}},
where the method for calculating the ellipses has been modified
from (Fox and Weisberg, 2011).}

\item{text.id}{Label the data points according \code{text.id} column,
such as "Row.names" column after \code{\link{merge}}.}

\item{text.or.point}{If 1, then display the text only;
if 2, the default, then only points;
if 3, then both the text and points.}

\item{point.size, point.alpha}{The feature of points for
\code{\link{geom_point}}. Default size to 3, alpha to 1.}

\item{text.data}{The data for \code{\link{geom_text}}
if they are different.}

\item{text.size, text.hjust, text.vjust, text.alpha}{The arguments to adjust text in \code{\link{geom_text}} in the line or scatter plot.}

\item{text.avoid.overlap}{If TRUE, text that overlaps previous text
in the same layer will not be plotted. Not recommended. Default to FALSE.}

\item{text.repel}{Apply \code{\link{geom_text_repel}} in \pkg{ggrepel}.
It is better than \code{text.avoid.overlap},
where the latter sometime removes overlapped texts.
Default to FALSE.}

\item{box.padding, point.padding, arrow, force}{The parameters refer to
\code{\link{geom_text_repel}}.}

\item{scale.limits.min}{Manually set the minimum data range of the colour scale,
for example, in legend. The code set \code{limits} in \code{\link{discrete_scale}}
to \code{c(scale.limits.min, max(df[,colour.id]))}.}

\item{line.or.point}{If 1, then display the line only; if 2, then only points;
if 3, the default, then both the line and points.}

\item{point.data}{An option to add extra points in the plot, such as \code{gtLine}.
The data to be displayed as points in \code{\link{geom_point}}, which can be
the subset of points to draw the line.
If NULL, the default, the data is inherited from the plot data as specified in the call.}

\item{density.pos}{Position adjustment for \code{ggDensityEstimate}, either as a string,
or the result of a call to a position adjustment function. Default to "identity".
Use "stack" to produce stacked density plots,
and "fill" to plot density estimate in percentage scale.}

\item{density.alpha}{Modify colour transparency when \code{fill.id} is assigned to
\code{ggDensityEstimate}. Default to 0.1. Refer to \code{\link{alpha}}.}

\item{fill.palette, colour.palette}{The palette for filling colours and outline colours.}

\item{df.to.melt}{A data frame required to \code{\link{melt}}
before making a \pkg{ggplot} object, such as input of \code{ggHeatmap}.
At least one column should be \code{melt.id}, and the other columns should be values,
unless they are fill.id, group.id, colour.id, etc.
\strong{Note:} any extra column will disturb the result of \code{\link{melt}} function.

If row.names is going to be \code{melt.id},
then it can be inserted into the data frame before the plot.

The data example \code{data(reads.phyla)}:
\tabular{rrrrr}{
  Phyla \tab 16s \tab 18s \tab ITS \tab TaxaGroup\cr
  Actinobacteria \tab 958 \tab 1 \tab 3 \tab Bacteria\cr
  Crenarchaeota \tab 1 \tab 0 \tab 0 \tab Archaea\cr
  Ascomycota \tab 2 \tab 765 \tab 971 \tab Fungi
}}

\item{melt.id}{A column name to \code{\link{melt}}
and used as a \code{\link{factor}}, such as "plot" column.}

\item{low, high, mid, na.value, midpoint, colours, values}{The colour range of heatmap.
Default to low="white", high="steelblue".
Refer to \code{\link{scale_fill_gradient}} or \code{\link{scale_fill_gradient2}}
(if midpoint is given).}

\item{limits, breaks}{Refer to \code{\link{discrete_scale}}.}

\item{add.label, label.digits}{\code{add.label} is logical, default to TRUE,
to add value into the tiles of heatmap.
\code{label.digits} determines the digit of the label.}

\item{auto.breaks.length, breaks.digits}{Heatmap ledgend breaks automatically.
If \code{auto.breaks.length <= 1}, then use \code{breaks}, the default is 5.
If values are between 0 and 1, then make sure \code{breaks.digits} is set properly,
such as 2 in this case.}

\item{log.scale.colour}{If TRUE, then use log scale to the colour of heat map.
Default to FALSE.}

\item{guide}{Type of legend. Use "colourbar" as default for continuous colour bar,
or "legend" for discrete colour legend, or set "FALSE" to turn off legend.
See \code{\link{scale_fill_gradient}}.}

\item{x.levels, y.levels}{The levels to order x or y.
The deafult y.levels=c() makes y labels sorted by alphabetical order,
but if y.levels is NA, there is no sorting applied.}
}
\value{
If the function returns a \code{\link{ggplot}} object,
then its name starts with "gg".
It needs to use \code{\link{pdf.ggplot}} to create pdf.
It also can be concatenated using '+'.

If the function returns a \code{gtable} object,
then its name starts with "gt".
It needs to use \code{\link{pdf.gtable}} to create pdf,
or \code{\link{plotgt}} to plot to console.
}
\description{
Simplify \pkg{ggplot} graphic coding into one-line functions that provides frequently
used graphies in a convenient way. They are very basic charts here, and can be used or
extended into various common charts.
}
\details{
\code{ggBarChart} is an one-line function to plot many types of bar chart,
such as normal bars, log-scaled bars, percentage bars, and also grouping.
Refer to \code{\link{geom_bar}}.

\code{ggHistogram} is an one-line function to plot a 1d distribution by dividing
into bins and counting the number of observations in each bin.
Refer to \code{\link{geom_histogram}}.

\code{ggBoxWhiskersPlot} creates box Whiskers plot.
Refer to \code{\link{geom_boxplot}}.

\code{ggScatterPlot} uses one-line function to plot many types of scatter chart.
Refer to \code{\link{geom_point}}.

\code{ggLineWithPoints} uses one-line function to plot a line or group of lines.
Refer to \code{\link{geom_line}}.

\code{ggDensityEstimate} is an one-line function to plot kernel density estimate (KDE),
useful for display the distribution of variables with underlying smoothness.
Give a column name \code{fill.id} to colour the filled area, and \code{colour.id}
to colour the curve. Use \code{density.pos} to produce different types of density.
Use \code{y.id="..count.."} to produce a conditional density estimate,
which counts (density * n) variable instead of the default density.
Refer to \code{\link{geom_density}}.

\code{ggHeatmap} creates a heat map using ggplot. It is devried and improved
from the code
\url{https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/}.
}
\note{
All basic charts are designed to return a \code{\link{ggplot}}
object for easy to extend, you may need to turn off clipping using
\code{unclip.ggplot} if there are overlaps between points/labels and axes.
}
\examples{
data(model.test)
model.test.mac <- model.test[model.test$OS=="Mac",]
ggBarChart(model.test.mac, x.id="test", y.id="performance", fill.id="model", x.text.angle=90)
ggBarChart(model.test.mac, x.id="test", y.id="performance", fill.id="model", x.text.angle=90, bar.pos="stack")
ggBarChart(model.test.mac, x.id="test", y.id="performance", fill.id="model", x.text.angle=90, bar.pos="fill", y.trans="per")

ggHistogram(model.test[model.test$performance<3,], x.id="performance", fill.id="model", binwidth=0.1, x.text.angle=90)

ggBoxWhiskersPlot(model.test, x.id="test", y.id="performance", colour.id="OS", x.text.angle=90)

# plot 2 clusters
df.clusters <- random2Clusters()
df.clusters$labels <- rownames(df.clusters)
df.clusters
gg.plot <- ggScatterPlot(df.clusters, x.id="x", y.id="y", colour.id="group", shape.id="group",
                         xintercept=0, yintercept=0, title="Clusters", palette="Set1")
# turns off clipping
g.table <- unclip.ggplot(gg.plot)
plot(g.table)

# selective labeling for points x > 3 and y > 6
ggScatterPlot(df.clusters, x.id="x", y.id="y", colour.id="group", ellipsed.id="group",
              text.id="labels", text.data=subset(df.clusters, x > 3 & y > 6),
              xintercept=0, yintercept=0, title="Clusters", palette="Set1")

# prepare log
data(mcmc.log)
names(mcmc.log)
mcmc.log$state <- as.double(rownames(mcmc.log))
burnin <- nrow(mcmc.log) * 0.1
b.log <- mcmc.log[burnin:nrow(mcmc.log),]

gg.plot <- ggLineWithPoints(mcmc.log[,c("TreeHeight.Species", "state")], x.id="state", y.id="TreeHeight.Species")
# turns off clipping
g.table <- unclip.ggplot(gg.plot)
plot(g.table)

ggLineWithPoints(mcmc.log[,c("TreeHeight.Species", "state")], x.id="state", y.id="TreeHeight.Species", line.or.point=1)
ggLineWithPoints(mcmc.log[,c("TreeHeight.Species", "state")], x.id="state", y.id="TreeHeight.Species", line.or.point=2, point.size=1)

require(reshape2)
df.melt <- melt(b.log, id="state")
df.TreeHeight <- df.melt[grep("TreeHeight", df.melt[,"variable"]),]

ggDensityEstimate(df.TreeHeight, x.id="value", colour.id="variable")
ggDensityEstimate(df.TreeHeight, x.id="value", fill.id="variable", colour.id="variable")
# stacked density plot to lose marginal densities
ggDensityEstimate(df.TreeHeight, x.id="value", fill.id="variable", density.pos="stack", density.alpha=1)
# conditional density plot to preserve marginal densities
ggDensityEstimate(df.TreeHeight, x.id="value", y.id="..count..", fill.id="variable", density.pos="stack", density.alpha=1)
# percentage scale
ggDensityEstimate(df.TreeHeight, x.id="value", fill.id="variable", density.pos="fill", density.alpha=1)

ranks.by.group <- data.frame(plot=c("Plot03","Plot02","Plot01"), `16s`=c(3,2,1), `18s`=c(1,2,3), ITS=c(2,1,3), check.names = FALSE)
ranks.by.group
ggHeatmap(ranks.by.group, melt.id="plot")

}
\references{
More details and tutorials are available at
\url{https://github.com/walterxie/gg1L}
}
\seealso{
Percentage bar chart \code{\link{ggPercentageBarChart}},
nonmetric multidimensional scaling plot \code{\link{gtNMDSPlot}},
PCA plot \code{\link{gtPCAPlot}},
rarefaction curves \code{\link{gtRarefactionCurve}}.

In addition, some useful charts that we derived from our publications,
such as group abundance bar chart \code{\link{ggGroupAbundanceBar}},
and Y across X bar chart \code{\link{ggGroupAbundanceBar}}.
Both refer to \url{http://dx.doi.org/10.1186/s13742-015-0086-1}.
}
\keyword{KDE}
\keyword{bar}
\keyword{box}
\keyword{chart}
\keyword{heatmap}
\keyword{histogram}
\keyword{line}
\keyword{plot}
\keyword{points}
\keyword{scatter}

