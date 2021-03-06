# GraphUtils
# Author: Walter Xie
# Accessed on 11 Mar 2016

#' @name pdf
#' @title Create pdf
#' @description
#' The extended methods for producing PDF graphics.
#'
#' @details \code{pdf.ggplot} creates pdf for ggplot object.
#'
#' @param gg.plot A \code{\link{ggplot}} object.
#' @param fig.path fig.path The full path of pdf file.
#' @param width,height Refer to \code{\link{pdf}}. Default to width=6, height=6.
#' @param useDingbats Defaults to TRUE, which produces smaller and better output.
#' Setting this to FALSE can work around font display problems in broken PDF viewers.
#' Refer to \code{\link{pdf}}.
#' @import grid
#' @import gridExtra
#' @import grDevices
#' @keywords graph
#' @export
#' @examples
#' data(model.test)
#' model.test.mac <- model.test[model.test$OS=="Mac",]
#' gg.plot <- ggBarChart(model.test.mac, x.id="test", y.id="performance", fill.id="model", x.text.angle=90)
#'
#' pdf.ggplot(gg.plot, fig.path="fig.pdf", width=8, height=8)
#'
#' @rdname pdf
pdf.ggplot <- function(gg.plot, fig.path, width=6, height=6, useDingbats=TRUE, ...) {
  pdf(fig.path, width=width, height=height, useDingbats=useDingbats, ...)
  print(gg.plot)
  invisible(dev.off())
}

#' @details \code{pdf.gtable} creates pdf for gtable object.
#'
#' @param g.table A \code{gtable} object.
#' @keywords graph
#' @export
#' @examples
#' g.table <- unclip.ggplot(gg.plot)
#' pdf.gtable(g.table, fig.path="fig.pdf", width=8, height=8)
#'
#' @rdname pdf
pdf.gtable <- function(g.table, fig.path, width=6, height=6, useDingbats=TRUE, ...) {
  pdf(fig.path, width=width, height=height, useDingbats=useDingbats, ...)
  plotgt(g.table)
  invisible(dev.off())
}

#' @details \code{plotgt} uses \code{\link{grid.draw}}
#' to plot gtable object in console.
#'
#' @keywords graph
#' @export
#' @examples
#' plotgt(g.table)
#'
#' @rdname pdf
plotgt <- function(g.table) {
  require(grid)
  grid.draw(g.table)
}

#' @details \code{unclip.ggplot} turns off clipping for a
#' \code{\link{ggplot}} object, but returns a
#' \code{gtable} object.
#' Refer to \url{http://stackoverflow.com/questions/9690648/point-clipped-on-x-axis-in-ggplot}.
#'
#' @keywords graph
#' @export
#' @rdname pdf
unclip.ggplot <- function(gg.plot) {
  gt <- ggplot_gtable(ggplot_build(gg.plot))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"
  return(gt)
}

#' @details \code{pdf.plot} creates pdf for the list returned from \code{\link{plot}}.
#' Tip: use "\%<a-\%" in \pkg{pryr} to save plots.
#'
#' @param plot The list returned from \code{\link{plot}}.
#' @param ... Other parameters pass to \code{\link{pdf}}.
#' @keywords graph
#' @export
#' @examples
#' library(pryr)
#' p %<a-% plot(1:5)
#' pdf.plot(p, fig.path="plot1-5.pdf")
#'
#' @rdname pdf
pdf.plot <- function(plot, fig.path, width=6, height=6, useDingbats=TRUE, ...) {
  pdf(fig.path, width=width, height=height, useDingbats=useDingbats, ...)
  plot
  invisible(dev.off())
}

#' \code{grid_arrange_shared_legend} shares a legend
#' between multiple plots using \code{\link{grid.arrange}}.
#' Return a \code{gtable} object.
#'
#' Modified from
#' \url{http://rpubs.com/sjackman/grid_arrange_shared_legend}.
#'
#' @param ... The list of \code{\link{ggplot}} objects.
#' @param input.list Default to FALSE to unwrap list(...) to
#' get the actual list if the input is a list of plots
#' @param legend.position The position of legends
#' ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param ncol,nrow Specify the grid.
#' @param widths A unit vector giving the width of each column in \code{gtable}.
#' \code{length(widths) == ncol}, for example, widths=c(1, 0.1, 0.1) for 3 columns.
#' @param unclip.ggplot If TRUE, as default, use \code{\link{unclip.ggplot}}
#' to turn off clipping.
#' @keywords graph
#' @export
#' @examples
#' library(ggplot2)
#' dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
#' p1 <- qplot(carat, price, data=dsamp, colour=clarity)
#' p2 <- qplot(cut, price, data=dsamp, colour=clarity)
#' p3 <- qplot(color, price, data=dsamp, colour=clarity)
#' p4 <- qplot(depth, price, data=dsamp, colour=clarity)
#' grid_arrange_shared_legend(p1, p2, p3, p4)
#'
#' grid_arrange_shared_legend(list(p1, p2, p3, p4), input.list=TRUE)
grid_arrange_shared_legend <- function(..., input.list=FALSE, legend.position="bottom",
                                       ncol=2, nrow=2, widths=c(1, 0.1), unclip.ggplot=TRUE) {
  plots <- list(...)
  if (!input.list && is(plots[[1]], "list"))
    stop("Invaild input: find a list of plots, please set input.list=T !")
  if (input.list && is(plots, "list"))
    plots <- plots[[1]]
  # validation
  if (! (is(plots, "list") && length(plots) > 0) )
    stop("Invaild input: either not a list, please set input.list=F, or empty input, length = ", length(plots), " !")

  cat("The grid allocates", length(plots), "plots in", ncol, "columns", nrow, "rows.\n")
  if ( !(length(plots) != ncol*nrow || length(plots) != ncol*nrow-1) )
    stop("Incorrect grid ", ncol, " * ", nrow, " for total ", length(plots), " plots !")

  require(ggplot2)
  g <- ggplotGrob(plots[[1]] + theme(legend.position=legend.position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  plots <- lapply(plots, function(x) x + theme(legend.position="none") )
  if (unclip.ggplot)
    plots <- lapply(plots, gg1L::unclip.ggplot)
  args.list <- c(plots, list(ncol=ncol, nrow=nrow))

  require(gridExtra)
  grid.arrange(
    do.call(arrangeGrob, args.list),
    legend, ncol=ncol, widths=widths)
}


#' Defining the scale change.
#' It is mostly used to force bars to start from a lower value than 0 in \pkg{ggplot2} \code{\link{geom_bar}} in R
#' @source \url{http://stackoverflow.com/questions/22295253/force-bars-to-start-from-a-lower-value-than-0-in-ggplot-geom-bar-in-r}
#'
#' @param base The base of logarithm to use. Default to exp(1).
#' @param from The value to start from. Default to 0.
#' @return
#' The scale.
#' @keywords utils
#' @export
#' @examples
#' # starts from 1e-2
#' library(ggplot2)
#' scale_y_continuous(trans = mylog_trans(base=10, from=-2))
mylog_trans <- function(base=exp(1), from=0) {
  trans <- function(x) log(x, base)-from
  inv <- function(x) base^(x+from)

  require(scales)
  trans_new("mylog", trans, inv, log_breaks(base=base), domain = c(base^from, Inf))
}

#' Scientific notation
#'
#' @param x The number (not string type).
#' @return
#' The mathematical \code{\link{expression}} in scientific notation of a given number.
#' @keywords graph
#' @export
#' @examples
#' #expression(10^04)
#' scientific_10(10000)
scientific_10 <- function(x) {
  suppressMessages(suppressWarnings(require(scales)))
  text=gsub("1e\\+00", "1", scientific_format()(x))
  text=gsub("1e\\+01", "10", text)
  text=gsub("0e\\+00", "0", text)
  text=gsub("1e\\-01", "0.1", text)
  text=gsub("1e\\+", "10^", text)
  parse(text=text)
}

#' Breaks of multiples of 10 for positive values.
#'
#' @param max.v The max value to create breaks.
#' @param start The vector of values before 10. Default to c(0.1, 1).
#' @return
#' The vector of multiples of 10 used for breaks.
#' Mostly used with \code{\link{scientific_10}} together.
#' @keywords utils
#' @export
#' @examples
#' get_breaks_positive_values(max(reads.phyla[,1:6]), start=c(0))
#'
#' library(ggplot2)
#' scale_y_continuous(trans = "log", labels = gg1L::scientific_10,
#' breaks = gg1L::get_breaks_positive_values(max(reads.phyla[,1:6]), start=c(0)))
get_breaks_positive_values <- function(max.v, start=c(0.1, 1)) {
  breaks=c(start, 10, 100, 1000, 10000, 100000, 1000000)
  if (max.v < 50) {
    breaks=c(start, 10)
  } else if (max.v < 500) {
    breaks=c(start, 10, 100)
  } else if (max.v < 5000) {
    breaks=c(start, 10, 100, 1000)
  } else if (max.v < 5e+04) {
    breaks=c(start, 10, 100, 1000, 1e+04)
  } else if (max.v < 5e+05) {
    breaks=c(start, 10, 100, 1000, 1e+04, 1e+05)
  } else if (max.v < 5e+06) {
    breaks=c(start, 10, 100, 1000, 1e+04, 1e+05, 1e+06)
  } else {
    breaks=c(start, 10, 100, 1000, 1e+04, 1e+05, 1e+06, 1e+07)
  }

  return(breaks)
}

#' Extract legend in \pkg{ggplot2}
#' @source \url{http://stackoverflow.com/questions/12041042/how-to-plot-just-the-legends-in-ggplot2}
#'
#' @param a.gplot The \code{\link{ggplot}} object.
#' @return The legend.
#' @import ggplot2
#' @keywords utils
#' @export
#' @examples
#' library(ggplot2)
#' my_hist<-ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()
#' legend <- gLegend(my_hist)
#' library(grid)
#' grid.draw(legend)
gLegend<-function(a.gplot){
  require(ggplot2)
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#' Customised palette with about 70 colours.
#'
#' @param add.grey Add grey colours in the begining of palette. Default to 0.
#' @param n The number of colors (>= 1) to be in the palette. If missing, then return the whole palette.
#' @return
#' The customised palette with about 70 colours.
#' @keywords utils
#' @export
#' @examples
#' myPalette <- getMyPalette()
#TODO: use http://tools.medialab.sciences-po.fr/iwanthue/
# https://github.com/hoesler/rwantshue
getMyPalette<-function(n, add.grey=0){
  greyPalette <- c("#CCCCCC", "#999999")
  myPalette <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#AE22EC", "#FAF629",
                 "#CAB2D6", "#6A3D9A", "#B15928", "#E69F00", "#FF6666", "#56B4E9", "#006600", "#009E73", "#F0E442", "#FF3333",
                 "#0072B2", "#D55E00", "#CC79A7", "#E6AB02", "#FFFF99", "#33ff99", "#fb9a99", "#556B2F", "#fdbf6f", "#cab2d6",
                 "#377eb8", "#ff9933", "#4daf4a", "#984ea3", "#cc0033", "#a65628", "#9999FF", "#f781bf","#26ED26", "#1f78b4",
                 "#b2df8a", "#a6cee3", "#A229FA", "#e41a1c", "#ffff33", "#99AC06", "#2F3C56", "#EC22DE", "#1DD3E8", "#9933FF",
                 "#CDC0B0", "#7FFFD4", "#D2691E", "#458B74", "#6495ED", "#FFF8DC", "#00FFFF", "#EE3B3B", "#CAFF70", "#8B008B",
                 "#00008B", "#A52A2A", "#EEC591", "#98F5FF", "#BF3EFF", "#8B4513", "#EE6A50", "#66CD00", "#8B0000", "#e31a1c")

  if (add.grey>0 && add.grey<=length(greyPalette))
    myPalette <- c(greyPalette[1:add.grey], myPalette)

  if (missing(n) || n < 1 || n > length(myPalette))
    n <- length(myPalette)

  return(myPalette[1:n])
}

