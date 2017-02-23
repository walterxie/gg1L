# Graph Base Functions
# Author: Walter Xie
# Accessed on 23 Feb 2017

#' @name ggBase
#' @title Basic functions to make up one-line functions
#'
#' @description
#' Start to simplify \pkg{ggplot} graphic coding into these basic functions first,
#' and reuse them to make up other one-line plot functions.
#'
#' @seealso Colour schemes \code{\link{ggOptPalette}};
#' position scales \code{ggOptScaleAxis}.
#'
#'
#' @return
#' A \code{\link{ggplot}} object, which can be concatenated using '+'.
#'
#' @references
#' More details and tutorials are available at
#' \url{https://github.com/walterxie/gg1L}
#'
#' @param df A data frame used for plot.
#' @param x.id,y.id,fill.id,group.id,colour.id
#' The string of column names in \code{df},
#' which is corresponding to \code{x, y, fill, group, colour}
#' in the aesthetic mapping \code{\link{aes}}.
#' @param p The ggplot object.
#'
#' @param verbose More messages. Default to TRUE.




#' @param x.facet.id,y.facet.id The string of column names in \code{df},
#' which creates facets (a formula) in \code{\link{facet_grid}}.
#' @param facet.scales,facet.space,facet.shrink,facet.drop
#' The parameters refer to \code{\link{facet_grid}}.



#' Default NULL to use \code{\link{ggplot}} default colours.
#' @param text.id Label the data points according \code{text.id} column,
#' such as "Row.names" column after \code{\link{merge}}.
#' @param text.size,text.hjust,text.vjust,text.alpha
#' The arguments to adjust text in \code{\link{geom_text}} in the line or scatter plot.
#' @param legend.title,legend.title.fill,legend.title.colour,legend.title.shape,legend.title.group,legend.title.size,
#' The title of legend created by fill, colour, shape, group, or size.
#' Set legend.title.*="" to remove legend.

#' @param legend.col,legend.row Customize the number of columns or rows for legend in bar chart.
#' They cannot be used at the same time. Default not to use them, legend.col=1, legend.row=0.
#' @param no.legend Turning off some legends, such as, fill, shape, colour.
#' Default to NULL, which is to do nothing.
#' @param no.panel.border Add panel border or not. Default to FALSE.



#' @details
#' \code{ggInit} initialises the plot, and define the aesthetic mapping.
#' \code{df} and \code{x.id} are compulsory.
#' @keywords base
#' @export
#' @examples
#' data(model.test)
#' p <- ggInit(model.test, x.id="test", y.id="performance", fill.id="model")
#'
#' @rdname ggBase
ggInit <- function(df, x.id, y.id=NULL, fill.id=NULL, group.id=NULL, colour.id=NULL, verbose=TRUE) {
  col.names <- colnames(df)
  if (!is.element(tolower(x.id), tolower(col.names)))
    stop("Data frame do NOT have column name \"", x.id, "\" !")

  require(ggplot2)
  aes.string <- paste0("aes(x=", x.id)
  if (! is.null(y.id)) {
    if (substring(y.id, 0, 2) != ".." && !is.element(tolower(y.id), tolower(col.names)))
      stop("Data frame do NOT have column name \"", y.id, "\" !")
    aes.string <- paste0(aes.string, ", y=", y.id)
  }
  if (! is.null(fill.id)) {
    if (!is.element(tolower(fill.id), tolower(col.names)))
      stop("Data frame do NOT have column name \"", fill.id, "\" !")
    aes.string <- paste0(aes.string, ", fill=", fill.id)
  }
  if (! is.null(group.id)) {
    if (!is.element(tolower(group.id), tolower(col.names)))
      stop("Data frame do NOT have column name \"", group.id, "\" !")
    aes.string <- paste0(aes.string, ", group=", group.id)
  }
  if (! is.null(colour.id))  {
    if (!is.element(tolower(colour.id), tolower(col.names)))
      stop("Data frame do NOT have column name \"", colour.id, "\" !")
    aes.string <- paste0(aes.string, ", colour=", colour.id)
  }
  aes.string <- paste0(aes.string, ")")

  if (verbose)
    cat("ggplot aesthetic mappings are ", aes.string, ".\n")

  p <- ggplot(df, eval(parse(text = aes.string)))
  return(p)
}

#' @details
#' \code{ggOptCoordCartesian} can set limits on the coordinate system to zoom the plot,
#' but this will not change the underlying data like setting limits on a scale will.
#' It is also used to flip the coordinate system.
#' @param x.lim.cart,y.lim.cart Setting limits on the coordinate system will zoom the plot,
#' and will not change the underlying data like setting limits on a scale will.
#' Refer to \code{\link{coord_cartesian}}.
#' Set lower bound only to y-axis using y.lim.cart=c(1000,NA). Default NULL.
#' @param coord.flip If TRUE, then flip cartesian coordinates so that horizontal
#' becomes vertical, and vertical becomes horizontal. Default to FALSE. Refer to
#' \code{\link{coord_flip}}.
#' @export
#' @examples
#' p <- ggOptCoordCartesian(p, model.test, x.id="test", y.id="performance", coord.flip=T)
#'
#' @rdname ggBase
ggOptCoordCartesian <- function(p, df, x.id, y.id, x.lim.cart=NULL, y.lim.cart=NULL,
                                coord.flip=FALSE, verbose=TRUE) {
  if (! is.null(x.lim.cart)) {
    if (length(x.lim.cart) != 2)
      stop("Invalid format, use x.lim.cart = c(1000,NA) !")
    if (anyNA(x.lim.cart)) {
      if (which(is.na(x.lim.cart))==1) {
        x.lim.cart[1] = min(df[,x.id])
      } else if (which(is.na(x.lim.cart))==2) {
        x.lim.cart[2] = max(df[,x.id])
      }
    }
    p <- p + coord_cartesian(xlim=x.lim.cart)
  }
  if (! is.null(y.lim.cart)) {
    if (length(y.lim.cart) != 2)
      stop("Invalid format, use y.lim.cart = c(1000,NA) !")
    if (anyNA(y.lim.cart)) {
      if (which(is.na(y.lim.cart))==1) {
        y.lim.cart[1] = min(df[,y.id])
      } else if (which(is.na(y.lim.cart))==2) {
        y.lim.cart[2] = max(df[,y.id])
      }
    }
    p <- p + coord_cartesian(ylim=y.lim.cart)
  }

  if (coord.flip) {
    p <- p + coord_flip()
    if (verbose)
      cat("Flip cartesian coordinates.\n")
  }
  return(p)
}

#' @details
#' \code{ggLabTitle} changes the title of graph, and the label on x or y-axis.
#' @param title Graph title, set title="" to remove it from the plot.
#' @param x.lab,y.lab The label of x-axis or y-axis, such as plot names.
#' Set x.lab="" to remove x-axis label from the plot. Default to NULL to do nothing.
#' @export
#' @examples
#' p <- ggLabTitle(p, title="", x.lab="test", y.lab="performance score")
#'
#' @rdname ggBase
ggLabTitle <- function(p, title, x.lab=NULL, y.lab=NULL) {
  p <- p + theme_bw() + ggtitle(title)

  if (! is.null(x.lab))
    p <- p + xlab(x.lab)

  if (! is.null(y.lab))
    p <- p + ylab(y.lab)

  return(p)
}

#' @details
#' \code{ggThemePanelBorder} uses \code{\link{theme}} to modify individual
#' components in the panel of a theme, such as grid, background, border, etc.
#' @param title.size Graph title font size.
#' @export
#' @rdname ggBase
ggThemePanelBorder <- function(p, title.size=10) {
  p <- p + theme(plot.title = element_text(size = title.size), panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), panel.background = element_blank())
  return(p)
}

# This function is broken in ggplot2, there is bug in axis.line,
# use theme_set(theme_bw(base_size=8)) instead.
ggThemeAxis <- function(p, title.size=10) {
  p <- p + theme(plot.title = element_text(size = title.size), panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), panel.background = element_blank(),
                 panel.border = element_blank())
  #p <- p + theme(axis.line = element_line(size = 3, colour = "black"))
  p <- p + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
  return(p)
}

#' @details
#' \code{ggThemeOthers} uses \code{\link{theme}} to modify the rest
#' components of a theme, such as ticks, margin, title, ledgend, etc.
#' @param x.text,y.text If FALSE, hide tick labels
#' along x-axis or y-axis in plot. Default to TRUE.
#' @param x.ticks,y.ticks If FALSE, hide tick marks
#' along x-axis or y-axis in plot. Default to TRUE.
#' @param x.text.angle The angle to rotate tick labels along x-axis.
#' @param plot.margin.cm margin around entire plot in a 4-element vector
#' (the sizes of the top, right, bottom, and left margins in cm),
#' such as \code{c(1,2,3,4)}.
#' @param title.hjust Numeric, adjust title position, default to 0.5 (middle),
#' 0 is left and 1 right.
#' @param legend.position Legend position, default to "right".
#' @param legend.direction The direction to display legend, default to "vertical".
#' @export
#' @rdname ggBase
ggThemeOthers <- function(p, x.text=TRUE, y.text=TRUE, x.ticks=TRUE, y.ticks=TRUE,
                          x.text.angle=0, plot.margin.cm=NULL, title.hjust=0.5,
                          legend.position="right", legend.direction="vertical",
                          verbose=TRUE) {
  theme.string <- "theme(legend.position=legend.position, legend.direction=legend.direction"
  # hide x or y axis labels
  if (!x.text)
    theme.string <- paste(theme.string, "axis.text.x = element_blank()", sep = ",")
  if (!y.text)
    theme.string <- paste(theme.string, "axis.text.y = element_blank()", sep = ",")
  # rotate x labels
  if (x.text.angle > 0)
    theme.string <- paste(theme.string, "axis.text.x = element_text(angle = x.text.angle, hjust = 1, vjust = 0.3)", sep = ",")
  # hide x or y axis ticks
  if (!x.ticks)
    theme.string <- paste(theme.string, "axis.ticks.x = element_blank()", sep = ",")
  if (!y.ticks)
    theme.string <- paste(theme.string, "axis.ticks.y = element_blank()", sep = ",")
  if (! is.null(plot.margin.cm))
    theme.string <- paste0(theme.string, ", plot.margin=ggplot2::unit(c(",
                           paste(plot.margin.cm, collapse = ","), "), 'cm')")
  # default title in middle
  theme.string <- paste0(theme.string, ", plot.title = element_text(hjust=", title.hjust, ")", " )")


  if (verbose)
    cat("theme : ", theme.string, "\n")

  p <- p + eval(parse(text = theme.string))
  return(p)
}


# reformulate(fac2,fac1) => fac1 ~ fac2
ggOptFacetGrid <- function(p, col.names, x.facet.id=NULL, y.facet.id=NULL,
                           scales = "fixed", space = "fixed", shrink = TRUE,
                           drop = TRUE, verbose=TRUE) {
  if (! is.null(x.facet.id) || ! is.null(y.facet.id)) {
    fac1 <- "."
    fac2 <- "."
    if (! is.null(x.facet.id)) {
      if (!is.element(tolower(x.facet.id), tolower(col.names)))
        stop("Data frame do NOT have column name \"", x.facet.id, "\" !")

      fac1 <- x.facet.id
    }
    if (! is.null(y.facet.id)) {
      if (!is.element(tolower(y.facet.id), tolower(col.names)))
        stop("Data frame do NOT have column name \"", y.facet.id, "\" !")
      fac2 <- y.facet.id
    }
    if (verbose)
      cat("facet_grid(", deparse(reformulate(fac2,fac1)), ").\n")

    p <- p + facet_grid(reformulate(fac2,fac1), scales = scales,
                        space = space, shrink = shrink, drop = drop)
  }
  return(p)
}

# if shape.id not NULL, no shape for points
# If NULL, the default, the data is inherited from the plot data.
ggOptPointAndShape <- function(p, col.names, shape.id=NULL, shapes=NULL,
                               data=NULL, point.alpha=1, point.size=3,
                               position=position_dodge(width=0)) {
  if (! is.null(shape.id)) {
    if (!is.element(tolower(shape.id), tolower(col.names)))
      stop("Data frame do NOT have column name \"", shape.id, "\" !")

    p <- p + geom_point(data=data, aes_string(shape=shape.id), size = point.size,
                        alpha=point.alpha, position=position)
    if (! is.null(shapes))
      p <- p + scale_shape_manual(values=shapes)
  } else {
    p <- p + geom_point(data=data, size = point.size, alpha=point.alpha,
                        position=position)
  }
  return(p)
}

# add text.id before ggOptText, such as df$row.names <- rownames(df)
# If check_overlap=TRUE, text that overlaps previous text in the same layer will not be plotted
ggOptText <- function(p, col.names, text.id=NULL, text.data=NULL, colour.id=NULL,
                      text.repel=FALSE, text.size=3, text.alpha=0.5,
                      text.hjust=-0.1, text.vjust=-0.2, text.avoid.overlap=FALSE,
                      box.padding = unit(0.25, "lines"), point.padding = unit(1e-06, "lines"),
                      arrow = NULL, force = 1, verbose=TRUE) {
  if (! is.null(text.id)) {
    if (!is.element(tolower(text.id), tolower(col.names)))
      stop("Data frame do NOT have column name \"", text.id, "\" !")

    # bug in 0.6.5: "Error in as.vector(y) : attempt to apply non-function"
    if(text.repel) require(ggrepel) # ggrepel >= 0.6.6
    aes.string <- paste0(ifelse(text.repel, "geom_text_repel( ", "geom_text( "),
                         "aes(label=", text.id)
    if (! is.null(colour.id)) {
      if (!is.element(tolower(colour.id), tolower(col.names)))
        stop("Data frame do NOT have column name \"", colour.id, "\" !")
      aes.string <- paste0(aes.string, ", colour=", colour.id)
    }

    if (length(text.size) > 1) { # text.size is vector
      # prevent Error: Aesthetics must be either length 1 or the same as the data (5): size
      if (is.null(text.data))
        stop("Data for text cannot be NULL if given size vector !")
      # geom_text(data=df, aes(size = text.size))
      aes.string <- paste0(aes.string, ", size=text.size), ")
    } else { # text.size is integer
      aes.string <- paste0(aes.string, "), size=text.size, ")
    }
    aes.string <- paste0(aes.string, "data=text.data, alpha=text.alpha, ")
    if (text.repel) {
      aes.string <- paste0(aes.string, "box.padding=box.padding, ",
                           "point.padding=point.padding, arrow=arrow, force=force )")
    } else {
      aes.string <- paste0(aes.string, "hjust=text.hjust, vjust=text.vjust, ",
                           "check_overlap = text.avoid.overlap )")
    }

    if (verbose)
      cat("text : ", aes.string, "\n")
    p <- p + eval(parse(text = aes.string))
  }
  return(p)
}

# normally ellipsed.id == colour.id to show clusters
ggOptEllipse <- function(p, col.names, ellipsed.id=NULL) {
  if (! is.null(ellipsed.id)) {
    if (!is.element(tolower(ellipsed.id), tolower(col.names)))
      stop("Data frame do NOT have column name \"", ellipsed.id, "\" !")
    p <- p + stat_ellipse(aes_string(mapping = ellipsed.id), type = "t", linetype = 2)
  }
  return(p)
}

#' Colour Schemes
#'
#' \code{ggOptPalette} provides colour schemes for graphs.
#'
#' It uses \code{scale.to}, \code{scale.type}, and \code{palette}
#' to determin which colour scale is applied.
#' If \code{scale.type} is NULL as default, then the length of
#' \code{palette} will be used to the colour scale:
#'
#' If \code{palette} length == 1, then use \code{\link{scale_*_brewer}}
#' (\url{http://www.datavis.ca/sasmac/brewerpal.html}), such as "Set1" (max 8 colours).
#' If length != 1 and length <= 3, then use \code{\link{scale_colour_gradientn}},
#' such as c("blue", "orange").
#' Otherwise \code{\link{scale_fill_manual}} will be used given a vector of customized colours.
#' Set \code{palette=c("red","darkgreen","darkgrey","")}, for example,
#' if \code{\link{scale_fill_manual}} has to be used when the actual length(palette) <=3.
#'
#' @param p The ggplot object.
#' @param scale.to Choose either "colour" or "fill" to make up
#' a full command, such as \code{\link{scale_colour_gradient}}.
#' @param scale.type Choose one from "brewer", "gradientn", and "manual"
#' to make up a full command, such as \code{\link{scale_fill_manual}}.
#' @param palette The colour palette for bar, box, scatter plot, etc.
#' @param limits A numeric vector of length two providing limits of the scale.
#' Use NA to refer to the existing minimum or maximum.
#' Refer to \code{\link{continuous_scale}}.
#' @export
#' @examples
#' p <- ggOptPalette(p, scale.to="fill", palette="Set1")
#' p <- ggOptPalette(p, scale.to="colour", palette=c("blue", "orange"))
ggOptPalette <- function(p, scale.to="colour", scale.type=NULL,
                         palette=NULL, limits = NULL, verbose=TRUE) {
  if (! is.null(palette)) {
    if (is.null(scale.type)) {
      if (length(palette) == 1)
        scale.string <- paste0("scale_", scale.to, "_brewer(palette=palette")
      else if (length(palette) <= 3)
        scale.string <- paste0("scale_", scale.to, "_gradientn(colours=palette")
      else
        scale.string <- paste0("scale_", scale.to, "_manual(values=palette")
    } else {
      if (scale.type == "brewer")
        scale.string <- paste0("scale_", scale.to, "_brewer(palette=palette")
      else if (scale.type == "gradientn")
        scale.string <- paste0("scale_", scale.to, "_gradientn(colours=palette")
      else
        scale.string <- paste0("scale_", scale.to, "_manual(values=palette")
    }

    if (!is.null(limits))
      scale.string <- paste0(scale.string, ", limits=c(", paste(limits, collapse = ",") , ")")
    scale.string <- paste0(scale.string, ")")

    if (verbose)
      cat("colour scale : ", scale.string, "\n")

    p <- p + eval(parse(text = scale.string))
  }
  return(p)
}

#' Position scales
#'
#' \code{ggOptScaleAxis} provides scales for discrete or continuous x and y aesthetics.
#'
#' @param p The ggplot object.
#' @param axis "x" or "y".
#' @param scale Choose either "continuous" or "discrete".
#' @param trans The string defines the data scale used in either x-axis or y-axis,
#' which can be "identity" standing for normal, or "per" standing for percentage,
#' moreover either the name of a transformation object for \code{\link{scale_x_continuous}}
#' or \code{\link{scale_y_continuous}} (e.g. \code{trans="log"}), or the object itself.
#' Built-in transformations include "asn", "atanh", "boxcox", "exp", "identity",
#' "log", "log10", "log1p", "log2", "logit", "probability", "probit", "reciprocal",
#' "reverse" and "sqrt". Default to "identity".
#' @param expand,breaks,labels Refer to \code{\link{scale_x_continuous}} or \code{\link{scale_x_discrete}}.
#' @param auto.scale.max If not NULL, then use \code{\link{get_breaks_positive_values}}
#' to create breaks regarding to the given maximun value.
#' @param breaks.start Refer to \code{\link{get_breaks_positive_values}}.
#' Use \code{start=c(0.1, 1)} to show 1 in a log-scaled axis.
#' @export
#' @examples
#' p <- ggOptPalette(p, axis="y", scale="continuous", trans="per")
#' p <- ggOptPalette(p, scale.to="colour", palette=c("blue", "orange"))
# 1) auto.scale.x.max = max(df[,x.id]) to determine positive breaks automatically,
# breaks=c(start, 10, 100, ...), default start=c(0.1, 1) in get_breaks_positive_values.
# 2) breaks.interval > 0, seq(interval.min, interval.max, breaks.interval)
ggOptScaleAxis <- function(p, axis="y", scale="continuous", trans="identity",
                           expand=waiver(), breaks=waiver(), labels=waiver(),
                           auto.scale.max=NULL, breaks.start=c(0), verbose=TRUE) {
  if ( ! is.element(axis, c("x", "y")) )
    stop("Incorrect axis ", axis, ", use x or y !")
  if ( ! is.element(scale, c("continuous", "discrete")) )
    stop("Incorrect scale ", scale, ", use continuous or discrete !")

  # scale_y_continuous(trans=trans, expand=expand, breaks=breaks, labels=labels)
  scale.string <- paste0("scale_", axis, "_", scale, "(expand=expand, breaks=breaks")
  if (scale=="continuous") {
    if (trans=="log")
      # show 1 at log scale
      scale.string <- paste0(scale.string, ", trans=gg1L::mylog_trans(base=10, from=-0.3)")
    else
      scale.string <- paste0(scale.string, ", trans=trans")
  }

  if (! is.null(auto.scale.max)) {
    breaks <- gg1L::get_breaks_positive_values(auto.scale.max, start=breaks.start)
    scale.string <- paste0(scale.string, ", labels=gg1L::scientific_10)")
  } else {
    if (trans=="per") {
      require(scales)
      trans="identity"
      scale.string <- paste0(scale.string, ", labels=percent_format())")
    } else {
      scale.string <- paste0(scale.string, ")")
    }
  }

  if (verbose) {
    cat("Axis", axis, "scale :", scale.string, "\n")
    cat("Axis", axis, "breaks :", paste(breaks, collapse = ","), "\n")
  }

  p <- p + eval(parse(text = scale.string))
  return(p)
}


ggOptLegend <- function(p, legend.title.fill=NULL, legend.title.colour=NULL,
                        legend.title.shape=NULL, legend.title.group=NULL,
                        legend.title.size=NULL, legend.col=1, legend.row=0,
                        no.legend=NULL, verbose=TRUE) {
  if (!is.null(legend.title.fill))
    p <- p + labs(fill=legend.title.fill)
  if (!is.null(legend.title.colour))
    p <- p + labs(colour=legend.title.colour)
  if (!is.null(legend.title.shape))
    p <- p + labs(shape=legend.title.shape)
  if (!is.null(legend.title.group))
    p <- p + labs(group=legend.title.group,
                  size=legend.title.size)
  if (!is.null(legend.title.size))
    p <- p + labs(size=legend.title.size)

  if (legend.col > 1 && legend.row > 0)
    warning("Cannot change legend.col and legend.row at the same time ! Skip both changes !")

  if (legend.col > 1 && legend.row == 0)
    p <- p + guides(fill=guide_legend(ncol=legend.col))
  if (legend.row > 0 && legend.col == 1) {
    p <- p + guides(fill=guide_legend(nrow=legend.row,byrow=TRUE))
  }

  if (!is.null(no.legend)) {
    guides.string <- paste0("guides(", no.legend, "=FALSE)")
    p <- p + eval(parse(text = guides.string))
    if (verbose)
      cat("no.legend = ", no.legend, "\n")
  }

  return(p)
}


# validation
validateAttr <- function(attr.df, colour.id=NULL, shape.id=NULL, link.id=NULL,
                         text.id=NULL, text.size.id=NULL) {
  attr.v <- c()
  if (! is.null(colour.id)) {
    if (! colour.id %in% colnames(attr.df))
      stop("Invalid colour.id,", colour.id,  "not exsit in meta data column names !\n")
    attr.v <- c(attr.v, colour.id)
  }
  if (! is.null(link.id)) {
    if (! link.id %in% colnames(attr.df) )
      stop("Invalid link.id,", link.id,  "not exsit in meta data column names !\n")
    attr.v <- c(attr.v, link.id)
  }
  if (! is.null(shape.id)) {
    if (! shape.id %in% colnames(attr.df) )
      stop("Invalid shape.id,", shape.id,  "not exsit in meta data column names !\n")
    attr.v <- c(attr.v, shape.id)
  }
  if (! is.null(text.id)) {
    if (! text.id %in% colnames(attr.df) )
      stop("Invalid shape.id,", text.id,  "not exsit in meta data column names !\n")
    attr.v <- c(attr.v, text.id)
  }
  if (! is.null(text.size.id)) {
    if (! text.size.id %in% colnames(attr.df) )
      stop("Invalid text.size.id,", text.size.id,  "not exsit in meta data column names !\n")
    attr.v <- c(attr.v, text.size.id)
  }
  return(attr.v)
}
