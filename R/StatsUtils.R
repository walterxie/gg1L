# StatsUtils
# Author: Walter Xie
# Accessed on 16 Feb 2017

#' @name StatsUtils
#' @title Data and Statistics Utils
#' @description
#' Useful functions to summarizse data.

#' @details \code{summarySE} gives count, mean, standard deviation,
#' standard error of the mean, and confidence interval (default 95%).
#' The code is copied from
#' \url{http://www.cookbook-r.com/Manipulating_data/Summarizing_data/}.
#'
#' @param data A data frame.
#' @param measurevar the name of a column that contains the variable
#' to be summariezed.
#' @param groupvars a vector containing names of columns that
#' contain grouping variables.
#' @param na.rm a boolean that indicates whether to ignore NA's.
#' @param conf.interval the percent range of the confidence interval,
#' such as .95 as default.
#' @param .drop should combinations of variables that do not appear
#' in the input data be preserved (FALSE) or dropped (TRUE, default).
#' @import plyr
#' @keywords statistics
#' @export
#' @examples
#' data(model.test)
#' summarySE(model.test, measurevar="performance", groupvars=c("model", "OS"))
#' # With a data set with NA's, use na.rm=TRUE
#' dataNA <- model.test
#' dataNA$performance[11:15] <- NA
#' summarySE(dataNA, measurevar="performance", groupvars=c("model", "OS"), na.rm=TRUE)
#'
#' @rdname StatsUtils
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  require(plyr)
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }

  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )

  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult

  return(datac)
}

#' @details \code{cmYAcrossX} aggregates a community matrix to another abundance matrix
#' to show the number of OTUs (y-axis) simultaneously appeared at the number of samples (x-axis).
#' The 'samples' is the number of samlpes listed in sequence,
#' the 'OTUs' is the number of OTUs simultaneously appeared only in that number of samlpes,
#' and the 'reads' is the number of reads assigned to those OTUs.
#'
#' @param community.matrix
#' \strong{Community matrix} in \pkg{ComMA} is a matrix or data.frame,
#' where rows are OTUs (therefore this matrix is called \emph{OTU table}) or individual species
#' and columns are sites or samples.
#' Matrix elements are abundance data or proportion (e.g. counts, percentage). For example,
#' \tabular{rrrr}{
#'   OTU_id \tab plot01 \tab plot02\tab ...\cr
#'   OTU_1 \tab 1 \tab 0 \tab ...\cr
#'   OTU_2 \tab 100 \tab 200 \tab ...\cr
#'   OTU_3 \tab 56 \tab 3 \tab ...
#' }
#' @param terms The terms to be used in names of the data frame,
#' which will be shown in the graph if using \code{\link{ggYAcrossXBar}}.
#' Default to c("OTUs", "samples", "reads").
#' Please be careful of the order if any change.
#' @import stats
#' @keywords statistics
#' @export
#' @examples
#' data(reads.phyla)
#' cm.aggre <- cmYAcrossX(reads.phyla[,1:6])
#' cm.aggre
#'
#' @rdname StatsUtils
cmYAcrossX <- function(community.matrix, terms=c("OTUs", "samples", "reads")) {
  require(Matrix)
  row.count.sum <- data.frame(row.names = rownames(community.matrix))
  row.count.sum[, terms[2]] <- apply(community.matrix, MARGIN=1, function(x) sum(x>0))
  row.count.sum$reads <- apply(community.matrix, MARGIN=1, sum)

  cm.aggre.c <- aggregate(as.formula(paste(". ~", terms[2])), data=row.count.sum, function(x) sum(x>0))
  names(cm.aggre.c)[names(cm.aggre.c)==terms[3]] <- terms[1] # 1: OTUs, 3: reads
  cm.aggre.s <- aggregate(as.formula(paste(". ~", terms[2])), data=row.count.sum, FUN=sum)
  cm.aggre <- merge(cm.aggre.c, cm.aggre.s, by = terms[2]) # 2: samples

  return(cm.aggre)
}

#' @details \code{random2Clusters} generates coordinates for 2 clusters.
#' \url{http://stackoverflow.com/questions/2397097/how-can-a-data-ellipse-be-superimposed-on-a-ggplot2-scatterplot}.
#'
#' @param n The number of points. Default to 100.
#' @param seed An integer seed for \code{\link{set.seed}}. Default to 101.
#' @keywords utils
#' @export
#' @examples
#' df.clusters <- random2Clusters()
#'
#' @rdname StatsUtils
random2Clusters <- function(n=100, seed=101) {
  #bootstrap
  set.seed(seed)
  x <- rnorm(n, mean=2)
  y <- 1.5 + 0.4*x + rnorm(n)
  df <- data.frame(x=x, y=y, group="A")
  x <- rnorm(n, mean=2)
  y <- 1.5*x + 0.4 + rnorm(n)
  df <- rbind(df, data.frame(x=x, y=y, group="B"))
}

