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
#' @keywords statistics
#' @export
#' @examples
#' summarySE(model.test, measurevar="performance", groupvars=c("model", "OS"))
#' # With a data set with NA's, use na.rm=TRUE
#' summarySE(data, measurevar="change", groupvars=c("sex", "condition"), na.rm=TRUE)
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

