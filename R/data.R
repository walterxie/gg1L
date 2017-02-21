#' Performance testing.
#'
#' A dataset containing the performance score of tests
#' in four GTR models in three operation systems using BEAST.
#'
#' @format A data frame with 180 rows and 4 columns:
#' \describe{
#'   \item{model}{a GTR model}
#'   \item{test}{test ID}
#'   \item{performance}{the performance score}
#'   \item{OS}{operation system}
#' }
#' @source \url{http://www.beast2.org/2016/04/05/beast-1-vs-2-performance-benchmarking/}
"model.test"

#' *BEAST log.
#'
#' A MCMC log from the *BEAST \url{mbe.oxfordjournals.org/content/27/3/570.full} model,
#' whose row names are the states, column names are the parameters.
#'
#' @format A data frame with 2001 rows and 18 columns:
#' \describe{
#'   \item{row.names}{the state number in MCMC}
#'   \item{posterior}{the posterior}
#'   ...
#'   \item{TreeHeight.Species}{the species tree height}
#'   ...
#' }
#' @source \url{http://beast2.org/}
"mcmc.log"

#' Prices of 50,000 round cut diamonds.
#'
#' A dataset containing the number of eDNA sequences (reads)
#' identified as the phyla listed in row names,
#' using a genetic mark in each column.
#'
#' @format A data frame with 110 rows and 7 columns,
#' the last column is the taxonomy group:
#' \describe{
#'   \item{16S}{16S genetic mark}
#'   \item{18S}{18S genetic mark}
#'   \item{ITS}{ITS genetic mark}
#'   \item{trnL}{trnL genetic mark}
#'   \item{COI}{COI genetic mark}
#'   \item{COI-spun}{COI genetic mark and samples were spun}
#'   \item{TaxaGroup}{the taxonomy group of phyla}
#' }
#' @source \url{http://dx.doi.org/10.1186/s13742-015-0086-1/}
"reads.phyla"
