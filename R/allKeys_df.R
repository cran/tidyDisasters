#' @title allKeys_df
#'
#' @description This data contains the original keys from the FEMA, GTD and
#'   EMDAT data set and a universal key created by the authors.
#' @docType data
#'
#' @usage data(allKeys_df)
#'
#' @format A tibble with
#' \describe{
#'   \item{femaID}{The original ID given by FEMA to each disaster}
#'   \item{emdatID}{The original ID given by EMDAT to each disaster}
#'   \item{smashedID}{The FEMA and EMDAT IDs concatenated together}
#'   \item{eventKEY}{The ID created by the authors to inform the year,
#'     state, and number of disasters in that particular place}
#' }
"allKeys_df"
