#' @title disastDates_df
#'
#' @description This data contains the beginning and end date of the disasters 
#'   registered in EMDAT and FEMA. The dates are given at the state level, this
#'   means that if different counties where affected in different dates this 
#'   will not be accounted for in this data set.   
#'   
#' @docType data
#'
#' @usage data(disastDates_df)
#'
#' @format A tibble with
#' \describe{
#'   \item{eventKey}{The ID created by the authors to inform the year,
#'      state, and number of disasters in that particular place}
#'   \item{eventStart}{Year, month and day an event started}
#'   \item{eventEnd}{Year, month and day an event ended}
#' }
"disastDates_df"
