#' @title disastCasualties_df
#'
#' @description This data contains the number of people 
#'   killed and wounded per event. 
#'   It is relevant to note that the number of people killed and wounded 
#'   correspond to the total casualties for the whole event, it does not relate
#'   to the particular number of casualties in each county or state
#'   
#' @docType data
#'
#' @usage data(disastCasualties_df)
#'
#' @format A tibble with
#' \describe{
#'   \item{eventKey}{The ID created by the authors to inform the year,
#'      state, and number of disasters in that particular place}
#'   \item{nKilled}{Number of people killed by the event, not discriminated by 
#'      location}
#'   \item{nWounded}{Number of people wounded by the event, not discriminated by
#'     location}
#' }
"disastCasualties_df"
