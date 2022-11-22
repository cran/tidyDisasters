#' @title disastTypes_df
#'
#' @description This data contains the type of disasters reported 
#'   by FEMA and EMDAT based on the classification  of the Hazard Definition and
#'   Classification Review Technical Report  published by the UN Office for 
#'   Disaster Risk Reduction (2020)
#'   
#' @docType data
#'
#' @usage data(disastTypes_df)
#'
#' @format A tibble with
#' \describe{
#'   \item{eventKey}{The ID created by the authors to inform the year,
#'      state, and number of disasters in that particular place}
#'   \item{incident_type}{It is the original type of disaster that the FEMA
#'     and EMDAT data sets reported}
#'   \item{hazard_type}{It is the related broad classification disaster type
#'     The category was assigned based on the Technical report by matching it to 
#'     the hazard cluster that was assigned by the authors}
#'   \item{hazard_cluster}{It is the related subtype classification assigned by 
#'     the authors according to the original incident type reported by FEMA or 
#'     EMDAT, and according to the broad classification that was already 
#'     assigned}
#' }
"disastTypes_df"