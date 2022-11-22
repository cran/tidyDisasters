#' @title fema_hazard_cluster_df
#'
#' @description This data contains the type of disasters reported 
#'   by FEMA  and the matched hazard cluster based on the classification  of 
#'   the Hazard Definition and  Classification Review Technical Report 
#'   published by the UN Office for  Disaster Risk Reduction (2020) 
#'   Annex 6 page 72. The matching was done by Dr. Mark Macgowan and 
#'   Catalina Canizares following the suggestions in the report. However, 
#'   this classification is subjective and should be used taking into 
#'   account this disclaimer.

#'   
#' @docType data
#'
#' @usage data(fema_hazard_cluster_df)
#'
#' @format A tibble with
#' \describe{
#'   \item{Source}{This vector will contain the information of the data set
#'   from which the information came from. In this case there are two sources
#'   FEMA and GTD}
#'   \item{incident_type}{It is the original type of disaster that the FEMA
#'     data sets reported}
#'   \item{hazard_cluster}{It is the related sub type classification assigned by 
#'     the authors according to the original incident type reported by FEMA  
#'     and according to the broad classification that was already assigned}
#' }
"fema_hazard_cluster_df"
