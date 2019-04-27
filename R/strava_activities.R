#' Get activities from Strava
#'
#' Get activities from the authenticated user as a data frame
#' 
#' @param scope Name of your developer app
#' @param token The Client ID of your developer app
#' @param app_secret The secret for your developer app
#'
#' @import httr
#' @import assertthat
#' @import plyr
#'
#' @export
strava_activities <- function(){
  if(!strava_check_existing_token()){
    return(invisible(FALSE))
  }
  
  url <- "https://www.strava.com/api/v3/activities/"
  data <- GET(url, config(token = StravaAuth$public_fields$token))
  
  data <- rjson::fromJSON(rawToChar(data$content))  
  
  activities <- NULL
  
  for(i in 1:(length(data))){
    act <- data.frame(matrix(unlist(data[[i]]), nrow=1))
    colnames(act) <- names(unlist(data[[i]]))
    activities <- plyr::rbind.fill(activities, act)
  }
  
  return(activities)
}
