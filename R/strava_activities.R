#' Get activities from Strava
#'
#' Get activities from the authenticated user as a data frame
#'
#' @param dateRange Between what dates the data should be collected. Must be in date format! c(date1, date2)
#'
#' @import httr
#' @import assertthat
#' @import plyr
#' @import rjson
#'
#' @export
#' @examples
#' strava_activities(dateRange = c(date1, date2))
strava_activities <- function(dateRange){
  if(!strava_check_existing_token()){
    return(invisible(FALSE))
  } else if(missing(dateRange)){
    stop("dateRange must be set!")
  }

 

  if(!assertthat::is.date(dateRange[1]) | !assertthat::is.date(dateRange[2])) {
    stop("dateRange must contain two date variables: c(date1, date2)")
  } else {
    start <- as.integer(as.POSIXct(dateRange[1]))
    end <- as.integer(as.POSIXct(dateRange[2]+1))
  }

  moreActivities <- true
  page <- 1
  activityList <- NULL
  while(moreActivities){
    url <- paste0("https://www.strava.com/api/v3/activities/?after=",start,"&before=",end, "&per_page=100&page=", page)
    data <- GET(url, config(token = StravaAuth$public_fields$token))
    data <- rjson::fromJSON(rawToChar(data$content))
    if(length(data) == 0){
      moreActivities <- false
    } else {
      activityList <- c(activityList, data)
      page <- page+1
    }
  }
  

  if(length(activityList) == 0){
    stop("Coudln't find any data during the chosen time period. Try a different time period.")
  }

  activities <- NULL

  for(i in 1:(length(activityList))){
    if (!is.null(unlist(activityList[[i]]))){
      act <- data.frame(matrix(unlist(activityList[[i]]), nrow=1))
      colnames(act) <- names(unlist(activityList[[i]]))
      activities <- plyr::rbind.fill(activities, act)
    }
  }

  return(activities)
}
