#' Get laps from Strava
#'
#' Get laps information by the specific id or vector of ids
#'
#' @param data The activity id or vector of ids
#' @param lapType Wether to import custom laps from data, or standard. Defaults to custom. c("custom", "standard")
#'
#' @import httr
#' @import assertthat
#' @import plyr
#' @import rjson
#'
#'
#' @export
#' @examples
#' strava_laps(12345678, lapType = c("custom", "standard"))
strava_laps <- function(data, lapType = "custom"){
  if(!strava_check_existing_token()){
    return(invisible(FALSE))
  } else if(!(lapType %in% c("custom","standard"))){
    stop("lapType must contain 'custom' or 'standard'")
  } else if(missing(data)){
    stop("data must contain at least one activity id")
  }

  url <- paste0("https://www.strava.com/api/v3/activities/", data)


  laps <- NULL
  for(i in 1:(length(url))){
    act <- GET(url[i], config(token = StravaAuth$public_fields$token))

    if(act$status_code == "404"){
      stop(paste0("Couldn't find the id: ",data[i]))
    }

    act <- rjson::fromJSON(rawToChar(act$content))

    if(lapType == "custom"){
      act <- act[['laps']]
    } else {
      act <- act[['splits_standard']]
    }

    for(j in 1:(length(act))){
      df <- data.frame(matrix(unlist(act[[j]]), nrow=1))
      colnames(df) <- names(unlist(act[[j]]))
      df$activity.id <- data[i]
      laps <- plyr::rbind.fill(laps, df)
    }

  }
  return(laps)

}
