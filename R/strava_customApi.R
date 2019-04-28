#' Get custom API request string
#'
#' Type in your own request string
#'
#' @param request Request string after https://www.strava.com/api/v3/, e.g. athletes
#'
#' @import httr
#' @import rjson
#'
#'
#' @export
#' @examples
#' strava_customApi(request)
strava_customApi <- function(request){
  url <- paste0("https://www.strava.com/api/v3/", request)
  tryCatch({
    data <- rjson::fromJSON(rawToChar(GET(url,
                                          config(token = StravaAuth$public_fields$token))$content))
    if(any(grepl("errors.code",names(unlist(data))))){
      cat(crayon::red("Error: Not an authorized API call"))
    } else{
      return(data)
    }

  }, error = function(e){
    cat(crayon::red("Error: Not an authorized API call"))
  })
}

