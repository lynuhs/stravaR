#' R6 environment to store authentication credentials
#'
#' Used to keep persistent state.
#' @export
StravaAuth <- R6::R6Class(
  "StravaAuth",
  public = list(
    token = NULL,
    method = NULL
  ),
  lock_objects = FALSE,
  parent_env = emptyenv()
)


#' Generata Strava API authentication token
#'
#' Generate a token for the user and the desired scope. The user is sent to the strava authentication page if he/she hasn't given permission to the app yet, else, is sent to the app webpage.
#'
#' @param app_name Name of your developer app
#' @param app_client_id The Client ID of your developer app
#' @param app_secret The secret for your developer app
#'
#' @import httr
#' @import assertthat
#'
#' @export
#' @examples
#' strava_auth(app_name = "APP Name", app_client_id = "App Client ID", app_secret = "APP Secret", token = NULL, new_user = FALSE)
strava_auth <- function(app_name = Sys.getenv("STRAVA_APP_NAME"), app_client_id = Sys.getenv("STRAVA_APP_ID"), app_secret = Sys.getenv("STRAVA_APP_SECRET"), token = NULL, new_user = FALSE){
  if(app_client_id == "" | app_secret == "" | app_name == ""){
    stop("Need a valid App name, App Client Id and App Secret in order to authorize connection!", call. = FALSE)
  } else {
    Sys.setenv(STRAVA_APP_NAME = app_name)
    Sys.setenv(STRAVA_APP_ID = app_client_id)
    Sys.setenv(STRAVA_APP_SECRET = app_secret)
  }


  checkEnvFile <- function(env){
    value <- Sys.getenv(env)
    value <- ifelse(value == "", return(NULL), return(value))
  }

  options("stravaR.httr_oauth_cache" = ifelse(is.null(getOption("stravaR.httr_oauth_cache")),
                                             ifelse(is.null(token), "strava.httr-oauth", token),
                                             getOption("stravaR.httr_oauth_cache")))

  options("stravaR.app_name" = checkEnvFile("STRAVA_APP_NAME"))
  options("stravaR.app_id" = checkEnvFile("STRAVA_APP_ID"))
  options("stravaR.app_secret" = checkEnvFile("STRAVA_APP_SECRET"))


  httr_file <- getOption("stravaR.httr_oauth_cache")


  if(assertthat::is.flag(httr_file)){
    stop("option('stravaR.httr_oauth_cache') must be set to
         valid cache file location,
         not TRUE or FALSE - (example: '.httr-oauth')",
         call. = FALSE)
  }

  assertthat::assert_that(assertthat::is.string(httr_file),
                          assertthat::is.flag(new_user))


  if(new_user){
    rm_old_user_cache(httr_file)
  }


  if(is.null(token)) {     ## supplied no token

    strava_token <- create_strava_token()

  } else if(is.token2.0(token)){     ## supplied a Token object

    legit <- is_legit_token(token)
    if(!legit){
      stop("Invalid token passed to function", call. = FALSE)
    }

    StravaAuth$set("public", "method", "passed_token", overwrite=TRUE)
    ## set the global session token
    StravaAuth$set("public", "token", token, overwrite=TRUE)

    ## just return it back
    strava_token <- token

  } else if(assertthat::is.string(token)){ ## a filepath

    if(file.exists(token)){
      strava_token <- read_cache_token(token_path = token)
    } else {
      cat(crayon::red(paste0("No httr_oauth_cache file found at ", token, " - creating new file.\n")))
      options("stravaR.httr_oauth_cache" = token)
      StravaAuth$set("public", "token", NULL, overwrite=TRUE)
      return(strava_auth(token = NULL))
    }


  } else {
    stop("Unrecognised token object - class ", class(token), call. = FALSE)
  }


  strava_check_existing_token()

  ## return strava_token above
  cat(crayon::green("Successfully authenticated Strava API!\n"))
  return(invisible(strava_token))


}






#' @noRd
#' @importFrom httr oauth_endpoints oauth_app oauth2.0_token
#' @import httpuv
create_strava_token <- function(){
  check_existing <- strava_check_existing_token()
  if(!check_existing){
    cat(crayon::red("Auto-refresh of token not possible, manual re-authentication required\n"))

    if(!interactive()){
      stop("Authentication options didn't match existing session token and not interactive session
           so unable to manually reauthenticate", call. = FALSE)
    }
  }

  endpoint <- oauth_endpoint(request = "https://www.strava.com/oauth/authorize?",
                             authorize = "https://www.strava.com/oauth/authorize",
                             access = "https://www.strava.com/oauth/token")

  app_name <- getOption("stravaR.app_name", "")
  app_client_id    <- getOption("stravaR.app_id", "")
  app_secret <- getOption("stravaR.app_secret", "")
  cache  <- getOption("stravaR.httr_oauth_cache", "")

  if(app_name == ""){
    stop("option('stravaR.app_name') has not been set", call. = FALSE)
  }

  if(app_client_id == ""){
    stop("option('stravaR.app_id') has not been set", call. = FALSE)
  }

  if(app_secret == ""){
    stop("option('stravaR.app_secret') has not been set", call. = FALSE)
  }

  if(cache == ""){
    stop("option('stravaR.httr_oauth_cache') has not been set", call. = FALSE)
  }

  app <- oauth_app(appname = app_name,
                   key = app_client_id,
                   secret = app_secret)


  strava_token <- oauth2.0_token(endpoint = endpoint,
                          app = app,
                          scope = "public",
                          cache = cache)

  stopifnot(is_legit_token(strava_token))

  StravaAuth$set("public", "token", strava_token, overwrite=TRUE)
  StravaAuth$set("public", "method", "new_token", overwrite=TRUE)

  #strava_token
}


#' @noRd
rm_empty_token <- function(token_path = getOption("stravaR.httr_oauth_cache")){
  ## delete token if 0B
  iz_0B <- file.info(token_path)$size == 0
  if(iz_0B){
    unlink(token_path)
  }
}



#' @noRd
rm_old_user_cache <- function(httr_file){
  StravaAuth$set("public", "token", NULL, overwrite=TRUE)
  if(file.exists(httr_file)){
    cat(crayon::red(paste0("Removing old cached credentials from: ", normalizePath(httr_file),"\n")))
    file.remove(httr_file)
  }
}


#' Reads a token from a filepath
#'
#' Also sets the option of token cache name to the supplied filepath
#'   "stravaR.httr_oauth_cache"
#'
#' httr cache files such as .httr-oauth can hold multiple tokens for different scopes,
#'   this only returns the first one and raises a warning if there are multiple
#'   in the rds file
#' @noRd
#' @import assertthat
read_cache_token <- function(token_path){

  assertthat::assert_that(assertthat::is.readable(token_path))

  cat(crayon::red("Reading token from file path\n"))

  strava_token <- tryCatch({readRDS(token_path)},
                             error = function(ex){
                               stop(sprintf("Cannot read token from alleged .rds file:\n%s",
                                            token_path),
                                    ex,
                                    call. = FALSE)
                             })

  if(is.list(strava_token)){
    cat(crayon::red("Multiple httr-tokens in cache ",token_path, ", only returning first found token\n"))
    strava_token <- strava_token[[1]]
  } else if(is.token2.0(strava_token)){
    cat(crayon::red("Read token successfully from file\n"))
  } else {
    stop("Unknown object read from ", token_path, " of class ", class(strava_token))
  }

  ## for existing tokens, set the options to what is in the token
  strava_token <- overwrite_options(strava_token, token_path = token_path)

  StravaAuth$set("public", "method", "filepath", overwrite=TRUE)
  ## set the global session token
  StravaAuth$set("public", "token", strava_token, overwrite=TRUE)

  strava_token
}


strava_token_info <- function(detail_level = getOption("stravaR.verbose", default = 3)){
  token  <- StravaAuth$public_fields$token
  method <- StravaAuth$public_fields$method
  message <- ""

  if(is.null(token)){
    message <- c(message, "No token found\n")
    return(NULL)
  }
  if(detail_level >= 3){
    message <- c(message, paste0("Authentication from cache file: ", token$cache_path,"\n"))

  }

  if(detail_level <= 2){
    if(!is.null(token$app$key)){
      message <- c(message, paste0("App key: ", token$app$key,"\n"))
    }

    message <- c(message, paste0("Method: ", method,"\n"))

  }

  if(detail_level == 1){
    message <- c(message, paste0("Hash: ", token$hash(),"\n"))
  }

  cat(crayon::red(message))
}


overwrite_options <- function(strava_token, token_path){
  options("stravaR.httr_oauth_cache" = token_path)
  strava_token$cache_path <- token_path

  if(is.null(strava_token$app)){
    cat(crayon::red("No App Client ID in token\n"))
    return(strava_token)
  }

  if(is.different(strava_token$app$key, "stravaR.app_id")){
    cat(crayon::red(paste0("Overwriting stravaR.app_id from", getOption("stravaR.app_id"),
                           "to ", strava_token$app$key,"\n")))
    options("stravaR.app_id" = strava_token$app$key)
  }

  if(is.different(strava_token$app$secret, "stravaR.app_secret")){
    cat(crayon::red(paste0("Overwriting stravaR.app_secret to ", strava_token$app$secret,"\n")))
    options("stravaR.app_secret" = strava_token$app$secret)
  }

  if(is.different(strava_token$app$appname, "stravaR.app_name")){
    cat(crayon::red(paste0("Overwriting stravaR.app_name to ", strava_token$app$appname,"\n")))
    options("stravaR.app_name" = strava_token$app$appname)
  }

  strava_token

}


is.token2.0 <- function(x){
  inherits(x, "Token2.0")
}



#' Retrieve Strava token from environment and configs for httr
#'
#' Get token if it's previously stored, else prompt user to get one.
#' @param shiny_return_token In a shiny session, this is passed instead.
#' @return a httr configured option for token
#' For shiny the token is passed from reactive session
#'
#' @keywords internal
#' @family authentication functions
#' @importFrom httr config
get_strava_token <- function(shiny_return_token=NULL) {

  if(any(which(grepl("with_mock_API", as.character(sys.calls()))))){
    cat(crayon::red("Skipping token checks as using with_mock_API\n"))
    return(NULL)
  }

  if(is.null(shiny_return_token)){
    token <- StravaAuth$public_fields$token

    if(is.null(token) || !is_legit_token(token)) {
      strava_auth()
    }


  } else { #shiny session
    StravaAuth$set("public", "method", "shiny", overwrite=TRUE)
    token <- shiny_return_token

  }

  config(token = token)

}

