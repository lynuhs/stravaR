% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checks.R
\name{strava_check_existing_token}
\alias{strava_check_existing_token}
\title{Check a token vs options}
\usage{
strava_check_existing_token(token = StravaAuth$public_fields$token)
}
\arguments{
\item{token}{A token to check, default current live session token}
}
\value{
\code{FALSE} if the options and current token do not match, \code{TRUE} if they do.
}
\description{
Useful for debugging authentication issues
}
\details{
Will compare the passed token's settings and compare to set options.
  If these differ, then reauthentication may be needed.
}
