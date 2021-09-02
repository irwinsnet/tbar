# Exported functions for retrieving Blue Alliance data.
#
# Dependencies:
#   dplyr
#   jsonlite
#   lubridate
#   tibble
#
# Stacy Irwin, 1 Sep 2021


#' Retrieves the status of the TBA Read API (V3)
#'
#' @return A named list.
#' @export
#'
#' @examples
#' GetStatus()
GetStatus <- function() {
  tba_resp <- .SendRequest("status")
  tba_resp$text %>%
    jsonlite::fromJSON() %>%
    .AddHTTPAttributes(tba_resp) %>%
    return()
}


#' Retrieves a dataframe of FIRST competition districts
#'
#' If a year is provided, returns all districts that were in effect
#' during that year. If a team key is provided, returns all districts
#' that have had that team as a member.
#'
#' @param year_or_teamkey A four-digit year or team key, i.e., 2019 or
#'   "frc1318". The year can be an integer or string.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' GetDistricts(2019)
#' GetDistricts("frc2976")
GetDistricts <- function(year_or_teamkey) {
  api_templates <- list(
    "{year}" = c("districts", "{year}"),
    "{team_key}" = c("team", "{team_key}", "districts")
  )

  tba_resp <- .CallTBA(year_or_teamkey, api_templates)
  tba_resp$text %>%
    jsonlite::fromJSON() %>%
    tibble::as_tibble() %>%
    .AddHTTPAttributes(tba_resp) %>%
    return()
}


GetTeams <- function(..., extent = "full") {
 api_templates <- list(
   "{district_key}" = c("district", "{district_key}", "teams"),
   "{event_key}" = c("event", "{event_key}", "teams"),
   "{page_num}" = c("teams", "{page_num}"),
   "{year}{page_num}" = c("teams", "{year}", "{page_num}"),
   "{page_num}{year}" = c("teams", "{year}", "{page_num}")
 )

   tba_resp <- .CallTBA(list(...), api_templates, extent)
   tba_resp$text %>%
     jsonlite::fromJSON(flatten = TRUE) %>%
     tibble::as_tibble() %>%
     return()
}


GetMatches <- function(..., extent = "full") {
  api_templates <- list(
    "{team_key}{event_key}" = c("team", "{team_key}", "event", "{event_key}",
                                "matches"),
    "{event_key}{team_key}" = c("team", "{team_key}", "event", "{event_key}",
                                "matches"),
    "{event_key}" = c("event", "{event_key}", "matches"),
    "{team_key}{year}" = c("team", "{team_key}", "matches", "{year}"),
    "{year}{team_key}" = c("team", "{team_key}", "matches", "{year}")
  )

  tba_resp <- .CallTBA(list(...), api_templates, extent)
  tba_resp$text %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_time"), .ConvertUnixTsColumn)) %>%
    .UnnestColumn("alliances.blue.team_keys") %>%
    .UnnestColumn("alliances.red.team_keys") %>%
    .AddHTTPAttributes(tba_resp) %>%
    return()
}



#' Gets data for a single FRC match.
#'
#' @param match_key A match key, e.g., 2020wasno_qm_42
#' @param extent Either "full" or "simple".
#'
#' @return A named list.
#' @export
#'
#' @examples
#' GetMatch("2020waspo_qf2m1")
GetMatch <- function(match_key, extent = "full") {
  api_templates <- list("{match_key}" = c("match", "{match_key}"))
  if (tolower(extent) == "keys") {
    stop("GetMatch does not accept `extent = 'keys' option.")
  }

  # Single match JSON does not parse well. Will take some work.
  tba_resp <- .CallTBA(list(match_key), api_templates, extent)
  tba_resp$text %>%
    jsonlite::fromJSON() %>%
    .AddHTTPAttributes(tba_resp) %>%
    return()
}


#' Converts column of Unix time stamps to readable date-time objects.
#'
#' TBA formats date-times as Unix time stamps, which is the elapsed
#' seconds since 12:00 AM, January 1st, 1970. The date times are in
#' coordinated universal time. Unless your competition is being held
#' in Greenland, Iceland, or West Africa, you'll need to convert the
#' date times to local time.
#'
#' @param col A dataframe column of Unix time stamps.
#'
#' @return A dataframe column of POSIXct date-time objects.
.ConvertUnixTsColumn <- function(col) {
  return(lubridate::as_datetime(col, origin = lubridate::origin, tz = "UTC"))
}


#' Converts a column containing a list to several dataframe columns.
#'
#' `.UnnestColumn()` checks if the dataframe contains the column to be
#' un-nested. If not, the dataframe is returned unaltered.
#'
#' @param dframe A datframe
#' @param col_name The name of the dataframe column that will be un-nested.
#'
#' @return A dataframe
.UnnestColumn <- function(dframe, col_name) {
  if (col_name %in% names(dframe)) {
    return(tidyr::unnest_wider(dframe, col_name, names_sep = "."))
  } else {
    return(dframe)
  }
}

#' Adds additional data from the HTTP response to the dataframe.
#'
#'
#' @param dframe A dataframe.
#' @param tba_response A list returned by `.SendRequest()`.
#'
#' @return A dataframe with additional attributes.
.AddHTTPAttributes <- function(dframe, tba_response) {
  attributes(dframe) <- c(attributes(dframe), tba_response[-1])
  return(dframe)
}
