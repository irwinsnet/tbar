# Internal TbaR functions
#
# Dependencies:
#   httr
#   keyring
#   magrittr
#   purrr
#
# Stacy Irwin, 1 Sep 2021


#' Assembles command that will be sent to the TBA Read API
#'
#' `.BuildApiCommand()` builds a correctly-formatted TBA command that can be
#' sent to *The Blue Alliance's* (TBA) Read API, version 3. It accepts two
#' arguments: a list of TBA API templates and a character vector with
#' arguments supplied by the user. `.BuildAPICommand()` builds a
#' correctly-formatted TBA command from these two objects.
#'
#' ## TBA Commands
#' The command is the latter portion of the URL. Specifically, it is
#' everything in the URL after "https://www.thebluealliance.com/api/v3".
#' Consider the following URL:
#' `https://www.thebluealliance.com/api/v3/district/2020pnw/teams`
#' The command is `district/2020pnw/teams`. This particular command will
#' return a JSON object with all FRC teams that were active in the Pacific
#' Northwest in 2020. The specifications for all TBA Read API commands are
#' at https://www.thebluealliance.com/apidocs/v3.
#'
#' Each command consists of segments separated by forward slashes. A
#' segment can be either an argument or an option.
#' * An argument is a value that is provided by the user. Arguments specify
#'   individual teams, events, matches, districts, etc. Examples include
#'   "2020wasno" (event), "frc1318" (team), and "2020pnw" (district).
#'   Commands can contain up to two arguments.
#' * Options are string constants that specify how the argument is
#'   interpreted by the TBA API and what data should be returned. Examples
#'   of options include "team", "districts", "events",  etc.
#'
#' ## API Calls
#' An API call is an element in a named list. When combined with arguments
#' supplied by the user, it contains all information needed to build a TBA
#' command. Here are example API calls from the `GetDistricts()` and
#' `GetMatches()` functions:
#'
#' ```
#' # GetDistricts() API Calls
#' list("{year}" = c("districts", "{year}"),
#'      "{team_key}" = c("team", "{team_key}", "districts")
#' )
#' # GetMatches() API Calls
#' list("{team_key}{event_key}" = c("team", "{team_key}",
#'                                  "event", "{event_key}",
#'                                  "matches"),
#'      "{event_key}{team_key}" = c("team", "{team_key}",
#'                                  "event", "{event_key}",
#'                                  "matches")
#' )
#' ```
#' Each character vector in the list contains segments in the order
#' required by TBA. The segments enclosed in curly braces are parameters
#' and the rest are options. Parameters are placeholders for user-supplied
#' arguments. The parameter value specifies the type of data
#' (i.e., team key, year, event key, etc.) that is allowed in that TBA
#' command.
#'
#' The name of each list element consists of the arguments
#' that are used by that API call. For `GetMatches()`, which accepts
#' two arguments, the API call is listed a second time with the order
#' of arguments in its name reversed. This allows users to reverse the
#' order of the arguments when they call `GetMatches()`.
#'
#' ## Arguments
#' Arguments are passed to `.BuildApiCommand()` as a named character vector.
#' For example:
#'
#' `c("{team_key} = "frc1318", "{event_key}" = "2020wasno")`
#'
#' The vector names are parameters and the values are the corresponding
#' arguments.
#'
#' ## Extent Argument
#' One argument is treated specially. Several TBA API calls accept an
#' optional final segment that can either be "simple" or "keys". Fewer
#' columns of data are returned if the "simple" segment is added. Only
#' one column of data is returned if the "keys" segment is added.
#' The column contains a list of keys, e.g., district keys, team keys,
#' match keys, etc.
#'
#' @param args A named character vector containing parameter-argument
#'   pairs.
#' @param api_templates A named list containing TBA API calls.
#' @param extent Optional. A string that should be "full", "simple",
#'   or "keys". Defaults to "full".
#'
#' @return The TBA command as a length-one character vector.
.BuildApiCommand <- function(args, api_templates, extent = "full") {
  # Build API call name
  parameters <- names(args)
  api_template_name <- paste0(parameters, collapse = "")

  # Select API call that corresponds to user-supplied arguments
  api_template <- api_templates[[api_template_name]]

  # Add extent option
  if (tolower(extent) %in% c("simple", "keys")) {
    api_template <- append(api_template, tolower(extent))
  }

  # Replace parameters with arguments and convert to string
  for (arg_name in parameters) {
    api_template[api_template == arg_name] <- args[arg_name]
  }
  api_template %>%
    paste(collapse = "/") %>%
    return()
}


#' Identifies Arguments' Parameter Type
#'
#' Takes a list of user-supplied arguments and identifies their
#' parameters. In other words, identifies if an argument is a year,
#' team key, event key, match key, etc. This function uses
#' `.ClassifyArg()` to convert each element in the list.
#'
#' @param arg_list A character vector containing argument values.
#'
#' @return A named character vector, where the values are the original
#'   argument and the names are the parameter. The return value can
#'   be passed to `.BuildApiCommand()`'s *args* parameter.
.ClassifyArgs <- function(arg_list) {
  arg_list %>%
    purrr::map(.ClassifyArg) %>%
    purrr::flatten_chr() %>%
    return()
}


#' Identifies a single argument's parameter type
#'
#' Takes any user-supplied argument and identifies it's parameter. In
#' other words, identifies if an argument is a year, team key, event key,
#' match key, etc.
#'
#' @param arg A length-one character vector
#'
#' @return A named character vector, where the value is the original
#'   argument and the name is the parameter.
#'
#' @examples
#' .ClassifyArg("frc1318") # Returns c("{team_key}" = "frc1318")
#' .ClassifyArg("2020wasno") # Returns c("{event_key}" = "2020wasno")
.ClassifyArg <- function(arg) {

  .CheckYear <- function(arg) {
    year_arg <- as.integer(substr(arg, 1, 4))
    if (year_arg < 1992 || year_arg > lubridate::year(lubridate::today())) {
      stop("Year must be between 1992 and current year.")
    }
    return(TRUE)
  }

  arg_str <- arg %>% as.character() %>% tolower()

  if (grepl(r"(^frc\d{1,5}$)", arg_str, perl = TRUE)) { # Teams
    classified_arg <- c("{team_key}" = arg_str)
  } else if (grepl(r"(^\d{4}$)", arg_str, perl = TRUE)) { # Years
    .CheckYear(arg_str)
    classified_arg <- c("{year}" = arg_str)
  } else if (grepl(r"(^\d{1,3}$)", arg_str, perl = TRUE)) { # Page numbers
    classified_arg <- c("{page_num}" = arg_str)
  } else if (grepl(r"(^\d{4}\w{3}$)", arg_str, perl = TRUE)) { # Districts
    .CheckYear(arg_str)
    classified_arg <- c("{district_key}" = arg_str)
  } else if (grepl(r"(^\d{4}[a-z]{4,}_(qm\d{1,3}|(qf|sf|f)\dm\d)$)", # Matches
                arg_str,
                perl = TRUE)) {
    .CheckYear(arg_str)
    classified_arg <- c("{match_key}" = arg_str)
  } else if (grepl(r"(^\d{4}[a-z]{4,}$)", arg_str, perl = TRUE)) { # Events
    .CheckYear(arg_str)
    classified_arg <- c("{event_key}" = arg_str)
  } else {
    stop("Argument must be 4-digit year or key (team, district, event, etc.).")
  }
  return(classified_arg)
}


#' Submits an HTTP request to the TBA API.
#'
#' @param arg_list A list of TBA arguments.
#' @param api_calls A list of API calls.
#' @param extent Either "full", "simple", or "keys". Specifies the number
#'   of columns that will be returned.
#'
#' @return The JSON text returned from the TBA API.
.CallTBA <- function(arg_list, api_calls, extent = "full") {
  arg_list %>%
    .ClassifyArgs() %>%
    .BuildApiCommand(api_calls, extent) %>%
    .SendRequest() %>%
    return()
}


#' Sends an HTTP Request
#'
#' Sends an HTTP request to The Blue Alliance (TBA) Read API V3. The TBA
#' authorization key must be stored by the *keyring* package. If not
#' specified as an argument, assumes the *keyring* service name is
#' "tba_auth_key".
#'
#' .SendRequest is called by other functions in the TbaR package.
#'
#' .SendRequest raises an http error if the HTTP request is unsuccessful.
#'
#' @param url_path Text that is added to the base TBA URL that specifies
#'   the TBA command.
#' @param key_service The name of the key, as stored by the keyring package.
#'
#' @return A named list with the following elements:
#'   * text: The content of the HTTP response.
#'   * URL: The URL to which the HTTP request was sent.
#'   * date: The date and time when the data was retrieved from TBA.
#'   * last_modified: The content of the HTTP response's last-modified
#'     header, showing when the TBA data was last updated.
.SendRequest <- function(url_path, key_service = "tba_auth_key") {
  # User can include backslash as path separator, or not, up to them
  if (substr(url_path, 1, 1) != "/") url_path <- paste0("/", url_path)
  base_url <- "https://www.thebluealliance.com/api/v3"
  url <- paste0(base_url, url_path)
  headers <- httr::add_headers(
    "X-TBA-Auth-Key" = keyring::key_get(key_service),
    "User-Agent" = "TbaR: R SDK for TBA Read API")
  resp <- httr::GET(url, headers)
  httr::stop_for_status(resp)

  tba_response <- list(text = httr::content(resp, "text"),
                       url = resp$url,
                       date = resp$date,
                       last_modified = resp$headers$`last-modified`)
  return(tba_response)
}
