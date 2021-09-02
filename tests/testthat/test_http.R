library(testthat)

test_that("Argument Classification", {
  curr_year <- lubridate::year(lubridate::today())

  # Team keys
  expect_error(.ClassifyArg("frc"))
  expect_equal(.ClassifyArg("frc1"), c("{team_key}" = "frc1"))
  expect_equal(.ClassifyArg("frc01"), c("{team_key}" = "frc01"))
  expect_equal(.ClassifyArg("frc234"), c("{team_key}" = "frc234"))
  expect_equal(.ClassifyArg("frc9015"), c("{team_key}" = "frc9015"))
  expect_equal(.ClassifyArg("frc10001"), c("{team_key}" = "frc10001"))
  expect_error(.ClassifyArg("frc777777"))
  expect_error(.ClassifyArg("1frc1318"))
  expect_error(.ClassifyArg("frc2046_"))
  expect_error(.ClassifyArg("frc_1983"))

  # Page Numbers
  expect_equal(.ClassifyArg("1"), c("{page_num}" = "1"))
  expect_equal(.ClassifyArg(1), c("{page_num}" = "1"))
  expect_equal(.ClassifyArg("293"), c("{page_num}" = "293"))
  expect_error(.ClassifyArg(-1))

  # Years
  expect_error(.ClassifyArg("1991"))
  expect_error(.ClassifyArg(1991))
  expect_equal(.ClassifyArg("1992"), c("{year}" = "1992"))
  expect_equal(.ClassifyArg(2021), c("{year}" = "2021"))
  expect_equal(.ClassifyArg(curr_year), c("{year}" = as.character(curr_year)))
  expect_error(.ClassifyArg(curr_year + 1))

  # District Keys
  expect_equal(.ClassifyArg("2020pnw"), c("{district_key}" = "2020pnw"))
  expect_error(.ClassifyArg("1991pnw"))
  expect_equal(.ClassifyArg("1992pnw"), c("{district_key}" = "1992pnw"))
  expect_error(.ClassifyArg("pnw2020"))
  expect_error(.ClassifyArg(paste0(curr_year + 1, "pnw")))

  # Event Keys
  expect_equal(.ClassifyArg("2020wasno"), c("{event_key}" = "2020wasno"))
  expect_equal(.ClassifyArg("2020wasn"), c("{event_key}" = "2020wasn"))
  expect_equal(.ClassifyArg("2020wasnowasno"),
               c("{event_key}" = "2020wasnowasno"))
  expect_error(.ClassifyArg(paste0(curr_year + 1, "wasno")))
  expect_error(.ClassifyArg("1991wasno"))
  expect_equal(.ClassifyArg("1992wasno"), c("{event_key}" = "1992wasno"))
  expect_error(.ClassifyArg("1991wa$no"))

  # Match Keys
  expect_equal(.ClassifyArg("2018wasno_qm11"),
               c("{match_key}" = "2018wasno_qm11"))
  expect_error(.ClassifyArg("2020wasno_q1"))
  expect_error(.ClassifyArg("1991wasno_qm1"))
  expect_equal(.ClassifyArg("2020waspo_sf1m1"),
               c("{match_key}" = "2020waspo_sf1m1"))
  expect_equal(.ClassifyArg("2020pncm_f1m2"),
               c("{match_key}" = "2020pncm_f1m2"))
  expect_equal(.ClassifyArg("2019turing_sf2m2"),
               c("{match_key}" = "2019turing_sf2m2"))
  expect_error(.ClassifyArg("2019wayak_z2m2"))

  # Multiple Arguments
  args <- list("2018wasno_qm11", "1992pnw", "2017turing", "5", "frc568")
  classified_args <- c("{match_key}" = "2018wasno_qm11",
                       "{district_key}" = "1992pnw",
                       "{event_key}" = "2017turing",
                       "{page_num}" = "5",
                       "{team_key}" = "frc568")
  expect_equal(.ClassifyArgs(args), classified_args)
  })

test_that("Building API Calls", {
  api_calls <- list(
    "{year}" = c("districts", "{year}"),
    "{team_key}" = c("team", "{team_key}", "districts"))
  api_call <- .BuildApiCommand(c("{year}" = "2016"), api_calls)
  expect_equal(api_call, "districts/2016")
  api_call <- .BuildApiCommand(c("{team_key}" = "frc2926"), api_calls)
  expect_equal(api_call, "team/frc2926/districts")


  api_calls <- list(
    "{team_key}{event_key}" = c("team", "{team_key}", "event", "{event_key}",
                                "matches"),
    "{event_key}{team_key}" = c("team", "{team_key}", "event", "{event_key}",
                                "matches"),
    "{event_key}" = c("event", "{event_key}", "matches"),
    "{team_key}{year}" = c("team", "{team_key}", "matches", "{year}"),
    "{year}{team_key}" = c("team", "{team_key}", "matches", "{year}"))
  api_call <- .BuildApiCommand(c("{team_key}" = "frc1899", "{year}" = "2018"),
                               api_calls, extent = "simple")
  expect_equal(api_call, "team/frc1899/matches/2018/simple")
  api_call <- .BuildApiCommand(c("{event_key}" = "2018pncmp",
                                 "{team_key}" = "frc3223"),
                               api_calls, extent = "keys")
  expect_equal(api_call, "team/frc3223/event/2018pncmp/matches/keys")
  api_call <- .BuildApiCommand(c("{event_key}" = "2018pncmp",
                                 "{team_key}" = "frc3223"),
                               api_calls, extent = "pizza")
  expect_equal(api_call, "team/frc3223/event/2018pncmp/matches")

})

test_that("HTTP Response", {
  tba_response <- .SendRequest("status")
  expect_type(tba_response, "list")
  expect_equal(names(tba_response), c("text", "url", "date", "last_modified"))
  expect_equal(tba_response$url,
               "https://www.thebluealliance.com/api/v3/status")
  expect_type(jsonlite::fromJSON(tba_response$text), "list")
  expect_false(is.na(lubridate::ymd_hms(tba_response$date)))
  expect_false(is.na(lubridate::dmy_hms(tba_response$last_modified)))
})
