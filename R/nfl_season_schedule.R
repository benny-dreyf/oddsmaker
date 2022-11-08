#' scrapes season schedule from pro-football-reference.com
#'
#' @param year the season you'd like to pull
#'
#' @return a tibble containing the full or a portion of the NFL schedule for a particualr season
#'
#' @example None
#'
#' @export
nfl_season_schedule<-function(year){
  nfl_year<- paste('https://www.pro-football-reference.com/years/', year, '/games.htm', sep= '')
  sched<- rvest::read_html(nfl_year) |>
    rvest::html_elements('table') |>
    rvest::html_table() |>
    dplyr::bind_rows() |>
    janitor::clean_names(case= 'snake') |>
    dplyr::filter(week != 'Week') |>
    dplyr::mutate(week= as.numeric(week)) |>
    dplyr::filter(week >= 10) |>
    dplyr::select(week, day, date, time, away= winner_tie, home= loser_tie) |>
    dplyr::group_by(week) |>
    dplyr::group_split() |>
    purrr::map(dplyr::mutate, game_num= dplyr::row_number()) |>
    dplyr::bind_rows() |>
    tidyr::unite(col = 'game_time', date, time, sep= " ") |>
    dplyr::mutate(game_time= lubridate::ymd_hm(game_time),
                  game_time= lubridate::as_datetime(format(game_time, format="%Y-%m-%d %I:%M:%S"))) |>
    tidyr::pivot_longer(cols= -c('week', 'day', 'game_time', 'game_num'), names_to = 'home_away', values_to = 'team') |>
    dplyr::select(week, day, game_time, game_num, home_away, team) |>
    dplyr::left_join(readr::read_csv('team_abbrev_match.csv'), by= 'team') |>
    dplyr::rename(team_abbrev= abrev) |>
    dplyr::select(week, day, game_time, game_num, team, team_abbrev, home_away)
  return(sched)
}
