#' updated version of odds_consensus, scraping oddsmaker 'public conensus picks' site
#'
#' @param NULL
#'
#' @return a tibble of picks consensus across all games on https://www.oddsshark.com/nfl/consensus-picks at a moment in time
#'
#' @example None
#'
#' @export
picks_consensus<-function(){
  shark_con<-rvest::read_html('https://www.oddsshark.com/nfl/consensus-picks') |>
    rvest::html_elements('table') |>
    rvest::html_table() |>
    bind_rows(.id= 'game_num') |>
    tidyr::separate(col= 'Spread Consensus', into= c('team', 'spread', 'spread_share'), sep = ' ') |>
    tidyr::separate(col= 'O/U Consensus', into= c('ou', 'ou_target', 'ou_share'), sep = '[ ]+') |>
    dplyr::select(game_num, matchup= `See Matchup`, team, spread, spread_share, spread_payout= 'Price...3', ou, ou_target, ou_share, ou_payout= 'Price...5') |>
    tibble::rowid_to_column() |>
    dplyr::mutate(date_pulled= lubridate::now(tzone = 'EST'),
                  team= stringr::str_extract(team, '^([A-Z]{3}|[A-Z]{2})'),
                  game_num= as.numeric(game_num),
                  home_away= dplyr::case_when(rowid %% 2 == 0 ~ 'home', TRUE ~ 'away'),
                  spread_share= as.numeric(stringr::str_replace(spread_share, '%', ""))/100,
                  spread_payout = dplyr::case_when(as.double(spread_payout) > 0 ~ as.double(spread_payout) * -1,
                                                   TRUE ~ as.double(spread_payout)),
                  ou_share= as.numeric(stringr::str_replace(ou_share, '%', ""))/100,
                  ou= dplyr::case_when(stringr::str_detect(ou, 'U') ~ 'Under',
                                       T ~ 'Over'),
                  spread= as.numeric(spread),
                  ou_target= as.numeric(ou_target),
                  ou_payout = dplyr::case_when(as.double(ou_payout) > 0 ~ as.double(ou_payout) * -1,
                                               TRUE ~ as.double(ou_payout))) |>
    dplyr::select(-rowid) |>
    dplyr::select(date_pulled, matchup,  game_num, team, home_away, dplyr::everything()) |>
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))
  shark_times<-rvest::read_html('https://www.oddsshark.com/nfl/consensus-picks') |>
    rvest::html_elements('div.pick-mobile-date') |>
    rvest::html_text() |>
    tibble::tibble() |>
    dplyr::group_by(row_number()) |>
    dplyr::group_split() |>
    purrr::map_df(.f = slice, rep(1, 2)) |>
    dplyr::rename(game_time= 1) |>
    dplyr::mutate(game_time= stringr::str_remove(game_time, pattern = 'Sun, |Mon, |Thu, |See Matchup'),
                  game_time= stringr::str_remove(game_time, pattern = ' See Matchup'),
                  game_time= update(object= lubridate::parse_date_time(game_time, '%B %d, HM'), year= lubridate::year(lubridate::today())),
                  week_num= dplyr::case_when(game_time > '2022-09-07' & game_time < '2022-09-14' ~ 'week_1',
                                             game_time > '2022-09-14' & game_time < '2022-09-21' ~ 'week_2',
                                             game_time > '2022-09-21' & game_time < '2022-09-28' ~ 'week_3',
                                             game_time > '2022-09-28' & game_time < '2022-10-05' ~ 'week_4',
                                             game_time > '2022-10-05' & game_time < '2022-10-12' ~ 'week_5',
                                             game_time > '2022-10-12' & game_time < '2022-10-19' ~ 'week_6',
                                             game_time > '2022-10-19' & game_time < '2022-10-26' ~ 'week_7',
                                             game_time > '2022-10-26' & game_time < '2022-11-02' ~ 'week_8',
                                             game_time > '2022-11-02' & game_time < '2022-11-09' ~ 'week_9',
                                             game_time > '2022-11-09' & game_time < '2022-11-16' ~ 'week_10',
                                             game_time > '2022-11-16' & game_time < '2022-11-23' ~ 'week_11',
                                             game_time > '2022-11-23' & game_time < '2022-11-30' ~ 'week_12',
                                             game_time > '2022-11-30' & game_time < '2022-12-07' ~ 'week_13',
                                             game_time > '2022-12-07' & game_time < '2022-12-14' ~ 'week_14',
                                             game_time > '2022-12-14' & game_time < '2022-12-21' ~ 'week_15',
                                             game_time > '2022-12-21' & game_time < '2022-12-28' ~ 'week_16',
                                             game_time > '2022-12-28' & game_time < '2023-01-04' ~ 'week_17',
                                             game_time > '2023-01-04' & game_time < '2022-01-11' ~ 'week_18',
                  )) |>
    dplyr::select(-2)
  consensus<- shark_con |> bind_cols(shark_times)
  return(consensus)
}
