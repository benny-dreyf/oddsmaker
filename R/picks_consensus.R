#' updated version of odds_consensus, scraping oddsmaker 'public conensus picks' site
#'
#' @param season denotes the NFL season
#'
#' @return a tibble of picks consensus across all games on https://www.oddsshark.com/nfl/consensus-picks at a moment in time
#'
#' @example None
#'
#' @export
picks_consensus<-function(season){
  shark_con<-rvest::read_html('https://www.oddsshark.com/nfl/consensus-picks') |>
    rvest::html_elements('table') |>
    rvest::html_table() |>
    dplyr::bind_rows(.id= 'game_num') |>
    tidyr::separate(col= 'Spread Consensus', into= c('team', 'spread', 'spread_share'), sep = ' ') |>
    tidyr::separate(col= 'O/U Consensus', into= c('ou', 'ou_target', 'ou_share'), sep = '[ ]+') |>
    dplyr::select(game_num, matchup= 2, team, spread, spread_share, spread_payout= 'Price...3', ou, ou_target, ou_share, ou_payout= 'Price...5') |>
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
                                               TRUE ~ as.double(ou_payout)),
                  week= dplyr::case_when(lubridate::today() >= '2024-09-03' & lubridate::today() < '2024-09-10' ~ 1,
                                             lubridate::today() >= '2024-09-10' & lubridate::today() < '2024-09-17' ~ 2,
                                             lubridate::today() >= '2024-09-17' & lubridate::today() < '2024-09-24' ~ 3,
                                             lubridate::today() >= '2024-09-24' & lubridate::today() < '2024-10-01' ~ 4,
                                             lubridate::today() >= '2024-10-01' & lubridate::today() < '2024-10-08' ~ 5,
                                             lubridate::today() >= '2024-10-08' & lubridate::today() < '2024-10-15' ~ 6,
                                             lubridate::today() >= '2024-10-15' & lubridate::today() < '2024-10-22' ~ 7,
                                             lubridate::today() >= '2024-10-22' & lubridate::today() < '2024-10-29' ~ 8,
                                             lubridate::today() >= '2024-10-29' & lubridate::today() < '2024-11-05' ~ 9,
                                             lubridate::today() >= '2024-11-05' & lubridate::today() < '2024-11-12' ~ 10,
                                             lubridate::today() >= '2024-11-12' & lubridate::today() < '2024-11-19' ~ 11,
                                             lubridate::today() >= '2024-11-19' & lubridate::today() < '2024-11-26' ~ 12,
                                             lubridate::today() >= '2024-11-26' & lubridate::today() < '2024-12-03' ~ 13,
                                             lubridate::today() >= '2024-12-03' & lubridate::today() < '2024-12-10' ~ 14,
                                             lubridate::today() >= '2024-12-10' & lubridate::today() < '2024-12-17' ~ 15,
                                             lubridate::today() >= '2024-12-17' & lubridate::today() < '2024-12-24' ~ 16,
                                             lubridate::today() >= '2024-12-24' & lubridate::today() < '2024-12-31' ~ 17,
                                             lubridate::today() >= '2024-12-31' & lubridate::today() < '2025-01-07' ~ 18,
                                             lubridate::today() >= '2025-01-07' & lubridate::today() < '2025-01-14' ~ 19,
                                             lubridate::today() >= '2025-01-14' & lubridate::today() < '2025-01-21' ~ 20,
                                             lubridate::today() >= '2025-01-21' & lubridate::today() < '2025-01-27' ~ 21,
                                             lubridate::today() >= '2025-01-27' & lubridate::today() < '2025-02-12' ~ 22,
                  )) |>
    dplyr::select(-rowid) |>
    dplyr::select(date_pulled, matchup,  game_num, team, home_away, dplyr::everything()) |>
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))
  consensus<- shark_con
  return(consensus)
}
