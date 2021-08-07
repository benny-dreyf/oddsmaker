#' scrapes the 'odds consensus' page from oddsshark.com to provide insight on where the public is betting on any given game
#' all you need to do is enter the amount of games for that week listed on the oddsshark consensus page (https://www.oddsshark.com/nfl/consensus-picks) and it will generate a table that lists the different game, their respective spreads at that point-in-time, and what share of wagers have gone in either direction
#'
#' @param week_num number week of NFL play are we currently in
#' @param num_games number of games listed on oddsshark.com consensus site that have percent shares listed
#'
#' @return a tibble of all games for that week
#'
#' @example None
#'
#' @export
#' @importFrom magrittr %>%
#'
odds_consensus<- function(week_num, num_games){
  games<- c(1:(num_games))
  url_list<- purrr::map(.x= '', paste, '#block-system-main > div > div.computer-picks.computer-picks-nfl > div > div.picks-table-wrap.picks-count-', games, ' > div.pick-table-content > table',
                        sep = "")
  data_pull<- tibble::tibble()
  table_gather<- function(item){
    xml2::read_html('https://www.oddsshark.com/nfl/consensus-picks') %>%
      rvest::html_node(item) %>%
      rvest::html_table()
  }
  final_table<- url_list[[1]] %>%
    purrr::map(.f = table_gather) %>%
    # purrr::map(dplyr::mutate_all, as.character) %>%
    dplyr::bind_rows(.id = 'game_num') %>%
    dplyr::mutate_all(.funs = as.character) %>%
    separate(col= 'Spread Consensus', into= c('team', 'spread', 'spread_share'), sep = ' ') %>%
    # mutate('O/U Consensus'= str_remove('O/U Consensus', '^[OU] ')) %>%
    tidyr::separate(col= 'O/U Consensus', into= c('ou', 'ou_target', 'ou_share'), sep = '[ ]+') %>%
    dplyr::select(game_num, team, spread, spread_share, spread_payout= 'Price...3', ou, ou_target, ou_share, ou_payout= 'Price...5') %>%
    tibble::rowid_to_column() %>%
    dplyr::mutate(date_pulled= lubridate::today(tzone = 'EST'),
                  team= stringr::str_extract(team, '^([A-Z]{3}|[A-Z]{2})'),
                  game_num= as.numeric(game_num),
                  home_away= dplyr::case_when(rowid %% 2 == 0 ~ 'home', TRUE ~ 'away'),
                  week= paste('Week', week_num, sep = " "),
                  spread_share= as.numeric(stringr::str_replace(spread_share, '%', ""))/100,
                  spread_payout = dplyr::case_when(as.double(spread_payout) > 0 ~ as.double(spread_payout) * -1,
                                                   TRUE ~ as.double(spread_payout)),
                  ou_share= as.numeric(stringr::str_replace(ou_share, '%', ""))/100,
                  ou= dplyr::case_when(stringr::str_detect(ou, 'U') ~ 'Under',
                                T ~ 'Over'),
                  spread= as.numeric(spread),
                  ou_target= as.numeric(ou_target),
                  ou_payout = dplyr::case_when(as.double(ou_payout) > 0 ~ as.double(ou_payout) * -1,
                                               TRUE ~ as.double(ou_payout))) %>%
    dplyr::select(-rowid) %>%
    dplyr::select(date_pulled,week,  game_num, team, home_away, dplyr::everything()) %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))
  return(final_table)
}
