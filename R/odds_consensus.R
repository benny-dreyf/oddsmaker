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
odds<- function(week_num, num_games){
  # create the list of games
  games<- c(2:(num_games+1))
  # create a list of html for each game at the url
  game_list<- purrr::map(.x= '', paste, '#block-system-main > div > div:nth-child(', games, ') > table',
                         sep = "")
  # create a blank tibble to start
  data_pull<- tibble::tibble()
  for (i in seq_along(game_list[[1]])){
    tmp<- xml2::read_html('https://www.oddsshark.com/nfl/consensus-picks') %>%
      rvest::html_node(game_list[[1]][i]) %>%
      rvest::html_table() %>%
      dplyr::mutate_all(as.character)
    data_pull<- dplyr::bind_rows(data_pull, tmp)
  }
  data_pull %>%
    dplyr::select(team= X2, spread_share= X3, spread= X4, spread_payout= X5,
                  ou= X6, ou_share=X7, ou_payout=X8) %>%
    dplyr::mutate(spread_share= as.numeric(stringr::str_replace(spread_share, '%', ""))/100,
                  ou_share= as.numeric(stringr::str_replace(ou_share, '%', ""))/100,
                  ou= as.numeric(stringr::str_replace(ou, 'O/U', "")),
                  date_pulled= lubridate::today(tzone = 'EST')) %>%
    dplyr::mutate(game_num= as.numeric(seq(from= 1, to= NROW(data_pull)))) %>%
    dplyr::mutate(home_away= dplyr::case_when(game_num %% 2 == 0 ~ 'home', TRUE ~ 'away'),
                  game_num= dplyr::case_when(game_num %% 2 == 0 ~ game_num/2, TRUE ~ game_num),
                  game_num= dplyr::case_when(game_num > dplyr::lead(game_num, n= 1) ~ dplyr::lead(game_num, n=1), TRUE ~ game_num),
                  game_num= dplyr::case_when(lubridate::wday(Sys.Date()) > 5 ~ game_num + 1,
                                             T ~ game_num),
                  week= paste('Week', week_num, sep = " "),
                  spread_payout = dplyr::case_when(as.double(spread_payout) > 0 ~ as.double(spread_payout) * -1,
                                                   TRUE ~ as.double(spread_payout)),
                  ou_payout = dplyr::case_when(as.double(ou_payout) > 0 ~ as.double(ou_payout) * -1,
                                               TRUE ~ as.double(ou_payout))) %>%
    dplyr::select(date_pulled, week, game_num, team, home_away, dplyr::everything()) %>%
    dplyr::mutate_at(dplyr::vars(6:11), as.double) %>%
    dplyr::mutate_if(is.numeric, ~replace(., is.na(.), 0))
}
