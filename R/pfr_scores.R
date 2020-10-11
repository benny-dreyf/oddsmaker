#' scrapes the final scores page on https://www.pro-football-reference.com to get each week's final scores
#' list the week #, the # of games, and the URL to gather final scores for each game
#'
#' @param week_num number week of NFL play are we currently in
#' @param num_games number of games listed on oddsshark.com consensus site that have percent shares listed
#' @param game_url provide the URL for the week of games on profootball reference
#'
#' @return a tibble of all games for that week
#'
#' @example pfr_pull()
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
pfr_scores<- function(week_num, num_games, game_url){
  num_games<- c(1:num_games)
  game_list<- purrr::map(.x= '', paste, '#content > div.game_summaries > div:nth-child(',
                         num_games, ') > table.teams',
                         sep = "")
  pfr_pull<- tibble::tibble()
  for (i in seq_along(game_list[[1]])){
    tmp<- xml2::read_html(game_url) %>%
      rvest::html_node(game_list[[1]][i]) %>%
      rvest::html_table()
    pfr_pull<- dplyr::bind_rows(pfr_pull, tmp)
  }
  pfr_pull %>%
    dplyr::mutate(game_date= dplyr::case_when(stringr::str_detect(X3, pattern = 'Final') ~ dplyr::lag(X3, n = 1),
                                              stringr::str_detect(X3, pattern = '""') ~ dplyr::lag(X3, n = 2), TRUE ~ X3)) %>%
    dplyr::mutate_all(dplyr::na_if,"") %>%
    dplyr::mutate_all(dplyr::na_if, "OT") %>%
    dplyr::slice(-c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46)) %>%
    dplyr::select(pfr_team= X1, score= X2, game_date) %>%
    dplyr::mutate(game_date= dplyr::case_when(is.na(game_date) ~ dplyr::lag(game_date, n=1), TRUE ~ game_date),
                  game_date= lubridate::mdy(game_date),
                  game_num= as.numeric(seq(from= 1, to= NROW(pfr_pull)/1.5)),
                  game_num= dplyr::case_when(game_num %% 2 == 0 ~ game_num/2, TRUE ~ game_num),
                  game_num= dplyr::case_when(game_num > dplyr::lead(game_num, n= 1) ~ dplyr::lead(game_num, n=1), TRUE ~ game_num),
                  week= paste('Week', week_num, sep = " "),
                  score= as.numeric(score)) %>%
    dplyr::select(week, game_date, game_num, pfr_team, score)
}
