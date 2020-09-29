#' scrapes the final scores page on https://www.pro-football-reference.com to get each week's final scores
#' list the week #, the # of games, and the URL to gather final scores for each game
#' 
#' @param week_num number week of NFL play are we currently in
#' @param num_games number of games listed on oddsshark.com consensus site that have percent shares listed
#' @param game_url provide the URL for the week of games on profootball reference
#' 
#' @return a tibble of all games for that week
#' 
#' @example None
#' 
#' @export
pfr_scores<- function(week_num, num_games, game_url){
  num_games<- c(1:num_games)
  game_list<- map(.x= '', paste, '#content > div.game_summaries > div:nth-child(', 
                  num_games, ') > table.teams', 
                  sep = "")
  pfr_pull<- tibble()
  for (i in seq_along(game_list[[1]])){
    tmp<- read_html(game_url) %>% 
      html_node(game_list[[1]][i]) %>% 
      html_table()
    pfr_pull<- bind_rows(pfr_pull, tmp) 
  }
  pfr_pull %>% 
    mutate(game_date= case_when(str_detect(X3, pattern = 'Final') ~ lag(X3, n = 1),
                                str_detect(X3, pattern = '""') ~ lag(X3, n = 2), TRUE ~ X3)) %>% 
    mutate_all(na_if,"") %>% 
    mutate_all(na_if, "OT") %>% 
    mutate(game_date= case_when(is.na(game_date) ~ lag(game_date, n=1), TRUE ~ game_date)) %>% 
    slice(-c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46)) %>% 
    select(-X3, team= X1, score= X2) %>% 
    mutate(game_date= lubridate::mdy(game_date)) %>% 
    mutate(game_num= as.numeric(seq(from= 1, to= NROW(pfr_pull)/1.5))) %>% 
    mutate(game_num= case_when(game_num %% 2 == 0 ~ game_num/2, TRUE ~ game_num)) %>% 
    mutate(game_num= case_when(game_num > lead(game_num, n= 1) ~ lead(game_num, n=1), TRUE ~ game_num)) %>% 
    mutate(week= paste('Week', week_num, sep = " ")) %>% 
    select(week, game_date, game_num, everything()) %>% 
    rename(pfr_team= team)
}