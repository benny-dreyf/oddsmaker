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
odds<- function(week_num, num_games){
# create the list of games
  games<- c(2:(num_games+1))
# create a list of html for each game at the url
  game_list<- map(.x= '', paste, '#block-system-main > div > div:nth-child(', games, ') > table', 
                 sep = "") 
# create a blank tibble to start
  data_pull<- tibble()
  for (i in seq_along(game_list[[1]])){
    tmp<- read_html('https://www.oddsshark.com/nfl/consensus-picks') %>% 
      html_node(game_list[[1]][i]) %>% 
      html_table()
    data_pull<- bind_rows(data_pull, tmp)
  }
  data_pull %>% 
    select(team= X2, spread_share= X3, spread= X4, spread_payout= X5, 
           over_under= X6, ou_share=X7, o_u_payout=X8) %>% 
    mutate(spread_share= as.numeric(str_replace(spread_share, '%', ""))/100, 
           ou_share= as.numeric(str_replace(ou_share, '%', ""))/100,
           over_under= as.numeric(str_replace(over_under, 'O/U', "")),
           date_pulled= lubridate::today(tzone = 'EST')) %>% 
    mutate(game_num= as.numeric(seq(from= 1, to= NROW(data_pull)))) %>% 
    mutate(home_away= case_when(game_num %% 2 == 0 ~ 'home', TRUE ~ 'away')) %>% 
    mutate(game_num= case_when(game_num %% 2 == 0 ~ game_num/2, TRUE ~ game_num)) %>% 
    mutate(game_num= case_when(game_num > lead(game_num, n= 1) ~ lead(game_num, n=1), TRUE ~ game_num)) %>% 
    mutate(week= paste('Week', week_num, sep = " ")) %>% 
    mutate(spread_payout = case_when(as.double(spread_payout) > 0 ~ as.double(spread_payout) * -1,
                                     TRUE ~ as.double(spread_payout)), 
           o_u_payout = case_when(as.double(o_u_payout) > 0 ~ as.double(o_u_payout) * -1,
                                  TRUE ~ as.double(o_u_payout))) %>% 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% 
    select(date_pulled, week_num, game_num, team, home_away, everything())
}