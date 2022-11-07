#' scrapes the 'odds consensus' page from oddsshark.com to provide insight on where the public is betting on any given game
#' all you need to do is enter the amount of games for that week listed on the oddsshark consensus page (https://www.oddsshark.com/nfl/consensus-picks) and it will generate a table that lists the different game, their respective spreads at that point-in-time, and what share of wagers have gone in either direction
#'
#' @param weeks number of weeks you want to see the schedule for
#' @param year the season you'd like to pull
#'
#' @return a tibble containing the full or a portion of the NFL schedule for a particualr season
#'
#' @example None
#'
#' @export
nfl_season_schedule<-function(year){
  nfl_year<- paste('https://www.pro-football-reference.com/years/', year, '/games.htm', sep= '')
  sched<-read_html(nfl_year) |> 
    html_elements('table') |> 
    html_table() |> 
    bind_rows() |> 
    janitor::clean_names(case= 'snake') |>
    filter(week != 'Week') |> 
    mutate(week= as.numeric(week)) |> 
    filter(week >= 10) |>
    select(week, day, date, time, away= winner_tie, home= loser_tie) |> 
    group_by(week) |> 
    group_split() |> 
    map(mutate, game_num= row_number()) |> 
    bind_rows() |> 
    unite(col = 'game_time', date, time, sep= " ") |> 
    mutate(game_time= lubridate::ymd_hm(game_time),
           game_time= as_datetime(format(game_time, format="%Y-%m-%d %I:%M:%S"))) |> 
    pivot_longer(cols= -c('week', 'day', 'game_time', 'game_num'), names_to = 'home_away', values_to = 'team') |> 
    select(week, day, game_time, game_num, home_away, team) |> 
    left_join(read_csv('team_abbrev_match.csv'), by= 'team') |> 
    rename(team_abbrev= abrev) |> 
    select(week, day, game_time, game_num, team, team_abbrev, home_away)
  return(sched)
}