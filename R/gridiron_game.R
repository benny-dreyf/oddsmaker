#' After using oddsmakr:odds to scrape data and you've compiled it into a dataframe, you can plot a time-series of spreads vs. share for each team by game using this function
#'
#' @param dat dataframe or tibble containing public consensus share of bets and game line for a single game for a given week pulled more than once by odds_consensus function from oddsmaker package
#' @param week_no week of nfl season to plot
#' @param game_no game of nfl week to plot
#'
#' @return a times-series chart showing public consensus and lines movement for one single nfl game during a given week
#'
#' @example None
#'
#' @export
#'
gridiron_game<- function(dataset, week_no, game_no){
  dataset<- dataset |> filter(week == week_no & game_num == game_no)
  colors_list<- c('ARI' = '#97233F', 'ATL' = '#A71930', 'BAL' = '#241773', 'BUF' = '#00338D', 'CAR' = '#0085CA',
                  'CHI' = '#00143F', 'CIN' = '#FB4F14', 'CLE' = '#22150C', 'DAL' = '#B0B7BC', 'DEN' = '#002244',
                  'DET' = '#046EB4', 'GB' = '#24423C', 'HOU' = '#00143F', 'IND' = '#003D79', 'JAC' = '#136677',
                  'KC' = '#CA2430', 'LAC' = '#2072BA', 'LAR' = '#002147', 'LV' = '#000000', 'MIA' = '#0091A0',
                  'MIN' = '#4F2E84', 'NE' = '#0A2342', 'NO' = '#A08A58', 'NYG' = '#A08A58', 'NYJ' = '#203731',
                  'PHI' = '#004C54', 'PIT' = '#FFC20E', 'SEA' = '#7AC142','SF' = '#C9243F', 'TB' = '#FF7900',
                  'TEN' = '#4095D1', 'WAS' = '#773141')
  colors_list<- colors_list[unique(dataset$team)]
  z<- ggplot2::ggplot(data = dataset, mapping = ggplot2::aes(x= id, y= spread_share, color= team, group= team)) +
    ggplot2::geom_line() +
    ggplot2::geom_text(ggplot2::aes(label= spread), vjust= -1,  size= 3.5, show.legend = F) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill= 'white'),
                   title = ggplot2::element_text(size= 11, face = 'bold'),
                   axis.text = ggplot2::element_text(size= 10, colour = 'black'),
                   axis.title = ggplot2::element_text(size = 10),
                   panel.grid.major.y= ggplot2::element_line(color= 'grey'),
                   panel.grid.major.x= ggplot2::element_blank(),
                   legend.key = ggplot2::element_rect(fill = "white")) +
    ggplot2::labs(title= 'Oddsshark % Share of Bet Volume & Points', subtitle= unique(dataset[10]), x= 'Oddsshark Site Scrape #', y= '% Share of Bet Volume', color= 'Team') +
    ggplot2::scale_x_discrete(breaks= seq(min(dataset$id), max(dataset$id), 1)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, .1), limits= c(0,1), labels =  scales::label_percent(accuracy= 1)) +
    ggplot2::scale_color_manual(values = colors_list)
  return(z)
}
