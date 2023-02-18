#' After using oddsmakr:odds to scrape data and you've compiled it into a dataframe, you can plot a time-series of spreads vs. % share of bet volume for each team by game using this function
#'
#' @param dataset dataframe or tibble containing public consensus share of bets and game line for multiples games for a given week pulled more than once by odds_consensus function from oddsmaker package
#' @param week_no designates the week number of the NFL season that you want to plot
#'
#' @return a set of times-series charts showing percent of public consensus and lines movement for all nfl games during a given week
#'
#' @example None
#'
#' @export
#' @importFrom magrittr %>%
#'
gridiron_all<-function(dataset, week_no){
  dataset<- dataset |> filter(week== week_no)
  p<- function(dataset){
    colors_list<- c('ARI' = '#97233F', 'ATL' = '#A71930', 'BAL' = '#241773', 'BUF' = '#00338D', 'CAR' = '#0085CA',
                    'CHI' = '#00143F', 'CIN' = '#FB4F14', 'CLE' = '#22150C', 'DAL' = '#B0B7BC', 'DEN' = '#002244',
                    'DET' = '#046EB4', 'GB' = '#24423C', 'HOU' = '#00143F', 'IND' = '#003D79', 'JAC' = '#136677',
                    'KC' = '#CA2430', 'LAC' = '#2072BA', 'LAR' = '#002147', 'LV' = '#000000', 'MIA' = '#0091A0',
                    'MIN' = '#4F2E84', 'NE' = '#0A2342', 'NO' = '#A08A58', 'NYG' = '#0B2265', 'NYJ' = '#203731',
                    'PHI' = '#004C54', 'PIT' = '#FFC20E', 'SEA' = '#7AC142','SF' = '#C9243F', 'TB' = '#FF7900',
                    'TEN' = '#4095D1', 'WAS' = '#773141')
    colors_list<- colors_list[unique(dataset$team)]
    z<- ggplot2::ggplot(data = dataset, mapping = ggplot2::aes(x= date_pulled, y= spread_share, color= team)) +
      ggplot2::geom_line() +
      ggplot2::geom_text(ggplot2::aes(label= spread), vjust= -1, size = 1.5, show.legend = F) +
      ggplot2::theme(panel.background = element_rect(fill= 'white'),
                     title = element_text(size= 8),
                     axis.text = element_text(size= 6),
                     axis.text.x = ggplot2::element_text(angle = 90),
                     panel.grid.major.y= element_line(color= 'grey'),
                     panel.grid.major.x= element_blank(),
                     legend.key = element_rect(fill = "white")) +
      ggplot2::labs(title= 'Oddsshark % Share of Bets @ the Spread', subtitle= unique(dataset[10]), x= 'Datetime of Data Pull',
                    y= '% Share of Bet Volume', color= 'Team') +
      ggplot2::scale_x_datetime(date_breaks= '12 hours', date_labels = '%m/%d/%y %H') +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, .1), limits= c(0,1), labels =  scales::label_percent(accuracy= 1)) +
      ggplot2::scale_color_manual(values = colors_list)
    return(z)
  }
  lop<- dataset %>%
    split(f = dataset$game_num) %>%
    purrr::map(p) %>%
    patchwork::wrap_plots()
  return(lop)
}
