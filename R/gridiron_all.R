#' After using oddsmakr:odds to scrape data and you've compiled it into a dataframe, you can plot a time-series of spreads vs. share for each team by game using this function
#'
#' @param dat dataframe or tibble containing public consensus share of bets and game line for multiples games for a given week pulled more than once by odds_consensus function from oddsmaker package
#'
#' @return a patchwork plot showing all spread and spread_share for each game
#'
#' @example None
#'
#' @export
#' @importFrom magrittr %>%
#'
gridiron_all<-function(dat){
  dat<- dat %>% dplyr::mutate(id= as.numeric(id), team= stringr::str_extract(team, '^([A-Z]{3}|[A-Z]{2})'))
  colors_list<- c('ARI' = '#97233F', 'ATL' = '#A71930', 'BAL' = '#241773', 'BUF' = '#00338D', 'CAR' = '#0085CA',
                  'CHI' = '#00143F', 'CIN' = '#FB4F14', 'CLE' = '#22150C', 'DAL' = '#B0B7BC', 'DEN' = '#002244',
                  'DET' = '#046EB4', 'GB' = '#24423C', 'HOU' = '#00143F', 'IND' = '#003D79', 'JAC' = '#136677',
                  'KC' = '#CA2430', 'LAC' = '#2072BA', 'LAR' = '#002147', 'LV' = '#000000', 'MIA' = '#0091A0',
                  'MIN' = '#4F2E84', 'NE' = '#0A2342', 'NO' = '#A08A58', 'NYG' = '#A08A58', 'NYJ' = '#203731',
                  'PHI' = '#004C54', 'PIT' = '#FFC20E', 'SEA' = '#7AC142','SF' = '#C9243F', 'TB' = '#D40909',
                  'TEN' = '#4095D1', 'WAS' = '#773141')
  colors_list<- colors_list[unique(dat$team)]
  p<- function(d){
    z<- ggplot2::ggplot(data = d, mapping = ggplot2::aes(x= id, y= spread_share, color= team)) +
      ggplot2::geom_line() +
      ggplot2::geom_text(aes(label= spread), vjust= -1, size = 1.5, show.legend = F) +
      ggplot2::theme(panel.background = element_rect(fill= 'white'),
                     title = element_text(size= 8),
                     axis.text = element_text(size= 6),
                     panel.grid.major.y= element_line(color= 'grey'),
                     panel.grid.major.x= element_blank(),
                     legend.key = element_rect(fill = "white")) +
      ggplot2::labs(title= 'Oddsshark % Share of Bet Volume & Points', subtitle= unique(d[3]), x= 'Oddsshark Site Scrape #', y= '% Share of Bet Volume') +
      ggplot2::scale_x_continuous(breaks= seq(min(d$id), max(d$id), 1)) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, .1), limits= c(.05,.95), labels =  scales::label_percent(accuracy= 1)) +
      scale_color_manual(values = colors_list)
    return(z)
  }
  f<- dat %>%
    split(f = dat$game_num) %>%
    purrr::map(p) %>%
    patchwork::wrap_plots()
  return(f)
}
