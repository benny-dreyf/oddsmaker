#' After using oddsmakr::odds_consensus to scrape data and you've compiled it into a dataframe, you can plot a time-series of over-under vs. % share of bet volume for each team by game using this function
#'
#' @param dat dataframe or tibble containing public consensus share of bets and over-under for multiples games for a given week pulled more than once by odds_consensus function from oddsmaker package
#'
#' @return a patchwork plot showing all spread and spread_share for each game
#'
#' @example None
#'
#' @export
#' @importFrom magrittr %>%
#'
gridiron_ou<-function(dat){
  dat<- dat %>% dplyr::mutate(id= as.numeric(id), team= stringr::str_extract(team, '^([A-Z]{3}|[A-Z]{2})'))
  # ou_colors<- c('Over' = 'blue', 'Under' = 'red')
  # ou_colors<- ou_colors[unique(dat$ou)]
  p<- function(d){
    z<- ggplot2::ggplot(data = d, mapping = ggplot2::aes(x= id, y= ou_share, color= ou)) +
      ggplot2::geom_line() +
      ggplot2::geom_text(aes(label= ou_target), vjust= -1, size = 1.5, show.legend = F) +
      ggplot2::theme(panel.background = element_rect(fill= 'white'),
                     title = element_text(size= 8),
                     axis.text = element_text(size= 6),
                     panel.grid.major.y= element_line(color= 'grey'),
                     panel.grid.major.x= element_blank(),
                     legend.key = element_rect(fill = "white")) +
      ggplot2::labs(title= 'Oddsshark % Share of Bet Volume & Over/Under', subtitle= unique(d[5]), x= 'Oddsshark Site Scrape #', y= '% Share of Bet Volume') +
      ggplot2::scale_x_continuous(breaks= seq(min(d$id), max(d$id), 1)) +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, .1), limits= c(0,1), labels =  scales::label_percent(accuracy= 1))
    return(z)
  }
  f<- dat %>%
    split(f = dat$game_num) %>%
    purrr::map(p) %>%
    patchwork::wrap_plots()
  return(f)
}
