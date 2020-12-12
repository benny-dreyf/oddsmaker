#' After using oddsmakr:odds to scrape data and you've compiled it into a dataframe, you can plot a time-series of spreads vs. share for each team by game using this function
#'
#' @param d dataframe or tibble you want to pass to the function
#'
#' @return a patchwork plot showing all spread and spread_share for each game
#'
#' @example None
#'
#' @export
#' @importFrom magrittr %>%
#' 
gridiron<- function(d){
  d<- d %>% dplyr::mutate(id= as.numeric(id), team= stringr::str_extract(team, '^([A-Z]{3}|[A-Z]{2})'))  
  p<- function(dat){
    z<- ggplot2::ggplot(data = dat, mapping = ggplot2::aes(x= id, y= spread_share, color= team)) + 
      ggplot2::geom_line() + 
      ggplot2::geom_text(aes(label= spread), vjust= -1, size = 1.5, show.legend = F) +
      ggplot2::theme(panel.background = element_rect(fill= 'white'), 
            title = element_text(size= 8),
            axis.text = element_text(size= 4),
            panel.grid.major.y= element_line(color= 'grey'),
            panel.grid.major.x= element_blank(),
            legend.key = element_rect(fill = "white")) +
      ggplot2::labs(title= 'Oddsshark % Share of Bet Volume', subtitle= unique(dat[3]), x= 'Odds Refresh #', y= '% Share of Bet Volume') +
      ggplot2::scale_x_continuous(breaks= seq(min(dat$id), max(dat$id), 1)) +
      ggplot2::ylim(0.05, 0.95)
    return(z)
  }
  f<- d %>% 
    split(f = d$game_num) %>% 
    purrr::map(p) %>%
    patchwork::wrap_plots()
  return(f)
}

