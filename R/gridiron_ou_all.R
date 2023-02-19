#' After using oddsmakr::odds_consensus to scrape data and you've compiled it into a dataframe, you can plot a time-series of over-under vs. % share of bet volume for each team by game using this function
#'
#' @param dataset dataframe or tibble containing public consensus share of bets and over-under for multiples games for a given week pulled more than once by odds_consensus function from oddsmaker package
#' @param week_no the week number to visualize
#'
#' @return a patchwork plot showing all spread and spread_share for each game
#'
#' @example None
#'
#' @export
#'
gridiron_ou<-function(dataset, week_no){
  dataset<- dataset |>
    dplyr::mutate(id= as.numeric(id), team= stringr::str_extract(team, '^([A-Z]{3}|[A-Z]{2})')) |>
    filter(week== week_no)
  # matches<-master |>
  #   filter(week== week_no) |>
  #   select(matchup) |>
  #   unique() |>
  #   c()
  p<- function(dataset){
    z<- ggplot2::ggplot(data = dataset, mapping = ggplot2::aes(x= date_pulled, y= ou_share, color= ou)) +
      ggplot2::geom_line() +
      ggplot2::geom_text(ggplot2::aes(label= ou_target), vjust= -1, size = 1.5, show.legend = F) +
      ggplot2::theme(panel.background = element_rect(fill= 'white'),
                     title = element_text(size= 8),
                     axis.text = element_text(size= 6),
                     axis.text.x = ggplot2::element_text(angle = 90, size = 8),
                     panel.grid.major.y= element_line(color= 'grey'),
                     panel.grid.major.x= element_blank(),
                     legend.key = element_rect(fill = "white")) +
      ggplot2::labs(title= 'Oddsshark % Share of Bets @ OU', subtitle= unique(dataset[10]), x= 'Datetime of Data Pull', y= '% Share of Bet Volume', color= 'Over-Under') +
      ggplot2::scale_x_datetime(date_breaks= '12 hours', date_labels = '%m/%d/%y %H') +
      ggplot2::scale_y_continuous(breaks = seq(0, 1, .1), limits= c(0,1), labels =  scales::label_percent(accuracy= 1)) +
      ggplot2::scale_color_manual(values= c('red', 'blue'))
    return(z)
  }
  f<- dataset %>%
    split(f = dataset$matchup) %>%
    purrr::map(p) %>%
    patchwork::wrap_plots()
  return(f)
}
