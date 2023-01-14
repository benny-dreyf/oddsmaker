#' After using oddsmaker::picks_consensus to scrape data and you've compiled it into a dataframe, you can plot a time-series of over-unders vs. share for each game using this function
#'
#' @param dat dataframe or tibble containing public consensus share of bets and game line for a single game for a given week pulled more than once by odds_consensus function from oddsmaker package
#'
#' @return a times-series chart showing public consensus on over-unders and line movement
#'
#' @example None
#'
#' @export
#'
ou_share_plot<- function(dat){
  z<- ggplot2::ggplot(data = dat, mapping = ggplot2::aes(x= date_pulled, y= ou_share,
                                                     color= ou, group= ou)) +
    ggplot2::geom_line() +
    ggplot2::geom_text(ggplot2::aes(label= ou_target),  size= 3.5, show.legend = F) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill= 'white'),
                   title = ggplot2::element_text(size= 11, face = 'bold'),
                   axis.text = ggplot2::element_text(size= 10, colour = 'black'),
                   axis.text.x = ggplot2::element_text(angle = 90, size = 8),
                   axis.title.x = ggplot2::element_text(size = 10, vjust = -1.5),
                   panel.grid.major.y= ggplot2::element_line(color= 'grey'),
                   panel.grid.major.x= ggplot2::element_blank(),
                   legend.key = ggplot2::element_rect(fill = "white")) +
    ggplot2::labs(title= 'Oddsshark % Share of Bets @ OU', x= 'Oddsshark Site Timestamp', y= '% Share of Bet Volume', color= 'Over-Under') +
    ggplot2::scale_x_datetime(date_breaks= '12 hours', date_labels = '%m/%d/%y %H') +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, .1), limits= c(0,1), labels =  scales::label_percent(accuracy= 1)) +
    ggplot2::scale_color_manual(values= c('red', 'blue'))
  return(z)
}
