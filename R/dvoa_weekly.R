#' scrapes the football-outsiders weekly dvoa pages to pull their model's assessment of the previous week's performance
#' the unfortunate fact is that the table required table is not in the same position every week, so you need to visit the page, find the table you want and denote it's number under table number
#' the only other piece you need to provide is the week_number
#'
#' @param week_num number week of NFL play you want to know dvoa of
#' @param table_num the ordinal value of where the table you want lives on the page relative to the other tables
#'
#' @return a tibble denoting the dvoa for the week designated
#'
#' @example dvoa_weekly()
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
dvoa_weekly<-function(week_num, table_num){
  t<- xml2::read_html(glue::glue('https://www.footballoutsiders.com/dvoa-ratings/2020/week-{week_num}-dvoa-ratings')) %>%
    rvest::html_table('#node-76144 > div.content.node__content > div.clearfix.text-formatted.field.field--name-body.field--type-text-with-summary.field--label-hidden.field__item > table.sticky-headers.sortable.stats.sticky-enabled.sticky-table > tbody', header= F) %>%
    purrr::pluck(table_num) %>%
    tibble::as_tibble(.name_repair = 'unique') %>%
    dplyr::slice(-1)
  n<- xml2::read_html(glue::glue('https://www.footballoutsiders.com/dvoa-ratings/2020/week-{week_num}-dvoa-ratings')) %>%
    rvest::html_table('#node-76144 > div.content.node__content > div.clearfix.text-formatted.field.field--name-body.field--type-text-with-summary.field--label-hidden.field__item > table.sticky-headers.sortable.stats.sticky-enabled.sticky-table > tbody', header= F) %>%
    purrr::pluck(table_num) %>%
    tibble::as_tibble(.name_repair = 'unique') %>%
    dplyr::slice(1) %>%
    purrr::set_names()
  t<-t %>%
    purrr::set_names(n) %>%
    janitor::clean_names(case= 'snake') %>%
    # rename(rank= rk) %>%
    tibble::as_tibble() %>%
    dplyr::mutate_all(~stringr::str_replace(., '%', '')) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches('dvoa|rank|week|dave|rk')), as.double) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::matches('dvoa|dave')), ~./100) %>%
    dplyr::mutate(week= week_num) %>%
    dplyr::select(week, dplyr::everything())
}
