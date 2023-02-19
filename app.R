# weekly data
df<- readr::read_csv('master_2022.csv') |>
    dplyr::arrange(week, date_pulled)


# UI creation ---- 
ui<- fluidPage(titlePanel('Weekly Legs'),
               
               shiny::sidebarLayout(
                   shiny::sidebarPanel(
                       width= 3,
                       shiny::h1('Pick a Week'),
                       shiny::selectInput(
                           inputId = "week_select",
                           label = "Select a Week",
                           choices = unique(df$week),
                           selected = 1),
                       
                       shiny::h2('Pick a Game'),
                       shiny::selectInput(
                           inputId = "game_select",
                           label = "Select a Game",
                           choices = NULL)),
                   
                   shiny::mainPanel(
                       shiny::tabsetPanel(
                           shiny::tabPanel(
                               plotly::plotlyOutput('gameplot', width = 800, height = 500),
                               div(style = "height:50px"),
                               plotly::plotlyOutput('ou_plot', width = 800, height = 500),
                               div(style = "height:50px"))
                       )
                   )
               )
)



# build app ----

server<- function(input, output){
    
    week_select <- reactive({
        dplyr::filter(df, week == input$week_select)
    })
    observeEvent(week_select(), {
        choices <- unique(week_select()$matchup)
        updateSelectInput(inputId = "game_select", choices = choices) 
    })
    
    game <- reactive({
        shiny::req(input$game_select)
        dplyr::filter(week_select(), matchup == input$game_select)
    })
    
    output$gameplot<- plotly::renderPlotly({
        req(input$game_select)
        week_select() |>  
            dplyr::filter(matchup == input$game_select) |> 
            oddsmaker::pick_share_plot()
    })
    
    output$ou_plot<- plotly::renderPlotly({
        req(input$game_select)
        week_select() |>  
            dplyr::filter(matchup == input$game_select) |> 
            oddsmaker::ou_share_plot()
    })
    
}

# run the app ----

shiny::shinyApp(ui= ui, server= server)