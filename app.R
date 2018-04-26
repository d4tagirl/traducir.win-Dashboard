library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(viridis)
library(emo)
library(tidyr)
library(plotly)
library(scales)

# load data

global_stats <- read_csv(url("https://db.traducir.win/api/queries/19/results.csv?api_key=cweyHWuaIkLYbnOfvZy3GAgcbvQLqE6WHPTZePQd"))

StringSuggestionHistory <- read_csv(url("https://db.traducir.win/api/queries/22/results.csv?api_key=YXohFY6W0Xk287vBwyvC6hD1bGWYHF7pLBb5Mhpw"))

users <- read_csv(url("https://db.traducir.win/api/queries/23/results.csv?api_key=YLxvVeOov6IvQtogh1huuYtvgObkIDLtttqyhFFR"))

suggestions_by_status <- read_csv(url("https://db.traducir.win/api/queries/25/results.csv?api_key=1ubehnIrkIWfldGs11iE9Aky9731x313GkiDtIWr")) %>% 
  rename(count = X2) %>% 
  full_join(data_frame(StateId = c(1:5))) %>% 
  mutate(count = coalesce(count, 0L))
  

## UI CONFIG

## Header
header <- dashboardHeader(title = paste0("Traducir.win ", emo::ji("unicorn")))

# Sidebar content
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem(text = "Health", tabName = "health", icon = icon("heartbeat")),
    menuItem(text = "Users' Activity", tabName = "users", icon = icon("users")),
    
    tags$footer(HTML('<p align="center"><a href="https://traducir.win" target="_blank">traducir.win</a></p>'),
              style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: black;
              z-index: 1000;")
  ))


## Body content
body <- dashboardBody(
  tabItems(
    
    # Overview
    tabItem(
      tabName = "overview", 
      
      fluidRow(
        valueBox(width = 12,
          value = global_stats$Strings[global_stats$Metrics == "Total"], 
          subtitle = "Strings Count", 
          icon = icon("fire"),
          color = "yellow")),
      
      fluidRow(
        valueBox(
          value = global_stats$Strings[global_stats$Metrics == "Translated"], 
          subtitle = "Strings Translated", 
          icon = icon("trophy"),
          color = "yellow"),
        valueBox(
          value = global_stats$Strings[global_stats$Metrics == "Remaining"], 
          subtitle = "Strings Remaining", 
          href = "https://traducir.win/filters?translationStatus=2",
          icon = icon("coffee"),
          color = "yellow")),
      
      fluidRow(
        valueBox(
          value = suggestions_by_status$count[suggestions_by_status$StateId == 2], 
          subtitle = "Suggestions Waiting review", 
          href = "https://traducir.win/filters?suggestionsStatus=2", 
          icon = icon("hourglass-start"),
          color = "purple"),
        valueBox(
          value = suggestions_by_status$count[suggestions_by_status$StateId == 1], 
          subtitle = "Suggestions Waiting approval", 
          href = "https://traducir.win/filters?suggestionsStatus=3",
          icon = icon("hourglass-half"),
          color = "purple"),
        valueBox(
          value = suggestions_by_status$count[suggestions_by_status$StateId == 4], 
          subtitle = "Suggestions Rejected", 
          href = "https://db.traducir.win/queries/24#table",
          icon = icon("ban"),
          color = "purple")),
      
      fluidRow(
        valueBox(
          value = length(unique(users$DisplayName)), 
          subtitle = "Users", 
          icon = icon("users"),
          color = "yellow"),
        valueBox(
          value = length(unique(users$DisplayName[users$LastSeenDate >= Sys.Date()-5])), 
          subtitle = "Active Users", 
          icon = icon("bolt"),
          color = "yellow")
      )
    ),
    
    
    # health plots
    tabItem(
      selected = TRUE, 
      tabName = "health", 
      navbarPage(
        title = 'Health',
        tabPanel(
          'Delay in solving',
          fluidRow(
            column(width = 4,
                   dateRangeInput('dateRange_delay',
                                  label = 'Date range:',
                                  start = Sys.Date() - 9, 
                                  end = Sys.Date())),
            column(width = 12,
                   plotlyOutput('histogram_health_plot'))),
            fluidRow(
              HTML("<p align='center'>Distribution of the approval/rejection's delay since a suggestion is created,</br>filtering out those that last less than a minute.</p>")
            )
          ),
          
        tabPanel(
          'Daily activity',
          fluidRow(
            column(width = 4,
                   dateRangeInput('dateRange_dailyact',
                                  label = 'Date range:',
                                  start = Sys.Date() - 15, 
                                  end = Sys.Date())),
            column(width = 12,
                   plotlyOutput('evol_suggest_plot'))
        )))),

    
    
    # users activity plots
    tabItem(
      selected = TRUE,
      tabName = "users",
      navbarPage(
        title = "Users' Activity",
        tabPanel(
          'Quality of suggestions',
          fluidRow(
            column(width = 4,
                   dateRangeInput('dateRange_quality',
                                  label = 'Date range:',
                                  start = Sys.Date() - 15, 
                                  end = Sys.Date())),
            column(width = 12,
                   plotlyOutput('quality_creators_plot'))
            )),
        tabPanel(
          'Top Accepted',
          fluidRow(
            column(width = 4,
                   dateRangeInput('dateRange_acc',
                                  label = 'Date range:',
                                  start = Sys.Date() - 15, 
                                  end = Sys.Date())),
            column(width = 12,
                   plotlyOutput('accepted_creators_plot'))
            )),
        tabPanel(
          'Top Rejected',
          fluidRow(
            column(width = 4,
                   dateRangeInput('dateRange_rej',
                                  label = 'Date range:',
                                  start = Sys.Date() - 15, 
                                  end = Sys.Date())),
            column(width = 12,
                   plotlyOutput('rejected_creators_plot'))
          ))
      ))
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "purple")

server <- function(input, output){
  
  ## HEALTH - DELAY

  health_delay <- reactive({
    
    validate(need(
      input$dateRange_delay[1] <= input$dateRange_delay[2],
      message = "Last date can't be prior to first date."))  
    
    validate(need(
      input$dateRange_delay[1] > input$dateRange_delay[2] - 10,
      message = "Maximum difference is 10 days.")) 
  
    dates <- data.frame(dates = seq(input$dateRange_delay[1], 
                                    input$dateRange_delay[2], "day")) %>% 
      mutate(dates = format(dates,format='%Y-%m-%d'))
  
    StringSuggestionHistory %>%
      filter(!UserId %in% c(81867, 81869),
             as.Date(CreationDate, format = "%Y-%m-%d") >= input$dateRange_delay[1],
             as.Date(CreationDate, format = "%Y-%m-%d") <= input$dateRange_delay[2]) %>% 
      group_by(StringSuggestionId) %>%
      filter(any(HistoryTypeId %in% (3:5))) %>%
      summarise(created = min(CreationDate),
                solved = max(CreationDate),
                delay = as.numeric(difftime(solved, created, units="hours"))) %>%
      filter(delay >= 1) %>%
      mutate(date_solved = format(solved, format='%Y-%m-%d')) %>% 
      full_join(dates, by = c("date_solved" = "dates")) 
  })
  
  output$histogram_health_plot <- renderPlotly({
    layout(
      ggplotly(
        health_delay() %>%
          ggplot(aes(x = delay, fill = date_solved)) +
          geom_histogram(show.legend = FALSE, binwidth = 6, boundary = 0) +
          scale_fill_viridis(discrete = TRUE) +
          scale_x_continuous(breaks =  seq(0, max(24, as.numeric(max(health_delay()$delay, na.rm = TRUE))), 6),
                             limits = c(0, max(24, max(health_delay()$delay, na.rm = TRUE)))) +
          facet_wrap(~date_solved, nrow = 2) +
          theme_minimal() +
          theme(axis.text.x  = element_text(angle=60, hjust = 1, vjust=0.5),
                panel.background = element_rect(fill="#ffffff"),
                plot.background = element_rect(fill="#EBF0F5"),
                legend.position = 'none', 
                axis.title.y = element_blank(),
                axis.title.x = element_blank()) +
          ggtitle("Solved suggestions according to the delay\nin solving it (in hours, every 6 hours)<br />\n "), 
        tooltip = c("count")), 
    margin=list(t = 100, b = 60))
  })

  outputOptions(output, "histogram_health_plot", suspendWhenHidden = FALSE)
    
    
  ## HEALTH - ACTIVITY
  
  activity <- reactive({
    
    validate(need(
      input$dateRange_dailyact[1] <= input$dateRange_dailyact[2],
      message = "Last date can't be prior to first date."))    
    
    dates_by_typeId <- data.frame(dates = seq(input$dateRange_dailyact[1], input$dateRange_dailyact[2], "day")) %>% 
      mutate(dates = format(dates,format='%Y-%m-%d')) %>% 
      uncount(5, .id = "HistoryTypeId")
    
    StringSuggestionHistory %>%
      filter(
        !UserId %in% c(81867, 81869),
        as.Date(CreationDate, format = "%Y-%m-%d") >= input$dateRange_dailyact[1],
        as.Date(CreationDate, format = "%Y-%m-%d") <= input$dateRange_dailyact[2]) %>%
      mutate(CreationDate = format(as.POSIXct(CreationDate, tz = "UTC"), format = '%Y-%m-%d')) %>%
      group_by(StringSuggestionId) %>%
      filter(any(HistoryTypeId %in% (1:5))) %>%
      ungroup() %>%
      filter(HistoryTypeId != 7) %>%
      full_join(dates_by_typeId,
                by = c("CreationDate" = "dates", "HistoryTypeId" = "HistoryTypeId")) %>%
      mutate(HistoryTypeId = case_when(
        HistoryTypeId == 1 ~ "created",
        HistoryTypeId %in% c(2, 3) ~ "approved",
        HistoryTypeId %in% c(4, 5) ~ "rejected")) %>%
      group_by(HistoryTypeId, CreationDate) %>%
      summarise(n = n()) %>%
      ungroup() %>%
      mutate(n = ifelse(is.na(n), NA, n))
  })

  output$evol_suggest_plot <- renderPlotly({
    layout(
      ggplotly(
        activity() %>%
          ggplot(
            aes(x = as.POSIXct(CreationDate, tz = "UTC"),
                y = n,
                group = HistoryTypeId,
                color = HistoryTypeId,
                text = paste('count: ', n))) +
          geom_line() +
          geom_point() +
          scale_x_datetime(
            breaks = seq(as.POSIXct(input$dateRange_dailyact[1], tz = "UTC"),
                         as.POSIXct(input$dateRange_dailyact[2], tz = "UTC"), "1 day"),
            labels = date_format("%b-%d", tz = "UTC"),
            limits = c(
            as.POSIXct(input$dateRange_dailyact[1], tz = "UTC"),
            as.POSIXct(input$dateRange_dailyact[2], tz = "UTC"))) +
          scale_color_viridis(discrete = TRUE) +
          theme_minimal() +
          theme(
            axis.text.x  = element_text(angle = 60, vjust = 0.5),
            panel.background = element_rect(fill = "#ffffff"),
            plot.background = element_rect(fill = "#EBF0F5"),
            axis.title.y = element_blank(),
            axis.title.x = element_blank()
          ) +
          ggtitle("Daily Activity"),
        tooltip = c("text")),
      margin = list(b = 70))
  })
  
  outputOptions(output, "evol_suggest_plot", suspendWhenHidden = FALSE)
  
  
  
  
  # USERS' ACTIVITY
  
  # Accepted suggestions
  
  accepted_creators <- reactive({
    
    validate(need(
      input$dateRange_acc[1] <= input$dateRange_acc[2],
      message = "Last date can't be prior to first date."))    
    
  StringSuggestionHistory %>%
    filter(!UserId %in% c(81867, 81869),
           as.Date(CreationDate, format = "%Y-%m-%d") >= input$dateRange_acc[1],
           as.Date(CreationDate, format = "%Y-%m-%d") <= input$dateRange_acc[2]) %>% 
    group_by(StringSuggestionId) %>%
    filter(any(HistoryTypeId == 1),
           any(HistoryTypeId == 3)) %>% 
    ungroup() %>% 
    filter(HistoryTypeId == 1) %>%
    count(UserId, sort = TRUE) %>%
    left_join(users, by = c("UserId" = "Id")) %>% 
    select(DisplayName, n) %>%  
    mutate(DisplayName = ifelse(
      nchar(DisplayName) > 10, 
      paste0(substr(DisplayName, 1, 7), "..."),
      DisplayName))
  })
  
  output$accepted_creators_plot <- renderPlotly({
    layout(
      ggplotly(
        accepted_creators() %>% 
          top_n(10) %>% 
          ggplot(aes(reorder(DisplayName, -n), n, 
                     fill = reorder(DisplayName, -n),
                     text = paste('count: ', n)))+
          geom_col() +
          scale_fill_viridis(discrete = TRUE, guide = FALSE) +
          # coord_flip() +
          xlab("") +
          ylab("Approved suggestions") +
          ggtitle("Top 10 users\nwith approved suggestions") +
          theme_minimal() +
          theme(panel.background = element_rect(fill="#ffffff"),
                plot.background = element_rect(fill="#EBF0F5"),
                legend.position = 'none'), 
        tooltip = c("text")), 
      margin=list(t = 100, b = 90),
      xaxis = list(tickangle = 60))
  })
  
  outputOptions(output, "accepted_creators_plot", suspendWhenHidden = FALSE)
  
  # Rejected suggestions
  
  rejected_creators <- reactive({
    
    validate(need(
      input$dateRange_rej[1] <= input$dateRange_rej[2],
      message = "Last date can't be prior to first date."))    
    
    StringSuggestionHistory %>%
    filter(!UserId %in% c(81867, 81869),
           as.Date(CreationDate, format = "%Y-%m-%d") >= input$dateRange_rej[1],
           as.Date(CreationDate, format = "%Y-%m-%d") <= input$dateRange_rej[2]) %>%
    group_by(StringSuggestionId) %>%
    filter(any(HistoryTypeId == 1),
           any(HistoryTypeId %in% c(4,5))) %>%
    ungroup() %>%
    filter(HistoryTypeId == 1) %>%
    count(UserId, sort = TRUE) %>%
    left_join(users, by = c("UserId" = "Id")) %>%
    select(DisplayName, n) %>%
    mutate(DisplayName = ifelse(
      nchar(DisplayName) > 10,
      paste0(substr(DisplayName, 1, 7), "..."),
      DisplayName))
  })
  
  output$rejected_creators_plot <- renderPlotly({
    
    layout(
      ggplotly(
        rejected_creators() %>% 
          top_n(10) %>%
          ggplot(aes(reorder(DisplayName, -n), n,
                     fill = reorder(DisplayName, -n),
                     text = paste('count: ', n))) +
          geom_col() +
          scale_fill_viridis(discrete = TRUE, guide = FALSE) +
          # coord_flip() +
          xlab("") +
          ylab("Rejected suggestions") +
          ggtitle("Top 10 users\nwith rejected suggestions")+
          theme_minimal() +
          theme(panel.background = element_rect(fill="#ffffff"),
                plot.background = element_rect(fill="#EBF0F5"),
                legend.position = 'none'), 
        tooltip = c("text")), 
      margin=list(t = 100, b = 90),
      xaxis = list(tickangle = 60))
  })
  
  outputOptions(output, "rejected_creators_plot", suspendWhenHidden = FALSE)

  # Ratio rejected/accepted suggestions
  
  quality_ratio <- reactive({
    
    validate(need(
      input$dateRange_quality[1] <= input$dateRange_quality[2],
      message = "Last date can't be prior to first date.")) 
    
    acc <- StringSuggestionHistory %>%
      filter(!UserId %in% c(81867, 81869),
             as.Date(CreationDate, format = "%Y-%m-%d") >= input$dateRange_quality[1],
             as.Date(CreationDate, format = "%Y-%m-%d") <= input$dateRange_quality[2]) %>% 
      group_by(StringSuggestionId) %>%
      filter(any(HistoryTypeId == 1),
             any(HistoryTypeId == 3)) %>% 
      ungroup() %>% 
      filter(HistoryTypeId == 1) %>%
      count(UserId, sort = TRUE) %>%
      left_join(users, by = c("UserId" = "Id")) %>% 
      select(DisplayName, n) %>%  
      mutate(DisplayName = ifelse(
        nchar(DisplayName) > 10, 
        paste0(substr(DisplayName, 1, 7), "..."),
        DisplayName))
    
    rej <- StringSuggestionHistory %>%
      filter(!UserId %in% c(81867, 81869),
             as.Date(CreationDate, format = "%Y-%m-%d") >= input$dateRange_quality[1],
             as.Date(CreationDate, format = "%Y-%m-%d") <= input$dateRange_quality[2]) %>%
      group_by(StringSuggestionId) %>%
      filter(any(HistoryTypeId == 1),
             any(HistoryTypeId %in% c(4,5))) %>%
      ungroup() %>%
      filter(HistoryTypeId == 1) %>%
      count(UserId, sort = TRUE) %>%
      left_join(users, by = c("UserId" = "Id")) %>%
      select(DisplayName, n) %>%
      mutate(DisplayName = ifelse(
        nchar(DisplayName) > 10,
        paste0(substr(DisplayName, 1, 7), "..."),
        DisplayName))
    
    acc %>%
      rename(acc = n) %>%
      left_join(rej) %>%
      rename(rej = n) %>%
      mutate(rej = coalesce(rej, 0L),
             rej_per_acc = ifelse(acc == 0, 0,
                                  rej/acc)) %>%
      filter(acc + rej > 20) %>%
      arrange(desc(rej_per_acc)) %>%
      top_n(10, rej_per_acc)
  
  })
  
  output$quality_creators_plot <- renderPlotly({
    
    validate(need(
      nrow(quality_ratio()) > 0,
      message = "No users with at least 20 solved suggestions in this period. Try expanding the date range.")) 
    
    layout(
      ggplotly(
        quality_ratio() %>%
          ggplot(aes(reorder(DisplayName, -rej_per_acc),
                     rej_per_acc,
                     fill = reorder(DisplayName, -rej_per_acc),
                     text = paste0('rej/acc: ', round(rej_per_acc * 100, 2), '%'))) +
          geom_col() +
          scale_fill_viridis(discrete = TRUE, guide = FALSE) +
          # coord_flip() +
          xlab("") +
          ylab("rejected/accepted suggestions") +
          ggtitle("Top 10 users with more\nrejections for every accepted suggestion")+
          theme_minimal() +
          theme(panel.background = element_rect(fill="#ffffff"),
                plot.background = element_rect(fill="#EBF0F5"),
                legend.position = 'none'), 
        tooltip = c("text")), 
      margin=list(t = 100, b = 90),
      xaxis = list(tickangle = 60))
    
  })
  
  outputOptions(output, "quality_creators_plot", suspendWhenHidden = FALSE)

}

shinyApp(ui, server)