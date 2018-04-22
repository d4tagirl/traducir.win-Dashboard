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

# load data

global_stats <- read_csv(url("https://db.traducir.win/api/queries/19/results.csv?api_key=cweyHWuaIkLYbnOfvZy3GAgcbvQLqE6WHPTZePQd"))

StringSuggestionHistory <- read_csv(url("https://db.traducir.win/api/queries/22/results.csv?api_key=YXohFY6W0Xk287vBwyvC6hD1bGWYHF7pLBb5Mhpw"))

users <- read_csv(url("https://db.traducir.win/api/queries/23/results.csv?api_key=YLxvVeOov6IvQtogh1huuYtvgObkIDLtttqyhFFR"))

#########
# health
#########

dates <- data.frame(dates = seq(Sys.Date() - 9, Sys.Date(), "day")) %>% 
  mutate(dates = format(dates,format='%Y-%m-%d'))
  
suggestion_health <- StringSuggestionHistory %>%
  filter(!UserId %in% c(81867, 81869)) %>% 
  mutate(CreationDate = as.POSIXct(CreationDate)) %>%
  group_by(StringSuggestionId) %>%
  filter(any(HistoryTypeId %in% (3:5))) %>%
  summarise(created = min(CreationDate),
            solved = max(CreationDate),
            delay = as.numeric(difftime(solved, created, units="mins"))) %>%
  mutate(date_solved = format(solved,format='%Y-%m-%d')) %>% 
  full_join(dates, by = c("date_solved" = "dates")) %>% 
  mutate(delay = ifelse(is.na(delay), 0, delay))
          
histogram_health <- suggestion_health %>%
  filter(date_solved > Sys.Date() - 10) %>%
  ggplot(aes(x = delay, fill = date_solved)) +
  geom_histogram(show.legend = FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  facet_wrap(~date_solved, nrow = 2) +
  theme_minimal() +
  theme(axis.text.x  = element_text(angle=60, vjust=0.5),
        panel.background = element_rect(fill="#ffffff"),
        plot.background = element_rect(fill="#EBF0F5"),
        legend.position = 'none', 
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  ggtitle("Solved suggestions according to the delay\nin solving it (in minutes)<br />\n ") 

dates_by_typeId <- data.frame(dates = seq(Sys.Date() - 9, Sys.Date(), "day")) %>% 
  mutate(dates = format(dates,format='%Y-%m-%d')) %>% 
  uncount(5, .id = "HistoryTypeId")

evol_suggest <- StringSuggestionHistory %>%
  filter(!UserId %in% c(81867, 81869),
         as.Date(CreationDate, format = "%Y-%m-%d") >= Sys.Date() - 9) %>% 
  mutate(CreationDate = format(as.POSIXct(CreationDate), format='%Y-%m-%d')) %>%
  group_by(StringSuggestionId) %>%
  filter(any(HistoryTypeId %in% (1:5))) %>%
  ungroup() %>% 
  filter(HistoryTypeId != 7) %>% 
  full_join(dates_by_typeId, by = c("CreationDate" = "dates", "HistoryTypeId" = "HistoryTypeId")) %>% 
  mutate(HistoryTypeId = case_when(
    HistoryTypeId == 1 ~ "created",
    HistoryTypeId %in% c(2, 3) ~ "approved",
    HistoryTypeId %in% c(4, 5) ~ "rejected")) %>% 
  group_by(HistoryTypeId, CreationDate) %>% 
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(n = ifelse(is.na(n), 0, n)
         # ,
         # CreationDate = as.Date(CreationDate)
         )

evol_suggest_plot <- evol_suggest %>% 
  ggplot(aes(x = CreationDate, y = n, 
             group = HistoryTypeId, 
             color = HistoryTypeId,
             text = paste('count: ', n))) +
  geom_line() +
  geom_point() +
  scale_color_viridis(discrete = TRUE) + 
  theme_minimal() +
  theme(axis.text.x  = element_text(angle=60, vjust=0.5),
        panel.background = element_rect(fill="#ffffff"),
        plot.background = element_rect(fill="#EBF0F5"),
        # legend.position = 'none', 
        axis.title.y = element_blank(),
        axis.title.x = element_blank()) +
  ggtitle("Daily Activity")

####################
# Users' Activity
###################

accepted_creators <- StringSuggestionHistory %>%
  filter(!UserId %in% c(81867, 81869)) %>% 
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

accepted_creators_plot <- accepted_creators %>% 
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
        legend.position = 'none')

rejected_creators <- StringSuggestionHistory %>%
  filter(!UserId %in% c(81867, 81869)) %>% 
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

rejected_creators_plot <- rejected_creators %>% 
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
        legend.position = 'none')

# ratio approv/rej

quality_creators <- accepted_creators %>% 
  rename(acc = n) %>% 
  left_join(rejected_creators) %>% 
  rename(rej = n) %>% 
  mutate(rej = coalesce(rej, 0L),
         rej_per_acc = ifelse(acc == 0, 0,
                            rej/acc)) %>% 
  filter(acc + rej > 20) %>%
  arrange(desc(rej_per_acc)) %>% 
  top_n(10, rej_per_acc) %>% 
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
        legend.position = 'none')

# trusted_approvers <- users_activity %>%
#   filter(HistoryTypeId == 2) %>%
#   select(-HistoryTypeId) %>% 
#   top_n(10) %>% 
#   ggplot(aes(reorder(DisplayName, n), n, fill = reorder(DisplayName, -n)))+
#   geom_col() +
#   scale_fill_viridis(discrete = TRUE, guide = FALSE) +
#   coord_flip() +
#   xlab("") +
#   ylab("número de sugerencias aprobadas") +
#   ggtitle("Top 10 trusted usuarios que aprueban sugerencias")
# 
# reviewers_approvers <- users_activity %>%
#   filter(HistoryTypeId == 3) %>%
#   select(-HistoryTypeId) %>% 
#   top_n(10) %>% 
#   ggplot(aes(reorder(DisplayName, n), n, fill = reorder(DisplayName, -n)))+
#   geom_col() +
#   scale_fill_viridis(discrete = TRUE, guide = FALSE) +
#   coord_flip() +
#   xlab("") +
#   ylab("número de sugerencias aprobadas") +
#   ggtitle("Top 10 reviewers que aprueban sugerencias")
# 
# rejectors <- users_activity %>%
#   filter(HistoryTypeId %in% c(4, 5)) %>%
#   select(-HistoryTypeId) %>% 
#   top_n(10) %>% 
#   ggplot(aes(reorder(DisplayName, n), n, fill = reorder(DisplayName, -n)))+
#   geom_col() +
#   scale_fill_viridis(discrete = TRUE, guide = FALSE) +
#   coord_flip() +
#   xlab("") +
#   ylab("número de sugerencias rechazadas") +
#   ggtitle("Top 10 usuarios que rechazan sugerencias")

## UI CONFIG

## Header
header <- dashboardHeader(title = paste0("Traducir.win ", emo::ji("unicorn")))

# Sidebar content
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Overview", tabName = "overview", icon = icon("dashboard")),
    menuItem(text = "Health", tabName = "health", icon = icon("heartbeat")),
    menuItem(text = "Users' Activity", tabName = "users", icon = icon("users"))
  )
)

## Body content
body <- dashboardBody(
  tabItems(
    
    # Front Page
    tabItem(
      tabName = "overview", 
      
      fluidRow(
        valueBox(width = 12,
          value = global_stats$Strings[global_stats$Metrics == "Total"], 
          subtitle = "Strings Count", 
          icon = icon("fire"),
          color = "yellow")
        ),
      
      fluidRow(
        valueBox(
          value = global_stats$Strings[global_stats$Metrics == "Translated"], 
          subtitle = "Translated", 
          icon = icon("fire"),
          color = "purple"),
        valueBox(
          value = global_stats$Strings[global_stats$Metrics == "Remaining"], 
          subtitle = "Remaining", 
          href = "https://traducir.win/filters?translationStatus=2",
          icon = icon("fire"),
          color = "purple"),
        valueBox(
          value = global_stats$Strings[global_stats$Metrics == "Waiting review"], 
          subtitle = "Waiting review", 
          href = "https://traducir.win/filters?suggestionsStatus=4", 
          icon = icon("fire"),
          color = "purple"),
        valueBox(
          value = global_stats$Strings[global_stats$Metrics == "Waiting approval"], 
          subtitle = "Waiting approval", 
          href = "https://traducir.win/filters?suggestionsStatus=3",
          icon = icon("fire"),
          color = "purple"),
        valueBox(
          value = global_stats$Strings[global_stats$Metrics == "Rejected"], 
          subtitle = "Rejected", 
          icon = icon("fire"),
          color = "purple"),
        valueBox(
          value = length(unique(users$DisplayName)), 
          subtitle = "Active Users", 
          icon = icon("fire"),
          color = "purple"
        )
      )
    ),
    

    # # health plot
    # tabItem(
    #   tabName = "health",
    #   mainPanel(
    #     h3(" Health"),
    #     width = 12,
    #     title = 'Health',
    #     plotlyOutput('plotly_histogram_health'),
    #     plotlyOutput('plotly_evol_suggest')
    #   )
    # ),
    
    # health plots
    tabItem(
      selected = TRUE, 
      tabName = "health", 
      navbarPage(
        title = 'Health',
        tabPanel(
          'Delay in solving',
          plotlyOutput('plotly_histogram_health')
        ),
        tabPanel(
          'Daily activity',
          plotlyOutput('plotly_evol_suggest')
        )
        )),

    # users activity plots
    tabItem(
      selected = TRUE,
      tabName = "users",
      navbarPage(
        title = "Users' Activity",
        tabPanel(
          'Quality of suggestions',
          plotlyOutput('plotly_quality_creators')
        ),
        tabPanel(
          'Top Accepted',
          plotlyOutput('plotly_accepted_creators')
        ),
        tabPanel(
          'Top Rejected',
          plotlyOutput('plotly_rejected_creators')
        )
      ))
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "purple")

server <- function(input, output){
  
  output$plotly_histogram_health <- renderPlotly(
    layout(ggplotly(histogram_health, tooltip = c("count", "delay")), 
           margin=list(t = 100, b = 60)))
  outputOptions(output, "plotly_histogram_health", suspendWhenHidden = FALSE)
  
  output$plotly_evol_suggest <- renderPlotly(
      layout(ggplotly(evol_suggest_plot, tooltip = c("text")), 
           margin=list(b = 70)))
  outputOptions(output, "plotly_evol_suggest", suspendWhenHidden = FALSE)
  
  
  output$plotly_quality_creators <- renderPlotly(
    layout(ggplotly(quality_creators, tooltip = c("text")), 
           margin=list(t = 100, b = 90),
           xaxis = list(tickangle = 60)))
  outputOptions(output, "plotly_quality_creators", suspendWhenHidden = FALSE)
  
  output$plotly_accepted_creators <- renderPlotly(
    layout(ggplotly(accepted_creators_plot, tooltip = c("text")), 
           margin=list(t = 100, b = 90),
           xaxis = list(tickangle = 60)))
  outputOptions(output, "plotly_accepted_creators", suspendWhenHidden = FALSE)
  
  output$plotly_rejected_creators <- renderPlotly(
    layout(ggplotly(rejected_creators_plot, tooltip = c("text")), 
           margin=list(t = 100, b = 90),
           xaxis = list(tickangle = 60)))
  outputOptions(output, "plotly_rejected_creators", suspendWhenHidden = FALSE)
  
}

shinyApp(ui, server)