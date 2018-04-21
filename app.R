library(shiny)
library(shinydashboard)

global_stats <- read.csv(url("https://db.traducir.win/api/queries/19/results.csv?api_key=cweyHWuaIkLYbnOfvZy3GAgcbvQLqE6WHPTZePQd"))

StringSuggestionHistory <- read_csv(url("https://db.traducir.win/api/queries/22/results.csv?api_key=YXohFY6W0Xk287vBwyvC6hD1bGWYHF7pLBb5Mhpw"))

users <- read_csv(url("https://db.traducir.win/api/queries/23/results.csv?api_key=YLxvVeOov6IvQtogh1huuYtvgObkIDLtttqyhFFR"))

suggestion_health <- StringSuggestionHistory %>%
  filter(!UserId %in% c(81867, 81869)) %>% 
  mutate(CreationDate = as.POSIXct(CreationDate)) %>%
  group_by(StringSuggestionId) %>%
  filter(any(HistoryTypeId %in% (3:5))) %>%
  summarise(created = min(CreationDate),
            solved = max(CreationDate),
            delay = as.numeric(difftime(solved, created, units="mins"))) %>%
  mutate(date_solved = format(solved,format='%Y-%m-%d'))

histogram_health <- suggestion_health %>%
  filter(date_solved > Sys.Date() - 9) %>%
  ggplot(aes(x = delay, fill = date_solved)) +
  geom_histogram(show.legend = FALSE) +
  scale_fill_viridis(discrete = TRUE) +
  facet_wrap(~date_solved, nrow = 2) +
  theme_minimal() +
  theme(axis.text.x  = element_text(angle=60, vjust=0.5),
        panel.background = element_rect(fill="#EBF0F5"),
        plot.background = element_rect(fill="#ffffff")) +
  ggtitle("Cantidad de cadenas resueltas según cuántos minutos pasan entre que se crea y se resuelve",
          subtitle = "Resolver: que sea aceptada o rechazada") +
  xlab("Cantidad de minutos") +
  ylab("Cantidad de cadenas")

accepted_creators <- StringSuggestionHistory %>%
  filter(!UserId %in% c(81867, 81869)) %>% 
  group_by(StringSuggestionId) %>%
  filter(any(HistoryTypeId == 1),
         any(HistoryTypeId == 3)) %>% 
  ungroup() %>% 
  filter(HistoryTypeId == 1) %>%
  count(UserId, sort = TRUE) %>%
  left_join(users, by = c("UserId" = "Id")) %>% 
  select(DisplayName, n)

accepted_creators_plot <- accepted_creators %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(DisplayName, n), n, 
             fill = reorder(DisplayName, -n)))+
  geom_col() +
  scale_fill_viridis(discrete = TRUE, guide = FALSE) +
  coord_flip() +
  xlab("") +
  ylab("número de sugerencias creadas") +
  ggtitle("Top 10 usuarios que crean\nsugerencias aprobadas") +
  theme_minimal() +
  theme(panel.background = element_rect(fill="#EBF0F5"),
        plot.background = element_rect(fill="#ffffff"))

rejected_creators <- StringSuggestionHistory %>%
  filter(!UserId %in% c(81867, 81869)) %>% 
  group_by(StringSuggestionId) %>%
  filter(any(HistoryTypeId == 1),
         any(HistoryTypeId %in% c(4,5))) %>% 
  ungroup() %>% 
  filter(HistoryTypeId == 1) %>%
  count(UserId, sort = TRUE) %>%
  left_join(users, by = c("UserId" = "Id")) %>% 
  select(DisplayName, n) 

rejected_creators_plot <- rejected_creators %>% 
  top_n(10) %>% 
  ggplot(aes(reorder(DisplayName, n), n, 
             fill = reorder(DisplayName, -n)))+
  geom_col() +
  scale_fill_viridis(discrete = TRUE, guide = FALSE) +
  coord_flip() +
  xlab("") +
  ylab("número de sugerencias creadas") +
  ggtitle("Top 10 usuarios que crean\nsugerencias rechazadas")+
  theme_minimal() +
  theme(panel.background = element_rect(fill="#EBF0F5"),
        plot.background = element_rect(fill="#ffffff"))

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
  ggplot(aes(reorder(DisplayName, rej_per_acc), 
             rej_per_acc, 
             fill = reorder(DisplayName, -rej_per_acc)))+
  geom_col() +
  scale_fill_viridis(discrete = TRUE, guide = FALSE) +
  coord_flip() +
  xlab("") +
  ylab("rejected for every accepted suggestion") +
  ggtitle("Top 10 users with more %\nrejections for every accepted suggestion")+
  theme_minimal() +
  theme(panel.background = element_rect(fill="#EBF0F5"),
        plot.background = element_rect(fill="#ffffff"))

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
header <- dashboardHeader(title = "Traducir.win")

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
          color = "yellow"
        )
        ),
      fluidRow(
        valueBox(
          value = global_stats$Strings[global_stats$Metrics == "Translated"], 
          subtitle = "Translated", 
          icon = icon("fire"),
          color = "purple"
        ),
        valueBox(
          value = global_stats$Strings[global_stats$Metrics == "Remaining"], 
          subtitle = "Remaining", 
          href = "https://traducir.win/filters?translationStatus=2",
          icon = icon("fire"),
          color = "purple"
        ),
        valueBox(
          value = global_stats$Strings[global_stats$Metrics == "Waiting review"], 
          subtitle = "Waiting review", 
          href = "https://traducir.win/filters?suggestionsStatus=4", 
          icon = icon("fire"),
          color = "purple"
        ),
        valueBox(
          value = global_stats$Strings[global_stats$Metrics == "Waiting approval"], 
          subtitle = "Waiting approval", 
          href = "https://traducir.win/filters?suggestionsStatus=3",
          icon = icon("fire"),
          color = "purple"
        ),
        valueBox(
          value = global_stats$Strings[global_stats$Metrics == "Rejected"], 
          subtitle = "Rejected", 
          icon = icon("fire"),
          color = "purple"
        ),
        valueBox(
          value = length(unique(users_activity$DisplayName)), 
          subtitle = "Active Users", 
          icon = icon("fire"),
          color = "purple"
        )
      )
    ),
    # health plot
    tabItem(
      tabName = "health",
      fluidRow(
        column(
          width = 12,
          plotOutput('histogram_health', width = 1000)
          ))),
    tabItem(
      tabName = "users",
      fluidRow(
        column(
          width = 6,
          plotOutput('quality_creators', width = 1000, height = 300)
        )),
      fluidRow(
        div(style = "height:50px;"),
        column(
          width = 6,
          plotOutput('accepted_creators_plot', width = 1000, height = 300)
        ),
        column(
          width = 6,
          plotOutput('rejected_creators_plot', width = 1000, height = 300)
        ),
        column(
          width = 6,
          plotOutput('rejectors', width = 1000, height = 300)
        ))
      )
  )
)


ui <- dashboardPage(header, sidebar, body, skin = "purple")

server <- function(input, output){
  output$histogram_health <- renderPlot(histogram_health, width = 1100)
  output$quality_creators <- renderPlot(quality_creators, width = 400)
  output$accepted_creators_plot <- renderPlot(accepted_creators_plot, width = 400)
  output$rejected_creators_plot <- renderPlot(rejected_creators_plot, width = 400)
  # output$rejectors <- renderPlot(rejectors, width = 400)
  
}

shinyApp(ui, server)