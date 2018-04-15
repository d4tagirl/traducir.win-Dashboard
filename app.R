library(shiny)
library(shinydashboard)

global_stats <- read.csv(url("https://db.traducir.win/api/queries/19/results.csv?api_key=cweyHWuaIkLYbnOfvZy3GAgcbvQLqE6WHPTZePQd"))

## UI CONFIG

## Header
header <- dashboardHeader(title = "Traducir")

# Sidebar content
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Overview", tabName = "overview", icon = icon("dashboard"))
  )
)

## Body content
body <- dashboardBody(
  fluidRow(
    valueBox(width = 12,
      value = global_stats$Strings[global_stats$Metrics == "Total"], 
      subtitle = "Strings Count", 
      icon = icon("fire"),
      color = "yellow"
    )),
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
    )
  )
)



ui <- dashboardPage(header, sidebar, body, skin = "purple")

server <- function(input, output){}

shinyApp(ui, server)