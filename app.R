library(shiny)
library(rsconnect)
source("ui.R")
source("server.R")

shinyApp(ui = wanderingui, server = wanderingserver)