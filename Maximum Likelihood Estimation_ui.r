library(shiny)
library(lattice)
library(shinydashboard)
library(plotly)
library(devtools)
library(readxl)
library(tidyverse)
library(survival)
library(DT)
library(survminer)
library(caret)
library(randomForest)
library(dplyr)
library(plot3D)

sidebar<-dashboardSidebar(
    
    sidebarMenu(
        menuItem(text = "Probability distribution",icon = icon("bar-chart-o"),
                 menuSubItem(text = 'Maximum Likelihood Estimation', tabName = 'MLE')
        )
        
    )## End of Sidebar menu
)

body <- dashboardBody(
tabItems(
###############################################################################
# MLE
###############################################################################
tabItem(tabName = 'MLE',
        fluidRow(box(width = 4,
                     #Choose number of observations
                     sliderInput("s",
                               label = "Number of Observations",
                               value = 3,min = 1,max = 250),
                     
                     #Distribution
                     selectInput("type", label = "Distribution",
                                 choices = c("Normal" = "norm","Bernoulli" = "bern", "Exponential"="exp","Poission"="pois", 
                                             "Geometric"="geom"), 
                                 selected = "bern"),
                     conditionalPanel(
                         condition = "input.type == 'norm'",
                         textInput("miu", label = "Mean", value = 0),
                         textInput("sigma", label = "Standard Deviation", value = 1)
                     ),
                     conditionalPanel(
                         condition = "input.type == 'exp'",
                         textInput("lambda", "Rate", value = 10)
                     ),
                     conditionalPanel(
                         condition = "input.type == 'bern'",
                         textInput("p", "Probability", value = 0.5)
                     ),
                     conditionalPanel(
                         condition = "input.type == 'pois'",
                         textInput("lambda2", "Rate", value = 20)
                     ),
                     conditionalPanel(
                         condition = "input.type == 'geom'",
                         textInput("p3", "Probability", value=0.5)
                     ),
                     conditionalPanel(
                         condition = "input.type == 'unif'",
                         textInput("a", "Minimum value", value = 0),
                         textInput("b", "Maximum value", value = 1)
                     )
                     ),
                 
                     box(width = 8, tabsetPanel(type="tabs",
                                                tabPanel(
                                                    "Plot",
                                                    plotOutput("plot_MLE")
                                                ),
                                                tabPanel("Definition")
                                                ))
                    
                 )

        )
) # end of tabItems
) # End of Dashbody

ui <- dashboardPage(header = dashboardHeader(title = "Shiny Stats"),
                    sidebar = sidebar,
                    body = body,
                    skin = "purple"
)
