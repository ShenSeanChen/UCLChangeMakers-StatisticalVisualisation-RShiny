library(shiny)
library(shinydashboard)
library(boot)
library(gMOIP)


sidebar<-dashboardSidebar(
  sidebarMenu(
    menuItem(text = "simplex calculator",tabName = "sc_simplex",startExpanded = TRUE,
             menuSubItem("2 variable simplex",tabName = "2sc_simplex"),
             menuSubItem("multi-variable simplex",tabName="multi_sc_simplex"))
  )
)
body<-dashboardBody(
  tabItems(
    tabItem(
      tabName = "2sc_simplex",
      fluidRow(infoBox(title=NULL,value="Key in instructions:",
                       subtitle=HTML(paste("E.g.:for objective function z = x1 + 2 x2: key in '1,2';","    for constraints x1 + 2 x2 <= 3, 2 x1 + 3x2 <= 5: key in '1,2,3,2,3,5'",sep="<br/>")),
                       icon=shiny::icon("pencil"),width=20),
               box(selectInput("maxmin_simplex",label = "Type of objective function", choices = c("max","min"),selected="max"),
                   textInput("objective_simplex","Key in the coefficients of the objective function:",
                             "1,2"),
                   numericInput("small_simplex","Key in the number of <= constraints:",0,min=0),
                   textInput("small_c_simplex","Key in the coefficients of <= constraints","1,2,3"),
                   numericInput("equal_simplex","Key in the number of = constraints:",0, min=0),
                   textInput("equal_c_simplex","Key in the coefficients of = constraints","1,2,3"),
                   numericInput("big_simplex","Key in the number of >= constraints:",0,min=0),
                   textInput("big_c_simplex","Key in the coefficients of >= constraints","1,2,3")
               ),
               box(verbatimTextOutput("result2_simplex")),
               box(plotOutput("plot_simplex"))
      )
    ),
    tabItem(
      tabName = "multi_sc_simplex"
    )
  )
)
ui<-dashboardPage(header = dashboardHeader(title="Simplex algorithm"),
                  sidebar = sidebar,
                  body = body)
