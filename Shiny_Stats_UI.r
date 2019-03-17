library(shiny)
library(ggplot2)
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
library(party)
library(mgcv)
library(voxel)
library(gridExtra)
library(boot)
library(gMOIP)
library(plot3D)
library(shinyhelper)

# Data Preparation for 2D GLM plot
myData <- USArrests
myData$DeathThreat[myData$Murder < 10] <- "Low"
myData$DeathThreat[myData$Murder >= 10] <- "High"
myData$rapeThreat[myData$Rape < 10] <- "Low"
myData$rapeThreat[myData$Rape >= 10] <- "High"


#Side bar
sidebar<-dashboardSidebar(
  sidebarMenu(
    # Search Engine --- to be improved
    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search..."),
    # A brief summary on Shiny Stats; links to important/useful websites
    menuItem(text = "Synopsis", tabName = 'Synopsis', icon = icon("dashboard"),
             menuSubItem(text = "GitHub", icon = icon("file-code-0"),
                         href = "https://github.com/ShenSeanChen/UCLChangeMakers-StatisticalVisualisation-RShiny"),
             menuSubItem(text = "ShinyDashboard", icon = icon("file-code-0"),
                         href = "https://rstudio.github.io/shinydashboard/index.html")),
    
    # Visualisation on Statistics 
    menuItem(text = 'Probability and Statistics', icon = icon("bar-chart-o"),
             menuSubItem(text = 'Central Limit Theorem', tabName = 'CLT'),
             menuSubItem(text = 'Quantile Plots and Skewness', tabName = 'QQ'),
             menuItem(text = 'Maximum Likelihood Estimation',icon = icon("chart-line"),
                      menuSubItem(text='Concepts',tabName='MLE_concept',icon=icon('lightbulb')),
                      menuSubItem(text = 'Plot and calculation',tabName = 'MLE',icon = icon('dashboard'))
             )
             ),
    
    menuItem(text = 'Normal Linear Regression', icon = icon("chart-line"),
             menuSubItem(text='DataTable, LM and ANOVA', tabName='MLR', icon = icon("table"))),
    
    menuItem(text = "Generalised Linear Model",  icon = icon("chart-line"),
             menuSubItem(text='GLM 2D Visualisation and Analytics',tabName='glm'),
             menuSubItem(text='High-Dimensional GLM',tabName='multi-G')),
    
    ######################## GAMs ####################################
    menuItem(text = 'Generalised Additive Model', icon = icon("chart-line"),
             menuSubItem(text='GAM 2D Visualisation and Analytics', tabName='GAM')),
    
    ####################### Non-parametric Regresssion ######################
    menuItem(text = 'Non-parametric Regression', icon = icon("chart-line"), 
             menuSubItem('Survival Analysis', tabName = "Surv", icon = icon("bar-chart-o"))),
    
    ###################### Stochastic Proces ##############################
    menuItem(text = 'Stochastic Process', icon = icon("random"),
             menuSubItem(text = "Brownian Motion", tabName = 'brownian'),
             menuSubItem(text = "Geometric Brownian Motion", tabName = "GBM")),

    ###################### Machine Learning ##########################
    menuItem(text = 'Machine Learning', icon = icon("chart-line"),
              menuItem(text = 'RandomForest', icon = icon("chart-line"),
                       menuSubItem(text='Concepts',tabName='concept',icon=icon('lightbulb')),
                       menuSubItem(text='Sketch of Random Forest',tabName='sketch',icon=icon('dashboard')),
                       menuSubItem(text='Playground', tabName='data_rf',icon=icon('table')))
             ),
    
    ###################### Optimisation ################################
    menuItem(text = "simplex calculator",tabName = "sc_simplex",startExpanded = TRUE,icon=icon("chart-line"),
             menuSubItem("2 variable simplex",tabName = "2sc_simplex"),
             menuSubItem("multi-variable simplex",tabName="multi_sc_simplex"))
  )# end of sidebar menu
)


#Body
body <- dashboardBody(

tabItems(
  
###############################################################################
# Sysnopsis
###############################################################################
tabItem(tabName = "Synopsis",
          h2("UCLChangeMakers-StatisticalVisualisation-RShiny")),
    
###############################################################################
# Probability Distribution and Central Limit Theorem 
###############################################################################
tabItem(tabName = "CLT",
       fluidRow(box(width=4,
                    #Pupulation size
                    sliderInput("pop_size",
                                label = "Population Size",
                                value = 2000, min = 1000, max = 100000),
                    
                    #Enter sample size
                    textInput("smpl_size",
                              label = "Enter Sample Size",
                              value = 50),
                    
                    #Distribution
                    selectInput("dist", label = "Distribution ",
                                choices = c("Normal" = "norm", "Uniform" = "unif", "Exponential"="exp",
                                            "Binomial"="binom", "Negative binomial"="nbinom", "Poission"="pois", 
                                            "Geometric"="geom", "Hypergeometric"="hyper", "Chi-squared"="chisq", 
                                            "Student's t"="t", "Beta"="beta", "Gamma"="gamma"), 
                                selected = "norm"),
                    conditionalPanel(
                      condition = "input.dist == 'norm'",
                      textInput("miu", label = "Mean", value = 0),
                      textInput("sigma", label = "Standard Deviation", value = 1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'unif'",
                      textInput("a", "Minimum value", value = 0),
                      textInput("b", "Maximum value", value = 1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'exp'",
                      textInput("lambda", "Rate", value = 1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'binom'",
                      textInput("p", "Probability", value = 0.5),
                      textInput("n", "Number of Trials", value = 1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'nbinom'",
                      textInput("p2", "Probability", value = 0.5),
                      textInput("r", "Number of Failures", value = 1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'pois'",
                      textInput("lambda2", "Rate", value = 1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'geom'",
                      textInput("p3", "Probability", value=0.5)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'hyper'",
                      textInput("M", "Number of Success States in Population", value=10),
                      textInput("N", "Population Size",value=20),
                      textInput("K", "Number of Draws", value=5)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'chisq'",
                      textInput("df", "Degrees of Freedom", value=1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 't'",
                      textInput("df2", "Degrees of Freedom", value=1)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'beta'",
                      textInput("Alpha", "First Shape", value=0.5),
                      textInput("Beta", "Second Shape", value=0.5)
                    ),
                    conditionalPanel(
                      condition = "input.dist == 'gamma'",
                      textInput("k", "Shape", value=0.5),
                      textInput("Theta", "Scale", value=0.5)
                    ),
                    
                    #Sampling iteration
                    sliderInput("smpl_iterate",
                                label = "Sampling Iteration",
                                value = 5, min = 1, max = 1000)),       
                
                box(width=8,tabsetPanel(type="tabs",
                                        #Plot tab
                                        tabPanel("Plot",
                                                 plotOutput("plot_pop"), #plotting histogram and density plot for population
                                                 plotOutput("plot_smpl_mean")), #plotting histogram and density plot for sample
                                        #Data tab
                                        tabPanel("Data_CLT", 
                                                 h4("Population"), #heading for the population summary and descriptive statistics
                                                 verbatimTextOutput("pop_summary"), #rendering population summary statistics
                                                 verbatimTextOutput("pop_structure"), 
                                                 h4("Sample"), #heading for the sample mean summary and descriptive statistics
                                                 verbatimTextOutput("smpl_mean_summary"), #rendering sample summary statistics
                                                 verbatimTextOutput("smpl_mean_structure"))
                ))
       )),

###############################################################################
# Quantile Plots and Skewness
###############################################################################
tabItem(tabName = "QQ",
        # header
        fluidRow(
          box(h4("Quantile Plots and Skewness"), 
             #file Input
             selectInput("type_QQ",
                         "What distribution would you like?",
                         choices = c("Light-tailed", 
                                     "Heavy-tailed", 
                                     "Normal", 
                                     "Negatively skewed", 
                                     "Positively skewed")),
             actionButton("update",
                          "Produce new sample!", 
                          width = "87.5%")),
             #  p("This applet aims to make QQ plots more intuitive by comparing 
             # sample quantiles against the corresponding normal quantiles in various ways.",
             #  align = "center")) 
          fluidRow(
            box(plotOutput("QQplots")))
        )),  

tabItem(tabName = 'MLE_concept',solidHeader = TRUE,
        h2("Maximum Likelihood Estimation"),
        fluidRow(
          box(title='Definition',width=8,"In statistics, maximum likelihood estimation (MLE) is a method of estimating the parameters of 
              a statistical model,given observations.The method obtains the parameter estimates by finding the parameter
              values that maximize the likelihood function.The estimates are called maximum likelihood estimates, 
              which is also abbreviated as MLE."),
          box(title='Step1',width=12,solidHeader = TRUE,
              status='primary',fluidRow(column(width=12,tags$img(src='formula1.png', 
                                                                 height='100px',width='300px')))),
          box(title="Step2",width=12,solidHeader = TRUE,
              status = 'primary',fluidRow(column(width=12,tags$img(src='formula2.png',
                                                                   height='100px',width='300px')))),
          box(title="Step3",width=12,solidHeader = TRUE,
              status = 'primary',fluidRow(column(width=12,tags$img(src='formula3.png',
                                                                   height='100px',width='300px')))),
          box(title="Step4",width=12,solidHeader = TRUE,
              status = 'primary',fluidRow(column(width=12,tags$img(src='formula4.png',
                                                                   height='100px',width='300px'))))
          
          )),
###############################################################################
# Maximum likelihood estimation-plot
###############################################################################
tabItem(tabName = 'MLE',
        fluidRow(box(width = 8,
                     #Choose number of observations
                     sliderInput("MLE_s",
                                 label = "Number of Observations",
                                 value = 3,min = 1,max = 250),
                     
                     #Distribution
                     selectInput("MLE_type", label = "Distribution",
                                 choices = c("Normal" = "norm","Bernoulli" = "bern", "Exponential"="exp","Poission"="pois", 
                                             "Geometric"="geom"), 
                                 selected = "bern"),
                     conditionalPanel(
                       condition = "input.MLE_type == 'norm'",
                       textInput("MLE_miu", label = "Mean", value = 0),
                       textInput("MLE_sigma", label = "Standard Deviation", value = 10)
                     ),
                     conditionalPanel(
                       condition = "input.MLE_type == 'exp'",
                       textInput("MLE_lambda", "Rate", value = 10)
                     ),
                     conditionalPanel(
                       condition = "input.MLE_type == 'bern'",
                       textInput("MLE_p", "Probability", value = 0.5)
                     ),
                     conditionalPanel(
                       condition = "input.MLE_type == 'pois'",
                       textInput("MLE_lambda2", "Rate", value = 20)
                     ),
                     conditionalPanel(
                       condition = "input.MLE_type == 'geom'",
                       textInput("MLE_p3", "Probability", value=0.5)
                     )
        ),
        
        box(width = 12, tabsetPanel(type="tabs",
                                    tabPanel(
                                      "Plot",
                                      plotOutput("plot_MLE")
                                    )
        ))
        
        )
        
),

###############################################################################
# Import data and Run MLR with CheckboxGroup
###############################################################################
tabItem(tabName = "MLR",
       # header
       fluidRow(box(h4("General linear model"), 
                #file Input
                fileInput("file_MLR","choose excel file"),
                radioButtons("fileType_Input_MLR",
                             label = h4("Choose File type"),
                             choices = list(".csv/txt" = 1, ".xlsx" = 2),
                             selected = 1,
                             inline = TRUE),
                selectInput("selectInput_MLR",
                            "Checkbox group input for response variable:",
                            c("label 1" = "option1",
                              "label 2" = "option2")),
                checkboxInput("select_all_var_MLR", "Select all variables"),
                checkboxGroupInput("inCheckboxGroup_MLR",
                                   "Checkbox group input for explanatory variable:",
                                   c("label 1" = "option1",
                                     "label 2" = "option2")),
                uiOutput("choose_columns_MLR")),
                
              box(tabsetPanel(type="tabs",
                            tabPanel("Data_MLR",div(style = 'overflow-x:scroll', dataTableOutput("contents_MLR")) ),
                            tabPanel("Model Summary",verbatimTextOutput("text1")),
                            tabPanel("Diagnostic Plots", plotOutput("diag_MLR")),
                            tabPanel("Anova",verbatimTextOutput("text2")))),
              # box(
              #   h4("Columns selected"),
              #   # checkboxGroupInput("")
              #   dataTableOutput(outputId = 'datatable_Transform')
              # ),
              
              box(
                h4("Visualisation on the selected features"),
                plotOutput(outputId = 'visualisation_features')
              )
        )),           
         
###############################################################################
# Build glm models and predict values based on that
###############################################################################
tabItem(
  tabName = "glm",
  fluidRow(box(selectInput("x", label = "Choose an explanatory variable (x)",
                           choices = colnames(myData), selected = colnames(myData[1])),
               selectInput("y", label = "Choose a response varialbe (y)",
                           choices = colnames(myData), selected = colnames(myData[2])),
               selectInput("family",
                           label = "Choose a family to run glm",
                           choices = c('poisson', 
                                       "gaussian",
                                       "Gamma", 
                                       "inverse.gaussian",
                                       "quasi",
                                       "quasibinomial",
                                       "quasipoisson",
                                       "binomial"
                           ),
                           selected = "gaussian"),
               selectInput("link",
                           label = "Change link function",
                           choices = c("Canonical Link",
                                       "()",
                                       "(link='log')",
                                       "(link='probit')",
                                       "(link='cauchit')", 
                                       "(link='cloglog')",
                                       "(link='identity')",
                                       "(link='logit')",
                                       "(link='sqrt')",
                                       "(link='1/mu^2')",
                                       "(link='inverse')"),
                           selected = "Canonical Link"),
               
               selectInput("t_x",
                           label = "Choose a certain transformation for the explanatory variable",
                           choices = c("None", 
                                       "log",
                                       "sqrt",
                                       "square",
                                       "third power"
                           ), 
                           selected = "None")),
           box(verbatimTextOutput("summary_glm1"))),
  
  fluidRow(box(plotOutput('plot_glm1')),
           box(plotOutput('diagnostic_plot1')))
           

  ),

###############################################################################
# Build higher dimensional glm models and predict values based on that 
###############################################################################
tabItem(tabName = 'multi-G',
        fluidRow(box(selectInput("family2",
                                 label = "Choose a family to run glm",
                                 choices = c('poisson', 
                                             "gaussian",
                                             "Gamma", 
                                             "inverse.gaussian",
                                             "quasi",
                                             "quasibinomial",
                                             "quasipoisson",
                                             "binomial"
                                 ),
                                 selected = "poisson"),
                     selectInput("link2",
                                 label = "Change link function",
                                 choices = c("Canonical Link",
                                             "()",
                                             "(link='log')",
                                             "(link='probit')",
                                             "(link='cauchit')", 
                                             "(link='cloglog')",
                                             "(link='identity')",
                                             "(link='logit')",
                                             "(link='sqrt')",
                                             "(link='1/mu^2')",
                                             "(link='inverse')"),
                                 selected = "Canonical Link")),
                 box(plotOutput("diagnostic_plot2"))),
        verbatimTextOutput("summary_glm2")
),


###############################################################################
# GAM 
###############################################################################
tabItem(tabName = "GAM",
        # header
        fluidRow(box(h4("Generalised Additive model"), 
                     #file Input
                     fileInput("file_GAM","choose excel file"),
                     radioButtons("fileType_Input_GAM",
                                  label = h4("Choose File type"),
                                  choices = list(".csv/txt" = 1, ".xlsx" = 2),
                                  selected = 1,
                                  inline = TRUE),
                     selectInput("selectInput_GAM",
                                 "Checkbox group input for response variable:",
                                 c("label 1" = "option1",
                                   "label 2" = "option2")),
                     checkboxGroupInput("inCheckboxGroup_GAM",
                                        "Checkbox group input for explanatory variable:",
                                        c("label 1" = "option1",
                                          "label 2" = "option2")),
                     uiOutput("choose_columns_GAM"),
                     selectInput("family_GAM",
                                 label = "Choose a family to run glm",
                                 choices = c('poisson', 
                                             "gaussian",
                                             "Gamma", 
                                             "inverse.gaussian",
                                             "quasi",
                                             "quasibinomial",
                                             "quasipoisson",
                                             "binomial"
                                 ),
                                 selected = "poisson"),
                     selectInput("link_GAM",
                                 label = "Change link function",
                                 choices = c("Canonical Link",
                                             "()",
                                             "(link='log')",
                                             "(link='probit')",
                                             "(link='cauchit')", 
                                             "(link='cloglog')",
                                             "(link='identity')",
                                             "(link='logit')",
                                             "(link='sqrt')",
                                             "(link='1/mu^2')",
                                             "(link='inverse')"),
                                 selected = "Canonical Link")),
                    
                 
                 box(tabsetPanel(type="tabs",
                                 tabPanel("data_GAM",div(style = 'overflow-x:scroll', dataTableOutput("contents_GAM")) ),
                                 tabPanel("Model Summary",verbatimTextOutput("text1_GAM")),
                                 tabPanel("ANOVA",verbatimTextOutput("text2_GAM"))
                                 )),
                 box(plotOutput("visualisation_features_GAM"))),
                
                 

                 
         fluidRow(box(h4("Plots of GAM"),
                      plotOutput(outputId = 'plot_GAM')),
                  box(plotOutput("diagnostic_plot_GAM")))
        ),  

###############################################################################
# Survival Analysis
###############################################################################
tabItem(tabName = 'Surv',
        h1('Survival Analysis'),
        box(title = 'Kaplan-Meier Estimate basics',withMathJax(uiOutput('K_M_info')),
            collapsible = TRUE, width = 12),
        selectInput('Dataset_SA', 'Select a dataset',c('lung','pbc')),
        #  selectInput('Covariates','Select a covariate', )
        dataTableOutput('DataSetTable_SA'),
        fluidRow(
          box(title = 'DataSet info',verbatimTextOutput('DataSetInfo_SA'),collapsible = TRUE, width = 12),
          box(title = 'DataSet Summary',verbatimTextOutput("summary_SA"),collapsible = TRUE, width = 12)
          
        ),
        
        fluidRow(box(title = 'plot',plotOutput('DataSetPlot1_SA',
                                               width = "500px", height = "500px")),
                 
                 box('select Covariate',selectInput('covariate_SA', 'Select a covariate', 'placeholder')) 
        )
        
        
), # end of the tab item

###############################################################################
# Brownian Motion
###############################################################################
tabItem(tabName = 'brownian',
        fluidRow(
        box(
        h1("Brownian Motion is cool!"),
        sliderInput("npt",
                    "Number of Points for Each Simulated Path",
                    min = 10,
                    max = 2000,
                    value = 100),
        sliderInput("T_brownian", "Time T for Each Simulated Path",
                    min = 0.1,
                    max = 2,
                    value =1),
        sliderInput("nsim", "Number of Simulations",
                    min = 1,
                    max = 200,
                    value = 5),
        checkboxInput('showave_brownian','showave_brownian')),

        box("Plots on Brownian Motion", plotOutput("distPlot_brownian"))
        )),
#############################################################################
# Geometric Brownian Motion
############################################################################
tabItem(tabName = "GBM",
        fluidRow(
          box(numericInput("drift",
                           "Drift Rate (%):",
                           min = 1,
                           value = 15),
              sliderInput("T_GBM", "Time T for Each Simulated Path",
                          min = 0,
                          max = 1000,
                          value =500),
              numericInput("stdev",
                           "Yearly Standard Deviation (%)",
                           min = 1,
                           value = 30),
              numericInput("initPrice",
                           "Initial Stock Price",
                           min = 1,
                           value = 100),
              numericInput("simul",
                           "Number of Simulations",
                           min = 1,
                           value = 2),
              checkboxInput("seeds",
                            "Set seed?"),
              numericInput("setseed",
                           "Select number of seed",
                           min = 1,
                           value = 1),
              checkboxInput('showave_GBM','showave_GBM')),
          box(plotOutput("gbm_ts"),
              # headerPanel(withMathJax("$$\\text{GBM Model: } S_0 \\exp\\left(\\mu t + \\sigma W_t\\right) $$")),
              headerPanel(withMathJax("$$\\text{GBM Model: } S_0 \\exp\\left(\\left(\\mu - \\frac{\\sigma^2}{2}\\right)t + \\sigma W_t\\right) $$")),
              h4("To run the simulation you have to enter the following inputs on the side bar:"),
              h4("Initial Stock Price is the current price of the stock;"),
              h4("Drift rate is the expected rate of return;"),
              h4("Yearly Standard Deviation is the volatility of the stock price;"),
              h4("Number of Simulation represents how many simulation of stock price you want to display;"),
              h4("In the side bar is also possible, through a check box, to set the seed to a fix value. Please mark the check box and select the value from the numeric box. If it is unmarked the seed will be assigned randomly.
                 As the calculation time increases with the number of simulation, there is a 'Submit' button to click as soon as the parameters are decided.")
              ))
        ),



# tabItem(tabName = 'Markov',
#         fluidRow(
#           titlePanel(title = "Discrete Time Markov Process"),
#           
#           box(h4("Setting up transition matrix"),
#                          numericInput("dimension", "Enter the number of states:", 
#                                       value = Init_States, min = Min_States, max = Max_States),
#                          helpText("You can choose between",Min_States,"and",Max_States, "states"),
#                          uiOutput("matrix"),
#                          h5("First Passage Probability"), 
#                          uiOutput("select"),
#                          numericInput("step", "Enter the number of steps: ", min = 1, value = 1),
#                          submitButton("Submit"),
#                          width = 5),
#             box(type = "tab",
#                           tabPanel("State Space Diagram", plotOutput("trans_plot")),
#                           tabPanel("Summary", verbatimTextOutput("summary")),
#                           tabPanel("First Passage Prob", verbatimTextOutput("pas_prob")))
#               
#             )
#           )

###############################################################################
# RandomForest
###############################################################################
tabItem(tabName = "data_rf",
        # header
        fluidRow(box(width=6,h4("Import your dataset(Please wrangle your dataset first)"),
                     #file Input
                     fileInput("file_rf","choose excel file"),
                     radioButtons("fileType_Input_rf",
                                  label = h4("Choose File type"),
                                  choices = list(".csv/txt" = 1, ".xlsx" = 2),
                                  selected = 1,
                                  inline = TRUE),
                     selectInput("selectInput_rf",
                                 "Checkbox group input for response variable:",
                                 c("label 1" = "option1",
                                   "label 2" = "option2")),
                     checkboxGroupInput("inCheckboxGroup_rf",
                                        "Checkbox group input for explanatory variable:",
                                        c("label 1" = "option1",
                                          "label 2" = "option2")),
                     uiOutput("choose_columns_rf")),
                 box(width=6,tabsetPanel(type="tabs",
                                         tabPanel("data_rf",dataTableOutput("contents_rf") )))),
        fluidRow(box(width=12,tabsetPanel(type="tabs",
                                          tabPanel("Decision Tree",h2("Explore decision tree!"),
                                                   fluidRow(box(width=7,
                                                                numericInput(inputId = 'max_depth',
                                                                             label = 'Maximum number of levels in the tree',
                                                                             min =1, max =6,
                                                                             value =3,step=1),
                                                                numericInput(inputId = 'node2',
                                                                             label = 'Minimum number of samples in node',
                                                                             min =0, max =300,
                                                                             value =10,step=10),
                                                                sliderInput(inputId = 'criterion',
                                                                            label = 'criterion(1 - p-value) for implementing a split.',
                                                                            min =0, max =1,
                                                                            value =0.95))
                                                   ),
                                                   fluidRow(

                                                     column(width=6,
                                                            box(width=NULL,plotOutput(outputId = 'treediagram')),verbatimTextOutput('metric')),
                                                     column(width=6,
                                                            box(width=NULL,verbatimTextOutput('tab'),
                                                                infoBox(width=NULL,title='Accuracy',value=textOutput('accuracy')),
                                                                infoBox(width=NULL,title='Sensitivity/Recall',value=textOutput('sen')),
                                                                infoBox(width=NULL,title='Specificity',value=textOutput('spe')),
                                                                infoBox(width=NULL,title='Precision',value=textOutput('pre')))

                                                     ))),
                                          tabPanel("Random Forest",h2("Welcome to the forest!"),
                                                   fluidRow(
                                                     box(width=7,sliderInput(inputId = 'ntree',
                                                                             label = 'number of trees',
                                                                             min =1, max =1000,
                                                                             value =100),
                                                         sliderInput(inputId = 'mtry',
                                                                     label = 'Number of variables considered in choosing each split',
                                                                     min =1, max =6,
                                                                     value =3),
                                                         sliderInput(inputId = 'node',
                                                                     label = 'Minimum number of samples in node',
                                                                     min =1, max =100,
                                                                     value =10))
                                                   )                                  ,
                                                   plotOutput(outputId = 'rf'),
                                                   fluidRow(box(width=12,textOutput(outputId ='error1' ),textOutput(outputId ='error2' ),
                                                                textOutput(outputId ='error3' ),textOutput(outputId ='error4' )))
                                                   ,
                                                   verbatimTextOutput(outputId ='rfoutcome')
                                          ))))
),


tabItem(tabName = "concept",solidHeader = TRUE,
        h2("Key Knowledge"),
        fluidRow(
          tabBox(
            title = "Concepts to mention",width=12,
            id = "concept", height = "420px",
            tabPanel("Ensemble", box(width=12,"This is an idea shared by both statistics(statistical 
                                     ensemble) and machine learning (ensemble learning). In ML, 
                                     it means 'use multiple learning algorithms to obtain better predictive
                                     performance than could be obtained from any of the constituent 
                                     learning algorithms alone'(Wikipedia). Concretely, for random forest, it combines
                                     decision tree classifiers to get better result. To give you a sense of what it means,
                                     look at the example below."),
                     box(title='Example of ensemble: Condorcet’s Jury Theorem',width=12,solidHeader = TRUE,
                         status='success',fluidRow(column(width=4,tags$img(src='Ensemble.png', 
                                                                           height='140px',width='300px')),
                                                   column(width=8,textOutput('ens_text1'),
                                                          textOutput('ens_text2'),
                                                          textOutput('ens_text3'),
                                                          textOutput('ens_text4'),
                                                          textOutput('ens_text5'),
                                                          textOutput('ens_text6')))
                     ))
            ,
            tabPanel("Decision Tree", box(width=12,"We only discuss about ‘classification tree’ here, 
                                          which is about using a tree-like predictive model to go from observations 
                                          about an item to conclusions about item’s target value(wikipedia). To 
                                          begin with, it is good to think of “20 Questions” game. The strategy 
                                          of guesser is to ask the questions that narrow down the most of the options.
                                          The same intuition applies to ‘decision tree’. Searching for splits in decision 
                                          tree is analogous to proposing the questions, but we have certain criterions 
                                          for the search. Here we have a more concrete example using ‘information gain’
                                          as the criterion(of course there are other metrics to choose from). This 
                                          is based on the concept of information entropy, which is the measure of 
                                          degree of chaos in the system. ‘Information gain’ captures the change 
                                          in entropy after splitting."), 
                     fluidRow(column(width=6,tags$img(src='DT1.png',height='220px',width='520px')),column(width=6,tags$img(src='DT2.png',height='220px',width='480px')))),
            
            tabPanel("Bootstrapping",box(width=12, "In statistics, bootstrapping is any test or metric that relies on random sampling 
                                         with replacement. This allows estimation of the sampling distribution of almost any statistic(wikipedia). Suppose that we 
                                         are drawing balls of different colours from a bag once at a time. The selected ball is returned to the bag so that each selection 
                                         has equal probability of drawing certain colour. More generally, for a sample X of size N, we draw N elements from it with 
                                         replacement and form a new sample(bootstrap sample) of the same size. We can repeat this process and compute statistics of 
                                         original distribution using a large number of bootstrap samples."),fluidRow(column(width=6,tags$img(src='Boot.png',height='280px',width='520px')))),
            
            tabPanel("Bagging", box(width=12,"Let’s combine what we have just learnt together. Suppose that we have generated 
                                    m bootstrap samples: X1,X2,…,Xm. We would like to train a decision tree classifier on each 
                                    bootstrap sample and average all the individual classifiers to build our final classifier. 
                                    This is called Bagging. However, though bagging is usually used with decision trees, it 
                                    works with other methods as well. This technique improves stability and accuracy of algorithms
                                    —— it can be proved that mean squared error is reduced by factor of m. Plus, bagging reduces
                                    variance and prevents overfitting. More concretely, it decreases the error generated when 
                                    training on different datasets. Intuitively, errors of models trained on different data are
                                    cancelled out with each other."), 
                     fluidRow(column(width=6,tags$img(src='bagging.png',height='280px',width='520px'))))
            )
            )
            ),
tabItem(tabName = "sketch",solidHeader = TRUE,
        h2("Simple sketch of Random Forest"),
        fluidRow(
          infoBox(width=12,title='Random Forest',value='Consider building a random forest consisting of N trees(for k=1,2,..,N)',icon=icon('bars'),fill=TRUE,color="purple")),
        fluidRow(
          infoBox(width=6,title='Step One',value='Generate bootstrap sample Xk',icon=icon('list-ol'),color='teal'),
          infoBox(width=6,title='Step Two',value='Build a decision tree based on that sample',icon=icon('list-ol'),color='teal')
        ),  
        
        fluidRow(infoBox(width=12,title='Step Three',value='From all the features(e.g. T variables) prepared to train the (entire) random forest model, randomly select M of them to feed into this decision tree',icon=icon('list-ol'),color='teal')),
        fluidRow(infoBox(width=12,title='Step Four',value='Select the first splitting, searching over all randomly chosen features, choosing the one maximizing information gain (e.g. entropy/gini method, more details to be added)',icon=icon('list-ol'),color='teal')),
        fluidRow(infoBox(width=12,title='Step Five',value='Repeat this process until the sample is exhausted under the restriction given(e.g. maximum level of tree)',icon=icon('list-ol'),color='teal')),
        fluidRow(infoBox(width=12,title='Step Six',value='Get N classifiers from N decision trees, and then average them to get the final classifier',icon=icon('list-ol'),color='teal'))
),

############################ Optimisation ###########################
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

) # End of TabItems
) # End of Dashbody


ui <- dashboardPage(header = dashboardHeader(title = "Shiny Stats"),
                    sidebar = sidebar,
                    body = body,
                    skin = "purple"
)


