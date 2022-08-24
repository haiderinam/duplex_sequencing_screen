library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)

library(shiny)
library(plotly)
library(knitr)
library(tictoc)
library(workflowr)
library(VennDiagram)
library(dplyr)
library(foreach)
library(doParallel)
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(devtools)
library(ggsignif)
library(shinydashboard)
library(shinycssloaders)
library(drc)


shinyUI(
  dashboardPage(skin="green",
    dashboardHeader(),
    dashboardSidebar(
      sliderInput("nreplicates","Please enter the number of replicates",1,10,1),
      sliderInput("nconc","Please enter the number concentration datapoints (include Vehicle Control)",1,20,1),
      textInput("conc","Please paste concentrations here","10	5	2.5	1.25	0.625	0.3125	0.15625	0.078125	0.0390625	0.01953125	0"),
      textInput("y1","Please paste dose response for first replicate","2.87E-04	1.76E-03	4.14E-02	5.91E-01	9.49E-01	9.17E-01	7.09E-01	1.24E+00	8.82E-01	9.28E-01	1.00E+00"),
      textInput("y2","Please paste dose response for second replicate","3.09E-04	1.86E-03	4.26E-02	4.71E-01	1.12E+00	1.18E+00	9.27E-01	1.10E+00	1.22E+00	1.20E+00	1.00E+00"),
      textInput("y3","Please paste dose response for third replicate","3.78E-04	3.06E-03	1.01E-01	9.21E-01	9.89E-01	1.30E+00	1.17E+00	1.45E+00	1.26E+00	1.40E+00	1.00E+00"),
      actionButton('update',"Apply",icon("refresh"),class = "btn btn-primary")
    ),
    dashboardBody(
      fluidRow(
        column(width=4,
               h2("Introduction")
        )
        ),

      fluidRow(
        box(width=8,
            box(
              title="Logistic Fit Results", width=NULL, solidHeader = T,
              # h4(tags$b(htmlOutput('take_action'))),
              plotOutput('plotlyplot') %>% withSpinner(color="#0dc5c1")
              )
            )
        )
    )
  )
)

# shinyUI(
#   dashboardPage(skin="green",
#                 # pageWithSidebar(
#                 # Application title
#                 # headerPanel("Pairwise Comparisons of Conditional Selections Demo!"),
#                 dashboardHeader(title = title,titleWidth = 250),
#
#                 # Sidebar with a slider input for number of observations
#                 # sidebarPanel(
#                 dashboardSidebar(
#                   sliderInput("nreplicates","Please select the number of replicates",1,10,1),
#                   sliderInput("nconc","Please enter the number of concentrations used, including Vehicle control",1,50,1),
#                   actionButton('update',"Apply",icon("refresh"),class = "btn btn-primary")
#                 ),
#
#                 # Show a plot of the generated distribution
#                 dashboardBody(
#                   fluidRow(
#                     column(width=4,
#                            h2("Introduction")
#                     )
#                     ),
#
#                   fluidRow(
#                     box(width=8,
#                         box(
#                           title="Pairwise Comparisons Results", width=NULL, solidHeader = T,
#                           h4(tags$b(htmlOutput('take_action'))),
#                           plotOutput('plotlyplot') %>% withSpinner(color="#0dc5c1")
#                           )
#                         )
#                     )
#                 )
#   )
# )
#ui.R
