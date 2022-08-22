#server.R
# setwd("shinyapp/")
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
source("code/four_param.R")
library(shiny)
library(drc)

# shinyServer(
#   function(input,output){
#
#   }
# )





shinyServer(function(input, output) {
  ###Intro Box
  output$intro<-renderText({
    "This is my four parameter logistic regression function."
  })
  #FourParameter Function
  y_mean_reactive=reactive({
    input$update
    y_mean=isolate(four_param(input$conc,input$y1,input$y2,input$y3,input$nreplicates,input$nconc))
    isolate(y_mean)
  })
  #Plot
  output$plotlyplot=renderPlot({
    input$update
    y_mean=y_mean_reactive()
    isolate(ggplot(y_mean,aes(conc,y.mean))+geom_point()+geom_errorbar(aes(ymin=y.mean-y.sd,ymax=y.mean+y.sd))+geom_line(aes(y=y_model),linetype="dashed")+geom_ribbon(aes(ymin=y_model-y.sd,ymax=y_model+y.sd,alpha=0.3))+scale_x_continuous(trans="log10"))
  })
})
