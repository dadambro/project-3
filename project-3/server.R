library(shiny)
library(tidyverse)
library(DT)



# Create graphs
function(input, output, session) {
  
#Create scatterplot
   output$scatter <- renderPlot({
    newData <- myData %>% filter(id.number <= as.numeric(input$gens))
    g <- ggplot(newData, aes_string(x = input$x.axis, y = input$y.axis))
    h <- geom_point()
    i <- geom_point(aes_string(col = input$colorcode2))
    
    #Conditionally update plot based on color code selections
    if(input$colorcode){
      g + i}
      else{
        g + h
      }})
   
#Create numeric summary
   output$summaryTable <- renderDataTable({
     newData <- myData %>% filter(id.number <= as.numeric(input$gens))
     var <- toString(input$var)
     if(input$tableType == 'type2'){
     tab <- newData %>% group_by(type2) %>% summarize(Average = round(mean(.data[[input$var]]),2),
                                                      Minimum = min(.data[[input$var]]),
                                                      Maximum = max(.data[[input$var]]),
                                                      SD = round(sd(.data[[input$var]]),2),
                                                      n = length(.data[[input$var]]))
     }
     else{tab <- newData %>% group_by(type1) %>% summarize(Average = round(mean(.data[[input$var]]),2),
                                                           Minimum = min(.data[[input$var]]),
                                                           Maximum = max(.data[[input$var]]),
                                                           SD = round(sd(.data[[input$var]]),2),
                                                           n = length(.data[[input$var]]))
     }
   })
}