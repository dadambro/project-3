library(shiny)
library(tidyverse)
library(DT)



# Create graphs
function(input, output, session) {
  
#Create scatterplot
   output$scatter <- renderPlot({
    newData <- myData %>% filter(id.number <= as.numeric(input$gens))
    newData <- newData %>% 
      mutate(newVar = if_else(type1 == input$colorcode2 | type2 == as.character(input$colorcode2), 
                              str_to_title(input$colorcode2) , 
                              paste0("Not ", str_to_title(input$colorcode2)), 
                              paste0("Not ", str_to_title(input$colorcode2))))
    
    g <- ggplot(newData, aes_string(x = input$x.axis, y = input$y.axis))
    h <- geom_point()
    i <- geom_point(aes_string(col = as.factor(newData$newVar)))
    
    #Conditionally update plot based on color code selections
    if(input$colorcode){
      g + i + geom_smooth(method = "lm", se = FALSE, aes(col = newVar)) + 
        theme_minimal() + theme(legend.title=element_blank())
      }
      else{
        g + h + theme_minimal()
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