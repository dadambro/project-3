library(shiny)
library(tidyverse)
library(DT)
library(caret)
library(randomForest)



# Create graphs
function(input, output, session) {

#Create figures  
##Create scatterplot
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
   
##Create frequency polygon
   output$freqPoly <- renderPlot({
     newData <- myData %>% filter(id.number <= as.numeric(input$gens))
     newData <- newData %>% 
       mutate(newVar = if_else(type1 == input$colorcode4 | type2 == input$colorcode4, 
                               str_to_title(input$colorcode4) , 
                               paste0("Not ", str_to_title(input$colorcode4)), 
                               paste0("Not ", str_to_title(input$colorcode4))))
     g <- ggplot(newData, aes_string(x = input$x.axis2, color = as.factor(newData$newVar)))
     h <- ggplot(newData, aes_string(x = input$x.axis2))
     i <- geom_freqpoly(stat = "density")

          if(input$colorcode3){
       g + i + theme_minimal() + theme(legend.title=element_blank())
     }
     else{
       h + i + theme_minimal()
     }})
##########
   
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
##########
   
#Build models
   
output$modz <- renderPrint({observeEvent(input$buildModels, {
  
  #Add status bar
  withProgress(message = 'Building models...',
               value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  
  #Compile parameters for models
  trainSplit <- as.numeric(input$trainSplit)*0.01
  cp <- as.numeric(input$treecp)
  mtry <- as.numeric(input$randForestmtry)
  
  #Generate overall dataset
  modData <- myData %>% filter(id.number <= as.numeric(input$gens))
  modData <- modData %>% 
    mutate(myVar = if_else(type1 == input$myType | type2 == input$myType, 
                            str_to_title(input$myType) , 
                            paste0("Not_", str_to_title(input$myType)), 
                            paste0("Not_", str_to_title(input$myType))))
  modData$myVar <- as.factor(modData$myVar)
  modData <- modData %>% select(height:myVar)
  
  #Create training and test data for decision tree
  set.seed(as.numeric(input$seed))
  
  trainIndex <- createDataPartition(modData$myVar, p = trainSplit, list = FALSE)
  modTrain <- modData[trainIndex,]
  modTest <- modData[-trainIndex,]
  
  #Train generalized linear model
  
  #Train decision tree model
  classTreeFit <- train(myVar ~ ., data = modTrain, 
                          method = "rpart",
                          preProcess = c("center", "scale"),
                          trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                          tuneGrid = data.frame(cp)
                        )
  
  #Train random forest model
  randomForestFit <- train(myVar ~ ., data = modTrain, 
                             method = "rf",
                             preProcess = c("center", "scale"),
                             trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                             tuneGrid = data.frame(mtry)
                           )
  
classTreeFit$results
})})
   

}