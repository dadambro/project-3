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
   
({observeEvent(input$buildModels, {
  
  #Conditional logic to not allow model building if all models have 0 variables selected
  if(length(input$lmVars) == 0 | length(input$treeVars) == 0 | length(input$randForestVars) == 0){
    output$trainModelTitle <- renderUI(
      h3("Warning!: All models need at least one variable selected prior to building!")
      ) 
  }
  
  #Proceed with model building provided each model has at least 1 variable selected
  else{
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
  
  #Set seed
  set.seed(as.numeric(input$seed))
  
  #Create overall training and test sets
  trainIndex <- createDataPartition(modData$myVar, p = trainSplit, list = FALSE)
  modTrain <- modData[trainIndex,]
  modTest <- modData[-trainIndex,]
  
  #Subset for glm
  glmmodTrain <- modTrain %>% select(myVar, all_of(input$lmVars))
  glmmodTest <- modTest %>% select(myVar, all_of(input$lmVars))
 
  #Subset for decision tree
  treemodTrain <- modTrain %>% select(myVar, all_of(input$treeVars))
  treemodTest <- modTest %>% select(myVar, all_of(input$treeVars))
 
  #Subset for random forest
  rfmodTrain <- modTrain %>% select(myVar, all_of(input$randForestVars))
  rfmmodTest <- modTest %>% select(myVar, all_of(input$randForestVars))
  
  #Train generalized linear model
  genLinearFit <- train(myVar ~ ., data = glmmodTrain,
                        method = "glm",
                        family = "binomial",
                        preProcess = c("center", "scale"),
                        trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3)
                        )
  
  #Train decision tree model
  classTreeFit <- train(myVar ~ ., data = treemodTrain, 
                          method = "rpart",
                          preProcess = c("center", "scale"),
                          trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                          tuneGrid = data.frame(cp)
                        )
  
  #Train random forest model
  randomForestFit <- train(myVar ~ ., data = rfmodTrain, 
                             method = "rf",
                             preProcess = c("center", "scale"),
                             trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                             tuneGrid = data.frame(mtry)
                           )
  
  #Compile results from training set, make into a table for output
  genLinearTable <- genLinearFit$results %>% mutate(Model = "Generalized Linear", `Variable(s) Included` = paste(colnames(select(glmmodTrain, -1)), collapse = " "), .before = parameter) %>% select(-3)
  classTreeTable <- classTreeFit$results %>% mutate(Model = "Classification Tree", `Variable(s) Included` = paste(colnames(select(treemodTrain, -1)), collapse = " "), .before = cp) %>% select(-3)
  randomForestTable <- randomForestFit$results %>% mutate(Model = "Random Forest", `Variable(s) Included` = paste(colnames(select(rfmodTrain, -1)), collapse = " "), .before = mtry) %>% select(-3)
  
  output$trainModelTitle <- renderUI({isolate(h5(strong(paste0("Model accuracy on training data for type '", input$myType, "' using ", input$gens, " out of a possible 1010 Pokemon:"))))})
  output$trainModelOutput <- renderTable({rbind(genLinearTable, classTreeTable, randomForestTable)})
  
    
}
  }
)
  }
)
##########

}