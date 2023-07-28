source("helper.R")

function(input, output, session) {

#Create random image for landing page
picNum <- sample(1:1010, 1)
shinyNum <- sample(1:25, 1)
if(shinyNum == 13){
src <- paste0("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/other/official-artwork/shiny/", picNum, ".png")}
else{src <- paste0("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/other/official-artwork/", picNum, ".png")}
output$randomPic <- renderUI({tags$img(src = src)})
picNameData <- myData %>% filter(id.number == picNum)
if(shinyNum == 13){picName <- paste0("It's SHINY ", picNameData$name, "!")}
else{picName <- paste0("It's ", picNameData$name, "!")}
output$picName <- renderUI({h4(tags$strong(picName))})

observeEvent(input$newPic, {
  picNum <- sample(1:as.numeric(input$gens), 1)
  shinyNum <- sample(1:25, 1)
  if(shinyNum == 13){
    src <- paste0("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/other/official-artwork/shiny/", picNum, ".png")}
  else{src <- paste0("https://raw.githubusercontent.com/PokeAPI/sprites/master/sprites/pokemon/other/official-artwork/", picNum, ".png")}
  output$randomPic <- renderUI({tags$img(src = src)})
  picNameData <- myData %>% filter(id.number == picNum)
  if(shinyNum == 13){picName <- paste0("It's SHINY ", picNameData$name, "!")}
  else{picName <- paste0("It's ", picNameData$name, "!")}
  output$picName <- renderUI({h4(tags$strong(picName))})
})
##########

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
  else if(input$myType == "dark" & input$gens == "151"){
    
    output$trainModelTitle <- renderUI(
      h4("Warning!: No Dark-type Pokemon exist in Generation I. Please choose another type, or include Pokemon from Generation II +")
    ) }
  else if(as.numeric(input$randForestmtry) > length(input$randForestVars)){
    
    output$trainModelTitle <- renderUI(
      h4("Warning!: mtry value for Random Forest is greater than the total number of variables! Please reduce this value, select more variables, or select 'auto-tune.' Note that setting mtry equal to the number of variables is technically 'bagging,' and not a random forest.")
    ) }
  
  #Proceed with model building provided each model has at least 1 variable selected
  else{
  #Add status bar
  withProgress(message = 'Building models...',
               value = 0, {
                 #Compile parameters for models
                 trainSplit <- as.numeric(input$trainSplit)*0.01
                 
                 cp <- if(input$cpAuto){
                          seq(0.001, 0.1, 0.001)}
                      else{as.numeric(input$treecp)}
                 
                 mtry <- if(input$mtryAuto){
                   1:(length(input$randForestVars) - 1)}
                 else{as.numeric(input$randForestmtry)}
                 
                 #Generate overall dataset
                 myNewData <- myData %>% filter(id.number <= as.numeric(input$gens))
                 myNewData <- myNewData %>% 
                   mutate(myVar = if_else(type1 == input$myType | type2 == input$myType, 
                                          str_to_title(input$myType) , 
                                          paste0("Not_", str_to_title(input$myType)), 
                                          paste0("Not_", str_to_title(input$myType))))
                 myNewData$myVar <- as.factor(myNewData$myVar)
                 modData <- myNewData %>% select(height:myVar)
                 
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
                 rfmodTest <- modTest %>% select(myVar, all_of(input$randForestVars))
                 
                 incProgress(0.25, detail = "Training GLM")
                 
                 #Train generalized linear model
                 genLinearFit <<- train(myVar ~ ., data = glmmodTrain,
                                       method = "glm",
                                       family = "binomial",
                                       preProcess = c("center", "scale"),
                                       trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3)
                 )
                #Run on test data
                genLinearPredict <- predict(genLinearFit, newdata = glmmodTest)
                
                #Make test data output
                genLinearCM <- confusionMatrix(genLinearPredict, glmmodTest$myVar, positive = str_to_title(input$myType))
                 
                 
                 incProgress(0.25, detail = "Training Decision Tree")
                 
                 #Train decision tree model
                 classTreeFit <<- train(myVar ~ ., data = treemodTrain, 
                                       method = "rpart",
                                       #preProcess = c("center", "scale"),
                                       trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                                       tuneGrid = data.frame(cp = cp)
                 )
                 #Run on test data
                 classTreePredict <- predict(classTreeFit, newdata = treemodTest)
                 
                 #Make test data output
                 classTreeCM <- confusionMatrix(classTreePredict, treemodTest$myVar, positive = str_to_title(input$myType))
                 
                 incProgress(0.25, detail = "Training Random Forest")
                 
                 #Train random forest model
                 randomForestFit <<- train(myVar ~ ., data = rfmodTrain, 
                                          method = "rf",
                                          #preProcess = c("center", "scale"),
                                          trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3),
                                          tuneGrid = data.frame(mtry = mtry)
                 )
                 
                 #Run on test data
                 randomForestPredict <- predict(randomForestFit, newdata = rfmodTest)
                 
                 #Make test data output
                 randomForestCM <- confusionMatrix(randomForestPredict, rfmodTest$myVar, positive = str_to_title(input$myType))
                 
                 incProgress(0.25, detail = "Making output")
                 
                 #Compile results from training set, make into a table for output
                 genLinearTable <- genLinearFit$results %>% 
                   mutate(Model = "Generalized Linear", `Variable(s) Included` = 
                            paste(colnames(select(glmmodTrain, -1)), collapse = " "), .before = parameter) %>% select(1,2,4)
                 
                 classTreeExtract <- classTreeFit$results %>% filter(cp == as.numeric(classTreeFit$bestTune))
                 classTreeTable <- classTreeExtract %>% 
                   mutate(Model = "Classification Tree", `Variable(s) Included` = 
                            paste(colnames(select(treemodTrain, -1)), collapse = " "), .before = cp) %>% select(1,2,4)
                 
                 randomForestExtract <- randomForestFit$results %>% filter(mtry == as.numeric(randomForestFit$bestTune))
                 randomForestTable <- randomForestExtract %>% 
                   mutate(Model = "Random Forest", `Variable(s) Included` = 
                            paste(colnames(select(rfmodTrain, -1)), collapse = " "), .before = mtry) %>% select(1,2,4)
                 
                 output$trainModelTitle <- renderUI({isolate(h5(strong(paste0("Model performance on training data for type '", 
                                                                              input$myType, "' using ", input$gens, 
                                                                              " out of a possible 1010 Pokemon:"))))})
                 
                 output$trainModelOutput <- renderTable({rbind(genLinearTable, classTreeTable, randomForestTable)})
                 
                 output$trainModelFootnote <- renderUI({paste0("The classification tree cp was: ", classTreeFit$bestTune[,1], 
                                                               "; the random forest mtry was: ", randomForestFit$bestTune[,1], 
                                                               ". Due to the imbalanced nature of these data, accuracy is likely misleading; 
                                                               consult the other fit statistics below to get a better idea of model performance")})
                 
                 output$confusionMatrixGLM <- renderPrint({genLinearCM})
                 output$modelSummaryGLM <- renderPrint({summary(genLinearFit$finalModel)})
                 
                 output$confusionMatrixClassTree <- renderPrint({classTreeCM})
                 output$plotClassTree <- renderPlot({plot(classTreeFit$finalModel)
                                                     text(classTreeFit$finalModel)})
                 
                 output$confusionMatrixRandomForest <- renderPrint({randomForestCM})
                 output$plotRandomForest <- renderPlot({varImpPlot(randomForestFit$finalModel, main = NULL)})
                 
                 genLinearTestVars <- genLinearCM$byClass[c(1,2,5:7)]
                 classTreeTestVars <- classTreeCM$byClass[c(1,2,5:7)]
                 randomForestTestVars <- randomForestCM$byClass[c(1,2,5:7)]
                 
                 testStatsData <- data.frame(rbind(genLinearTestVars, classTreeTestVars, randomForestTestVars))
                 testStatsTable <- testStatsData %>% 
                   mutate(Model = c("Generalized Linear", "Classification Tree", "Random Forest"), .before = Sensitivity)
                 
                 output$testStats <- renderTable({testStatsTable})
                 
                 output$testModelTitle <- renderUI({isolate(h5(strong(paste0("Model performance on test data for type '", 
                                                                             input$myType, "':"))))})
                 
                 output$glmSummary <- renderPrint({summary(genLinearFit$finalModel)})
                 #Render UI objects for Predict tab
                 output$heightSlider <- renderUI(sliderInput("heightSlider", "Height:",
                                                             min = min(myNewData$height),
                                                             max = max(myNewData$height),
                                                             value = mean(myNewData$height)))
                 output$weightSlider <- renderUI(sliderInput("weightSlider", "Weight:",
                                                             min = min(myNewData$weight),
                                                             max = max(myNewData$weight),
                                                             value = mean(myNewData$weight)))
                 output$attackSlider <- renderUI(sliderInput("attackSlider", "Attack:",
                                                             min = min(myNewData$attack),
                                                             max = max(myNewData$attack),
                                                             value = mean(myNewData$attack)))
                 output$defenseSlider <- renderUI(sliderInput("defenseSlider", "Defense:",
                                                             min = min(myNewData$defense),
                                                             max = max(myNewData$defense),
                                                             value = mean(myNewData$defense)))
                 output$specialAttackSlider <- renderUI(sliderInput("specialAttackSlider", "Special Attack:",
                                                             min = min(myNewData$special.attack),
                                                             max = max(myNewData$special.attack),
                                                             value = mean(myNewData$special.attack)))
                 output$specialDefenseSlider <- renderUI(sliderInput("specialDefenseSlider", "Special Defense:",
                                                             min = min(myNewData$special.defense),
                                                             max = max(myNewData$special.defense),
                                                             value = mean(myNewData$special.defense)))
                 output$speedSlider <- renderUI(sliderInput("speedSlider", "Speed:",
                                                             min = min(myNewData$speed),
                                                             max = max(myNewData$speed),
                                                             value = mean(myNewData$speed)))
                 output$hpSlider <- renderUI(sliderInput("hpSlider", "HP:",
                                                             min = min(myNewData$hp),
                                                             max = max(myNewData$hp),
                                                             value = mean(myNewData$hp)))
                 
                 typePoke <- myNewData %>% filter(myVar == str_to_title(input$myType))
                 notTypePoke <- myNewData %>% filter(myVar != str_to_title(input$myType))
                 output$myTypeList <- renderUI(selectInput("myTypeList", "Pokemon of Predicted Type:",
                                                           typePoke$name))
                 output$notMyTypeList <- renderUI(selectInput("notMyTypeList", "Pokemon NOT of Predicted Type:",
                                                           notTypePoke$name))
                 
               })
}
  }
)
}
)
##########

#Predict stuff
({observeEvent(input$predict, {
  
##Custom variable values prediction
if(input$predictSelect == "custom"){
#Custom slider input
customPoke <- data.frame(height = as.numeric(input$heightSlider),
                         weight = as.numeric(input$weightSlider),
                         hp = as.numeric(input$hpSlider),
                         attack = as.numeric(input$attackSlider),
                         defense = as.numeric(input$defenseSlider),
                         special.attack = as.numeric(input$specialAttackSlider),
                         special.defense = as.numeric(input$specialDefenseSlider),
                         speed = as.numeric(input$speedSlider))

glmcustomPoke <- customPoke %>% select(all_of(input$lmVars))
glmcustomPokePredict <- predict(genLinearFit, newdata = glmcustomPoke)

classTreeCustomPoke <- customPoke %>% select(all_of(input$treeVars))
classTreeCustomPokePredict <- predict(classTreeFit, newdata = classTreeCustomPoke)

randomForestCustomPoke <- customPoke %>% select(all_of(input$randForestVars))
randomForestCustomPokePredict <- predict(randomForestFit, newdata = randomForestCustomPoke)

customPokeTable <- data.frame(Model = c("Generalized Linear", "Classification Tree", "Random Forest"),
                              Prediction = c(as.character(glmcustomPokePredict), 
                                             as.character(classTreeCustomPokePredict), 
                                             as.character(randomForestCustomPokePredict)
                                             )
                              )
output$customPokeTable <- renderTable({customPokeTable})
}

#"Right" type Pokemon prediction
else if(input$predictSelect == "myType"){
rightTypePoke <- myData %>% filter(name == input$myTypeList)

glmRightTypePoke <- rightTypePoke %>% select(all_of(input$lmVars))
glmRightTypePokePredict <- predict(genLinearFit, newdata = glmRightTypePoke)

classTreeRightTypePoke <- rightTypePoke %>% select(all_of(input$treeVars))
classTreeRightTypePokePredict <- predict(classTreeFit, newdata = classTreeRightTypePoke)

randomForestRightTypePoke <- rightTypePoke %>% select(all_of(input$randForestVars))
randomForestRightTypePokePredict <- predict(randomForestFit, newdata = randomForestRightTypePoke)

rightTypePokeTable <- data.frame(Model = c("Generalized Linear", "Classification Tree", "Random Forest"),
                              Prediction = c(as.character(glmRightTypePokePredict), 
                                             as.character(classTreeRightTypePokePredict), 
                                             as.character(randomForestRightTypePokePredict)
                                             )
)
output$rightTypePokeInfo <- renderTable({select(rightTypePoke, name, height:speed)})
output$rightTypePokeTable <- renderTable({rightTypePokeTable})
}

#"Wrong" type Pokemon prediction
else{
wrongTypePoke <- myData %>% filter(name == input$notMyTypeList)

glmWrongTypePoke <- wrongTypePoke %>% select(all_of(input$lmVars))
glmWrongTypePokePredict <- predict(genLinearFit, newdata = glmWrongTypePoke)

classTreeWrongTypePoke <- wrongTypePoke %>% select(all_of(input$treeVars))
classTreeWrongTypePokePredict <- predict(classTreeFit, newdata = classTreeWrongTypePoke)

randomForestWrongTypePoke <- wrongTypePoke %>% select(all_of(input$randForestVars))
randomForestWrongTypePokePredict <- predict(randomForestFit, newdata = randomForestWrongTypePoke)

wrongTypePokeTable <- data.frame(Model = c("Generalized Linear", "Classification Tree", "Random Forest"),
                                 Prediction = c(as.character(glmWrongTypePokePredict), 
                                                as.character(classTreeWrongTypePokePredict), 
                                                as.character(randomForestWrongTypePokePredict)
                                 )
)
output$wrongTypePokeInfo <- renderTable({select(wrongTypePoke, name, height:speed)})
output$wrongTypePokeTable <- renderTable({wrongTypePokeTable})
}
})
               })
##########
   
#Data export and subset
observeEvent(input$preview, {
  #Include Pokemon from generation radio button filter
  exportData <- myData %>% filter(id.number <= as.numeric(input$gens))
  #Deal with binary type-specific filtering
  if(input$typeFilter){exportData <- exportData %>% 
    mutate(myVar = if_else(type1 == input$myTypeFilter | type2 == input$myTypeFilter, 
                           str_to_title(input$myTypeFilter) , 
                           paste0("Not_", str_to_title(input$myTypeFilter)), 
                           paste0("Not_", str_to_title(input$myTypeFilter))))
  if(input$typeFilterOption == 'include'){
    exportData <- exportData %>% filter(myVar == str_to_title(input$myTypeFilter))
  }
  else{exportData <- exportData %>% filter(myVar != str_to_title(input$myTypeFilter))
  }
  exportData <<- exportData %>% select(all_of(input$exportVars))
  }
  else{exportData <<- exportData %>% select(all_of(input$exportVars))}
})
  #Make data table preview
observeEvent(input$preview, {output$allData <- renderDataTable({exportData})})
  #Export data to .csv
observeEvent(input$download,{
  fileName <- paste0("pokemon-subset-", Sys.Date(), ".csv")
  write.csv(exportData, file = fileName, row.names = FALSE)
})
}