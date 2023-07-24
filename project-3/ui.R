library(shiny)

fluidPage(

    # Application title
    titlePanel("Project 3"),
    
    navlistPanel(
      "About",
      tabPanel("Start here!",
               h3("Welcome!"),
               "The purpose of this app is to explore whether or not one can 
               predict a Pokemon's typing based on its base stats, height, and weight.",
               h4("Data Exploration"),
               "These tabs allows for graphical and numerical exploration of 
               Pokemon types and base stats. Play around as you see fit, 
               generate figures/summaries, and see if certain stats/heights/weights seem to 
               associate with certain types. Use this info to inform your model!",
               h4("Modeling"),
               "These tabs provide basic information about some of the modeling 
               approaches, and allows you to design, fit, and train a model to 
               predict Pokemon type. Finally, you can put your model to the test
               to see if it can correctly predict a Pokemon's typing.",
               h4("Subset and Export"),
               "Create your very own subset of Pokemon data, export it, and use 
               it as you see fit!",
               br(),
               br(),
               "Note, the radio buttons at the bottom allows you to filter the 
               data used by generation. As the Pokemon franchise has expanded, 
               one might expect certain trends related to base stats and type 
               seen in earlier games (e.g., 'Electric Pokemon have high speed!') 
               to become weaker, as more diverse Pokemon with various typing/stat 
               combinations are introduced into the game. The radio buttons 
               allow one to explore this idea by seeing if limiting the dataset 
               causes trends to become more apparent, models to perform 
               differently, etc."
               ),
      "----------",
      
      "Data Exploration",
      tabPanel("Graphical Summaries",
               uiOutput("pokes"),
               radioButtons("displayGraph", "Choose graph to create:",
                            c("Scatter plot" = "scatter",
                              "Frequency polygon" = "freqpoly")),
               
               conditionalPanel(condition = "input.displayGraph == 'scatter'",
               plotOutput("scatter"),
               selectInput("x.axis", "X-axis Variable",
                           c("Height" = "height",
                             "Weight" = "weight",
                             "HP" = "hp", 
                             "Attack" = "attack",
                             "Defense" = "defense",
                             "Special Attack" = "special.attack",
                             "Special Defense" = "special.defense",
                             "Speed" = "speed"
                           )),
               selectInput("y.axis", "Y-axis Variable",
                           c("Height" = "height",
                             "Weight" = "weight",
                             "HP" = "hp", 
                             "Attack" = "attack",
                             "Defense" = "defense",
                             "Special Attack" = "special.attack",
                             "Special Defense" = "special.defense",
                             "Speed" = "speed"
                           )),
               checkboxInput("colorcode", "Color points by a specific type?"),
               conditionalPanel(condition = "input.colorcode == 1",
                                selectInput("colorcode2", "Select type",
                                             c("Bug" = "bug",
                                               "Dark" = "dark",
                                               "Dragon" = "dragon",
                                               "Electric" = "electric",
                                               "Fairy" = "fairy",
                                               "Fighting" = "fighting",
                                               "Fire" = "fire",
                                               "Flying" = "flying",
                                               "Ghost" = "ghost",
                                               "Grass" = "grass",
                                               "Ground" = "ground",
                                               "Ice" = "ice",
                                               "Normal" = "normal",
                                               "Poison" = "poison",
                                               "Psychic" = "psychic",
                                               "Rock" = "rock",
                                               "Steel" = "steel",
                                               "Water" = "water")))
               ),
               conditionalPanel(condition = "input.displayGraph == 'freqpoly'",
                                plotOutput("freqPoly"),
                                selectInput("x.axis2", "X-axis Variable",
                                            c("Height" = "height",
                                              "Weight" = "weight",
                                              "HP" = "hp", 
                                              "Attack" = "attack",
                                              "Defense" = "defense",
                                              "Special Attack" = "special.attack",
                                              "Special Defense" = "special.defense",
                                              "Speed" = "speed"
                                            )),
                                checkboxInput("colorcode3", "Color polygons by a specific type?"),
                                conditionalPanel(condition = "input.colorcode3 == 1",
                                                 selectInput("colorcode4", "Select type",
                                                             c("Bug" = "bug",
                                                               "Dark" = "dark",
                                                               "Dragon" = "dragon",
                                                               "Electric" = "electric",
                                                               "Fairy" = "fairy",
                                                               "Fighting" = "fighting",
                                                               "Fire" = "fire",
                                                               "Flying" = "flying",
                                                               "Ghost" = "ghost",
                                                               "Grass" = "grass",
                                                               "Ground" = "ground",
                                                               "Ice" = "ice",
                                                               "Normal" = "normal",
                                                               "Poison" = "poison",
                                                               "Psychic" = "psychic",
                                                               "Rock" = "rock",
                                                               "Steel" = "steel",
                                                               "Water" = "water")))
               )),
      
      tabPanel("Numerical Summaries",
               selectInput("var", "Select base stat",
                           c("Height" = "height",
                             "Weight" = "weight",
                             "HP" = "hp", 
                             "Attack" = "attack",
                             "Defense" = "defense",
                             "Special Attack" = "special.attack",
                             "Special Defense" = "special.defense",
                             "Speed" = "speed"
                           )),
               radioButtons("tableType", "Select type to summarize by:",
                            c("Primary type" = "type1",
                              "Secondary type" = "type2")),
               dataTableOutput("summaryTable")
               ),
      
      "Modeling",
      tabPanel("Modeling Info",
               h2("This panel will provide general information about modeling")),
      
      tabPanel("Model Fitting",
               numericInput("seed", "Set seed (optional):",
                            1337,
                            min = -1*.Machine$integer.max,
                            max = .Machine$integer.max),
               selectInput("myType", "Choose the type you wish to predict:",
                           c("Bug" = "bug",
                             "Dark" = "dark",
                             "Dragon" = "dragon",
                             "Electric" = "electric",
                             "Fairy" = "fairy",
                             "Fighting" = "fighting",
                             "Fire" = "fire",
                             "Flying" = "flying",
                             "Ghost" = "ghost",
                             "Grass" = "grass",
                             "Ground" = "ground",
                             "Ice" = "ice",
                             "Normal" = "normal",
                             "Poison" = "poison",
                             "Psychic" = "psychic",
                             "Rock" = "rock",
                             "Steel" = "steel",
                             "Water" = "water")
                           ),
               conditionalPanel(condition = "input.myType == 'dark'",
                                "Note: Dark-type Pokemon do not exist in Generation I.
                                If filtering by generation with the radio buttons below,
                                make sure you are including Pokemon through at least Generation II.",
                                br()
               ),
               conditionalPanel(condition = "input.myType == 'fairy'",
                                "Note: Fairy-type Pokemon were introduced in Generation VI,
                                although some Pokemon in prior generations were retroactively
                                assigned this typing after its introduction.
                                If filtering by generation with the radio buttons below,
                                better results may be had by including Pokemon through 
                                at least Generation VI.",
                                br()
               ),
               conditionalPanel(condition = "input.myType == 'steel'",
                                "Note: Steel-type Pokemon were introduced in Generation II,
                                although some Pokemon in Generation I were retroactively
                                assigned this typing after its introduction.
                                If filtering by generation with the radio buttons below,
                                better results may be had by including Pokemon through 
                                at least Generation II.",
                                br()
               ),
               
               sliderInput("trainSplit", "Percent of data to train on:",
                           min = 5,
                           max = 95,
                           value = 60),
               
               selectInput("myModel", "Choose the model you wish to build:",
                           c("Linear" = "lm",
                             "Decision tree" = "tree",
                             "Random forest" = "randForest")),
               
               conditionalPanel(condition = "input.myModel == 'lm'",
                                checkboxGroupInput("lmVars", "Select linear model variables",
                                                  c("Height" = "height",
                                                    "Weight" = "weight",
                                                    "HP" = "hp", 
                                                    "Attack" = "attack",
                                                    "Defense" = "defense",
                                                    "Special Attack" = "special.attack",
                                                    "Special Defense" = "special.defense",
                                                    "Speed" = "speed"
                                            ))),
               
               conditionalPanel(condition = "input.myModel == 'tree'",
                                checkboxGroupInput("treeVars", "Select decision tree variables",
                                                   c("Height" = "height",
                                                     "Weight" = "weight",
                                                     "HP" = "hp", 
                                                     "Attack" = "attack",
                                                     "Defense" = "defense",
                                                     "Special Attack" = "special.attack",
                                                     "Special Defense" = "special.defense",
                                                     "Speed" = "speed"
                                                   )),
                                numericInput("treecp", "Select complexity parameter value:",
                                             0.01,
                                             min = 0.001,
                                             max = 0.1,
                                             step = 0.001),
                                h4("OR..."),
                                checkboxInput("cpAuto", "Auto-tune complexity parameter")),
               
               conditionalPanel(condition = "input.myModel == 'randForest'",
                                checkboxGroupInput("randForestVars", "Select random forest variables:",
                                                   c("Height" = "height",
                                                     "Weight" = "weight",
                                                     "HP" = "hp", 
                                                     "Attack" = "attack",
                                                     "Defense" = "defense",
                                                     "Special Attack" = "special.attack",
                                                     "Special Defense" = "special.defense",
                                                     "Speed" = "speed"
                                                   )),
                                numericInput("randForestmtry", "Select mtry value:",
                                             4,
                                             min = 1,
                                             max = 8),
                                h4("OR..."),
                                checkboxInput("mtryAuto", "Auto-tune mtry")),
               actionButton("buildModels", "Build models!"),
               
               uiOutput("trainModelTitle"),
               tableOutput("trainModelOutput"),
               uiOutput("trainModelFootnote"),
               #tableOutput("glmConfusionMatrix"),
               uiOutput("testModelTitle"),
               tableOutput("testStats")
               ),
      
      tabPanel("Prediction",
               conditionalPanel("input.buildModels == 0",
               h2("Build models first!")
               ),
               conditionalPanel("input.buildModels != 0",
                                radioButtons("predictSelect", "Select prediction type:",
                                             c("Custom input" = "custom",
                                               "Pokemon of predicted type" = "myType",
                                               "Pokemon NOT of predicted type" = "notMyType")),
                                conditionalPanel("input.predictSelect == 'custom'",
                                                 div("Customize height/weight and base stats to see what the various models will predict. 
                                                     Maximum and minimum slider values reflect the overall maximum and minimum values for the 
                                                     Pokemon used in building the models. Values are present at the overall median values."),
                                                 br(),
                                                  uiOutput("heightSlider"),
                                                  uiOutput("weightSlider"),
                                                  uiOutput("hpSlider"),
                                                  uiOutput("attackSlider"),
                                                  uiOutput("defenseSlider"),
                                                  uiOutput("specialAttackSlider"),
                                                  uiOutput("specialDefenseSlider"),
                                                  uiOutput("speedSlider"),
                                                  tableOutput("customPokeTable")),
                                conditionalPanel("input.predictSelect == 'myType'",
                                                 div("All Pokemon of the predicted type are below. Select one to see if the models will 
                                                     correctly predict."),
                                                 br(),
                                                  uiOutput("myTypeList"),
                                                 tableOutput("rightTypePokeTable")),
                                conditionalPanel("input.predictSelect == 'notMyType'",
                                                 div("All Pokemon NOT of the predicted type are below. Select one to see if the models will 
                                                     correctly predict."),
                                                 br(),
                                                 uiOutput("notMyTypeList"),
                                                 tableOutput("wrongTypePokeTable")),
                                actionButton("predict", "Predict!")
               )
               ),
      
      "Data",
      tabPanel("Subset and Export",
               h2("This panel will allow the user to subset and export the data as they see fit"),
               checkboxGroupInput("exportVars", "Select variables to include:",
                                  choices = names(myData), selected = names(myData)),
               actionButton("download", "Download as .csv"))
      
    ),
    sidebarPanel(radioButtons(
      "gens",
      "Include Pokemon through Generation:",
      c("I" = "151",
        "II" = "251",
        "III" = "386",
        "IV" = "493",
        "V" = "649",
        "VI" = "721",
        "VII" = "809",
        "VIII" = "905",
        "IX" = "1010"),
      selected = "1010",
      inline = TRUE
    ))
)