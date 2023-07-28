source("helper.R")

fluidPage(
  withMathJax(),
    # Application title
    titlePanel("Project 3"),
    
    navlistPanel(
      "About",
      tabPanel("Start here!",
               h3("Welcome!"),
               "The purpose of this app is to explore whether or not one can 
               predict a Pokemon's typing based on its base stats, height, and weight. 
               This is accomplished by selecting a type, determining if each Pokemon's
               primary or secondary type ", em("is"), " said type, and binarily classifying 
               it as 'Type' or 'Not_Type'. The models then attempt to predict this binary
                membership for each Pokemon",
               p(),
               "Although the data used by this app is imported from a .csv file to expedite its
               startup time, the data in said .csv was originally queried from", tags$a(href= "http://pokeapi.co", "PokeApi"),
               ". Custom R functions to interact with this API (which were used to create the .csv) can be found on my", 
               tags$a(href= "http://dadambro.github.io/project-1/", "Project-1"), " page.",
               
               h4("Data Exploration"),
               "These tabs allows for graphical and numerical exploration of 
               Pokemon types and base stats. Play around as you see fit, 
               generate figures/summaries, and see if certain stats/heights/weights seem to 
               associate with certain types.",
               
               h4("Modeling"),
               "These tabs provide some basic information about the modeling 
               approaches, and allows you to design, fit, and train a model to 
               predict Pokemon type. Finally, you can put your models to the test
               to see if it can correctly predict a Pokemon's typing.",
               
               h4("Subset and Export"),
               "Create your very own subset of Pokemon data, export it, and use 
               it as you see fit!",
               p(),
               h4("General notes"),
               "The radio buttons at the bottom allows you to filter the 
               data used by generation. As the Pokemon franchise has expanded, 
               one might expect certain trends related to base stats and type 
               seen in earlier games (e.g., 'Electric Pokemon have high speed!') 
               to become weaker, as more diverse Pokemon with various typing/stat 
               combinations are introduced into the game. The radio buttons 
               allow one to explore this idea by seeing if limiting the dataset 
               causes trends to become more apparent, models to perform 
               differently, etc. HOWEVER, there is a tradeoff here; these data are 
               inherently imbalanced, as there are always more Pokemon that are 'not' of 
               a specific type versus the number of Pokemon that are of said type, 
               regardless of generation. This imbalance tends to become more 
               pronounced in earlier generations of Pokemon games. Sometimes, 
               filtering data by an early generation will not allow models 
               to be properly generated for certain types. In general, it is best
               to include all 1010 Pokemon (i.e., throuh Generation IX) for best 
               modeling performance.",
               p(),
               "Below is a random image of a Pokemon retrieved from the web. 
               Note that if the image appears to be broken, check your Internet 
               connection/try running this app externally in a browser instead of 
               within R. A new image can be retrieved by clicking the button below. 
               The radio buttons controlling 'Generation' also work on the picture as well;
                selecting 'Generation I' will only return images from Pokemon etc.",
               p(),
               "In the Pokemon games, there is always a small chance of a encountering a 'shiny' Pokemon 
               (i.e, a Pokemon with a unique color palette relative to the original design). To reflect that 
               (and since we ", em("are"), " working with R 'Shiny'), there is a 1 in 25 chance of a shiny Pokemon image
               appearing below:",
               p(),
               actionButton("newPic", "Generate new picture!"),
               p(),
               uiOutput("picName"),
               uiOutput("randomPic")
                              ),
      "----------",
      
      "Data Exploration",
      tabPanel("Graphical Summaries",
               h3("Graphical Summaries"),
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
               h3("Numerical Summaries"),
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
               h3("Modeling Information"),
               p(),
               "This app allows for the creation of models using 3 different approaches:",
               
               h4("Generalized Linear Model"),
               p(),
               "The generalized linear model in this app uses logistic regression to model the 
               probability of a Pokemon belonging to a specified type. A generic example of such 
               a model is below:",
               p(),
               "$$ln(p/1-p) = B_0 + B_1 x_1$$",
               p(),
               "Where 'x subscript 1' is some predictor variable (e.g., 'base attack'). An advantage of generalized linear 
               models is that they are fairly easy to interpret for someone with some basic background knowledge 
               of general statistics; one can view coefficient estimates and significance to deduce both if and how the various 
               predictors are impacting the overall estimate. It should be noted that for the particular model in this app, the estimate is the 'log odds,' 
               which may not be as straightforward for one to deduce. Disadvantages of the generalized linear models include reliance on 
               certain assumptions between the response and predictors (which many not always hold) and an inability to account 
               for interactions between predictors unless explicitly coded to do so. As of now, this app does not have the ability to account 
               for interactions among predictors in the generalized linear model.",
               p(),
               "In addition to providing some fit statistics related to the training and test datasets, the app provides a confusion matrix for 
               the generalized linear model, along with a summary output of the various coefficients.",
               p(),
               h4("Decision Tree Model"),
               p(),
               "The decision tree model in this app works by taking the various continuous predictors and establishing a 'threshold' for 
               each; observations then progress through the tree based on their value relative to the various thresholds they encounter until 
               a decision is reached. A major advantage of a decision tree is that it is easy to understand; a human can readily view a decision 
               tree diagram and follow along to generate a prediction. Further, decisions can naturally handle interactions unlike the generalized 
               linear model. There are some disadvantages, however. Generally, decision trees can be quite sensitive to small data changes. They are 
               also prone to overfitting.",
               p(),
               "In addition to providing some fit statistics related to the training and test datasets, the app provides a confusion matrix for the 
               decision tree model, along with a plot of the tree itself.",
               h4("Random Forest Model"),
               "The random forest model in this app functions similarly to the decision tree, but takes an ensemble approach wherein multiple trees 
               are fit to bootstrap samples of the data. Only a random subset of predictors are used in each tree, the value of which is dictated by the 
               'mtry' input in this app. There are a couple advantages of the random tree approach relative to the single decision tree. First, the ensemble 
               approach, which creates multiple trees, can help guard against biases from a single tree. Second, the random subset of variables used in each 
               tree helps to reduce correlation among trees, which in turn allows for a greater reduction in variation once the outcome of all trees are queried. 
               The major disadvantage of the random tree approach is its interpretability; unlike a single decision tree, it is very difficult for a human to 
               'follow along' with the random tree process",
               p(),
               "In addition to provding some fit statistics related to the training and test datasets, the app provides a confusion matrix for the random tree model,
                in addtion to a variable importance plot"),
      
      tabPanel("Model Fitting",
               h3("Model Fitting"),
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
                           value = 80),
               
               selectInput("myModel", "Choose the model you wish to edit:",
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
                                             0.001,
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
                                             1,
                                             min = 1,
                                             max = 8),
                                h4("OR..."),
                                checkboxInput("mtryAuto", "Auto-tune mtry")),
               actionButton("buildModels", "Build models!"),
               
               uiOutput("trainModelTitle"),
               tableOutput("trainModelOutput"),
               uiOutput("trainModelFootnote"),
               br(),
               uiOutput("testModelTitle"),
               tableOutput("testStats"),
               br(),
               selectInput("viewMoreOutput", "View more output for model:",
                           c("Generalized Linear" = "glm",
                             "Classification Tree" = "ct",
                             "Random Forest" = "rf")),
               conditionalPanel("input.viewMoreOutput == 'glm'",
                                verbatimTextOutput("confusionMatrixGLM"),
                                verbatimTextOutput("modelSummaryGLM")),
               conditionalPanel("input.viewMoreOutput == 'ct'",
                                verbatimTextOutput("confusionMatrixClassTree"),
                                plotOutput("plotClassTree")),
               conditionalPanel("input.viewMoreOutput == 'rf'",
                                verbatimTextOutput("confusionMatrixRandomForest"),
                                plotOutput("plotRandomForest"))
               ),
      
      tabPanel("Prediction",
               h3("Prediction"),
               conditionalPanel("input.buildModels == 0",
               h3("Build models first!")
               ),
               conditionalPanel("input.buildModels != 0",
                                radioButtons("predictSelect", "Select prediction type:",
                                             c("Custom input" = "custom",
                                               "Pokemon of predicted type" = "myType",
                                               "Pokemon NOT of predicted type" = "notMyType")),
                                conditionalPanel("input.predictSelect == 'custom'",
                                                 div("Customize height/weight and base stats to see what the various models will predict. 
                                                     Maximum and minimum slider values reflect the overall maximum and minimum values for the 
                                                     Pokemon used in building the models (i.e., the selected Generation). Values are present at the overall median values."),
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
                                                 tableOutput("rightTypePokeInfo"),
                                                 tableOutput("rightTypePokeTable")),
                                conditionalPanel("input.predictSelect == 'notMyType'",
                                                 div("All Pokemon NOT of the predicted type are below. Select one to see if the models will 
                                                     correctly predict."),
                                                 br(),
                                                 uiOutput("notMyTypeList"),
                                                 tableOutput("wrongTypePokeInfo"),
                                                 tableOutput("wrongTypePokeTable")),
                                actionButton("predict", "Predict!")
               )
               ),
      
      "Data",
      tabPanel("Subset and Export",
               h3("Subset and Export"),
               h4("Create a customized subset of data, and export as a .csv with the button below:"),
               checkboxGroupInput("exportVars", "Select variables to include:",
                                  choices = names(myData), selected = names(myData)),
               checkboxInput("typeFilter", "Subset using binary type variable?"),
               conditionalPanel(condition = "input.typeFilter == 1",
                                selectInput("myTypeFilter", "Select type",
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
                                              "Water" = "water")),
               radioButtons("typeFilterOption", "Include or exclude selected type?",
                            c("Include (only Pokemon of selected type will be returned)" = "include",
                              "Exclude (only Pokemon NOT of selected type will be returned)" = "exclude"))),
               actionButton("preview", "Implement changes and preview report"),
               p(),
               conditionalPanel(condition = "input.preview",
                dataTableOutput("allData"),
                actionButton("download", "Download as .csv"),
                p(),
               strong("NOTE!"), "If changes are made to any filters after generating the preview, 
               click the", em("'Implement changes and preview report'"), " button again to confirm the preivew matches 
               what you wish to export. Failure to do so may result in an exported .csv that does not match 
               what you intended to export."))
      
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