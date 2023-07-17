library(shiny)

fluidPage(

    # Application title
    titlePanel("Project 3"),
    
    navlistPanel(
      "About",
      tabPanel("Start here!",
               h3("Welcome!"),
               "The purpose of this app is to explore whether or not one can predict a Pokemon's typing based on its base stats.",
               h4("Data Exploration"),
               "These tabs allows for graphical and numerical exploration of Pokemon types and base stats. Play around as you see fit, generate figures/summaries, and see if certain stats seem to associate with certain types. Use this info to inform your model!",
               h4("Modeling"),
               "These tabs provide basic information about some of the modeling approaches, and allows you to design, fit, and train a model to predict Pokemon type. Finally, you can put your model to the test to see if it can correctly predict a Pokemon's typing.",
               h4("Subset and Export"),
               "Create your very own subset of Pokemon data, export it, and use it as you see fit!",
               br(),
               br(),
               "Note, the radio buttons at the bottom allows you to filter the data used by generation."
               ),
      "----------",
      
      "Data Exploration",
      tabPanel("Graphical Summaries",
               h2("This panel will allow the user to generate graphical summaries of the data")),
      
      tabPanel("Numerical Summaries",
               h2("This panel will allow the user to generate numeric summaries of the data")),
      
      "Modeling",
      tabPanel("Modeling Info",
               h2("This panel will provide general information about modeling")),
      
      tabPanel("Model Fitting",
               h2("This panel will allow the user to fit models")),
      
      tabPanel("Prediction",
               h2("This panel will allow the user to run predictions with their fit models")),
      
      "Data",
      tabPanel("Subset and Export",
               h2("This panel will allow the user to subset and export the data as they see fit"))
      
    ),
    sidebarPanel(radioButtons(
      "gens",
      "Include Pokemon through Generation:",
      c("I" = "gen1",
        "II" = "gen2",
        "III" = "gen3",
        "IV" = "gen4",
        "V" = "gen5",
        "VI" = "gen6",
        "VII" = "gen7",
        "VIII" = "gen8",
        "IX" = "gen9")
    ))
)