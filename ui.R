require(shiny)
require(shinyFiles)

if(interactive()) {
mue_ui <- fluidPage(
    titlePanel("MUE GUI"),
    # Browse files
    fileInput("file1", "Choose MUE input file", 
    multiple = FALSE, 
    accept = c(
        'text/csv',
        'text/comma-separated-values',
        # 'text/tab-separated-values',
        'text/plain',
        '.csv'
        # ,'tmp'
    ),
    buttonLabel = "Browse...",
    placeholder = "No file selected"),

    radioButtons("button", "Choose a cluster", 
                choiceNames = list(
                    "Hubert's Gamma",
                    "Silhouette"
                ),
                choiceValues = list(
                    # 1 for Hubert's Gamma; 0 for Silhouette
                    1, 0
                )),


    # Display the results
    mainPanel(
        tableOutput("rawData"),
        # plotOutput("Oplot")
        uiOutput("huresult"),
        uiOutput("silhresult")
    )
)
shinyUI(mue_ui)
}