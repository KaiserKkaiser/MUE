require(shiny)
require(shinyFiles)
require(ggplot2)

if(interactive()) {
mue_ui <- fluidPage(
    titlePanel("MUE GUI"),
    # Browse files
    downloadButton("sample", "Sample Data Download"),
    fileInput("file1", "Choose MUE input file", 
    multiple = FALSE, 
    accept = c(
        'text/csv',
        'text/comma-separated-values',
        'text/plain',
        '.csv'
    ),
    buttonLabel = "Browse...",
    placeholder = "No file selected"),
    radioButtons("button", "Choose cluster diagonistic", 
                choiceNames = list(
                    "Hubert's Gamma",
                    "Silhouette"
                ),
                choiceValues = list(
                    # 1 for Hubert's Gamma; 0 for Silhouette
                    1, 0
                )),
    textInput("noC", "Number of Clusters"),
    verbatimTextOutput("Number of Clusters"),
    # Display the results
    mainPanel(
        tableOutput("rawData"),
        plotOutput("inputData1"),
        downloadLink("rawDataDownload", "Download Raw Data Plot"),
        plotOutput("huplot"),
        downloadLink("huplotDownload", "Download Hubert's Gamma Plot"),
        plotOutput("silplot"),
        uiOutput("huresult"),
        uiOutput("silhresult")
    )
)
shinyUI(mue_ui)
}