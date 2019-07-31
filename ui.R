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
    textInput("noS", "Number of Simulations"),
    verbatimTextOutput("Number of Simulation"),

    ### Action buttons ###
    actionButton("rd", "Display result plots"),
    ## Second part, choose the number of cluseters##
    textInput("noC", "Number of Clusters"),
    verbatimTextOutput("Number of Clusters"),
    # Display the results
    mainPanel(
        tableOutput("rawData"),
        plotOutput("inputData1"),
        plotOutput("inputData2"),
        downloadLink("rawDataDownload", "Download Raw Data Plot"),
        downloadLink("rawDataDownloadCV", "Download Raw Data CV Plot"),
        
        plotOutput("comparePlotHuHu"),
        plotOutput("comparePlotHuSil"),
        plotOutput("comparePlotSilSil"),
        plotOutput("comparePlotSilHu"),

        plotOutput("huplot"),
        downloadLink("huplotDownload", "Download Hubert's Gamma Plot"),
        plotOutput("silplot"),
        downloadLink("silplotDownload", "Download Silhouette plot"),
        uiOutput("huresult"),
        uiOutput("silhresult")
    )
)
## TODO: add display panel on the right
shinyUI(mue_ui)
}