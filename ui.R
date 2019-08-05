require(shiny)
require(shinyFiles)
require(ggplot2)

if(interactive()) {
mue_ui <- fluidPage(
    titlePanel("MUE GUI"),
    sidebarLayout(
        sidebarPanel(
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
    actionButton("rs", "Data Read In"),
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
    verbatimTextOutput("Number of Clusters")
        ),
    # Display the results
    mainPanel(
        tabsetPanel(
            tabPanel("Raw Data First Glance",
        tableOutput("contents"),
        tableOutput("rawData"),
        plotOutput("inputData1"),
        plotOutput("inputData2"),
        downloadButton("rawDataDownload", "Download Raw Data Plot"),
        downloadButton("rawDataDownloadCV", "Download Raw Data CV Plot")
        ),
            tabPanel("Compare Plots",        
        plotOutput("comparePlotHuHu"),
        downloadButton("cphh", "Download Compare Plot of Hubert Gamma Hu"),
        plotOutput("comparePlotHuSil"),
        downloadButton("cphs", "Download Compare Plot of Silhouette Hu"),
        plotOutput("comparePlotSilSil"),
        downloadButton("cpss", "Download Compare Plot of Silhouette Sil"),
        plotOutput("comparePlotSilHu"),
        downloadButton("cpsh", "Download Compare Plot of Hubert Gamma Sil")
            ),
            tabPanel("Final Result",

        plotOutput("huplot"),
        downloadButton("huplotDownload", "Download Hubert's Gamma Plot"),
        plotOutput("silplot"),
        downloadButton("silplotDownload", "Download Silhouette plot"),
        uiOutput("huresult"),
        uiOutput("silhresult"),
        downloadButton("resultDownload", "Download final result")
            )
    )
    )
)
)
shinyUI(mue_ui)
}