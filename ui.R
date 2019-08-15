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
    actionButton("rd", "Run Diagonistics"),
    ## Second part, choose the number of cluseters##
    textInput("noC", "Number of Clusters"),
    verbatimTextOutput("Number of Clusters")
        ),
    # Display the results
    mainPanel(
        tabsetPanel(
            tabPanel("Raw Data First Glance",
        # tableOutput("contents"),
        # tableOutput("rawData"),
        plotOutput("inputData1"),
        downloadButton("rawDataDownload", "Download Raw Data Plot"),
        plotOutput("inputData2"),
        downloadButton("rawDataDownloadCV", "Download Raw Data CV Plot")
        ),
        tabPanel("Compare Plots",
        p("Below are plots that show how many clusters are best supported by the data for two cluster validity diagnostics:", style = "font-family: 'times'; font-si18pt"),
        p("1) Hubert gamma ", span("(top panel)", style = "color:grey"), "2) Silhouette ", span("(bottom panel)", style = "color:grey"), ". ", style = "font-family: 'times'; font-si18pt"),
        p("The cluster with the highest value is best supported by the data and can be used to determine the final number of clusters to input into the \"Number of Clusters\" box on the left."),
        plotOutput("comparePlotHuHu"),
        downloadButton("cphh", "Download Cluster Validity plot for Hubert Gamma metric"),
        plotOutput("comparePlotHuSil"),
        downloadButton("cphs", "Download Cluster Validity plot for Silhouette metric")
       ),
        tabPanel("Final Result",
        plotOutput("huplot"),
        downloadButton("huplotDownload", "Download Plot"),
        downloadButton("resultDownload", "Download final result(.DMP)")
            )
    )
    )
)
)
shinyUI(mue_ui)
}