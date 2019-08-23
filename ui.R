require(shiny)
require(shinyFiles)
require(ggplot2)

if(interactive()) {
mue_ui <- fluidPage(
    titlePanel("MUE GUI"),
    p("The Management Unit Estimator (MUE) is a tool that looks for structure in indices of abundance given the amount of uncertainty contained in those indices. By including uncertainty in the clustering of yearly index values, this method takes into consideration both measurement and process error when determining population structure on ecological time scales. Results from this can help identify proper stock assessment or management units, as well as important localized dynamics relevant to other ecological studies."),
    p("The MUE requires an input of relative or absolute abundance measures and the associated uncertainty measured as a coefficient of variation. An example of what the simple data input looks can be found by clicking on the \"Sample Data Download\"  button. Further reading on this method can be found in Cope and Punt 2009."),
    sidebarLayout(
        sidebarPanel(
    # Browse files
    
    downloadButton("sample", "Sample Data Download"),
    # p("Note: Input data must have no less than 3 areas data.", style = "font-family: 'times'; font-si18pt"),
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
    p("Note: Input data must have no less than 3 areas.", style = "font-family: 'calibri'; font-si20pt"),
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
    p("Go to \"RUN Cluster Validity Diagonistics\" panel to see the result.", style = "font-family: 'calibri'; font-si20pt"),
    ## Second part, choose the number of cluseters##
    textInput("noC", "Number of Clusters"),
    verbatimTextOutput("Number of Clusters"),
    p("Go to \"MUE\" panel to see the final result of MUE based on the clusters.", style = "font-family: 'calibri'; font-si20pt")
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
        tabPanel("Run Cluster Validity Diagonistics",
        p("Below are plots that show how many clusters are best supported by the data for two cluster validity diagnostics:", style = "font-family: 'times'; font-si18pt"),
        p("1) Hubert gamma ", span("(top panel)", style = "color:grey"), "2) Silhouette ", span("(bottom panel)", style = "color:grey"), ". ", style = "font-family: 'times'; font-si18pt"),
        p("The cluster with the highest value is best supported by the data and can be used to determine the final number of clusters to input into the \"Number of Clusters\" box on the left."),
        plotOutput("comparePlotHuHu"),
        downloadButton("cphh", "Download Cluster Validity plot for Hubert Gamma metric"),
        plotOutput("comparePlotHuSil"),
        downloadButton("cphs", "Download Cluster Validity plot for Silhouette metric")
       ),
        tabPanel("MUE",
        plotOutput("huplot"),
        downloadButton("huplotDownload", "Download Plot"),
        downloadButton("resultDownload", "Download R object of results")
            )
    )
    )
)
)
shinyUI(mue_ui)
}