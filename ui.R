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
    # Display the data input as a table
    mainPanel(
        tableOutput("rawData"),
        plotOutput("Oplot")
    )
)
shinyUI(mue_ui)
}