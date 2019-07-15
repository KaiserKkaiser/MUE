if(!require(shiny)){install.packages("shiny")}
library(shiny)
if(!require(ggplot2))(install.packages("ggplot2"))
library(ggplot2)
source("MUE_code.r")
mue_server <- function (input, output) {
    ### Process Sample Data ###
    dat.in<-read.csv(paste0("example_B_CVs.csv"),header=T)
    output$sample <- downloadHandler(
        filename = "sampleData.csv",
        content = function(file) {
            write.csv(dat.in, file, row.names=FALSE)
        }
    )

    # output$contents <- renderTable({
        # Display the data as a table; testing method
    M_vals_all<- reactive({
    inFile <- input$file1
    if(is.null(inFile))
        return(NULL)
    M_vals_all<- data.frame(read.csv(inFile$datapath, header = T))    
        # rv <- reactiveValues(index, CVs, years)
    })

    #### Uncomment to display raw data ##### Testing only #######
    # output$rawData <- renderTable({
    #     if(!anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
    #     indexNum <- (((ncol(M_vals_all())-1)/2)+1)
    #     index <<- M_vals_all()[, 2 : indexNum]
    #     CVs<<-M_vals_all()[,(((ncol(M_vals_all())-1)/2)+2):ncol(M_vals_all())]
    #     years<<-M_vals_all()[,1]
    #     # CVs
    #     # years
    #     # index
    #     }
    #    #  M_vals_all()
    #    ans <- c(min(CVs), max(CVs))
    #    df <- data.frame(x=rep(1:5, 9), val=sample(1:100, 45), 
    #                variable=rep(paste0("category", 1:9), each=5))
    #     newY <- years
    #     newA <- index[, 1]
    #     color <- rep("Area1", times=length(years))
    #     for(i in 2:length(index)) {
    #         # newY <- newY.cbind(years)
    #         # newA <- newA.cbine(index[, i])
    #         newY <- c(newY, years)
    #         newA <- c(newA, index[, i])
    #         color <- c(color, rep(paste0("Area", i), times=length(years)))
    #     }
    #     data <- data.frame(newY, newA, color)
    #     data
    #     # numberOfCluster <- as.numeric(input$noC)
    #     # numberOfCluster
    # })

 ### Plot the input values when people upload a file ###
    output$inputData1 <- renderPlot({
        inFile <- input$file1
        if(!is.null(input$file1)) {
        newY <- years
        newA <- index[, 1]
        # newC is potentially the span
        newC <- CVs[, 1]
        color <- rep("Area1", times=length(years))
        for(i in 2:length(index)) {
            # newY <- newY.cbind(years)
            # newA <- newA.cbine(index[, i])
            newY <- c(newY, years)
            newA <- c(newA, index[, i])
            newC <- c(newC, CVs[, i])
            color <- c(color, rep(paste0("Area", i), times=length(years)))
        }
        data <- data.frame(newY, newA, color)
        rdp <<- reactive(ggplot(data, aes(x=newY, y=newA)) + geom_line(aes(colour=color)) + ggtitle("Area with respect of time(year)"))
        print(rdp())
        }
    })

    ### Download Raw Data ###
    output$rawDataDownload <- downloadHandler(
        filename = "rawData.png",
        content = function(file) {
            png(file)
            print(rdp())
            dev.off()
        })

 ### Let the user to choose either of the methods

    #Hubert's gamma for the assignment clusters
    output$huresult <- renderUI({
        if(input$button == 1 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
        numberOfCluster <- as.numeric(input$noC)
        # 1000 here is the simulate population that user wants to run
        spp.Hg<<-CPUE.sims.SPP(index,numberOfCluster,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=2,Z_score=T)
        spp.Hg
        }
    })

    output$silhresult <- renderUI({
        if(input$button == 0 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
        numberOfCluster <- as.numeric(input$noC)
        spp.Sil<<-CPUE.sims.SPP(index,numberOfCluster,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=1,Z_score=T) 
        spp.Sil
        }
    })

    output$huplot <- renderPlot({
        if(input$button == 1 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
        # spp.Hg<<-CPUE.sims.SPP(index,1000,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=2,Z_score=T)
        huPlot<<-plot(pam(spp.Hg$D.matrix,2,diss=TRUE), main="HUBERT's GAMMA used for cluster assignment")
        abline(v=c(0.25,0.5,0.75),col="red",lwd=c(1,2,3))
        print(huPlot)
        }
    })
        # Download Hubert's gamma plot #
    output$huplotDownload <- downloadHandler(
        filename = "hub.png",
        content = function(file) {
            png(file)
            print(huPlot)
            dev.off},
        contentType = "image/png"
    )

#### Plot: 
    # Avg.Sil
    # Hubert 
    # horizontal: the number of cluster - Final.Cluster.Stats;
                    # or maybe just take the length of the vertical data, then start at 2
#### Based on the result above, allow user to choose number of cluster


    output$silplot <- renderPlot({
        if(input$button == 0 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
        # spp.Sil<-CPUE.sims.SPP(index,1000,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=1,Z_score=T) 
        
        # 2 after the matrix is the number of the user that user wants to choose
        silPlot <- plot(pam(spp.Sil$D.matrix,2,diss=TRUE), main="SILHOUETTE used for cluster assignment")
        abline(v=c(0.25,0.5,0.75),col="red",lwd=c(1,2,3))
        print(silPlot)
        }
    })



shinyServer(mue_server)
}