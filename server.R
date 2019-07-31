if(!require(shiny)){install.packages("shiny")}
library(shiny)
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(cluster)){install.packages("cluster")}
if(!require(factoextra)) {install.packages("factoextra")}
if(!require(reshape2)) {intall.packagesrt("reshape2")}
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
    M_vals_all<- eventReactive(input$rd,{
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
    # })

 ### Plot the input values when people upload a file ###

 ################ Initialize data and plot raw data and sample data ###############
    output$inputData1 <- renderPlot({
            inFile <- input$file1
            if(!is.null(input$file1)) {
            newY <- years
            newA <- index[, 1]
            # newC is potentially the span
            newC <- CVs[, 1]
            color <- rep("Area1", times=length(years))
            for(i in 2:length(index)) {
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

    output$inputData2 <- renderPlot({
        inFile <- input$file1
        if(!is.null(input$file1)) {
        newY <- years
        newA <- index[, 1]
        # newC is potentially the span
        newC <- CVs[, 1]
        color <- rep("Area1", times=length(years))
        for(i in 2:length(index)) {
            newY <- c(newY, years)
            newA <- c(newA, index[, i])
            newC <- c(newC, CVs[, i])
            color <- c(color, rep(paste0("Area", i), times=length(years)))
        }
        data2 <- data.frame(newY, newC, color)
        rdp2 <<- reactive(ggplot(data2, aes(x=newY, y=newC)) + geom_line(aes(colour=color)) + ggtitle("CV each Area with respect of time(year)"))
        print(rdp2())
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

    output$rawDataDownloadCV <- downloadHandler(
        filename = "rawDataCV.png",
        content = function(file) {
            png(file)
            print(rdp2())
            dev.off()
        })


 ################################ Run The Result #################################
    #Hubert's gamma for the assignment clusters
    output$huresult <- renderUI({
        if(input$button == 1 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
        numberOfSimul <- as.numeric(input$noS)
        # 1000 here is the simulate population that user wants to run
        spp.Hg<<-CPUE.sims.SPP(index,numberOfSimul,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=2,Z_score=T)
        spp.Hg
        }
    })

    output$silhresult <- renderUI({
        if(input$button == 0 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
        numberOfSimul <- as.numeric(input$noS)
        spp.Sil<<- CPUE.sims.SPP(index,numberOfSimul,rep(1,length(index)),CVs,19,colnames(index),cutoff=1,op.type=c(0,1,0,1,1,1,0,0),k.max.m=1,Z_score=T)
        spp.Sil
        }
    })

    ### Print Comparable Plot for User to Decide the nubmer of clusters ###
    output$comparePlotHuHu <- renderPlot({
        if(input$button == 1 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
            cpHU <- plot(c(2:(length(CVs)-1), spp.Hg$Final.Cluster.Stats$Hubert.gamma), 
                main="Average of HG with Areas, HG method", xlab = "Clusters", ylab = "Average Hubert Gamma")
        }
        print(cpHU)
    })
    output$comparePlotHuSil <- renderPlot({
        if(input$button == 1 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
            cpHU <- plot(c(2:(length(CVs)-1), spp.Hg$Final.Cluster.Stats$Avg.Sil), 
                main="Average of Avg with Areas, HG diagonistic", xlab = "Clusters", ylab = "Average Silhouette")
        }
        print(cpHU)
    })
    ## Sil method ##
    output$comparePlotSilSil <- renderPlot({
        if(input$button == 0 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
            cpSil <- plot(c(2:(length(CVs)-1), spp.Sil$Final.Cluster.Stats$Avg.Sil), 
                main="Average of Sil with Areas, Sil diagonistic", xlab = "Clusters", ylab = "Average Silhouette")
        }
        print(cpSil)
    })
    output$comparePlotSilHu <- renderPlot({
        if(input$button == 0 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
            cpSil <- plot(c(2:(length(CVs)-1), spp.Sil$Final.Cluster.Stats$Hubert.gamma), 
                main="Average of Sil with Areas, Sil diagonistic", xlab = "Clusters", ylab = "Average Silhouette")
        }
        print(cpSil)
    })

####################### Let the User Run with Their Number of Clusters #########################

    output$huplot <- renderPlot({
        if(input$button == 1 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
            numberOfCluster <- as.numeric(input$noC)
            pp <- pam(spp.Hg$D.matrix,numberOfCluster,diss=TRUE)
            hp <- fviz_silhouette(pp)
            avghp <- dcast(hp$data,cluster~1,mean,value.var ="sil_width")
            print(hp + geom_hline(yintercept=0.27, linetype="dashed", 
                color = "red", size=1))
        }
    })
        # Download Hubert's gamma plot #
    output$huplotDownload <- downloadHandler(
        filename = "hub.png",
        content = function(file) {
            png(file)
            print(hp())
            dev.off()
        })

    ## Silhouette's Plot##
    output$silplot <- renderPlot({
        if(input$button == 0 && !anyNA(M_vals_all()) && length(M_vals_all()) > 0) {
        spp <- pam(spp.Sil$D.matrix,2,diss=TRUE)
        sp <- fviz_silhouette(pp)
        print(sp)
        }
    })
## Add lines of avg
    # Download Sil plot #
    output$silplotDownload <- downloadHandler(
        filename = "sil.png",
        content = function(file) {
            png(file)
            print(sp())
            dev.off()
        })

shinyServer(mue_server)
}