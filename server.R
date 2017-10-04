# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  setwd("/media/DataDir/ShinyApps/ScAndTech_PCA")
  FilesToRead <- list.files(path = getwd(), pattern = ".csv$", recursive = T, full.names = T)
  FilesToRemove <- grep(FilesToRead, pattern = "Metadata", ignore.case = T)
  FilesToRemove <- c(FilesToRemove,grep(FilesToRead, pattern = "mod", ignore.case = T))
  FilesToRead <- FilesToRead[-FilesToRemove]
  library(ggplot2)
  library (stringr)
  library (readr)
  library (dplyr)
  library (ggrepel)
  ### Read Data ######
  MyFullData <- NULL
  MyFullData <- list(MyFullData)
  for (i in 1:length(FilesToRead))
  {
    MyFullData[[i]] <- read_csv(file = FilesToRead[i], skip = 4)
  }
  MyFullData_2015 <- NULL
  MyData_BM_2015 <- list(MyFullData_2015)
  ##############################
  output$LinePlot <- renderPlot({
    #### Process data and Normalize ###########
    if (length(input$YearToPlot) < 1)
    {
      YearToLook <- "2010"
    } else
    {
      YearToLook <- input$YearToPlot
    }
    
    
    for (i in 1:length(MyFullData))
    {
      colnames(MyFullData[[i]])[1] <- "CountryName"
      colnames(MyFullData[[i]]) <- str_replace_all(colnames(MyFullData[[i]]), " ","")
      TempPrefix <- levels(factor(MyFullData[[i]]$IndicatorName))
      TempPrefix <- str_replace_all(TempPrefix, " ", "_")
      colnames(MyFullData[[i]])[5:61] <- paste0(TempPrefix, "_Yr", colnames(MyFullData[[i]])[5:61])
      ExtractedYear <- colnames(MyFullData[[i]])[grep(colnames(MyFullData[[i]]), pattern = paste0("Yr", YearToLook))]
      ExtractedYear <- grep(colnames(MyFullData[[i]]), pattern = paste0("Yr", YearToLook))
      MyFullData_2015[[i]] <- MyFullData[[i]] %>% select(CountryName,ExtractedYear)
      RemoveIndex <- !(is.na(MyFullData_2015[[i]][,2]))
      MyFullData_2015[[i]] <- MyFullData_2015[[i]][RemoveIndex,]
    }
    JoinedTibble <- inner_join(MyFullData_2015[[1]], MyFullData_2015[[2]])
    i <- 3
    while ( i <= length(MyFullData_2015))
    {
      JoinedTibble <- inner_join(JoinedTibble, MyFullData_2015[[i]])
      i <- i + 1
    }
    
    JoinedTibble.df <- as.data.frame(JoinedTibble)
    My_Normalize_FUN <- function (x, MyData)
    {
      if ((class(MyData[,x]) == "numeric" | class(MyData[,x]) == "integer"))
      {
        TempValues <- MyData[,x]
        TempValues <- TempValues - mean(TempValues)
        TempValues <- TempValues / (max(TempValues) - min(TempValues))
        MyData[,x] = TempValues
      }
      return(MyData)
    }
    
    
    ColsToNormalize <- 2:ncol(JoinedTibble.df)
    for (i in ColsToNormalize)
    {
      JoinedTibble.df <- My_Normalize_FUN (x = i, MyData = JoinedTibble.df)
    }
    pca <- prcomp(JoinedTibble.df[,2:ncol(JoinedTibble.df)])
    #########################
    DataToPlot <- as.data.frame(pca$x)
    DataToPlot$CountryName <- JoinedTibble.df$CountryName
    #### k-means #####
    MyClusts <- kmeans (x = as.matrix(DataToPlot[,1:2]), centers = input$clusters, iter.max = 100)
    DataToPlot$Clusters <- MyClusts$cluster
    
    p1 <- ggplot(data = DataToPlot) + geom_point(aes(PC1, PC2, color = factor(Clusters)), size = 3.5)
    p1 <- p1 + geom_text_repel(data = DataToPlot, aes(PC1, PC2, label = CountryName))
    p1 <- p1 + theme(panel.background = element_rect(fill = "white"),
                     panel.grid.major = element_line(color = "grey90"),
                     plot.background = element_rect(fill = "grey90"),
                     axis.title.x = element_text(size = 15),
                     axis.title.y = element_text(size = 15),
                     legend.title = element_blank(),
                     legend.position = "none",
                     legend.text = element_text(size = 12),
                     axis.text.x = element_text(size = 12),
                     axis.text.y = element_text(size = 12))
    p1 <- p1 + labs (title = paste0("Countries clustered based on their 'Science and Technology' indicators from year ", YearToLook), x = "Principal Component 1", y = "Principal Component 2")
    p1
    
    
    
  })
  
}
