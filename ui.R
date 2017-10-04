library(dplyr)
library(readr)
#########################
setwd("/media/DataDir/ShinyApps/ScAndTech_PCA")
FilesToRead <- list.files(path = getwd(), pattern = ".csv$", recursive = T, full.names = T)
MyData <- read_csv("/media/DataDir/ShinyApps/ScAndTech_PCA/API_BM/API_BM.GSR.ROYL.CD_DS2_en_csv_v2.csv", skip = 4)
YearsToChooseFrom <- colnames(MyData)[5:61]
YearsToChooseFrom <- YearsToChooseFrom[YearsToChooseFrom %in% c(1996:2015)]
#########################

ui <- fluidPage(
  
  
  titlePanel("Science and Technology Development"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      
      selectInput (inputId = "YearToPlot",
                   label = "Year To Plot",
                   choices = YearsToChooseFrom,
                   multiple = F,
                   selectize = F,
                   size = 10
      ),
      numericInput('clusters', 'Cluster count', 4,
                   min = 1, max = 6)
      
    ),
    
    
    mainPanel(
      
      
      plotOutput(outputId = "LinePlot")
      
    )
  )
)
