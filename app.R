if("networkD3" %in% rownames(installed.packages()) == FALSE) {install.packages("networkD3")}
if("shiny" %in% rownames(installed.packages()) == FALSE) {install.packages("shiny")}
library(shiny)
library(networkD3)


myfile <- file.path("data", "data.csv") 
df <- read.csv(myfile, header=T, sep=",")




#### Server ####
server <- function(input, output) {
  
  
  output$force <- renderForceNetwork({
    
    
    
    if(!grepl(input$country,"All"))
        df <- df[which(df$Country == input$country),]
    
    if(!grepl(input$purpose,"All"))
      df <- df[which(df$Purpose == input$purpose),]
    
    if(!grepl(input$category,"All"))
      df <- df[which(df$Item_category == input$category),]
      
    if(nrow(df) == 0)  {
      
      data <-  data.frame(0,0,0)
      colnames(data) <- c("source","target","value")
      nodes <- data.frame(0,"0",0,0)
      colnames(nodes) <- c("id","name","group","size")
      
      forceNetwork(Links = data, Nodes = nodes, Source = "source",
                   Target = "target", Value = "value", NodeID = "name",
                   Group = "group", opacity = 0.8, fontSize = 36)
      
    }else{
    
      resp <- unique(df$GRespID)
      if(grepl(input$category,"All"))
        values <- unique(df$Item_category)
      else
        values <- unique(df$Standard_item)
      
      if(grepl(input$purpose,"Home")){
        values <- values[-1]
        values <- values[-4]  
      }
        
      #values <- values[-2]
      #values <- values[-2]
    
      data <-  data.frame(source = numeric(0), target = numeric(0), value = numeric(0));
      
      count = 1
      i = 1
      for(e in resp){
        sub <- df[which(df$GRespID==e),]
        total <- sum(sub$Amount)
        j = 1
        for(v in values){
          if(grepl(input$category,"All"))
            sub2 <- sub[which(sub$Item_category== v),]
          else
            sub2 <- sub[which(sub$Standard_item== v),]
          subtotal <- sum(sub2$Amount)
          
          if((total != 0) && (subtotal/total > 0.05)){
            data <- rbind(data,c(i, j + length(resp),1))#(subtotal/total)*20))
            
          }
          
          j = j + 1
          
        }
        i = i + 1
      }
      
      colnames(data) <- c("source","target","value")
      
      nodes <- data.frame(id = numeric(0),name = character(0), group = numeric(0), size = numeric(0))
      for(i in 1:length(resp)){
        tmp <- data.frame(i, as.character(resp[i]),1,10)
        colnames(tmp) <- colnames(nodes)
        nodes <- rbind(nodes,tmp)
      }
      
      for(i in 1:length(values)){
        if(grepl(input$category,"All"))
          sub <- df[which(df$Item_category==values[i]),]
        else
          sub <- df[which(df$Standard_item==values[i]),]
        total <- sum(sub$Amount)
        #print(total)
        tmp <- data.frame(j+length(resp),as.character(values[i]),2,total)
        colnames(tmp) <- colnames(nodes)
        nodes <- rbind(nodes,tmp)
      }
      
      
      data$source = data$source - 1
      data$target = data$target -1
      nodes$id = nodes$id-1    
    
    
    
    forceNetwork(Links = data, Nodes = nodes, Source = "source",
                 Target = "target", Value = "value", NodeID = "name",
                 Group = "group", opacity = 0.8, fontSize = 36)
    }
  })
  output$text1 <- renderText({ 
    
    "Dark blue nodes represent different households, while the light blue ones show the transaction types categorized by country, purpose, and item category."
  })
  output$text2 <- renderText({ 
    "Data Source: http://www.cgap.org/data/data-financial-diaries-smallholder-families#microdata"
  }) 
  
  
}

#### UI ####

ui <- shinyUI(fluidPage(
  
  titlePanel("Financial Diaries Network Analysis"),
  
  sidebarLayout(
    sidebarPanel( width = 3,
                  selectInput("country", "Country List:", choices = c("All",levels(unique(df$Country))),selected = "All"),
                  
                  selectInput("purpose", "Purpose List:", choices = c("All","Business"="B","Home"="H"),selected = "All"),
                  
                  
                  selectInput("category", "Item List:", choices = c("All",levels(unique(df$Item_category))),selected = "All")
                  
      
    ),
    mainPanel(
      tabsetPanel(
        
        tabPanel("Network Graph", 
                 forceNetworkOutput("force"),
                 textOutput("text1"), hr(), textOutput("text2"), hr(), textOutput("text3"))
      )
    )
  )
))



#### Run ####
shinyApp(ui = ui, server = server)