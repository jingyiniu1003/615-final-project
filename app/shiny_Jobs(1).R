library(dplyr)
library(ggplot2)
library(wordcloud)
library(tidytext)
library(shiny)
library(jsonlite)


load("jobs.rdata")



ui <- navbarPage(
  h5("Jobs Shiny App",style="color: orange;"),
  
  tabPanel(

    "Job boards",
    sidebarLayout(
      
      sidebarPanel(
     
        h3("select job boards",
           style = "font-weight: 500; color: #4d3a7d;"),
        
        #css tyle 3
        selectInput("y", h4("select a supported job board", style = "color: red;"), 
                    choices = c("cybercoders","Github jobs")),
        sliderInput("y3", "how many words display?", min = 10, 100, value = 50),
        
        selectInput("y2", h4("select a salary level", style = "color: red;"), 
                    choices = c("all level", "high level > $100k","low level < $100k"))
        
      ),
      
      mainPanel(
        
        tabsetPanel(
          
          tabPanel("Jobs bar plot", plotOutput("plot1")),
          tabPanel("Jobs wordcloud", plotOutput("plot2"))
          
        )
      )
    )
    
  )
  
)


server <- function(input, output) {
  
  mydata <- reactive({
  
    input$y
  })
  
  
  mydata2 <- reactive({
    input$y2
  })
  
  mydata3 <- reactive({
    input$y3
  })
  
  output$plot2 <- renderPlot( {
    if(mydata() == "cybercoders") {
      dt2$job_position <- as.character(dt2$job_position)
      r3 <- dt2 %>%
        unnest_tokens(word, job_position) %>%
        count(  word, sort = TRUE) %>% filter(nchar(word) > 6)
      head(r3)
      
      r3 <- data.frame(r3)
      wordcloud(r3[,1],r3[,2], max.words = mydata3())
    } else {
      r2 <- r %>%
        unnest_tokens(word, description) %>%
        count(  word, sort = TRUE) %>% filter(nchar(word) > 6)
      head(r2)
      
      r2 <- data.frame(r2)
      wordcloud(r2[,1],r2[,2], max.words = mydata3())
    }
   
  })
  
  output$plot1 <- renderPlot( {
    if(  mydata2() ==  "all level") {
      skills <- unlist(strsplit(dt2$preferred_skills, split = ";"))
      df <- as.data.frame(table(skills))
      
      df2 <- df %>% arrange(-Freq) %>% slice(1:10)
      df2
      df2$skills <- factor(df2$skills, levels = df2$skills)
      df2 %>% ggplot(aes(skills, Freq)) + geom_col(fill="lightblue") 
    } else if(mydata2() == "high level > $100k") {
      skills <- unlist(strsplit(dt2$preferred_skills[dt2$salary %in% c(
        "$110k - $150k" ,
        "$100k - $150k" ,
        "$100k - $130k" )], split = ";"))
      df <- as.data.frame(table(skills))
      df2 <- df %>% arrange(-Freq) %>% slice(1:10)
      df2
      df2$skills <- factor(df2$skills, levels = df2$skills)
      df2 %>% ggplot(aes(skills, Freq)) + geom_col(fill="lightblue") + ggtitle("High level salary - lowest salary > dollars 100k") 
    } else {
      skills <- unlist(strsplit(dt2$preferred_skills[!dt2$salary %in% c(
        "$110k - $150k" ,
        "$100k - $150k" ,
        "$100k - $130k" )], split = ";"))
      df <- as.data.frame(table(skills))
      df2 <- df %>% arrange(-Freq) %>% slice(1:10)
      df2
      df2$skills <- factor(df2$skills, levels = df2$skills)
      df2 %>% ggplot(aes(skills, Freq)) + geom_col(fill="lightblue") + ggtitle("Low level salary - lowest salary < dollars 100k") 
    }
     
  })

  

  
}


shinyApp(ui, server)
