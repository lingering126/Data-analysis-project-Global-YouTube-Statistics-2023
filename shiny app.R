library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggplot2)
#library(plotly)
library(DT)
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(NbClust)

library(rpart.plot)
library(ggpubr)

load("1.Rdata")



data <- Youtubesta5[, c('subscribers', 'video.views',"earnings_views",
                        "videoviews_monthlygrowthrate", "subscribers_for_last_30_days",
                        'highest_monthly_earnings_fix1',"Gross.tertiary.education.enrollment....","Population")]


data_scaled <- scale(data)
#num_1 <- NbClust(data_scaled, min.nc=2, max.nc=10, method="kmeans")  
#table(num_1$Best.n[1,]) 


#
d <- dist(data_scaled) 

fit.average <- hclust(d, method="average") 

#table(clusters)





ui <- dashboardPage(
  dashboardHeader(title = "Youtube analysis"),
  dashboardSidebar(
    
    sliderInput("clustering",
                "clustering",
                min = 1,
                max = 10,
                value = 2),
    
    selectInput(inputId = 'feature_selection',
                label = 'feature_selection',
                choices = c("Fisher" = "Fisher",
                            "Information gain" = "Information gain"
                            
                ),
                selected = "Fisher"),
    
    
    
    selectInput(inputId = 'classifier',
                label = 'classifier',
                choices = c("decision tree" = "decision tree",
                            "Bayes" = "Bayes"
                            
                            
                ),
                selected = "decision tree")
    
    
  ),
  
  dashboardBody(
    
    tabBox(
      title = " ", height = "1440px", width = 36, 
      
      tabPanel("single variable", 
               
               
               
               plotOutput("Plot0"),
               plotOutput("Plot_0")
               
               
               
      ),
      
      
      tabPanel("data summary", 
               
               
               
               dataTableOutput('table')
               
               
               
               
      ),
      tabPanel("two classification", 
               
               
               plotOutput("Plot6")
               
               
      ),
      
      
      tabPanel("K-means", 
               
              
               plotOutput("Plot1"),
               plotOutput("Plot2"),
               plotOutput("Plot3")

               
      ),
      
      
      tabPanel("hierarchical clustering", 
               
               plotOutput("Plot4"),
               plotOutput("Plot5")
               
               
      ),
      
      
      
      
    ),
    
  )
)



server <- function(input, output) {
  
  
  data1=reactive(
    {
      results_df
    }
  )
  
  
  data2=reactive(
    {
      
      data_scaled
      
    }
  )
  
  
  
  
  
  
  
  
  output$table <- renderDataTable(data1()) 
  
  
  output$Plot0<- renderPlot({
    
    
    feature_names_nb1 <- c("PGDP2022")
    for (feature in feature_names_nb1) {
      print(nb_density_plot(nb_plot1_data, feature, nb_plot1_data$nbpred1_train))
    }
    
    
  })
  
  output$Plot_0<- renderPlot({
    
    
    # Create separate density plots for each feature
    feature_names_nb4 <- c("video_views")
    for (feature in feature_names_nb4) {
      print(nb_histogram_plot(nb_plot4_data, feature, nb_plot4_data$nbpred_videoviews))
    }
    
    
  })
  
  
  output$Plot6<- renderPlot({
    
    
    if(input$feature_selection=='Fisher' &&input$classifier=='decision tree'){
      rpart.plot(tree_model1, extra = 101, type = 2, fallen.leaves = TRUE)
    }
    if(input$feature_selection=='Information gain' &&input$classifier=='decision tree'){
      rpart.plot(tree_model3, extra = 101, type = 2, fallen.leaves = TRUE)
      
    }
    if(input$feature_selection=='Fisher' &&input$classifier=='Bayes'){
      # Create separate density plots for each feature
      feature_names_nb2 <- c("highest_monthly_earnings",  
                             "Gross_tertiary_education_enrollment", "Population","video_views_monthly_growth_rate")
      for (feature in feature_names_nb2) {
       print(nb_density_plot2(nb_plot2_data, feature, nb_plot2_data$nbpred2_train))
     }
   
    
      
    }
    if(input$feature_selection=='Information gain' &&input$classifier=='Bayes'){
      # Create separate density plots for each feature
      feature_names_nb3 <- c("highest_monthly_earnings", 
                             "video.views", "earnings_views","subscribers_for_last_30_days","subscribers", "video_views_monthly_growth_rate")
      for (feature in feature_names_nb3) {
        print(nb_density_plot3(nb_plot3_data, feature, nb_plot3_data$nbpred3_train))
      }
    }
    
    
  })
  
  
  
  
  
  output$Plot1<- renderPlot({
    
    
    # Elbow method  
    p1<-fviz_nbclust(data2(), kmeans, method = "wss") +
      geom_vline(xintercept = 2, linetype = 2)+
      labs(subtitle = "Elbow method")
    p1
    
    
  })
  
  
  
  output$Plot2<- renderPlot({
    
    k5 <- kmeans(data_scaled, centers = input$clustering, nstart = 25)
    k5
    
    p5<-fviz_cluster(k5, data = data2(), geom="point",main = "K-means plot")
    
    p5

    
   
    
    
  })
  
  
  output$Plot3<- renderPlot({
    k5 <- kmeans(data_scaled, centers = input$clustering, nstart = 25)
    k5
    
    Pk5<- data2() %>%
      as_tibble() %>%
      mutate(cluster = k5$cluster
      ) %>%
      ggplot(aes(video.views,highest_monthly_earnings_fix1, color = factor(cluster)))+
      geom_point()
    
    Pk5
    
  })
  
 
  
  
  
  output$Plot4 <- renderPlot({
    
    clusters <- cutree(fit.average, k=input$clustering)
    
    p_hclust<-fviz_cluster(list(data = data2(), cluster = clusters),main = "hierarchical  clustering plot")  ## from ‘factoextra’ package 
    p_hclust
    
    
    
    
  })
  output$Plot5 <- renderPlot({
    clusters <- cutree(fit.average, k=input$clustering)
    
    
    p_hclust_5<- data2() %>%
      as_tibble() %>%
      mutate(cluster = clusters
      ) %>%
      ggplot(aes(video.views,highest_monthly_earnings_fix1, color = factor(cluster)))+
      geom_point()+
      labs(title = "hierarchical clustering ")
    
    
    
    p_hclust_5
    
        
        
    
    
  })
  
  
  
  
  
}
shinyApp(ui, server)
