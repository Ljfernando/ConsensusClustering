#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage("Clustering",
             
    # ~~~~~~~ K MEANS ~~~~~~
    tabPanel("Kmeans",
  
    # Application title
    titlePanel("Kmeans Clustering"),
    
    sidebarLayout(
      sidebarPanel(
         sliderInput("k_k",
                     h3("K"),
                     min = 1,
                     max = 25,
                     value = 5,
                     step = 1),
         sliderInput("means_nrs",
                     h3("Resampling Iterations"),
                     min = 2,
                     max = 30,
                     value = 10,
                     step = 2),
         selectInput(inputId = "k_method",
                     label = h3("Distance Metric"),
                     choices = c("euclidean", "maximum", "manhattan",
                                 "canberra", "binary", "pearson", "abspearson",
                                 "abscorrelation", "correlation", "spearman",
                                 "kendall"),
                     selected = "euclidean")
      ),
      
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("PCA",         
                             plotOutput("kmeans_pca",  height = "600px",
                                        dblclick = "plot_dblclick",
                                        brush = brushOpts(id = "plot_brush",
                                                          resetOnNew = TRUE))
                    ),
                    tabPanel("Heatmap",
                             d3heatmapOutput("kmeans_hm", height = "600px"))
        )
      )
    )
  ),
  
  # ~~~~~ Hierarchical Clustering ~~~~~
  tabPanel("Hclust",
           
   # Application title
   titlePanel("Hierarchical Clustering"),
   
   sidebarLayout(
     sidebarPanel(
       sliderInput("k_hier",
                   h3("K"),
                   min = 1,
                   max = 25,
                   value = 5,
                   step = 1),
       sliderInput("hier_nrs",
                   h3("Resampling Iterations"),
                   min = 2,
                   max = 30,
                   value = 10,
                   step = 2),
       selectInput(inputId = "hier_method",
                   label = h3("Distance Metric"),
                   choices = c("euclidean", "maximum", "manhattan",
                               "canberra", "binary", "minkowski"),
                   selected = "euclidean"),
       selectInput(inputId = "hier_link",
                   label = h3("Linkage Method"),
                   choices = c("single", "complete", "average", "median"),
                   selected = "average")
     ),
     
     mainPanel(
       tabsetPanel(type = "tabs",
                   tabPanel("PCA",         
                            plotOutput("hier_pca",  height = "600px",
                                       dblclick = "plot_dblclick",
                                       brush = brushOpts(id = "plot_brush",
                                                         resetOnNew = TRUE))
                   ),
                   tabPanel("Heatmap",
                            d3heatmapOutput("hier_hm", height = "600px"))
       )
     )
   )
  ),
  # ~~~~~ K Medoids ~~~~~
  tabPanel("K Medoids",
           
   # Application title
   titlePanel("K Medoids"),
   
   sidebarLayout(
     sidebarPanel(
       sliderInput("k_med",
                   h3("K"),
                   min = 1,
                   max = 25,
                   value = 5,
                   step = 1),
       sliderInput("med_nrs",
                   h3("Resampling Iterations"),
                   min = 2,
                   max = 30,
                   value = 10,
                   step = 2),
       selectInput(inputId = "kmed_method",
                   label = h3("Distance Metric"),
                   choices = c("euclidean", "maximum", "manhattan",
                               "canberra", "binary", "minkowski"),
                   selected = "euclidean")
     ),
     
     # Show a plot of the generated distribution
     mainPanel(
       tabsetPanel(type = "tabs",
                   tabPanel("PCA",         
                            plotOutput("kmed_pca",  height = "600px",
                                       dblclick = "plot_dblclick",
                                       brush = brushOpts(id = "plot_brush",
                                                         resetOnNew = TRUE))
                   ),
                   tabPanel("Heatmap",
                            d3heatmapOutput("kmed_hm", height = "600px"))
       )
     )
   )
  ),
  # ~~~~~ Spectral Clustering ~~~~~
  tabPanel("Spectral",
   
   # Application title
   titlePanel("Spectral Clustering"),
   
   sidebarLayout(
     sidebarPanel(
       sliderInput("k_spec",
                   h3("K"),
                   min = 1,
                   max = 25,
                   value = 5,
                   step = 1),
     sliderInput("spec_nrs",
                 h3("Resampling Iterations"),
                 min = 2,
                 max = 30,
                 value = 10,
                 step = 2)),
     
     # Show a plot of the generated distribution
     mainPanel(
         tabsetPanel(type = "tabs",
              tabPanel("PCA",         
                  plotOutput("spec_pca",  height = "600px",
                            dblclick = "plot_dblclick",
                            brush = brushOpts(id = "plot_brush",
                                              resetOnNew = TRUE))
         ),
              tabPanel("Heatmap",
                  d3heatmapOutput("spec_hm", height = "600px"))
       )
     )
   )
  )
))
