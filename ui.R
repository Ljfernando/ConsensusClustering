
library(shiny)
library(d3heatmap)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage("Consensus Clustering", id = "algorithm",
             
    # ~~~~~~~ K MEANS ~~~~~~
    tabPanel("Kmeans", value = 'kmeans',
  
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
                    tabPanel(title = "PCA", value = "what",         
                             plotOutput("kmeans_pca",  height = "500px",
                                        dblclick = "plot_dblclick",
                                        brush = brushOpts(id = "plot_brush",
                                                          resetOnNew = TRUE))
                    ),
                    tabPanel(title = "Heatmap",
                             d3heatmapOutput("kmeans_hm", height = "500px")),
                    tabPanel(title = "ClusterCons",
                             plotOutput("kmeans_cc", height = "500px")),
                    tabPanel(title = "ItemCons", 
                             dataTableOutput("kmeans_ic")),
                    tabPanel(title = "Distribution",
                             plotOutput("kmeans_cd", height = "500px"))
                    
        )
      )
    )
  ),
  
  # ~~~~~ Hierarchical Clustering ~~~~~
  tabPanel("Hclust", value = "hier",
           
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
                            plotOutput("hier_pca",  height = "500px",
                                       dblclick = "plot_dblclick",
                                       brush = brushOpts(id = "plot_brush",
                                                         resetOnNew = TRUE))
                   ),
                   tabPanel("Heatmap",
                            d3heatmapOutput("hier_hm", height = "500px")),
                   tabPanel(title = "ClusterCons",
                            plotOutput("hier_cc", height = "500px")),
                   tabPanel(title = "ItemCons", 
                            dataTableOutput("hier_ic")),
                   tabPanel(title = "Distribution",
                            plotOutput("hier_cd", height = "500px"))
                   
       )
     )
   )
  ),
  # ~~~~~ K Medoids ~~~~~
  tabPanel("K Medoids", value = 'kmed',
           
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
     
     mainPanel(
       tabsetPanel(type = "tabs",
                   tabPanel("PCA",         
                            plotOutput("kmed_pca",  height = "500px",
                                       dblclick = "plot_dblclick",
                                       brush = brushOpts(id = "plot_brush",
                                                         resetOnNew = TRUE))
                   ),
                   tabPanel("Heatmap",
                            d3heatmapOutput("kmed_hm", height = "500px")),
                   tabPanel(title = "ClusterCons",
                            plotOutput("kmed_cc", height = "500px")),
                   tabPanel(title = "ItemCons", 
                            dataTableOutput("kmed_ic")),
                   tabPanel(title = "Distribution",
                            plotOutput("kmed_cd", height = "500px"))
                   
       )
     )
   )
  ),
  # ~~~~~ Spectral Clustering ~~~~~
  tabPanel("Spectral", value = 'spec',
   
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
     
     mainPanel(
         tabsetPanel(type = "tabs",
              tabPanel("PCA",         
                  plotOutput("spec_pca",  height = "500px",
                            dblclick = "plot_dblclick",
                            brush = brushOpts(id = "plot_brush",
                                              resetOnNew = TRUE))
              ),
              tabPanel("Heatmap",
                  d3heatmapOutput("spec_hm", height = "500px")),
              tabPanel(title = "ClusterCons",
                       plotOutput("spec_cc", height = "500px")),
              tabPanel(title = "ItemCons", 
                       dataTableOutput("spec_ic")),
              tabPanel(title = "Distribution",
                       plotOutput("spec_cd", height = "500px"))
              
       )
     )
   )
  )
))
