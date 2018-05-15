source("script.R")
library(shiny)
library(d3heatmap)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  df <- read.csv("top50_spotify.csv")
  y <- df$music.name.1.50.
  dataset <<- scale(df[,-1]) %>% as.data.frame()
  rownames(dataset) <- y
  
  # Holds x and y limits for zooming
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # Double-click zooming
  observeEvent(input$plot_dblclick, {
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$kmeans_pca <- renderPlot({
    consClustering(dataset, K = input$k_k, method = input$k_method,
                   func = "kmeans", nrs = input$means_nrs)$clustering %>% 
      genBiPCPlot(mat = dataset) + 
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    
  })
  
  output$kmeans_hm <- renderD3heatmap({
    consClustering(dataset, K = input$k_k, method = input$k_method,
                   nrs = input$means_nrs, func = "kmeans")$cons.mat %>% 
      d3heatmap(dendrogram = 'none', colors = "Blues")
    
  })
  
  output$hier_pca <- renderPlot({
    consClustering(dataset, K = input$k_hier, method = input$hier_method,
                   linkage = input$hier_link, nrs = input$hier_nrs, func = "hier")$clustering %>% 
      genBiPCPlot(mat = dataset) + 
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    
  })
  
  output$hier_hm <- renderD3heatmap({
    consClustering(dataset, K = input$k_hier, method = input$hier_method,
                   linkage = input$hier_link, nrs = input$hier_nrs, func = "hier")$cons.mat %>% 
      d3heatmap(dendrogram = 'none', colors = "Blues")

  })
  
  
  output$kmed_pca <- renderPlot({
    consClustering(dataset, K = input$k_med,
                   method = input$kmed_method, nrs = input$med_nrs,
                   func = "kmed")$clustering %>% 
      genBiPCPlot(mat = dataset)+ 
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })
  
  output$kmed_hm <- renderD3heatmap({(
    consClustering(dataset, K = input$k_med,
                   func = "kmed", nrs = input$med_nrs,
                   method = input$kmed_method)$cons.mat %>% 
      d3heatmap(dendrogram = 'none', colors = "Blues"))
    })
  
  
  output$spec_pca <- renderPlot({
    consClustering(dataset, K = input$k_spec,
                   nrs = input$spec_nrs, func = "spec")$clustering %>% 
      genBiPCPlot(mat = dataset) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  })
  
  output$spec_hm <- renderD3heatmap({(
    consClustering(dataset, K = input$k_spec,
                   func = "spec", nrs = input$spec_nrs)$cons.mat %>% 
      d3heatmap(dendrogram = 'none', colors = "Blues"))
    })
})

