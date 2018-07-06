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
  
  method <- reactiveValues()
  
  observe({
    clust_out <- switch(input$algorithm, 
                        kmeans = consClustering(dataset, K = input$k_k, method = input$k_method,
                                                    func = input$algorithm, nrs = input$means_nrs),
                        hier = consClustering(dataset, K = input$k_hier, method = input$hier_method,
                                              linkage = input$hier_link, nrs = input$hier_nrs,
                                              func = input$algorithm),
                        kmed = consClustering(dataset, K = input$k_med,
                                              func = input$algorithm, nrs = input$med_nrs,
                                              method = input$kmed_method),
                        spec = consClustering(dataset, K = input$k_spec,
                                              func = input$algorithm, nrs = input$spec_nrs))
   # PCA plot
    method$pc <- clust_out$clustering %>% 
      genBiPCPlot(mat = dataset) +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    
    # Cluster Heatmap
    method$hm <- clust_out$norm.mat %>% 
      d3heatmap(dendrogram = 'none', colors = "Blues")
    
    # Cluster consensus plot
    method$cc <- computeClustCons(clust_out$clustering, clust_out$norm.mat)
    
    # Item consensus plot
    method$ic <- computeItemCons(clust_out$clustering, clust_out$norm.mat)
    
    method$cd <- computeDistribution(clust_out$norm.mat)
  })

  output$kmeans_pca <- renderPlot({
    method$pc
  })

  output$kmeans_hm <- renderD3heatmap({
    method$hm
  })

  output$kmeans_cc <- renderPlot({
    method$cc
  })
  
  output$kmeans_ic <- renderDataTable({
    method$ic
  }, options = list(pageLength = 10))
  
  output$kmeans_cd <- renderPlot({
    method$cd
  })
  
  output$hier_pca <- renderPlot({
    method$pc
  })
  
  output$hier_hm <- renderD3heatmap({
    method$hm
  })
  
  output$hier_cc <- renderPlot({
    method$cc
  })
  
  output$hier_ic <- renderDataTable({
    method$ic
  }, options = list(pageLength = 10))
  
  output$hier_cd <- renderPlot({
    method$cd
  })
  
  
  output$kmed_pca <- renderPlot({
    method$pc
  })
  
  output$kmed_hm <- renderD3heatmap({
    method$hm
    })
  
  output$kmed_cc <- renderPlot({
    method$cc
  })
  
  output$kmed_ic <- renderDataTable({
    method$ic
  }, options = list(pageLength = 10))
  
  output$kmed_cd <- renderPlot({
    method$cd
  })
  
  output$spec_pca <- renderPlot({
    method$pc
  })
  
  output$spec_hm <- renderD3heatmap({
    method$hm
    })
  
  output$spec_cc <- renderPlot({
    method$cc
  })
  
  output$spec_ic <- renderDataTable({
    method$ic
  }, options = list(pageLength = 10))
  
  output$spec_cd <- renderPlot({
    method$cd
  })
  
  
})

