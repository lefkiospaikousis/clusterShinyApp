# Cluster analysis app
# Lefkios Paikousis


# Libraries ---------------------------------------------------------------

library(waiter)
#library(ggforce) # not used yet

# For clustering
library(cluster)
library(fpc)

# shiny
library(shiny)
library(shinyWidgets)

library(shinydashboard)
library(shinyjs)

library(tidyverse)


# Functions ---------------------------------------------------------------

scale2 <- function(x, na.rm = TRUE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

ggplot2::theme_set(theme_classic(14))

# Cluster Methods
clust_methods <- c("Hierarchical", "k- Medoids (PAM)", "k - means")

# HC methods
hc_methods  <- c("ward.D", "ward.D2", "single", "complete", "average") #, "mcquitty", "median", "centroid"
hc_distance <- c("euclidean", "manhattan")
n_clusters  <- seq(2,7)

# a list of statistics for silhouette summaries
sil_summary = list(
  ~mean(., na.rm = TRUE),
  ~sd(., na.rm = TRUE),
  ~median(., na.rm = TRUE),
  ~min(., na.rm = TRUE),
  ~max(., na.rm = TRUE)
)

# Cluster statistics the package {fpc} produces using the fpc::cluster.stats(dissMatrix, cluster_membership)
stats_per_clust <- c("average.distance" = "within cluster average distances"
                     , "median.distance" = "within cluster distance medians"
                     , "separation" = "minimum distances of a point in the cluster to a point of another cluster"
                     , "average.toother" = "average distances of a point in the cluster to the points of other clusters"
                     , "clus.avg.silwidths" = "average silhouette widths"
                     #,"cluster.size" = "cluster size"
)

stats_overall <- c("average.between" = "average distance between clusters"
                   , "average.within" = "average distance within clusters"
                   , "avg.silwidth" = "average silhouette width"
                   , "sindex" = "adjusted separation index"
                   # for sindex see the documentation. less sensitive to a single or a few ambiguous points for 
                   #,"within.cluster.ss" = "a generalisation of the within clusters sum of squares"
                   ,"ch"
                   )


# Modules -----------------------------------------------------------------

# Doenload button for plots
# The module UI function
plotDownloadButton_UI <- function(id, buttonLabel = "Download figure") {
  
  # this will create the button
  # I will be able to use this function in the UI of the app
  # like this: `downloadButton_UI("blabla")` and it will create a button to download
  ns <- NS(id)
  tagList(
    shinyWidgets::downloadBttn(ns("download_button")
                               , label = buttonLabel,size = "xs"
                               ,style = "minimal"
    )
  )
}

# The module server function
plotDownloadButton <- function(input, output, session, plot_name, the_plot) {
  
  # this will create the download handler
  # Remember that the handler's output$_name_ has to be the same as the `downloadButton`'s id
  # in this case its download_plot
  output$download_button <- downloadHandler(
    
    filename = function() {
      paste("plot-",plot_name,".png", sep="")
    },
    
    # See here that i wrapped the `the_plot` with `()`
    # In the plot = ... we need a reactive element, not an output element
    content = function(file) {
      ggsave(file, plot = the_plot())
    }
  )
}

# The UI of the application -----------------------------------------------

ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  shinyWidgets::useShinydashboard(),
  
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("paper"),
  
  titlePanel("Cluster Analysis"),
  
  #p("First load the dataset, then go Cluster, then evaluate the clusters"),
  tabsetPanel(
    tabPanel("Data",
             p(""),
             sidebarLayout(
               sidebarPanel(
                 h3("How to use this app"),
                 p("1. First upload a dataset"),
                 p("Please note that the app currently accepts only .csv and .tsv files"),
                 #br(),
                 fileInput("file", NULL, accept = c(".csv", ".tsv")),
                 p("2. Go to ", em("Clustering"),
                   " tab and select variables to do the clustering. There, you will
                   also get descriptives of ", em("Internal validation"), "statistics.",
                   a(href = "https://arxiv.org/pdf/1503.02059.pdf", "see p.25 here")),

                 p("3. Go to ", em("Evaluate"), " tab and select variables
                   to see their distribution within the clusters, with graphs and tables"),
                 hr(),
                 h4("A few words on the app"),
                 p("The app is using the R packages: ",code("cluster"), "for clustering, the",
                   code("fpc"), "for internal validation statistics, and of course 
                   uses the ", code("tidyverse"), "collection of packages
                   for data wrangling", "The graphs are produced with the ", code("ggplot2"),
                   "package.", "I also try to improve the UI using the ", code("shinyWidgets"),
                   "package. (not there yet... :). Lastly, I managed to incorporate some",
                   a(href = "https://shiny.rstudio.com/articles/modules.html", "shiny modules"), 
                   "functionality (not much yet, just the download buttons) to manage the code complexity"),
                 hr(),
                 p("This app is created by", a(href = "https://www.linkedin.com/in/lefkios",
                                               "Lefkios Paikousis."), 
                   br(),
                 "The code for the app can be found in my", 
                   a(href="https://github.com/lefkiospaikousis/clusterShinyApp", "Github page"),
                 br(), "You can also find me on ", a(href = "https://twitter.com/lefkiospaik", 
                                                "twitter")),
                 p("As this is a work in progres, please send me your comments 
                   and suggestions on how to improve this app. Thanks for visiting"),
                 width = 3,
               ),
               
               mainPanel(
                 
                 p("The Data"),
                 hr(),
                 DT::dataTableOutput("data_tbl"),
                 width = 8
               )
             )
    ),
    tabPanel("Clustering",
             
             p("Clustering isn't a 'plug and play' task!"),
             p("Make sure you select the method appropriate for your purposes.
               See more info in the 'INFO' tab"),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 p(strong("Note:"), "The dissimilrity matrix is always using
                   the ", code("Euclidean"), " distance, except when
                   a categorical variable is selected which automaticaly
                   turns to the ", a(href = "https://www.rdocumentation.org/packages/cluster/versions/2.1.0/topics/daisy",
                                     "gower"), " distance" ),
                 shinyWidgets::radioGroupButtons(
                   inputId = "clust_method",
                   label = "Clustering Method",
                   choices = clust_methods,
                   selected = "k - means",
                   individual = TRUE,
                   checkIcon = list(
                     yes = tags$i(class = "fa fa-check-square",
                                  style = "color: steelblue"),
                     no = tags$i(class = "fa fa-square-o",
                                 style = "color: steelblue"))
                 ),
                 hr(),
                 uiOutput("hc_linkage"),
                 sliderInput("clustN", "Number of clusters",
                             value = 2, step = 1, ticks = FALSE,
                             min = min(n_clusters), max = max(n_clusters)
                 ),
                 uiOutput("vars_cluster"),
                 p("Note!: You have used the following variables to create the clusters:"),
                 textOutput("vars_cluster_length"),
                 verbatimTextOutput("cluster_variables_tabCl", placeholder = TRUE)
                 
                 , width = 3
                 
               ),
               mainPanel(
                 fluidRow(
                   column(7,
                          h4("Internal validation"),
                          #Select the # of clusters and see their size and silhouette statistics per cluster"),
                          p(strong("-Silhouette Information-")),
                          tableOutput("cluster_info"),
                          hr(),
                          p(strong("-Cluster Stats-")),
                          tableOutput("stats_cluster"),
                          checkboxInput("show_fpcStats", "What are these?"),
                          uiOutput("show_fpcStats"),
                          hr(),
                          p(strong("-Separation Table-")),
                          tableOutput("cluster_sepMatrix"),
                          shinyWidgets::prettySwitch(
                            inputId = "show_clustSepPlot",
                            label = "show plot",
                            status = "success",
                            fill = TRUE
                          ),
                          uiOutput("show_clustSepPlot"),
                          hr(),
                          p(strong("-Correlations between the", code("numeric"),"variables-")),
                          p("This will allow you also to check on outliers"),
                          plotOutput("plot_scatterMatrix")
                          
                   ),
                   column(5, 
                          conditionalPanel(
                            condition = "input.clust_method == 'Hierarchical'",
                            checkboxInput("show_dendro", "Show the dendrogram"),
                            uiOutput("var_names"),
                            plotOutput("hc_plot", "90%", 1000)
                          )
                   )
                 )
                 
                 
               )
               
               
               
             )
             
    ),
    tabPanel("Evaluate",
             h3("Explore the cluster composition"),
             fluidRow(
               column(4,
                      p("Here you can visually explore the cluster composition on ", code("numeric"), "variables"),
                      p("Any variables that are ", code("nominal"), "ie.", em("categorical"),
                        "will be ignored for the plots, but will be considered for the tables"),
                      p("Note!: You have used the following variables to create the clusters:"),
                      verbatimTextOutput("cluster_variables_tabEv"),
                      uiOutput("vars_evaluate")
               ),
               column(8,
                      h3("Cluster compositions plots"),
                      p("The density distribution of",code("numeric"), "evaluation variables"),
                      shinydashboard::box(
                        title = "Density plots",
                        plotDownloadButton_UI("plot_density"),
                        plotOutput("plot_density", height = 500),
                        collapsible = TRUE, collapsed = TRUE,
                        width = NULL
                      ),
                      shinydashboard::box(
                        title = "Box plots",
                        plotDownloadButton_UI("plot_box"),
                        radioButtons("box_type", "Type of box plot",
                                     choices = c("Single", "Faceted"),
                                     selected = "Single", inline = TRUE),
                        plotOutput("plot_box", height = 500),
                        collapsible = TRUE, collapsed = TRUE,
                        width = NULL
                      ),
                      shinydashboard::box(
                        title = "Tile plots",
                        p("This plot show the average standardised value (z-score) of each variable in each cluster"),
                        p("Its an indication of whether the cluster is on average Higher or lower than
                        the overall average"),
                        plotOutput("tile_plot", height = 500),
                        
                        collapsible = TRUE, collapsed = TRUE,
                        width = NULL
                      ),
                      shinydashboard::box(
                        title = "Cluster descriptive statistics",
                        hr(),
                        p("The table shows the descriptive statistics
                          of the chosen variables across the clusters"),
                        tableOutput("tbl_descriptives"),
                        
                        collapsible = TRUE, collapsed = TRUE,
                        width = NULL
                      )
               )
             )
             
             
    ),
    tabPanel("Download",
             hr(),
             h4("Cluster and its silhouette width"),
             p("Download full table with cluster memebership and silhouette width"),
             DT::dataTableOutput("data_updated"),
    ),
    tabPanel("INFO",
             hr(),
             h4("Cluster methods"),
             p("The following are taken form ",
               a(href = "https://arxiv.org/pdf/1503.02059.pdf", "this paper")),
             p(strong("K-means")),
             tags$ul(
               tags$li("Outliers can have a strong impact"),
               p("The squared Euclidean distance penalizes large distances within clusters strongly"),
               tags$li("Emphasizes homogeneity rather than separation"),
               p("It is usually more successful regarding small within-cluster
              dissimilarities than regarding finding gaps between clusters")
             ),
             p(strong("K-medoids")),
             tags$ul(
               tags$li("similar to K-means, but it uses unsquared dissimilarities"),
               p("This means that it may allow larger dissimilarities within clusters"),
               tags$li("More flexible regarding outliers"),
             ),
             p(strong("Hierarchical")),
             p("Different hierarchical methods produce quite different clusters.
                Both Single and Complete Linkage are rather too extreme for many applications,
                although they may be useful in a few specific cases"),
             tags$ul(
               tags$li("Single linkage"),
               p("focuses totally on separation, i.e., keeping the closest points of different
                clusters apart from each other"),
               tags$li("Complete linkage"),
               p("focuses totally on keeping the largest dissimilarity within a cluster low"),
               tags$li("Other linkage"),
               p("Most other hierarchical
                methods are a compromise between the above two extremes")
             ),
             
             
    )
  )
  
)


# The server logic --------------------------------------------------------

server <- function(input, output) {
  
  # disable download buttton on page load
  shinyjs::disable("down_data")
  
  check_data_input <-  function(){
    
    validate(
      need(input$file, "Upload a dataset")
    )
  }
  
  check_cluster_var_selection <- function(){    validate(
    need(length(input$vars_cluster)>0, "--Select at least one variable to cluster with--")
  )
    
  }
  
  # DATA
  data <- reactive({
    
    req(input$file)
    
    ext <- tools::file_ext(input$file$name)
    
    switch(ext,
           csv = vroom::vroom(input$file$datapath, delim = ",") %>%
             mutate_if(is.numeric, ~round(., 4)) %>%
             drop_na()
           ,
           tsv = vroom::vroom(input$file$datapath, delim = "\t") ,
           
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
    
  })
  
  data_scaled <- reactive({
    
    data() %>% dplyr::mutate_if(is.numeric, ~ scale2(., na.rm = TRUE)) %>%
      # cluster::daisy with character vars not working, need to be factors
      mutate_if(is.character, ~ factor(.))
    
  })
  
  data_updated <- reactive({
    
    req(data())
    
    data() %>%
      tibble::add_column(cluster = clust_member()) %>%
      dplyr::group_by(cluster) %>%
      dplyr::mutate(clust_size = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(cluster = paste0(cluster, " (n=", clust_size, ")")) %>%
      dplyr::mutate(obs =  dplyr::row_number()) %>%
      left_join(clust_sil() %>%
                  select(obs, sil_width)
                , by = "obs"
      )
  })
  
  vars_for_cluster <- reactive({ input$vars_cluster})
  
  vars_for_evaluation <- reactive({ input$vars_evaluate})
  
  vars_for_evaluation_num <- reactive({
    data() %>%
      select(vars_for_evaluation()) %>%
      select_if(is.numeric) %>%
      names()
  })
  vars_for_evaluation_cat <- reactive({
    data() %>%
      select(vars_for_evaluation()) %>%
      select_if(purrr::negate(is.numeric)) %>%
      names()
  })
  vars_char_fact <- reactive({
    
    vars <- data() %>% select_if(negate(is.numeric)) %>% names()
    if(purrr::is_empty(vars)) return(NULL)
    return(vars)
  })
  
  dissMatrix <- reactive({
    cluster::daisy(
      # default method = eucledian unless we have catecorical data
      # where the metric changes on its own to 'gower'. see ?cluster::daisy
      data_scaled()[, vars_for_cluster()]
      #, metric = "gower"
    )
  })
  
  kmeds <- reactive({
    
    req(vars_for_cluster())
    
    cluster::pam(
      dissMatrix()
      , k = input$clustN
    )
  })
  
  hc <- reactive({
    
    req(vars_for_cluster())
    
    hclust(dissMatrix(), method = input$hc_linkage)
  })
  
  k_means <- reactive({
    
    req(vars_for_cluster())
    
    set.seed(123)
    
    data <- data_scaled() %>%
      select(vars_for_cluster()) %>%
      purrr::keep(is.numeric)
    
    kmeans(data, input$clustN, nstart = 25)
    
  })
  # Cluster Membership
  hc_clust_member <- reactive({
    req(hc())
    cutree(hc(), input$clustN)
  })
  
  kmeds_clust_member <- reactive({
    req(kmeds())
    kmeds()$clustering
  })
  
  kmeans_clust_member <- reactive({
    req(k_means())
    k_means()$cluster
  })
  
  clust_member <- reactive({
    
    switch(input$clust_method,
           
           "Hierarchical" = hc_clust_member(),
           "k- Medoids (PAM)" = kmeds_clust_member(),
           "k - means" = kmeans_clust_member()
    )
  })
  
  # Cluster Statistics
  hc_clust_info <- reactive({
    
    
    by_group_summary <- hc_clust_sil() %>%
      group_by(cluster = as.character(cluster)) %>%
      summarise_at(vars(sil_width), sil_summary)
    
    total_summary <- hc_clust_sil() %>%
      summarise_at(vars(sil_width), sil_summary) %>%
      mutate(cluster = "ALL")
    
    hc_clust_member() %>%
      as.character() %>% fct_count(prop = TRUE) %>%
      rename("cluster" = f, "size" = n, "prop" = p) %>%
      left_join(by_group_summary, by = "cluster") %>%
      bind_rows(total_summary)
  })
  
  kmeds_clust_info <- reactive({
    
    by_group_summary <- kmeds_clust_sil() %>%
      group_by(cluster = as.character(cluster)) %>%
      summarise_at(vars(sil_width), sil_summary)
    
    total_summary <- kmeds_clust_sil() %>%
      summarise_at(vars(sil_width), sil_summary) %>%
      mutate(cluster = "ALL")
    
    
    kmeds_clust_member() %>%
      as.character() %>% fct_count(prop = TRUE) %>%
      rename("cluster" = f, "size" = n, "prop" = p) %>%
      left_join(by_group_summary, by = "cluster") %>%
      bind_rows(total_summary)
    
  })
  
  kmeans_clust_info <- reactive({
    
    by_group_summary <- kmeans_clust_sil() %>%
      group_by(cluster = as.character(cluster)) %>%
      summarise_at(vars(sil_width), sil_summary)
    
    total_summary <- kmeans_clust_sil() %>%
      summarise_at(vars(sil_width), sil_summary) %>%
      mutate(cluster = "ALL")
    
    
    kmeans_clust_member() %>%
      as.character() %>% fct_count(prop = TRUE) %>%
      rename("cluster" = f, "size" = n, "prop" = p) %>%
      left_join(by_group_summary, by = "cluster") %>%
      bind_rows(total_summary)
    
  })
  
  # Silhouette width per observation
  hc_clust_sil <- reactive({
    # a tibble of silhouette widths, cluster, and neighbor across each observations
    # the cluster::silhouette() result seems uncoercable to data.frame
    sil_matrix <- cluster::silhouette(hc_clust_member(), dissMatrix())
    
    tibble(
      cluster = sil_matrix[,1],
      neighbor = sil_matrix[,2],
      sil_width = sil_matrix[,3]
    ) %>%
      rowid_to_column("obs")
  })
  
  kmeans_clust_sil <- reactive({
    # a tibble of silhouette widths, cluster, and neighbor across each observations
    # the cluster::silhouette() result seems uncoercable to data.frame
    sil_matrix <- cluster::silhouette(kmeans_clust_member(), dissMatrix())
    
    tibble(
      cluster = sil_matrix[,1],
      neighbor = sil_matrix[,2],
      sil_width = sil_matrix[,3]
    ) %>%
      rowid_to_column("obs")
  })
  
  kmeds_clust_sil <- reactive({
    # a tibble of silhouette widths, cluster, and neighbor across each observations
    # the cluster::silhouette() result seems uncoercable to data.frame
    #sil_matrix <- cluster::silhouette(kmeds_clust_member(), dissMatrix())
    
    # row.names creates
    #kmeds()$silinfo$widths %>% as.data.frame() %>% mutate(obs = as.numeric(row.names(.)))
    kmeds()$silinfo$widths %>% as.data.frame() %>% rowid_to_column("obs")
  })
  
  clust_sil <- reactive({
    switch(input$clust_method,
           
           "Hierarchical" = hc_clust_sil(),
           "k- Medoids (PAM)" = kmeds_clust_sil(),
           "k - means" = kmeans_clust_sil()
    )
  })
  
  cluster_stats <- reactive({
    
    req(dissMatrix())
    
    fpc::cluster.stats(dissMatrix(), clust_member())
  })
  
  cluster_sepMatrix <- reactive({
    
    sep_matrix <- cluster_stats()$separation.matrix
    
    colnames(sep_matrix) <- paste0("Cluster ", seq_len(input$clustN))
    row.names(sep_matrix) <- paste0("Cluster ", seq_len(input$clustN))
    
    sep_matrix %>%
      as_tibble(rownames = "Cluster")
    
  })
  
  # PLOTS ####
  
  hc_plot <- reactive({
    
    hc <- hc()
    
    if(!is.null(input$id_var)){
      new_hc_labels <- data()[hc$order,] %>% pull(input$id_var)
      
      hc$labels <- new_hc_labels
    }
    
    ggdendro::ggdendrogram(hc, rotate = TRUE, theme_dendro = FALSE, size = 2)
    
  })
  
  plot_corr <- reactive({
    
    validate(
      need(length(input$vars_cluster)>1, "Select at least two variables")
    )
    
    data() %>%
      dplyr::select_at(input$vars_cluster) %>%
      corrr::correlate() %>%
      corrr::as_matrix() %>%
      corrplot::corrplot(order = "hclust")
    
    
  })
  
  plot_scatterMatrix <- reactive({
    
    validate(
      need(length(input$vars_cluster)>1, "Select at least two numeric variables")
    )
    # and then check within the data
    validate(
      need(
        
        sum(data()[input$vars_cluster] %>% map_lgl(is.numeric))>1, "One variable is not numeric"
      )
    )
    
    # Make sure that only numeric variables are selected
    # and form those pick any of the cluster vars
    data() %>%
      select_if(is.numeric) %>%
      # dplyr::select_if(is.numeric) %>%
      dplyr::select(any_of(input$vars_cluster)) %>%
      PerformanceAnalytics::chart.Correlation()
  })
  
  # Cluster Exploration/ validation ####
  
  plot_density <- reactive({
    chain_stats <-
      data() %>%
      select(vars_for_evaluation_num()) %>%
      gather(key, value) %>%
      group_by(key) %>%
      summarise(avg = mean(value),
                median = median(value)
      )
    
    
    data_updated() %>%
      select(vars_for_evaluation_num(), cluster) %>%
      # remove character or factors variable
      gather(key, value, - cluster) %>%
      ggplot(aes(value,..scaled.. ,fill = cluster))+
      geom_density(alpha = 0.5, adjust = 1.2)+
      geom_vline(data = chain_stats, aes(xintercept = median, colour = "Median"),
                 linetype = "dotted"
      )+
      geom_vline(data = chain_stats, aes(xintercept = avg, colour = "Average"),
                 linetype = "dashed"
      )+
      facet_wrap(~key, scales = "free")+
      labs(linetype = "")+
      scale_color_manual(name = "Chain stats", values = c(Median = "black", Average = "blue"))+
      scale_fill_brewer(type = "qual", palette = 2)
    
  })
  
  plot_box_facet <- reactive({
    
    data_updated() %>%
      select(vars_for_evaluation_num(), cluster) %>%
      gather(key, value, -cluster) %>%
      ggplot(aes(fct_rev(cluster),value ))+
      geom_boxplot(aes(fill = cluster),alpha = 0.5)+
      geom_jitter(width = 0.2, alpha = 0.5)+
      coord_flip()+
      facet_wrap(~key, scales= "free")+
      scale_fill_brewer(type = "qual", palette = 2)
    
    
  })
  
  plot_box_single <- reactive({
    
    data_scaled() %>%
      add_column(cluster =
                   data_updated()$cluster
      ) %>%
      select(vars_for_evaluation_num(), cluster) %>%
      gather(key, value, -cluster) %>%
      #mutate(key = fct_rev(key)) %>%
      ggplot(aes(cluster,value, fill = key ))+
      geom_boxplot(alpha = 0.5)+
      # geom_jitter(width = 0.2, alpha = 0.5)+
      coord_flip()+
      geom_hline(yintercept = 0, linetype = "dashed", colour = "blue")+
      scale_fill_brewer(type = "qual", palette = 3)+
      guides(fill = guide_legend(reverse = TRUE))+
      labs(
        title = "Distribution of (standardised) evaluation variables within each cluster",
        subtitle = "Points are shops",
        fill = "Variable",
        y = "Z -score",
        x = "Cluster"
      )+
      theme_classic(14)
    
  })
  
  plot_clustSeparation <- reactive({
    
    check_cluster_var_selection()
    
    cluster_sepMatrix() %>%
      gather(key, value, -Cluster) %>%
      arrange(Cluster, key) %>%
      ggplot(aes(Cluster, key))+
      geom_tile(aes(fill = value))+
      scale_fill_gradient2(high = "#018571", low = "#d7191c")+
      geom_text(aes(label = round(value, 2)))+
      labs(x = "", y= "", fill = "Separation\nlevel",
           title = "Separation between clusters",
           subtitle = "Higher value ~ higher separation between the pair of clusters")+
      theme_light(14)
    
  })
  
  
  # Outputs -----------------------------------------------------------------
  
  
  output$hc_linkage <- renderUI({
    
    req(input$clust_method == "Hierarchical")
    
    list(
      p("Hierarchical clustering needs a ",strong("linkage method.")),
      p(strong("Note that"), " Linkage methods may have  strong
        impact on the cluster formation. See the INFO tab"),
      shinyWidgets::radioGroupButtons(
        inputId = "hc_linkage",
        label = "Linkage methods",
        choices = hc_methods,
        selected = "complete",
        individual = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-check-square",
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-square-o",
                      style = "color: steelblue"))
      ),
      hr()
    )
  })
  output$table_clustStatsExplained <- renderTable({
    enframe(stats_per_clust, "Statistic", "Explanation")} )
  
  output$plot_clustSeparation <- renderPlot({plot_clustSeparation()})
  
  output$show_clustSepPlot<- renderUI({
    
    req(input$show_clustSepPlot)
    
    list(
      p("This is  tile plot that shows the separation values among pairs of clusters"),
      plotOutput("plot_clustSeparation")
    )
    
  })
  
  output$show_fpcStats <- renderUI({
    
    req(input$show_fpcStats)
    list(
      tableOutput("table_clustStatsExplained")
    )
  })
  
  output$cluster_sepMatrix <- renderTable({
    check_cluster_var_selection()
    cluster_sepMatrix()
  })
  
  output$tile_plot <- renderPlot({
    
    
    validate(
      # need(hc(), "ffff"),
      need(length(input$vars_evaluate)>0, "Select at least one evaluation variable")
    )
    
    
    
    #  A tile plot with the sdandrdised vars across clusters
    data_scaled() %>%
      select(all_of(vars_for_evaluation_num())) %>%
      add_column(cluster =
                   data_updated()$cluster
      ) %>%
      gather(var, value, -cluster) %>%
      group_by(cluster, var) %>%
      summarise(value = mean(value)) %>%
      ggplot(aes(cluster, var, fill = value ))+
      geom_tile()+
      scale_fill_gradient2(high = "#018571", low = "#d7191c")+
      labs(y = "", fill = "Z-score",
           title = "Cluster composition representation",
           subtitle = "Positive (negative) z-score indicated above (below) chain average")+
      theme_light(14)
    
    
  })
  
  output$plot_corr <- renderPlot({ plot_corr() })
  
  output$plot_scatterMatrix <- renderPlot({ plot_scatterMatrix() })
  
  output$evaluation_variables <- renderText(vars_for_evaluation())
  
  output$dplyrFilter<- renderUI({
    
    req(input$showFilter)
    
    check_data_input()
    
    list(
      code("Under Construction!! Not working yet"),
      p("You may want to exlude cases such as extreme values, or group of cases"),
      p("You can use standard R syntax to filter the data ex. ", br(),
        code("age > 20"), " or ", code("gender == 'Female'"),
        br(),
        "You can use these filter functions",
        br(),
        code("=="), code(">"), code(">="), code("!="), code("&"), code("|"), code("!"),
        code("is.na()"),
        "etc, or even ", a(href="https://dplyr.tidyverse.org/reference/filter.html", "dplyr"),
        "functions like ", code("between()"), code("near()")
      ),
      textInput(
        "filterText",
        "Type the filter statement",
        value = ""
      )
    )
  })
  
  output$vars_cluster<- renderUI({
    
    list(
      h3("Select variables for cluster analysis"),
      
      shinyWidgets::pickerInput(
        "vars_cluster",
        "Variables in the database",
        choices = names(data()),
        selected = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE,
                       `live-Search`  = TRUE,
                       liveSearchStyle = "contains"
        )
      )
    )
  })
  
  output$var_names<- renderUI({
    
    req(input$show_dendro)
    
    list(
      p("Select the",  code("nominal"),"variable to identify the observations by"),
      p("This is used for the dendrogram. Nothing to do with the clustering."),
      selectInput(
        "id_var",
        "Nominal variables in the dataset",
        vars_char_fact()
        , selected = NULL)
    )
  })
  
  output$vars_evaluate<- renderUI({
    
    # this clears the evaluation variables whenI change the clustering inputs
    # don;t know why yet
    # validate(
    #   need(length(input$vars_cluster)>0, "Do the clustering first!")
    # )
    
    list(
      h4("Select variables for cluster evaluation"),
      #checkboxInput("add_clust_vars", "Add cluster variables?"),
      
      shinyWidgets::pickerInput(
        "vars_evaluate",
        "Variables in the database",
        choices = names(data()),
        selected = NULL,
        multiple = TRUE,
        options = list(`actions-box` = TRUE,
                       `live-Search`  = TRUE,
                       liveSearchStyle = "contains"
        )
      )
      
    )
  })
  
  output$tbl_descriptives <- renderTable({
    
    validate(
      need(length(input$vars_evaluate)>0, "Select at least 1 variable in the Evaluation Tab")
    )
    
    # Descriptives by cluster
    # Table of descriptives by Cluster
    # Since many are skwed I am using the median
    finalfit::summary_factorlist(
      data_updated(),
      dependent = "cluster",
      explanatory = vars_for_evaluation(),
      cont = "median",
      #cont = "mean",
      cont_cut = 1,
      column = TRUE,
      digits = c(2,2,3,1),
      p = TRUE
    )
  }, striped = TRUE)
  
  # separate outpus for tab Cluster and tab_Evluate
  output$cluster_variables_tabCl <- renderPrint(vars_for_cluster())
  output$cluster_variables_tabEv <- renderPrint(vars_for_cluster())
  
  output$vars_cluster_length <- renderPrint(length(vars_for_cluster()))
  
  output$hc_plot <- renderPlot({
    
    check_data_input()
    check_cluster_var_selection()
    
    hc_plot()
  })
  
  output$plot_density <- renderPlot({
    
    validate(
      #need(hc(), "ffff"),
      need(length(input$vars_evaluate)>0, "Select at least one variable")
    )
    
    plot_density()
    
    
  })
  
  output$plot_box <- renderPlot({
    
    validate(
      # need(hc(), "ffff"),
      need(length(input$vars_evaluate)>0, "Select at least one variable")
    )
    
    type <- input$box_type
    
    switch( type,
            "Single" =  plot_box_single(),
            "Faceted" = plot_box_facet()
    )
    
  })
  
  output$cluster_info <- renderTable({
    
    check_cluster_var_selection()
    
    switch(input$clust_method,
           
           "Hierarchical" = hc_clust_info(),
           "k- Medoids (PAM)" = kmeds_clust_info(),
           "k - means" = kmeans_clust_info()
           
    )
  }, na = "-", striped = TRUE)
  
  output$stats_cluster <- renderTable({
    
    check_cluster_var_selection()
    
    cluster_stats()[names(stats_per_clust)] %>%
      as_tibble() %>%
      rowid_to_column("cluster")
  }, striped = TRUE)
  
  output$data_tbl <- DT::renderDataTable({
    
    check_data_input()
    
    n_cols <- ncol(data())
    cols_to_show <- 3
    
    data()
    
  }, filter = "top", extensions = 'Buttons',
  #rownames = FALSE, screws them up
  options = list(dom = 'Bfrtip',
                 
                 # shows first 2 columns only . the rest can be selected to be shown
                 columnDefs = list(list(visible=FALSE, targets=c(4:ncol(data())))),
                 
                 buttons = list(list(extend = 'collection',
                                     buttons = c("csv", "excel"),
                                     text = "Download"
                 ),
                 I("colvis")
                 )
                 
  )
  )
  
  output$data_updated <- DT::renderDataTable({
    
    data_updated() %>%
      #mutate(cluster = as.factor(cluster)) %>%
      mutate(sil_width = round(sil_width, 2)) %>%
      select(input$id_var, cluster, sil_width, everything())
    
  }, filter = "top", extensions = 'Buttons',
  #rownames = FALSE, screws them up
  options = list(dom = 'Bfrtip',
                 # shows first 2 columns only . the rest can be selected to be shown
                 columnDefs = list(list(visible=FALSE, targets=c(4:ncol(data_updated())))),
                 
                 buttons = list(list(extend = 'collection',
                                     buttons = c("csv", "excel"),
                                     text = "Download"
                 ),
                 I("colvis")
                 )
  )
  )
  
  # Observe Events ----------------------------------------------------------
  
  observeEvent(input$show_dendro, {
    # every time the button is pressed, alternate between hiding and showing the plot
    shinyjs::toggle("hc_plot")
  })
  
  # Call Modules ------------------------------------------------------------
  
  callModule(plotDownloadButton, id = "plot_density", plot_name = "density", the_plot = plot_density)
  callModule(plotDownloadButton, id = "plot_box", plot_name = "plot_box", the_plot = plot_box_facet)
  callModule(plotDownloadButton, id = "plot_tile", plot_name = "tile_plot", the_plot = tile_plot)
  
  
}


# Run the application
shinyApp(ui = ui, server = server)
