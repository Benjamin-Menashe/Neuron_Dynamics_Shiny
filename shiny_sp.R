library(shiny)
library(visNetwork)
library(ggplot2)
library(ggpubr)

ui <- fluidPage(
  titlePanel("Neuron Dynamics"),
  h4("Created by Benjamin Menashe"),
  tabsetPanel(
    tabPanel("Neurons Setup", sidebarLayout(
      sidebarPanel(
        h3("Add Neurons"),
        numericInput("baseline_fr", "Baseline Firing Rate (0-1000 Hz)", value = 100, min = 0, max = 1000, step = 50),
        actionButton("add_node", "Add Neuron"),
        actionButton("remove_node", "Remove Neuron"),
        h3("Add Connections"),
        fluidRow(
          column(6, selectInput("from_node", "From Neuron", choices = NULL)),
          column(6, selectInput("to_node", "To Neuron", choices = NULL)),
        ),
        fluidRow(
          column(6, numericInput("connection_start", "Effect Delay (ms)", value = 10, min = 1, max=50 , step = 1)),
          column(6, numericInput("connection_duration", "Effect Duration (ms)", value = 5, min = 1, max=950, step=1))
        ),
        numericInput("edge_label_numeric", "Effect on firing rate (-1000 to +1000 Hz)", value = 0, min = -1000, max = 1000, step=50),
        actionButton("add_edge", "Add Connection"),
        actionButton("remove_edge", "Remove Connection"),
        actionButton("remove_all_edges", "Remove All Connections"),
        h3("Simulate Dynamics"),
        actionButton("simulate_dynamics", "Simulate Dynamics"),
        width = 3
      ),
      mainPanel(
        visNetworkOutput("network")
      )
    )),
    tabPanel("Analysis", 
             sidebarLayout(
               sidebarPanel(
                 h3("Spike Train and TIH"),
                 fluidRow(
                   column(6, selectInput("isi_node", "Neuron", choices = NULL)),
                   column(6, style = "margin-top: 20px;", checkboxInput("log_scale", "Log Scale", value = FALSE))
                 ),
                 actionButton("show_isi", "Show Graphs"),
                 h3("Correlation"),
                 fluidRow(
                   column(6, selectInput("cc_node_a", "Neuron 1", choices = NULL)),
                   column(6, selectInput("cc_node_b", "Neuron 2", choices = NULL))
                 ),
                 sliderInput("max_lag", "Time Lag", value = 500, min = 100, max = 1000, step = 100),
                 actionButton("show_cross_correlation", "Show Graph"),
                 h3("Survival and Hazard"),
                 selectInput("haz_node", "Neuron", choices = NULL),
                 actionButton("show_haz", "Show Graphs"),
                 width = 3
               ),
               mainPanel(
                 plotOutput("isi_plots"),
                 plotOutput("cross_correlation_plot"),
                 plotOutput("haz_plots")
               )
             )
    ),
    tabPanel("Instructions", 
             fluidPage(
               h3("Welcome to the Neuron Dynamics Shiny App"),
               tags$p("This app allows you to simulate and analyze simulated neuron dynamics.Set up your network of neurons, with baseline firing rates and effects on each other's firing rate."),
               tags$p("The app will randomly simulate 20s of neural activity (in 1ms resolution), which you can then analyze."),
               tags$p("How to use the app:"),
               
               h4("Setup"),
               tags$ol(
                 tags$li("Start by navigating to the 'Neuron Setup' tab."),
                 tags$li("Add up to 5 neurons to the network using the 'Add Neuron' button. Specify the Baseline Firing Rate (in Hz) for each neuron before adding it."),
                 tags$li("Create connections between neurons using the 'Add Connection' button. Adjust the firing rate effect of the connection, the delay until the effect takes place (in ms) and its duration (in ms) for each connection. The effects are added to the post-synaptic neuron's baseline firing rate every time the pre-synaptic neuron fires."),
                 tags$li("If necessary, you can move the neurons around or change the zoom using the mouse. Add and remove connections until you set up the desired network."),
                 tags$li("For a refractory period, add a connection from a neuron to itself with a larget negative effect (i.e., -1000Hz after 1ms for 5ms)."),
                 tags$li("After configuring the desired network, click the 'Simulate Dynamics' button to run the simulation."),
               ),
               
               h4("Analysis"),
               tags$ol(
                 tags$li("After clicking 'Simulate Dynamics', switch to the Analysis tab."),
                 tags$li("Select a neuron or neurons for analysis using the dropdown menus."),
                 tags$li("Click the appropriate 'Show Graphs' button to generate analysis graphs."),
                 tags$li("For autocorreltion, select the same neuron twice in the 'Correlation' window.")
               ),
               
               tags$p("Feel free to explore the app and use it for analyzing neuron dynamics.")
             )
    )
  )
)

server <- function(input, output, session) {
  nodes_data <- reactiveVal(data.frame(id = character(0), label = character(0), baseline_fr = numeric(0)))
  edges_data <- reactiveVal(data.frame(from = character(0), to = character(0), label = character(0), connection_start = numeric(0), connection_duration = numeric(0)))
  
  dynamics_df <- reactiveVal(data.frame(Time = -1000:20000))
  
  observeEvent(input$add_node, {
    if (nrow(nodes_data()) < 5) {
      new_id <- LETTERS[nrow(nodes_data()) + 1]
      new_label <- paste(new_id, "\n", input$baseline_fr, "Hz", sep = "")
      new_baseline_fr <- input$baseline_fr
      nodes_data(rbind(nodes_data(), data.frame(id = new_id, label = new_label, baseline_fr = new_baseline_fr)))
      
      # Update choices for from_node, to_node, cc_node_a, and cc_node_b
      updateSelectInput(session, "from_node", choices = LETTERS[1:nrow(nodes_data())])
      updateSelectInput(session, "to_node", choices = LETTERS[1:nrow(nodes_data())])
      updateSelectInput(session, "isi_node", choices = LETTERS[1:nrow(nodes_data())])
      updateSelectInput(session, "cc_node_a", choices = LETTERS[1:nrow(nodes_data())])
      updateSelectInput(session, "cc_node_b", choices = LETTERS[1:nrow(nodes_data())])
      updateSelectInput(session, "haz_node", choices = LETTERS[1:nrow(nodes_data())])
      
    }
  })
  
  observeEvent(input$remove_node, {
    if (nrow(nodes_data()) > 0) {
      nodes_data(nodes_data()[-nrow(nodes_data()), ])
      
      # Update choices for from_node, to_node, cc_node_a, and cc_node_b
      updateSelectInput(session, "from_node", choices = LETTERS[1:nrow(nodes_data())])
      updateSelectInput(session, "to_node", choices = LETTERS[1:nrow(nodes_data())])
      updateSelectInput(session, "isi_node", choices = LETTERS[1:nrow(nodes_data())])
      updateSelectInput(session, "cc_node_a", choices = LETTERS[1:nrow(nodes_data())])
      updateSelectInput(session, "cc_node_b", choices = LETTERS[1:nrow(nodes_data())])
      updateSelectInput(session, "haz_node", choices = LETTERS[1:nrow(nodes_data())])
      
    }
  })
  
  observeEvent(input$add_edge, {
    new_edge <- data.frame(
      from = input$from_node,
      to = input$to_node,
      effect = input$edge_label_numeric,
      label = paste(input$edge_label_numeric, "Hz", "\nafter", input$connection_start, "ms", "\nfor", input$connection_duration, "ms", sep = ""),
      connection_start = input$connection_start,
      connection_duration = input$connection_duration,
      color = ifelse(input$edge_label_numeric >= 0, "green", "red"),
      length = log(input$connection_start) * 100
    )
    edges_data(rbind(edges_data(), new_edge))
  })
  
  observeEvent(input$remove_edge, {
    edges_data(edges_data()[!(edges_data()$from == input$from_node & edges_data()$to == input$to_node), ])
  })
  
  observeEvent(input$remove_all_edges, {
    edges_data(data.frame(from = character(0), to = character(0), label = character(0), connection_start = numeric(0), connection_duration = numeric(0)))
  })
  
  observeEvent(input$simulate_dynamics, {
    new_dynamics_df <- data.frame(Time = -1000:20000)
    
    for (node_id in nodes_data()$id) {
      new_dynamics_df[[node_id]] <- rep(0, length(new_dynamics_df$Time))
    }
    
    for (i in 1:20000) {
      for (node_id in nodes_data()$id) {
        baseline_fr <- nodes_data()[nodes_data()$id == node_id, "baseline_fr"]
        sum_edges <- 0
        
        incoming_edges <- edges_data()[edges_data()$to == node_id, ]

        if (nrow(incoming_edges) > 0) {
          from_nodes <- incoming_edges$from
          for (from_node in from_nodes) {
            start_time <- max(1, i - incoming_edges$connection_start + 1000)
            end_time <- max(1, start_time - incoming_edges$connection_duration)
            if (any(new_dynamics_df[start_time:end_time, from_node] == 1)) {
              sum_edges <- sum_edges + incoming_edges$effect[incoming_edges$from == from_node]
            }
          }
        }
        probability <- (baseline_fr + sum_edges) / 1000
        new_dynamics_df[i + 1000, node_id] <- ifelse(runif(1) <= probability, 1, 0)
      }
    }
    new_dynamics_df <- subset(new_dynamics_df, new_dynamics_df$Time >= 0)
    dynamics_df(new_dynamics_df)
  })
  
  output$network <- renderVisNetwork({
    visNetwork(nodes_data(), edges_data()) %>%
      visNodes(shape = "circle", label = "label") %>%
      visEdges(
        arrows = 'to',
        label = "label",
        color = "color"
      )
  })
  
  # Analysis tab
  
  observeEvent(input$show_isi, {
    observeEvent(input$show_isi, {
      output$isi_plots <- renderPlot({
        isi_node <- input$isi_node
        
        selected_column <- dynamics_df()[,c(isi_node)]
        
        df <- data.frame(Time = dynamics_df()$Time[1:500], Value=selected_column[1:500])
        
        spk_plot <- ggplot(df, aes(x = Time, y = Value)) +
          geom_line(linewidth=0.1) +
          labs(x = "Time (ms)", y="") +
          ggtitle(paste("Spike Train for", isi_node)) +
          theme_minimal() +
          theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.grid = element_blank())
        
        isi_values <- diff(which(dynamics_df()[, isi_node]==1))
        isi_df <- data.frame(ISI = isi_values)
        
        isi_plot <- ggplot(isi_df, aes(x=ISI)) +
          geom_histogram(aes(y=after_stat(density)), binwidth=1) + 
          scale_x_continuous(limits = c(0, max(isi_values)+10)) +
          theme_minimal()
        
        if (input$log_scale) {
          isi_plot <- isi_plot + scale_y_log10() +
            labs(x = "Interval (ms)", y="log probability") + 
            ggtitle(paste("ISI histogram for", isi_node, "log-transformed"))
        } else {
          isi_plot <- isi_plot +
            labs(x = "Interval (ms)", y="P(spike)") + 
            ggtitle(paste("ISI histogram for", isi_node))
        }
        
        ggarrange(spk_plot, isi_plot, ncol=2)
      })
    })
  })
  
  observeEvent(input$show_cross_correlation, {
    observeEvent(input$show_cross_correlation, {
      output$cross_correlation_plot <- renderPlot({
        cc_node_a <- input$cc_node_a
        cc_node_b <- input$cc_node_b
        
        selected_columns <- dynamics_df()[, c("Time", cc_node_a, cc_node_b)]
        nA <- selected_columns[[cc_node_a]]
        nB <- selected_columns[[cc_node_b]]
        
        mid = input$max_lag
        
        # change this to actual CC
        AB_CC <- rep(0, 2*mid + 1)
        for (i in 1:(2*mid + 1)) {
          denom <- sum(nA[(mid + 1):(length(nA) - mid)])
          AB_CC[i] <- sum(nA[(mid + 1):(length(nA) - mid)] & nB[i:(length(nB) - 2*mid + i - 1)]) / denom
        }
        
        cc_df <- data.frame(Lag = (-1*mid):mid, CCF = AB_CC)
        
        if (cc_node_a == cc_node_b) {
          title <- paste("Auto-correlation of", cc_node_a)
        } else {
          title <- paste("Cross-Correlation between", cc_node_a, "and", cc_node_b)
        }
        
        # Create the ggplot with the appropriate title
        ggplot(cc_df, aes(x = Lag, y = CCF)) +
          geom_line() +
          labs(x = "Lag (ms)", y = "Correlation") +
          scale_y_continuous(limits = c(0, max(cc_df$CCF))) + 
          ggtitle(title) + 
          theme_minimal()
      })
    })
  })
  
  observeEvent(input$show_haz, {
    observeEvent(input$show_haz, {
      output$haz_plots <- renderPlot({
        haz_node <- input$haz_node
        
        ISIs <- diff(which(dynamics_df()[, haz_node]==1))
        hist_result <- hist(ISIs, breaks = seq(0, max(ISIs), by = 1), plot = FALSE)
        z <- hist_result$density
        surv_df <- data.frame(surv = 1 - cumsum(z))

        surv_plot <- ggplot(surv_df, aes(x=1:max(ISIs), y=surv)) + geom_line() + 
          labs(x = "Interval (ms)", y="P(survival)") + 
          ggtitle(paste("Survival plot for", haz_node)) +
          theme_minimal()
        
        h = z/(1 - cumsum(z)) # make hazard function
        lenny = max(ISIs)-1
        haz_df <- data.frame(haz = h[1:lenny])
        
        haz_plot <- ggplot(haz_df, aes(x=1:lenny, y=haz)) + geom_line() + 
          labs(x = "Interval (ms)", y="P(spike|survival)") + 
          ggtitle(paste("Hazard plot for", haz_node)) +
          theme_minimal()
        
        ggarrange(surv_plot, haz_plot, ncol=2)
      })
    })
  })
}

shinyApp(ui, server)
