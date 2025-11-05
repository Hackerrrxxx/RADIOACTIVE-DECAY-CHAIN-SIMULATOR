# Load required libraries
if (!require(shiny)) install.packages("shiny", dependencies = TRUE)
if (!require(deSolve)) install.packages("deSolve", dependencies = TRUE)
library(shiny)
library(deSolve)

# Define the decay function
decay_chain <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dA <- -lambda1 * A
    dB <- lambda1 * A - lambda2 * B
    dC <- lambda2 * B
    list(c(dA, dB, dC))
  })
}

# Shiny UI
ui <- fluidPage(
  titlePanel("☢️ Radioactive Decay Chain Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda1", "Decay Constant λ₁ (A → B):", 
                  min = 0.01, max = 0.2, value = 0.1, step = 0.01),
      sliderInput("lambda2", "Decay Constant λ₂ (B → C):", 
                  min = 0.01, max = 0.2, value = 0.05, step = 0.01),
      numericInput("A0", "Initial Quantity of A:", 100, min = 10, max = 500),
      sliderInput("time", "Simulation Time:", min = 50, max = 500, value = 100, step = 10)
    ),
    
    mainPanel(
      plotOutput("decayPlot"),
      tableOutput("summaryTable")
    )
  )
)

# Shiny Server
server <- function(input, output) {
  output$decayPlot <- renderPlot({
    # Initial state
    state <- c(A = input$A0, B = 0, C = 0)
    parameters <- c(lambda1 = input$lambda1, lambda2 = input$lambda2)
    time <- seq(0, input$time, by = 1)
    
    # Solve the system
    out <- ode(y = state, times = time, func = decay_chain, parms = parameters)
    out_df <- as.data.frame(out)
    
    # Plot results
    plot(out_df$time, out_df$A, type = "l", col = "red", ylim = c(0, input$A0),
         xlab = "Time", ylab = "Quantity", lwd = 2,
         main = "Radioactive Decay Chain (A → B → C)")
    lines(out_df$time, out_df$B, col = "blue", lwd = 2)
    lines(out_df$time, out_df$C, col = "green", lwd = 2)
    legend("right", legend = c("Isotope A", "Isotope B", "Isotope C"),
           col = c("red", "blue", "green"), lty = 1, bty = "n", lwd = 2)
  })
  
  output$summaryTable <- renderTable({
    state <- c(A = input$A0, B = 0, C = 0)
    parameters <- c(lambda1 = input$lambda1, lambda2 = input$lambda2)
    time <- seq(0, input$time, by = 1)
    
    out <- ode(y = state, times = time, func = decay_chain, parms = parameters)
    head(as.data.frame(out), 10)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
