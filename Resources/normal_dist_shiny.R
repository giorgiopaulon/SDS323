
library(shiny)
library(shinyMatrix)
library(rsconnect)
library(RColorBrewer)
library(rgl)
library(rglwidget)
library(plot3D)
library(mvtnorm)
library(latex2exp)
library(scales)
cols <- brewer.pal(9, "Set1")

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("2D Normal Distribution"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            numericInput('n', 'Number of data points', 200,
                         min = 1, max = 1000), 
            sliderInput('mu_x', 'X Mean', -10, 10, 0, step = 0.1), 
            sliderInput('mu_y', 'Y Mean', -10, 10, 0, step = 0.1), 
            sliderInput('sigma_x', 'X Sd', 0.01, 5, 1, step = 0.1), 
            sliderInput('sigma_y', 'Y Sd', 0.01, 5, 1, step = 0.1), 
            sliderInput('rho', 'Correlation', -1, 1, 0, step = 0.1)
        ),

        # Show plots
        mainPanel(
           plotOutput("twod"),
           plotOutput("threed")
        )
    )
)

# Define server logic required
server <- function(input, output) {


    output$twod <- renderPlot({
        mu <- c(input$mu_x, input$mu_y)
        Sigma <- matrix(c(input$sigma_x^2, 
                          input$sigma_x * input$sigma_y * input$rho, 
                          input$sigma_x * input$sigma_y * input$rho, 
                          input$sigma_y^2), 2, 2, byrow = T)
        
        set.seed(123)     
        X <- rmvnorm(input$n, mu, Sigma)
        xgrid <- seq(-10, 10, length.out = 100)
        ygrid <- seq(-10, 10, length.out = 100)
        z <- outer(xgrid, ygrid, function(x, y, mean, sigma){return(dmvnorm(cbind(x, y), mean, sigma))}, 
                   mean = mu, sigma = Sigma)
        
        par(mar=c(4,4,2,2), family = 'serif')
        plot(X, pch = 16, xlab = TeX("$X_1$"), ylab = TeX("$X_2$"), 
             asp = 1, xlim = c(-10, 10), ylim = c(-10, 10))
        abline(v = 0, lty = 2)
        abline(h = 0, lty = 2)
        contour(xgrid, ygrid, z, nlevels = 10, add = T, col = cols[1], lwd = 2)
    })
    
    output$threed <- renderPlot({
        mu <- c(input$mu_x, input$mu_y)
        Sigma <- matrix(c(input$sigma_x^2, 
                          input$sigma_x * input$sigma_y * input$rho, 
                          input$sigma_x * input$sigma_y * input$rho, 
                          input$sigma_y^2), 2, 2, byrow = T)
        
        set.seed(123)     
        X <- rmvnorm(input$n, mu, Sigma)
        xgrid <- seq(-10, 10, length.out = 100)
        ygrid <- seq(-10, 10, length.out = 100)
        z <- outer(xgrid, ygrid, function(x, y, mean, sigma){return(dmvnorm(cbind(x, y), mean, sigma))}, 
                   mean = mu, sigma = Sigma)
        
        nbcol <- 100
        nrz <- nrow(z)
        ncz <- ncol(z)
        color <- alpha(rev(rainbow(nbcol, start = 0/6, end = 4/6)), 0.8)
        zfacet <- z[-1, -1] + z[-1, -ncz] + z[-nrz, -1] + z[-nrz, -ncz]
        zcol <- cut(zfacet, nbcol)
        
        persp(xgrid, ygrid, z, 
              col = color[zcol], xlab = 'X1', ylab = 'X2', zlab = "density", 
              theta = 25, phi = 55, r = 30, border = NA)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
