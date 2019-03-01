# Define server logic required to draw a histogram
Server <- shinyServer(function(input, output, session) {
    
    # MLE
    output$plot_MLE <- renderPlot(
        if(input$type == "bern"){
            x <- rbinom(as.numeric(input$s),1,as.numeric(input$p))
            m <- seq(from=0,to=1,length.out = 10000)
            likelihood <- 1
            for(i in 1:length(x)){
                if(x[i] == 1){
                    likelihood <- likelihood*m
                }
                else{ 
                    likelihood <- likelihood*(1-m)
                }
            }
            plot(x = m, y = likelihood,type = "l",xlab="p",ylim = range(likelihood))
            points(x = m[likelihood==max(likelihood)],y = max(likelihood),col="red",pch = 19)
        }
        else if(input$type == "geom"){
            x <- rgeom(as.numeric(input$s),as.numeric(input$p3))
            m <- seq(from=0,to=1,length.out = 10000)
            likelihood <- 1
            for(i in 1:length(x)){
                likelihood <- likelihood*((1-m)^(x[i]))*m
            }
            plot(x=m,y= likelihood,type = "l",xlab = "p",ylim = range(likelihood))
            points(x = m[likelihood==max(likelihood)],y= max(likelihood),col="red",pch = 19)
        }
        else if(input$type == "pois"){
            x <- rpois(as.numeric(input$s),as.numeric(input$lambda2))
            m <- seq(from=0,to=100,length.out = 10000)
            likelihood <- 1
            for(i in 1:length(x)){
                likelihood <- likelihood*exp(-m)*((m)^(x[i]))/factorial(x[i])
            }
            plot(x=m,y= likelihood,type = "l",xlab = "mu",ylim = range(likelihood))
            points(x = m[likelihood==max(likelihood)],y= max(likelihood),col="red",pch = 19)
        }
        else if(input$type == "exp"){
            x <- rexp(as.numeric(input$s),as.numeric(input$lambda))
            m <- seq(from=0,to=100,length.out = 10000)
            likelihood <- 1
            for(i in 1:length(x)){
                likelihood <- likelihood*m*exp(-m*x[i])
            }
            plot(x=m,y= likelihood,type = "l",xlab = "mu",ylim = range(likelihood))
            points(x = m[likelihood==max(likelihood)],y= max(likelihood),col="red",pch = 19)
        }
        else if(input$type == "norm"){
            x <- rnorm(as.numeric(input$s),as.numeric(input$miu),as.numeric(input$sigma))
            m <- seq(from=as.numeric(input$miu)-20,to=as.numeric(input$miu)+20,length.out = 100)
            n <- seq(from=1,to=as.numeric(input$sigma)+10,length.out = 100)
            likelihood <- matrix(1,nrow = 100,ncol = 100)
            for(j in 1:length(m)){
                for(t in 1:length(n)){
                    for(i in 1:length(x)){
                        likelihood[j,t] <- likelihood[j,t]*exp(-((x[i]-m[j])^2)/(2*(n[t]^2)))/(n[t]*sqrt(2*pi))
                    }
                }
            }
            persp3D(m,n,likelihood, nticks=5, ticktype="detailed",xlab="mean",ylab="standard deviation",zlab="likelihood")
            
        }
    )

    
})# End of the ShinyServer
shinyApp(ui, Server)