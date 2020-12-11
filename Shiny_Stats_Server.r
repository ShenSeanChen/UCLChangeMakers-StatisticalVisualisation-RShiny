# Define server logic required to draw a histogram
Server <- shinyServer(function(input, output, session) {
  
  ###############################################################################
  # Probability Distribution and Central Limit Theorem
  ###############################################################################
  
  # Defining population function for storing population data
  population <- reactive({ #executes the code inside {} and render data to population object
    if (input$dist == "norm") {rnorm(input$pop_size,mean=as.numeric(input$miu),sd=as.numeric(input$sigma))}
    else if (input$dist == "unif") {runif(input$pop_size,min=as.numeric(input$a),max=as.numeric(input$b))}
    else if (input$dist == "exp") {rexp(input$pop_size, rate = as.numeric(input$lambda))}
    else if (input$dist == "binom") {rbinom(input$pop_size, size=as.numeric(input$n), prob=as.numeric(input$p))}
    else if (input$dist == "nbinom") {rnbinom(input$pop_size, size=as.numeric(input$r), prob=as.numeric(input$p2))}
    else if (input$dist == "pois") {rpois(input$pop_size, lambda=as.numeric(input$lambda2))}
    else if (input$dist == "geom") {rgeom(input$pop_size, prob=as.numeric(input$p3))}
    else if (input$dist == "hyper") {rhyper(input$pop_size,m=as.numeric(input$M), n=as.numeric(input$N), k=as.numeric(input$K))}
    else if (input$dist == "chisq") {rchisq(input$pop_size,df=as.numeric(input$df))}
    else if (input$dist == "t") {rt(input$pop_size, df=as.numeric(input$df2))}
    else if (input$dist == "beta") {rbeta(input$pop_size, shape1=as.numeric(input$Alpha), shape2=as.numeric(input$Beta))}
    else if (input$dist == "gamma") {rgamma(input$pop_size, shape=as.numeric(input$k), scale=as.numeric(input$Theta))}
  }
  )
  
  #Defining sample mean function for storing sample mean data
  smpl_mean <- reactive({ #executes the code inside {} and render data to smpl_mean object
    for (i in 1:input$smpl_iterate) {
      if (i==1) {
        smpl_mean <- c(mean(sample(population(), input$smpl_size, replace = TRUE ))) #creating object for the first time
      } else {
        smpl_mean <- c(smpl_mean,mean(sample(population(), input$smpl_size, replace = TRUE ))) #apending data to existing smpl_mean object
      }
    }
    smpl_mean #printing smpl_mean object in order to return via reactive function to main smpl_mean object
  })
  
  #Rendering summary statistics and data information of population and sample mean data
  output$pop_summary <- renderPrint({summary(population())})
  output$pop_structure <- renderPrint({str(population())})
  output$smpl_mean_summary <- renderPrint({summary(smpl_mean())})
  output$smpl_mean_structure <- renderPrint({str(smpl_mean())})
  
  #Rendering population plot
  output$plot_pop <-renderPlot({
    plot(density(population()),axes=FALSE,xlab="",ylab="",main="", col="blue",lwd=2) #density plot
    par(new=TRUE) #new plot should not clean the frame before drawing as if it were on a new device
    hist(population(), freq = FALSE,main="Population Histogram & Density Plot", xlab = "") #ploting histogram
    abline(v = mean(population()), col = "red", lwd = 2) #ploting straight vertical red line for mean
    text(x=mean(population()), y=0,labels="Mean",col="red")
  })
  
  #Rendering sample plot
  output$plot_smpl_mean <-renderPlot({
    par(mfrow = c(1, 2))
    max_y<-max(hist(smpl_mean(),plot = FALSE)$density,
               dnorm(seq(min(smpl_mean()), max(smpl_mean()), 0.0001),mean(smpl_mean()), sd(smpl_mean())))
    hist(smpl_mean(), freq = FALSE, main="Sample Mean Histogram",  
         cex.main = 1.25, ylim=c(0,max_y), xlab="Blue line = N(sample mean, sample variance)")
    abline(v = mean(smpl_mean()), col = "red", lwd = 2)
    text(x=mean(population()), y=0,labels="Mean",col="red")
    lines(x = seq(min(smpl_mean()), max(smpl_mean()), 0.0001), 
          y = dnorm(seq(min(smpl_mean()), max(smpl_mean()), 0.0001),mean(smpl_mean()), sd(smpl_mean())),
          col = "blue", lwd = 2)
    qqnorm(smpl_mean())
    qqline(smpl_mean(), col = "blue",lwd=2)
  })
  
  
  ###############################################################################
  # Quantile Plots and Skewness
  ###############################################################################
  output$QQplots <- renderPlot({
    input$update
    n <- 1000
    x <- if (input$type_QQ == "Light-tailed"){
      runif(n)
    } else if(input$type_QQ == "Heavy-tailed"){
      rt(n, 2)
    } else if(input$type_QQ == "Normal"){
      rnorm(n)
    } else if(input$type_QQ == "Negatively skewed"){
      rbeta(n, 15, 2)
    } else if(input$type_QQ == "Positively skewed"){
      rgamma(n, 2)
    }
    
    par(mfrow = c(2, 2), mar = c(2, 2, 2, 2), oma = c(0, 0, 5, 0))
    
    qqnorm(x)
    qqline(x, col = "red")
    ### Parameters of corresponding normal distribution
    mu <- mean(x)
    sigma <- sd(x)
    ### Create vectors m quantiles of normal and chosen distributions
    normal_quantiles <- qnorm(c(.001, seq(.05, .99, .05), .999), mu, sigma)
    names(normal_quantiles) <- paste0(seq(0, 100, 5), "%")
    sample_quantiles <- quantile(x, seq(0, 1, .05))
    
    if (input$type_QQ == "Light-tailed"){
      density_quantiles <- dunif(sample_quantiles)
    } else if(input$type_QQ == "Heavy-tailed"){
      density_quantiles <- dt(sample_quantiles, 2)
    } else if(input$type_QQ == "Normal"){
      density_quantiles <- dnorm(sample_quantiles, mu, sigma)
    } else if(input$type_QQ == "Negatively skewed"){
      density_quantiles <- dbeta(sample_quantiles, 15, 2)
    } else if(input$type_QQ == "Positively skewed"){
      density_quantiles <- dgamma(sample_quantiles, 2)
    }
    ### Plot both distributions and quantiles
    plot(normal_quantiles, dnorm(normal_quantiles, mu, sigma), type = "b",
         xlim = range(normal_quantiles, sample_quantiles), 
         ylim = range(dnorm(normal_quantiles, mu, sigma), density_quantiles),
         col = "red", pch = 0,
         main = "Densities (lines) and quantiles (symbols)")
    lines(sample_quantiles, density_quantiles, type = "b")
    
    barplot(height = rbind(sample_quantiles, normal_quantiles), beside = TRUE,
            col = c("black", "red"),
            main = "Sample quantiles vs Normal quantiles")
    boxplot(x, rnorm(length(x), mu, sigma), border = c("black", "red"),
            names = c("Sample", "Corresponding Normal"), 
            main = "Sample quantiles vs Normal quantiles")
    title(main = if (input$type_QQ == "Light-tailed"){
      "Sample quantiles drawn from Uniform(0,1) (black) against corresponding normal quantiles (red)"
    } else if(input$type_QQ == "Heavy-tailed"){
      expression(bold(paste("Sample quantiles drawn from t"[2], " (black) against corresponding normal quantiles (red)")))
    } else if(input$type_QQ == "Normal"){
      paste0("Sample quantiles drawn from Normal(", round(mu, 1), ",", round(sigma^2, 1), ") (black) against corresponding normal quantiles (red)")
    } else if(input$type_QQ == "Negatively skewed"){
      "Sample quantiles drawn from Beta(15,2) (black) against corresponding normal quantiles (red)"
    } else if(input$type_QQ == "Positively skewed"){
      "Sample quantiles drawn from Gamma(2) (black) against corresponding normal quantiles (red)"
    }, outer = TRUE, cex.main = 1.5)
  })
  
  ###############################################################################
  # Maximum Likelihood Estimation
  ###############################################################################
  # MLE
  output$plot_MLE <- renderPlot(
    if(input$MLE_type == "bern"){
      x <- rbinom(as.numeric(input$MLE_s),1,as.numeric(input$MLE_p))
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
    else if(input$MLE_type == "geom"){
      x <- rgeom(as.numeric(input$MLE_s),as.numeric(input$MLE_p3))
      m <- seq(from=0,to=1,length.out = 10000)
      likelihood <- 1
      for(i in 1:length(x)){
        likelihood <- likelihood*((1-m)^(x[i]))*m
      }
      plot(x=m,y= likelihood,type = "l",xlab = "p",ylim = range(likelihood))
      points(x = m[likelihood==max(likelihood)],y= max(likelihood),col="red",pch = 19)
    }
    else if(input$MLE_type == "pois"){
      x <- rpois(as.numeric(input$MLE_s),as.numeric(input$MLE_lambda2))
      m <- seq(from=0,to=100,length.out = 10000)
      likelihood <- 1
      for(i in 1:length(x)){
        likelihood <- likelihood*exp(-m)*((m)^(x[i]))/factorial(x[i])
      }
      plot(x=m,y= likelihood,type = "l",xlab = "mu",ylim = range(likelihood))
      points(x = m[likelihood==max(likelihood)],y= max(likelihood),col="red",pch = 19)
    }
    else if(input$MLE_type == "exp"){
      x <- rexp(as.numeric(input$MLE_s),as.numeric(input$MLE_lambda))
      m <- seq(from=0,to=100,length.out = 10000)
      likelihood <- 1
      for(i in 1:length(x)){
        likelihood <- likelihood*m*exp(-m*x[i])
      }
      plot(x=m,y= likelihood,type = "l",xlab = "mu",ylim = range(likelihood))
      points(x = m[likelihood==max(likelihood)],y= max(likelihood),col="red",pch = 19)
    }
    else if(input$MLE_type == "norm"){
      x <- rnorm(as.numeric(input$MLE_s),as.numeric(input$MLE_miu),as.numeric(input$MLE_sigma))
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
      persp3D(m,n,likelihood, nticks=5, ticktype="detailed",xlab="mu",ylab="sd",zlab="likelihood" )
      
    }
  )
  
  
  ###############################################################################
  # Import data and run MLR with CheckboxGroup
  ###############################################################################
  dsnames <- c()
  data_set <- reactive({
    req(input$file_MLR)
    inFile_MLR <- input$file_MLR
    if (is.null(inFile_MLR)) {
      return(NULL) }
    
    if (input$fileType_Input_MLR == "1") {
      data_set<-read.csv(inFile_MLR$datapath,
                         header = TRUE,
                         stringsAsFactors = FALSE)
    } else {
      data_set<-read_excel(inFile_MLR$datapath)
    }
  })
  
  observe({
    req(input$file_MLR)
    dsnames <- names(data_set())
    
    selected <- reactive({
      if (input$select_all_var_MLR) {selected = unlist(dsnames)}
      if (!input$select_all_var_MLR) {selected = ""}
      selected
    })
    
    updateSelectInput(session, "selectInput_MLR",
                      label = "Select a response variable",
                      choices = dsnames,  
                      selected = dsnames[1])

    updateCheckboxGroupInput(session, "inCheckboxGroup_MLR",
                             label = "Select explanatory variables",
                             choices = dsnames,  
                             selected = selected())

  })
  
  
  output$contents_MLR<- renderDataTable({
    req(input$inCheckboxGroup_MLR)
    req(input$selectInput_MLR)
    selected_data_MLR <- data_set()[,c(input$selectInput_MLR,input$inCheckboxGroup_MLR)]
    datatable(data = selected_data_MLR, 
              options = list(lengthChange = FALSE,pageLength = 10),rownames = TRUE, width=80,
              extensions = 'Select', selection = list(target = 'column'))
  })
  
  output$datatable_Transform <- renderDataTable({
    if (length(input$contents_MLR_columns_selected) != 0) {
      datatable(data_set()[, input$contents_MLR_columns_selected, drop = FALSE])}
  }) 
  
  output$visualisation_features <- renderPlot({
    data_visual <- na.omit(data_set()[,input$contents_MLR_columns_selected])
    nums <- unlist(lapply(data_visual, is.numeric))  
    plot(data_visual[,nums])
  })
  
  
  output$choose_columns_MLR <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
  })
  
  model_MLR <- reactive({
    model<-lm(as.formula(paste(input$selectInput_MLR,"~",paste(input$inCheckboxGroup_MLR,collapse="+"))),
              data=data_set())
    model
  })
  
  output$text1<-renderPrint({
    summary(model_MLR())
    
  })
  
  output$diag_MLR <- renderPlot({
    par(mfrow=c(2,2))
    plot(model_MLR(), which = 1:4)
  })
  
  output$text2<-renderPrint({
    anova(model_MLR())
  })
  
  ###############################################################################
  # Build glm models and predict values based on that
  ###############################################################################
  input_link <- reactive({
    # Define glm family and link functions
    input_family <- input$family
    input_link <- input$link
    if (input_link == "Canonical Link")
    {if (input_family == "binomial"){input_link = "(link='logit')"}
      if (input_family == "gaussian"){input_link = "(link='identity')"}
      if (input_family == "Gamma"){input_link = "(link='inverse')"}
      if (input_family == "inverse.gaussian"){input_link = "(link='1/mu^2')"}
      if (input_family == "poisson"){input_link = "(link='log')"}
      if (input_family == "quasi"){input_link = "(link='identity')"}
      if (input_family == "quasibinomial"){input_link = "(link='logit')"}
      if (input_family == "quasipoisson"){input_link = "(link='log')"}}
    input_link
  })
  
  glm1 <- reactive({
    input_x <- input$x
    input_y <- input$y
    indVar <- myData[,which(colnames(myData) == input_x)]
    depVar <- myData[,which(colnames(myData) == input_y)]

    input_family <- input$family
    family_link <- eval(parse(text = paste(input_family, input_link(), sep = "")))
    
    if (!is.numeric(depVar)) {depVar <- factor(depVar)}
    if (!is.numeric(indVar)) {indVar <- factor(indVar)}

    if (input$t_x == "None"){glm1 <- glm(depVar ~ indVar, family=family_link)}
    if (input$t_x == "log"){glm1 <- glm(depVar ~ log(indVar), family=family_link)}
    if (input$t_x == "sqrt"){glm1 <- glm(depVar ~ sqrt(indVar), family=family_link)}
    if (input$t_x == "square"){glm1 <- glm(depVar ~ indVar**2, family=family_link)}
    if (input$t_x == "third power"){glm1 <- glm(depVar ~ indVar**3, family=family_link)}
    glm1
    
  })
  
  # Make predictions for glm1
  glm1pred <- reactive({
    input_x <- input$x
    input_y <- input$y
    indVar <- myData[,which(colnames(myData) == input_x)]
    depVar <- myData[,which(colnames(myData) == input_y)]
    
    # if (is.numeric(indVar) & is.numeric(depVar)) {x.grid <- seq(min(c(indVar,depVar)),max(c(indVar,depVar)),by=0.1)}
    # else if (!is.numeric(indVar)) {x.grid <- indVar}
    # else if (is.numeric(indVar) & !is.numeric(depVar)) {x.grid <- seq(min(indVar),max(indVar),by=0.1)}
    
    
    newdataframe = data.frame(indVar=indVar)
    glmpred <- predict(glm1(),newdata=newdataframe,se.fit=TRUE)
    
    z <- qnorm(0.975,mean=0,sd=1,lower.tail=TRUE)     # That's the z-value
    if (input_link() == "(link='log')"){glmpred$UL95 <- exp(glmpred$fit + (z*glmpred$se.fit))
                                        glmpred$LL95 <- exp(glmpred$fit - (z*glmpred$se.fit))
                                        glmpred$fit <- exp(glmpred$fit)}
    else if (input_link() == "(link='inverse')"){glmpred$UL95 <- 1/(glmpred$fit + (z*glmpred$se.fit))
                                glmpred$LL95 <- 1/(glmpred$fit - (z*glmpred$se.fit))
                                glmpred$fit <- 1/(glmpred$fit)}
    else if (input_link() == "(link='1/mu^2')"){glmpred$UL95 <- sqrt(1/(glmpred$fit + (z*glmpred$se.fit)))
                                glmpred$LL95 <- sqrt(1/(glmpred$fit - (z*glmpred$se.fit)))
                                glmpred$fit <- sqrt(1/(glmpred$fit))}
    else if (input_link() == "(link='logit')"){glmpred$UL95 <- exp(glmpred$fit + (z*glmpred$se.fit))/(1+exp(glmpred$fit + (z*glmpred$se.fit)))
                                glmpred$LL95 <- exp(glmpred$fit - (z*glmpred$se.fit))/(1+exp(glmpred$fit - (z*glmpred$se.fit)))
                                glmpred$fit <- exp(glmpred$fit)/(1+exp(glmpred$fit))}
    else if (input_link() == "(link='probit')"){glmpred$UL95 <- pnorm(glmpred$fit + (z*glmpred$se.fit))
                                glmpred$LL95 <- pnorm(glmpred$fit - (z*glmpred$se.fit))
                                glmpred$fit <- pnorm(glmpred$fit)}
    # else if (input_link() == ""){glmpred$UL95 <- exp(glmpred$fit + (z*glmpred$se.fit))
    #                             glmpred$LL95 <- exp(glmpred$fit - (z*glmpred$se.fit))
    #                             glmpred$fit <- exp(glmpred$fit)}
    # else if (input_link() == ""){glmpred$UL95 <- exp(glmpred$fit + (z*glmpred$se.fit))
    #                             glmpred$LL95 <- exp(glmpred$fit - (z*glmpred$se.fit))
                                # glmpred$fit <- exp(glmpred$fit)}
    else {
      glmpred$UL95 <- glmpred$fit + (z*glmpred$se.fit)
      glmpred$LL95 <- glmpred$fit - (z*glmpred$se.fit)
    }
    
    glmpred
    
  })
  
  # output scatterplot and regression lines
  output$plot_glm1 <- renderPlot({
    input_x <- input$x
    input_y <- input$y
    indVar <- myData[,which(colnames(myData) == input_x)]
    depVar <- myData[,which(colnames(myData) == input_y)]
    
    if (is.numeric(indVar) & is.numeric(depVar)) {x.grid <- seq(min(c(indVar,depVar)),max(c(indVar,depVar)),by=0.1)}
    else if (!is.numeric(indVar)) {x.grid <- indVar}
    else if (is.numeric(indVar) & !is.numeric(depVar)) {x.grid <- seq(min(indVar),max(indVar),by=0.1)}
    
    if (!is.numeric(depVar)) {depVar <- factor(depVar)}
    if (!is.numeric(indVar)) {indVar <- factor(indVar)}
    
    # Plot the scatterplot with the data given
    # plot(depVar ~ indVar,col="lightblue",
    #      pch=15,log="x",xlab=input_x,ylab=input_y)


    UL95 <- glm1pred()$UL95   # Upper and lower CI limits
    LL95 <- glm1pred()$LL95  
    # for linear predictor
    # 
    # if(is.numeric(indVar) & is.numeric(depVar)){
    # lines(x.grid,glm1pred()$fit,col="blue")
    #       # xlim = c(min(c(indVar,depVar)), max(c(indVar,depVar))),
    #       # ylim = c(min(c(indVar,depVar)), max(c(indVar,depVar))), asp=1)
    # lines(x.grid,UL95,lty=2,col="blue")
    #       # xlim = c(min(c(indVar,depVar)), max(c(indVar,depVar))),
    #       # ylim = c(min(c(indVar,depVar)), max(c(indVar,depVar))), asp=1)
    # lines(x.grid,LL95,lty=2,col="blue")
    #       # xlim = c(min(c(indVar,depVar)), max(c(indVar,depVar))),
    #       # ylim = c(min(c(indVar,depVar)), max(c(indVar,depVar))), asp=1)
    # 
    # legend("topleft",col=c("lightblue","blue","blue"),
    #        pch=c(15,NA,NA),lty=c(NA,1,2),
    #        legend=c("Observations","Fitted relationship","95% CI"))}
    
    if(is.numeric(indVar) & is.numeric(depVar)){
      data_ggplot <- data.frame(indVar, depVar,glm1pred()$fit)
      
      ggplot(data_ggplot, aes(indVar, depVar)) +
        geom_point(position=position_jitter(height=0.03, width=0))+
        geom_line(aes(indVar, glm1pred()$fit),alpha=0.2, size=1) +
        geom_ribbon(aes(ymin=LL95, ymax=UL95), alpha = 0.2)+
        xlab(input_x) + ylab(input_y)
    }
    else {
      rapeThreat <- myData$rapeThreat
      data_bi <- data.frame(indVar, depVar, rapeThreat)
      
      ggplot(data_bi, aes(indVar, as.numeric(depVar)-1, color=rapeThreat)) +
        stat_smooth(method="glm", formula=y~x, family = input$family,
                    alpha=0.2, size=2, aes(fill=rapeThreat)) +
        geom_point(position=position_jitter(height=0.03, width=0)) +
        xlab(input_x) + ylab(input_y)}
  })
  
  # diagnostic plot for glm1
  output$diagnostic_plot1 <- renderPlot({
    # RegressionPlots(glm1())
    par(mfrow = c(2,2))
    plot(glm1(), which = 1:4)
  })
  
  output$summary_glm1 <- renderPrint({
    summary(glm1())
  })
  
  # Make predictions for ggplot glm
  ggplotglm1pred <- reactive({
    input_x <- input$x
    input_y <- input$y
    indVar <- myData[,which(colnames(myData) == input_x)]
    depVar <- myData[,which(colnames(myData) == input_y)]
    newdataframe = data.frame(indVar=indVar)
    predict(glm1(),newdata=newdataframe,se.fit=TRUE)
  })
  
  
  ###############################################################################
  # Build higher dimensional glm models and predict values based on that
  ###############################################################################
  glm2 <- reactive({
    selected_data_MLR <- data_set()[,c(input$selectInput_MLR,input$inCheckboxGroup_MLR)]
    indVar <- as.matrix(selected_data_MLR[,-1])
    depVar <- as.matrix(selected_data_MLR[,1])
    
    
    
    # Define glm family and link functions
    input_family <- input$family2
    input_link <- input$link2
    
    if (input_link == "Canonical Link")
    {if (input_family == "binomial"){input_link = "(link='logit')"}
      if (input_family == "gaussian"){input_link = "(link='identity')"}
      if (input_family == "Gamma"){input_link = "(link='inverse')"}
      if (input_family == "inverse.gaussian"){input_link = "(link='1/mu^2')"}
      if (input_family == "poisson"){input_link = "(link='log')"}
      if (input_family == "quasi"){input_link = "(link='identity')"}
      if (input_family == "quasibinomial"){input_link = "(link='logit')"}
      if (input_family == "quasipoisson"){input_link = "(link='log')"}}
    
    family_link <- eval(parse(text = paste(input_family, input_link, sep = "")))
    
    #glm2 <- glm(as.formula(paste(input$selectInput_MLR,"~",paste(input$inCheckboxGroup_MLR,collapse="+"))), 
     #           data = data_set(), family = family_link, na.action = na.omit)
    glm2 <- glm(depVar ~ indVar, family=family_link)
    glm2
  })
  
  # diagnostic plot for glm1
  output$diagnostic_plot2 <- renderPlot({
    # RegressionPlots(glm1())
    par(mfrow = c(2,2))
    plot(glm2(), which = 1:4)
  })
  
  output$summary_glm2 <- renderPrint({
    summary(glm2())
  })

  
  ###############################################################################
  # Gams
  ###############################################################################
  dsnames <- c()
  data_set_GAM <- reactive({
    req(input$file_GAM)
    inFile_GAM <- input$file_GAM
    if (is.null(inFile_GAM)) {
      return(NULL) }
    
    if (input$fileType_Input_GAM == "1") {
      data_set<-read.csv(inFile_GAM$datapath,
                         header = TRUE,
                         stringsAsFactors = FALSE)
    } else {
      data_set<-read_excel(inFile_GAM$datapath)
    }
  })
  
  observe({
    req(input$file_GAM)
    dsnames <- names(data_set_GAM())
    # cb_options <- list()
    # cb_options[dsnames] <- dsnames
    updateCheckboxGroupInput(session, "inCheckboxGroup_GAM",
                             label = "Select explanatory variables",
                             choices = dsnames,  
                             selected = "")
    updateSelectInput(session, "selectInput_GAM",
                      label = "Select a response variable",
                      choices = dsnames,  
                      selected = "")
  })
  
  
  output$contents_GAM<- renderDataTable({
    req(input$inCheckboxGroup_GAM)
    req(input$selectInput_GAM)
    selected_data_GAM <- data_set_GAM()[,c(input$selectInput_GAM,input$inCheckboxGroup_GAM)]
    datatable(data = selected_data_GAM, 
              options = list(lengthChange = FALSE,pageLength = 10),rownames = TRUE, width=80,
              extensions = 'Select', selection = list(target = 'column'))
  })
  

  
  family_link_GAM <- reactive({
      # Define glm family and link functions
      input_family_GAM <- input$family_GAM
      input_link_GAM <- input$link_GAM
      
      if (input_link_GAM == "Canonical Link")
      {if (input_family_GAM == "binomial"){input_link_GAM = "(link='logit')"}
        if (input_family_GAM == "gaussian"){input_link_GAM = "(link='identity')"}
        if (input_family_GAM == "Gamma"){input_link_GAM = "(link='inverse')"}
        if (input_family_GAM == "inverse.gaussian"){input_link_GAM = "(link='1/mu^2')"}
        if (input_family_GAM == "poisson"){input_link_GAM = "(link='log')"}
        if (input_family_GAM == "quasi"){input_link_GAM = "(link='identity')"}
        if (input_family_GAM == "quasibinomial"){input_link_GAM = "(link='logit')"}
        if (input_family_GAM == "quasipoisson"){input_link_GAM = "(link='log')"}}
      
      family_link_GAM <- eval(parse(text = paste(input_family_GAM, input_link_GAM, sep = "")))
      family_link_GAM
  })
  
  # Build the GAM model
  model_GAM <- reactive({
  #  model_GAM<-gam(as.formula(paste(input$selectInput_GAM," ~ s(", 
   #                paste(names(data_set_GAM()[,input$contents_GAM_columns_selected]),collapse=")+s(" ), ")")),
    #               family = family_link_GAM(), data=data_set_GAM())
    data_smooth <- na.omit(data_set_GAM()[,input$contents_GAM_columns_selected])
    nums <- unlist(lapply(data_smooth, is.numeric))  
    model_GAM <- gam(as.formula(paste(input$selectInput_GAM, " ~ s(", 
                                    paste(names(data_smooth[,nums]),collapse=",k=2)+s(" ), ",k=2)"
                                   # , "+",paste(names(data_set_GAM()[,-which(names(data_set_GAM()) %in% names(data_smooth[,nums]))]), collapse="+")
                                   )),
                   family=family_link_GAM(), data=data_set_GAM())
    model_GAM
  })
  
  # Summary Output
  output$text1_GAM<-renderPrint({
    summary(model_GAM())
  })
  
  # ANOVA Output
  output$text2_GAM<-renderPrint({
    anova(model_GAM())
  })
  
  # Visualiation on the columns selected: contents_XXX_columns_selected
  output$visualisation_features_GAM <- renderPlot({
    data_visual <- na.omit(data_set_GAM()[,input$contents_GAM_columns_selected])
    nums <- unlist(lapply(data_visual, is.numeric))  
    plot(data_visual[,nums])
  })
  
  # Just in case no columns are selected
  output$choose_columns_GAM <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
  })
  
  # Plot the GAM model
  output$plot_GAM <- renderPlot({
    data_smooth <- na.omit(data_set_GAM()[,input$contents_GAM_columns_selected])
    nums <- unlist(lapply(data_smooth, is.numeric))
    vars <- names(data_smooth[,nums])
    
    map(vars, function(x){
      p <- plotGAM(model_GAM(), smooth.cov = x) 
          # geom_point(data = data_set_GAM(), aes_string(y = input$selectInput_GAM, x = x), alpha = 0.2) 
          #geom_rug(data = data_set_GAM(), aes_string(y = input$selectInput_GAM, x = x ), alpha = 0.2)
      g <- ggplotGrob(p)
    }) %>%
    {grid.arrange(grobs = (.), ncol = 1, nrow = length(vars))}
    
  })
  
  
  # output$plot_GAM <- renderPlot({
  #   data_smooth <- na.omit(data_set_GAM()[,input$contents_GAM_columns_selected])
  #     nums <- unlist(lapply(data_smooth, is.numeric))
  #     vars <- names(data_smooth[,nums])
  #     
  #   ggplot(data_smooth, aes(indVar, depVar)) +
  #     geom_point(position=position_jitter(height=0.03, width=0))+
  #     geom_line(aes(indVar, glm1pred()$fit),alpha=0.2, size=1) +
  #     geom_ribbon(aes(ymin=LL95, ymax=UL95), alpha = 0.2)+
  #     xlab(input_x) + ylab(input_y)
  # })
  
  output$diagnostic_plot_GAM <- renderPlot({
    gam.check(model_GAM())
  })
  
 # model<-lm(as.formula(paste(input$selectInput_MLR,"~",paste(input$inCheckboxGroup_MLR,collapse="+"))),data=data_set())
  
  ###############################################################################
  # Survival Analysis
  ###############################################################################
  datasetInput_SA <- reactive({
    switch(input$Dataset_SA,
           "lung" = lung,
           # "cgd" = cgd,
           "pbc" = pbc)
  })
  
  # for the suitable covariates
  dynamicCovariate_SA <- reactive({
    dataset_SA <- datasetInput_SA()
    col_num = c()
    for (i in 1:ncol(dataset_SA)){
      if (length(unique(dataset_SA[i])[,1]) <= 3){
        col_num <- append(col_num, i)
      }
    }
    dataset_SA[,col_num]
  })
  
  output$K_M_info <- renderUI({
    withMathJax(
      helpText('n individuals, $$t_1, t_2,\\dots,t_n$$ event time, r death times among individuals')
      #,
      #   helpText('$$r \\leq n$$'),
      #   helpText('rearranging event time, $$t_{(1)} < t_{(2)} < \\dots < t_{(n)}$$'),
      #   helpText('$$n_{j}$$ people are alive just before the $$t_{(j)}$$'),
      #   helpText('$$d_j$$ people died at $$t_{(j)}$$, $$j = 1,2,\\dots,r$$'),
      #   helpText('P(One dies between $$t_{(k)} - \\delta$$ and $$t_{(j)}$$) = $$d_j/n_j$$\\
      # P(One survives between $$t_{(k)} - \\delta$$ and $$t_{(j)}$$) = $$(n_j - d_j )/n_j$$\\
      # P(One survives from $$t_{(j)}$$ to $$t_{(j+1)} - \\delta$$) is unity\\
      # P(One survives from $$t_{(j)} - \\delta$$ to $$t_{(j)}$$ and from $$t_{(j)}$$ to $$t_{(j+1)}$$) = $$(n_j - d_j )/n_j$$\\
      # an estimate of the probability of surviving the interval from $$t_{(j)}$$ to $$t_{(j+1)}$$ as $$\\delta \\rightarrow 0$$\\
      # P(one survives at anytime t in the $$k$$th time interval from $$t_{(j)}$$ to $$t_{(j+1)}$$) = $$\\hat{S}(t) = \\prod_{j = 1}^k\\left(\\frac{n_j-d_j}{n_j}\\right)$$, $$t_{(r+1)}$$ is $$\\infty$$, $$t_{(j)}$$ to $$t_{(j+1)}$$, $$k = 1,2,\\dots,r$$'
      #            )
    )
  })
  
  
  
  output$summary_SA <- renderPrint({
    dataset_SA <- datasetInput_SA()
    summary(dataset_SA)
  })
  
  covariateInput_SA <- reactive({
    input$covariate_SA
  })
  
  
  output$DataSetTable_SA <- renderDataTable({dataset_SA <- datasetInput_SA()
  datatable(dataset_SA)})
  output$DataSetPlot1_SA <- renderPlot({
    dataset_SA <- datasetInput_SA()
    covariate_SA <- as.character(noquote(covariateInput_SA()))
    # print(class(covariate))
    fit_SA <- survfit(Surv(time, status) ~ dataset_SA[,covariate_SA], data = dataset_SA)
    # p <- ggsurvplot(fit)
    # print(dataset)
    if (length(covariate_SA) == 3){col_cov_SA = c("Red","Blue",'Black')
    line_type_SA = c(1,1,1)}
    else {col_cov_SA = c('Red', 'Blue')
    line_type_SA = c(1,1)}
    #p <- plot(fit,col= col_cov)
    # c(unique(dataset[,covariate]))
    plot(fit_SA,col= col_cov_SA)
    legend("topright", as.character(unique(dataset_SA[,covariate_SA])), col = col_cov_SA,lty = line_type_SA)
    # p <- ggsurvplot(fit
    #                 # ,
    #                 # data = dataset,
    #                 # title = paste("Survival Curves"),
    #                 # pval = TRUE, pval.method = TRUE,    # Add p-value &  method name
    #                 # surv.median.line = "hv",            # Add median survival lines
    #                 # # legend.title = covariate,               # Change legend titles
    #                 # # legend.labs = c("Male", "female"),  # Change legend labels
    #                 # palette = "jco",                    # Use JCO journal color palette
    #                 # risk.table = TRUE,                  # Add No at risk table
    #                 # cumevents = TRUE,                   # Add cumulative No of events table
    #                 # tables.height = 0.15,               # Specify tables height
    #                 # tables.theme = theme_cleantable(),  # Clean theme for tables
    #                 # tables.y.text = FALSE               # Hide tables y axis text
    # )
    # p
  })
  
  # observe({
  #   updateSelectInput(session, "covariate", choices = names(datasetInput())) 
  # })
  observe({
    updateSelectInput(session, "covariate_SA", choices = names(dynamicCovariate_SA())) 
  })
  
  # fit <- survfit(Surv(time, status) ~ sex, data = lung)
  # p <- ggsurvplot(fit, data = lung,
  #                 title = "Survival Curves",
  #                 pval = TRUE, pval.method = TRUE,    # Add p-value &  method name
  #                 surv.median.line = "hv",            # Add median survival lines
  #                 legend.title = "Sex",               # Change legend titles
  #                 legend.labs = c("Male", "female"),  # Change legend labels
  #                 palette = "jco",                    # Use JCO journal color palette
  #                 risk.table = TRUE,                  # Add No at risk table
  #                 cumevents = TRUE,                   # Add cumulative No of events table
  #                 tables.height = 0.15,               # Specify tables height
  #                 tables.theme = theme_cleantable(),  # Clean theme for tables
  #                 tables.y.text = FALSE               # Hide tables y axis text
  # )
  # 
  # output$DataSetPlot1 <- renderPlot({
  #   p
  # })
  
  
  
  
  observe(if (input$Dataset_SA == 'lung'){ output$DataSetInfo_SA <- renderText({
    paste('NCCTG Lung Cancer Data',
          'Survival in patients with advanced lung cancer from the North Central Cancer Treatment Group. Performance scores rate how well the patient can perform usual daily activities.',
          
          '',
          
          'inst:	 Institution code',
          'time: Survival time in days',
          'status: censoring status 1 = censored, 2 = dead',
          'age: age in years',
          'sex: Male = 1 Female = 2',
          'ph.ecog: ECOG performance score(0 = good 5 = dead',
          'ph.karno: Karnofsky performance score (bad=0-good=100) rated by physician',
          'pat.karno: Karnofsky performance score as rated by patient',
          'meal.cal: Calories consumed at meals',
          'wt.loss: Weight loss in last six months',
          '',
          'Source: Terry Therneau',
          sep="\n")
  })
  
  })
  observe(if (input$Dataset_SA == 'cgd'){output$DataSetInfo_SA <- renderText({
    paste('Chronic Granulotomous Disease data',
          'Data are from a placebo controlled trial of gamma interferon in chronic granulotomous disease (CGD). Contains the data on time to serious infections observed through end of study for each patient.',
          '',
          'The cgd0 data set is in the form found in the references, with one line per patient and no recoding of the variables. The cgd data set (this one) has been cast into (start, stop] format with one line per event, and covariates such as center recoded as factors to include meaningful labels.',
          '',
          'id subject identification number',
          'center enrolling center',
          'random date of randomization',
          'treatment placebo or gamma interferon',
          'sex sex',
          'age age in years, at study entry',
          'height height in cm at study entry',
          'weight weight in kg at study entry',
          'inherit pattern of inheritance',
          'steroids use of steroids at study entry,1=yes',
          'propylac use of prophylactic antibiotics at study entry',
          'hos.cat a categorization of the centers into 4 groups',
          'tstart, tstop start and end of each time interval',
          'status 1=the interval ends with an infection',
          'enum observation number within subject',
          '',
          'source: Fleming and Harrington, Counting Processes and Survival Analysis, appendix D.2.',
          sep="\n")
  })})
  
  observe(if (input$Dataset_SA == 'pbc'){output$DataSetInfo_SA <- renderText({
    paste('Mayo Clinic Primary Biliary Cirrhosis Data',
          '',
          'This data is from the Mayo Clinic trial in primary biliary cirrhosis (PBC) of the liver conducted between 1974 and 1984. A total of 424 PBC patients, referred to Mayo Clinic during that ten-year interval, met eligibility criteria for the randomized placebo controlled trial of the drug D-penicillamine. The first 312 cases in the data set participated in the randomized trial and contain largely complete data. The additional 112 cases did not participate in the clinical trial, but consented to have basic measurements recorded and to be followed for survival. Six of those cases were lost to follow-up shortly after diagnosis, so the data here are on an additional 106 cases as well as the 312 randomized participants.',
          '',
          'age:	 in years',
          'albumin:	 serum albumin (g/dl)',
          'alk.phos:	 alkaline phosphotase (U/liter)',
          'ascites:	 presence of ascites',
          'ast:	 aspartate aminotransferase, once called SGOT (U/ml)',
          'bili:	 serum bilirunbin (mg/dl)',
          'chol:	 serum cholesterol (mg/dl)',
          'copper:	 urine copper (ug/day)',
          'edema:	 0 no edema, 0.5 untreated or successfully treated 1 edema despite diuretic therapy',
          'hepato:	 presence of hepatomegaly or enlarged liver',
          'id:	 case number',
          'platelet:	 platelet count',
          'protime:	 standardised blood clotting time',
          'sex:	 m/f',
          'spiders:	 blood vessel malformations in the skin',
          'stage:	 histologic stage of disease (needs biopsy)',
          'status:	 status at endpoint, 0/1/2 for censored, transplant, dead',
          'time:	 number of days between registration and the earlier of death,',
          'transplantion, or study analysis in July, 1986',
          'trt:	 1/2/NA for D-penicillmain, placebo, not randomised',
          'trig:	 triglycerides (mg/dl)',
          sep="\n")
  })})
  
  ##############################################################################
  # Simplex Algorithm
  ##############################################################################
  a_simplex<-reactive({as.vector(as.numeric(unlist(strsplit(input$objective_simplex,","))))})
  A1_full_simplex<-reactive({
    if (req(input$small_simplex)!=0){
      A1_full_simplex<-matrix(as.numeric(unlist(strsplit(input$small_c_simplex,","))),nrow=input$small_simplex,byrow=TRUE)
      #A1_simplex<-A1_full_simplex[,1:(ncol(A1_full_simplex)-1)]
      #b1_simplex<-A1_full_simplex[,ncol(A1_full_simplex)]
    }
  })
  A3_full_simplex<-reactive({
    if (req(input$equal_simplex)!=0){
      A3_full_simplex<-matrix(as.numeric(unlist(strsplit(input$equal_c_simplex,","))),nrow=input$equal_simplex,byrow=TRUE)
    }
  })  
  A2_full_simplex<-reactive({
    if (req(input$big_simplex)!=0){
      A2_full_simplex<-matrix(as.numeric(unlist(strsplit(input$big_c_simplex,","))),nrow=input$big_simplex,byrow=TRUE)
    }
  })  
  
  
  lp_simplex<-reactive({
    
    if (req(input$small_simplex)==0 & req(input$equal_simplex)==0 & req(input$big_simplex)==0){
      lp_simplex<-return("Input invalid")
    }
    else if (req(input$small_simplex)!=0 & req(input$equal_simplex)==0 & req(input$big_simplex)==0){
      lp_simplex<-simplex(a=a_simplex(),
                          A1=A1_full_simplex()[,1:(ncol(A1_full_simplex())-1),drop=FALSE],b1=as.vector(A1_full_simplex()[,3]))
      if (req(input$maxmin_simplex)=="max"){update(lp_simplex,maxi=TRUE)}
      else if (req(input$maxmin_simplex)=="min"){update(lp_simplex,maxi=FALSE)}
    }
    else if (req(input$small_simplex)==0 & req(input$equal_simplex)!=0 & req(input$big_simplex)==0){
      lp_simplex<-simplex(a=a_simplex(),
                          A3=A3_full_simplex()[,1:(ncol(A3_full_simplex())-1),drop=FALSE],b3=as.vector(A3_full_simplex()[,3]))
      if (input$maxmin_simplex=="max"){update(lp_simplex,maxi=TRUE)}
      else if (req(input$maxmin_simplex)=="min"){update(lp_simplex,maxi=FALSE)}
    } 
    else if (req(input$small_simplex)==0 & req(input$equal_simplex)==0 & req(input$big_simplex)!=0){
      lp_simplex<-simplex(a=a_simplex(),
                          A2=A2_full_simplex()[,1:(ncol(A2_full_simplex())-1),drop=FALSE],b2=as.vector(A2_full_simplex()[,3]))
      if (input$maxmin_simplex=="max"){update(lp_simplex,maxi=TRUE)}
      else if (req(input$maxmin_simplex)=="min"){update(lp_simplex,maxi=FALSE)}
    }
    else if (req(input$small_simplex)!=0 & req(input$equal_simplex)!=0 & req(input$big_simplex)==0){
      lp_simplex<-simplex(a=a_simplex(),
                          A1=A1_full_simplex()[,1:(ncol(A1_full_simplex())-1),drop=FALSE],b1=as.vector(A1_full_simplex()[,3]),
                          A3=A3_full_simplex()[,1:(ncol(A3_full_simplex())-1),drop=FALSE],b3=as.vector(A3_full_simplex()[,3]))
      if (input$maxmin_simplex=="max"){update(lp_simplex,maxi=TRUE)}
      else if (req(input$maxmin_simplex)=="min"){update(lp_simplex,maxi=FALSE)}
    }
    else if (req(input$small_simplex)!=0 & req(input$equal_simplex)==0 & req(input$big_simplex)!=0){
      lp_simplex<-simplex(a=a_simplex(),
                          A1=A1_full_simplex()[,1:(ncol(A1_full_simplex())-1),drop=FALSE],b1=as.vector(A1_full_simplex()[,3]),
                          A2=A2_full_simplex()[,1:(ncol(A2_full_simplex())-1),drop=FALSE],b2=as.vector(A2_full_simplex()[,3]))
      if (input$maxmin_simplex=="max"){update(lp_simplex,maxi=TRUE)}
      else if (req(input$maxmin_simplex)=="min"){update(lp_simplex,maxi=FALSE)}
    }
    else if (req(input$small_simplex)==0 & req(input$equal_simplex)!=0 & req(input$big_simplex)!=0){
      lp_simplex<-simplex(a=a_simplex(),
                          A2=A1_full_simplex()[,1:(ncol(A1_full_simplex())-1),drop=FALSE],b2=as.vector(A2_full_simplex()[,3]),
                          A3=A3_full_simplex()[,1:(ncol(A3_full_simplex())-1),drop=FALSE],b3=as.vector(A3_full_simplex()[,3]))
      if (input$maxmin_simplex=="max"){update(lp_simplex,maxi=TRUE)}
      else if (req(input$maxmin_simplex)=="min"){update(lp_simplex,maxi=FALSE)}
    }
    else if (req(input$small_simplex)!=0 & req(input$equal_simplex)!=0 & req(input$big_simplex)!=0){
      lp_simplex<-simplex(a=a_simplex(),
                          A1=A1_full_simplex()[,1:(ncol(A1_full_simplex())-1),drop=FALSE],b1=as.vector(A1_full_simplex()[,3]),
                          A2=A2_full_simplex()[,1:(ncol(A2_full_simplex())-1),drop=FALSE],b2=as.vector(A2_full_simplex()[,3]),
                          A3=A3_full_simplex()[,1:(ncol(A3_full_simplex())-1),drop=FALSE],b3=as.vector(A3_full_simplex()[,3]))
      if (input$maxmin_simplex=="max"){update(lp_simplex,maxi=TRUE)}
      else if (req(input$maxmin_simplex)=="min"){update(lp_simplex,maxi=FALSE)}
    }
  })
  
  output$result2_simplex<-renderPrint({
    lp_simplex()
  })
  
  output$plot_simplex <- renderPlot({
    coeff <- a_simplex()
    
    num_constr_small <- length(A1_full_simplex())/3
    num_constr_big <- length(A2_full_simplex())/3
    num_constr_equal <- length(A3_full_simplex())/3
    num_constr_total <- num_constr_small + num_constr_big + num_constr_equal
    
    A <- rbind(A1_full_simplex()[c((1:num_constr_small)*3 - 2, (1:num_constr_small)*3 - 1)],
               (-1)*A2_full_simplex()[c((1:num_constr_big)*3 - 2, (1:num_constr_big)*3 - 1)])
    b <- rbind(A1_full_simplex()[(1:num_constr_small)*3],
               (-1)*A2_full_simplex()[(1:num_constr_big)*3])
    
    ### Ignoring equality constraints
    cPoints <- cornerPoints(A, b)
    #iPoints <- integerPoints(A, b)
    
    plotPolytope(cPoints, cPoints, iso = coeff)
  })
  
  ################################################################################
  # Brownian Motion
  ###############################################################################
  output$distPlot_brownian <- renderPlot({
    
    # Take the inputs
    n = input$npt
    T = input$T_brownian
    nsim = input$nsim;
    
    # Build up matrices for plotting
    t = seq(0, T, by = T/n)
    x <- matrix(sqrt(T/n)*rnorm(n*nsim, mean=0, sd=1), nrow=nsim)
    xt <- cbind(rep(0, nsim), t(apply(x, 1, cumsum)))
    xtave <- apply(xt,2,mean)
    
    # Visualise the simulations
    plot(t, xt[1, ], xlab = "time 0:T", ylab = "W(t)", 
         ylim=c(-3*sqrt(T), 3*sqrt(T)), type="l", col="blue")
    title(paste(nsim, "simulated paths of Brownian motion with", 
                n, "points each"))
    if (nsim>1) 
      apply(xt[2:nsim,],1,function(x,t) lines(t,x,col="blue"),t=t)
    if (input$showave_brownian)
      lines(t, xtave, type="l", col="red", lwd = 2)
  })
  
  ################################################################################
  # Geometric Brownian Motion
  #################################################################################
  output$gbm_ts <- renderPlot({
    if (input$seeds == TRUE) {
      set.seed(input$setseed)
    }
    mu <- input$drift/100
    sigma <- input$stdev/100
    S0 <- input$initPrice
    nsim <- input$simul
    t <- input$T_GBM
    
    
    gbm <- data.frame(nrow = t, ncol = nsim)
    for (simu in 1:nsim) {
      for (day in 2:t) {
        epsilon <- rnorm(t)
        dt = 1 / 3650
        gbm[1, simu] <- S0
        gbm[day, simu] <- exp((mu - sigma * sigma / 2) * dt + sigma * epsilon[day] * sqrt(dt))
        # gbm[day, simu] <- exp((mu) * dt + sigma * epsilon[day] * sqrt(dt))
      }
    }
    gbm <- apply(gbm, 2, cumprod)
    # 
    # time <- c(1:t)
    # cbind(gbm, time)
    # gbm_data <- data.frame(gbm, )
    # 
    # p <- plot_ly(x=~1:t, y = ~gbm[,1], mode = 'lines', color = list(col=rainbow(10)), line = list(width = 3))%>%
    #   add_lines(y = ~gbm[,2], mode = 'lines', color = list(col=rainbow(10)), line = list(width = 3)) %>%
    #   layout(xaxis = list(title = 'Time'), yaxis = list(title = 'Stock Price'))
    # 
    ts.plot(gbm, gpars = list(col=rainbow(10)))
    
    xtave <- apply(gbm,1,mean)
    if (input$showave_GBM)
      lines(c(1:t), xtave, type="l", col="black", lwd = 3)
    # 
    # plot_ly(x = ~1:t, y = ~gbm[,1], mode = 'lines', color = list(col=rainbow(10)), line = list(width = 3)) %>%
    #   add_lines(y=~gbm[ ,2:ncol(gbm)], mode = 'lines') %>%
    #   layout(xaxis = list(title = "Time"), yaxis = list(title = "Stock Price"))
    # 
    
    # p <- plot_ly(source = "source") %>%
    #   add_lines(x=~1:t, y = ~gbm, type = 'scatter',mode = 'lines', color = list(col=rainbow(10)), line = list(width = 3))%>%
    #   layout(xaxis = list(title = 'Time'), yaxis = list(title = 'Stock Price'))
    
    # # for (j in 1:ncol(gbm)){
    # p <- p %>%
    #     add_lines(y = ~gbm[,2:ncol(gbm)], type = 'scatter',mode = 'lines', color = list(col=rainbow(10)), line = list(width = 3))
    # # }
    # 
    #     p
    
  })
   
  ###############################################################################
  # Markov Process
  ###############################################################################
  # output$matrix <- renderUI({
  #   matrixInput("trans_prob", "Input the transition probablity matrix",
  #               as.data.frame(diag(1, nrow = input$dimension, ncol = input$dimension)))
  # })
  # 
  # var <- reactive({
  #   stateNames <-  c()
  #   for (i in 1:input$dimension){
  #     stateNames[i] = paste("state",i, sep = " ")
  #   }
  #   stateNames
  # }) 
  # 
  # output$select <- renderUI({
  #   selectInput("start", h6("Select starting state: "), 
  #               choices = var())
  # })
  # 
  # output$pas_prob <- renderPrint({
  #   trans_matrix <- matrix(input$trans_prob, nrow = input$dimension, 
  #                          ncol = input$dimension)
  #   matrix_row_sum <- rowSums(trans_matrix)
  #   norm_matrix <- round(trans_matrix / matrix_row_sum, 2)
  #   norm_matrix[,input$dimension] <- 1-rowSums(norm_matrix[,-input$dimension])
  #   stateNames = c()
  #   for (i in 1:input$dimension){
  #     stateNames[i] = paste("state",i, sep = " ")
  #   }
  #   dcmc <- new("markovchain", transitionMatrix = norm_matrix, 
  #               states = stateNames)
  #   round(firstPassage(dcmc, input$start, input$step),3)
  #   
  # })
  # 
  # output$trans_plot <- renderPlot({
  #   trans_matrix <- matrix(input$trans_prob, nrow = input$dimension, 
  #                          ncol = input$dimension)
  #   matrix_row_sum <- rowSums(trans_matrix)
  #   norm_matrix <- round(trans_matrix / matrix_row_sum, 2)
  #   norm_matrix[,input$dimension] <- 1-rowSums(norm_matrix[,-input$dimension])
  #   stateNames = c()
  #   for (i in 1:input$dimension){
  #     stateNames[i] = paste("state",i, sep = " ")
  #   }
  #   dcmc <- new("markovchain", transitionMatrix = norm_matrix, 
  #               states = stateNames)
  #   plot(dcmc, main = "State Space Diagram")
  #   mtext("same coulored dots indicate the states are in the same class")
  # })
  # 
  # output$summary <- renderPrint({
  #   trans_matrix <- matrix(input$trans_prob, nrow = input$dimension, 
  #                          ncol = input$dimension)
  #   matrix_row_sum <- rowSums(trans_matrix)
  #   norm_matrix <- round(trans_matrix / matrix_row_sum, 2)
  #   norm_matrix[,input$dimension] <- 1-rowSums(norm_matrix[,-input$dimension])
  #   stateNames = c()
  #   for (i in 1:input$dimension){
  #     stateNames[i] = paste("state",i, sep = " ")
  #   }
  #   dcmc <- new("markovchain", transitionMatrix = norm_matrix, 
  #               states = stateNames)
  #   summary_dcmc <- summary(dcmc)
  #   
  # })
  
  ####################################################################
  # RandomForest --- Titanic Data
  ###############################################################################
  dsnames_rf <- c()
  data_set_rf <- reactive({
    req(input$file_rf)
    inFile_rf <- input$file_rf
    if (is.null(inFile_rf)) {
      return(NULL) }
    
    if (input$fileType_Input_rf == "1") {
      data_set_rf<-read.csv(inFile_rf$datapath,
                            header = TRUE,
                            stringsAsFactors = TRUE)
    } else {
      data_set_rf<-read_excel(inFile_rf$datapath)
    }
  })
  
  
  observe({
    req(input$file_rf)
    dsnames_rf <- names(data_set_rf())
    # cb_options <- list()
    # cb_options[dsnames_rf] <- dsnames_rf
    updateCheckboxGroupInput(session, "inCheckboxGroup_rf",
                             label = "Select explanatory variables",
                             choices = dsnames_rf,  
                             selected = "")
    updateSelectInput(session, "selectInput_rf",
                      label = "Select a response variable",
                      choices = dsnames_rf,  
                      selected = "")
  })
  
  
  output$contents_rf<- renderDataTable({
    #Add req function to check whether the input is truthy, which refrains error message when not selecting variables
    req(input$inCheckboxGroup_rf)
    req(input$selectInput_rf)
    selected_data_rf <- data_set_rf()[,c(input$selectInput_rf,input$inCheckboxGroup_rf)]
    datatable(data = selected_data_rf, 
              options = list(pageLength = 10), 
              rownames = FALSE)
  })
  
  
  output$choose_columns_rf <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
  })
  
  
  
  output$rf<-renderPlot({
    req(input$inCheckboxGroup_rf)
    req(input$selectInput_rf)
    set.seed(17)
    forest=randomForest(x =data_set_rf()[,c(input$inCheckboxGroup_rf)], y =as.factor(data_set_rf()[,c(input$selectInput_rf)]), ntree =input$ntree,nodesize =input$node,mtry=input$mtry) 
    
    plot(forest)
    output$rfoutcome <- renderPrint({
      print(forest, digits = 3, signif.stars = FALSE)
    })
  })
  output$error1<-renderText({
    
    print('The green curve represents the error in predicting the passenger to be alive.')
  })
  output$error2<-renderText({
    
    print('The red curve represents the error in predicting the passenger to be dead.')
  })
  output$error3<-renderText({
    
    print('The black curve represents the out of bag error.')
  })
  output$error4<-renderText({
    
    print('More details of the model are listed below:')
  })
  
  
  output$treediagram<-renderPlot({
    
    req(input$inCheckboxGroup_rf)
    req(input$selectInput_rf)
    train_index=sample(nrow(data_set_rf()),0.7*nrow(data_set_rf()),FALSE)
    f=as.formula(paste(input$selectInput_rf,"~",paste(input$inCheckboxGroup_rf,collapse="+")))
    dt<-ctree(f,data=data_set_rf()[train_index,],controls=ctree_control(maxdepth=input$max_depth,mincriterion=input$criterion,minbucket =input$node2,mtry=0))
    plot(dt,type='simple')
    prediction=unlist(predict(dt,data_set_rf()[-train_index,input$inCheckboxGroup_rf]))>0.5
    outcome=table(prediction,data_set_rf()[-train_index,input$selectInput_rf])
    output$tab<-renderPrint({print(outcome)})
    a=outcome[1,1]
    b=outcome[1,2]
    c=outcome[2,1]
    d=outcome[2,2]
    
    output$metric<-renderPrint({cat(paste(' Definition of some metrics:',"\n","\n" ,'Sensitivity:true positive/(true positive+false positive)',"\n",'Specificity:true negative/(true negative+false negative)',"\n",'Precision:true positive/(true positive+false negative)'))})
    output$accuracy<-renderText({print((a+d)/(a+b+c+d))})
    output$sen<-renderText({print((a)/(a+c))})
    output$spe<-renderText({print((d)/(b+d))})
    output$pre<-renderText({print((a)/(a+b))})
    
  })
  
  
  output$ens_text1<-renderText({print('- Consider a situation where the jury is making a decision via majority voting')})
  output$ens_text2<-renderText({print('- N is the total number of jurors')})
  output$ens_text3<-renderText({print('- p is the possibility that each single juror make the correct choice(for simplification, assume constant over all jurors)')})
  output$ens_text4<-renderText({print('- μ is the chance that the whole jury makes a correct decision')})
  output$ens_text5<-renderText({print('- In this setting, if p>0.5(juror tends to make correct decision), adding more juror into the jury should let μ approach 1.')})
  output$ens_text6<-renderText({print('- It’s an analogy to combining sensible algorithms to improve predictive performance.')})
  
  ################################################################################

 
   
}) # End of the ShinyServer
shinyApp(ui, Server)