server<-shinyServer(function(input,output,session){
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

})
shinyApp(ui,server)