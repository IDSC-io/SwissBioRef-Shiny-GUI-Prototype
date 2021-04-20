###################################################################
###################################################################
#######                                                     #######
#####                                                         #####
####                                                           ####
####    SwissBioRef - A tool for reference value estimation    ####
####                                                           ####
####                  Server source code                       ####
#####                                                         #####
#######                                                     #######
###################################################################
###################################################################


##################
# INITIALIZATION #
##################


source("config.R", local = TRUE) # Loads the configuration file, where you can specify global parameters

# Error logs, Only needed if run on the HPC --> accessible for Devs only
if(config.onServer && config.createLogs){
  con <- file(paste0(config.log,"/",config.appName,".log"))
  sink(con, append=TRUE, type= c("output","message"))
}

# Loading packages
library(shiny, lib=config.Rlib)
library(shinyjs, lib=config.Rlib)
library(MASS)
library(markdown)
library(ggplot2, lib=config.Rlib)
library(dplyr, lib=config.Rlib)
library(shinymanager, lib=config.Rlib)
library(scrypt, lib=config.Rlib)

# Sourcing JS and R functions from external R scripts
source("utils.R",local = TRUE) # Loads external functions (for stat and plots)
source("helpers.R", local = TRUE) # Load all the code needed to show feedback on a button click

# Dataimport & variable setting
today <- Sys.time()

## ALREADY CHECKS FOR CONSENT AND REMOVES DATAPOINTS THAT ARE ZERO!!
data_sql <- read.csv(paste0("data/",config.data),header = TRUE, sep = ";", stringsAsFactors = FALSE) %>% 
  filter(gen_consent=="y") %>% 
  filter(lab_value >= 0)
analyteDF <- read.csv(paste0("data/",config.analytes),header = TRUE, sep = ";", stringsAsFactors = FALSE) #%>% as.list()
preCondDF <- read.csv(paste0("data/",config.ICD10), header = TRUE, sep = ";", stringsAsFactors = FALSE)


###################
##  MAIN SCRIPT  ##
###################


shinyServer(function(input, output, session) {
  
  # Include your own css for formatting
  inlineCSS(config.serverCSS)
  
  # authentication module (For LOGIN)
  auth <- callModule(
    module = auth_server,
    id = "auth",
    check_credentials = check_credentials(config.credentials)
  )
  
  
  ## JAVASCRIPT CODE   --> using functions from the shinyjs package
  shinyjs::onclick("popupPermission",
                   shinyjs::alert(paste0("User \"",auth$user, "\" with permissions \"", auth$user_info$permission,"\"")))
  
  # Logout
  observeEvent(input$logout, {
    shinyjs::alert(paste("Log out successfull"))
    delay(100, js$logoutWindow())
    delay(400, stopApp())
  })
  
  # Toggling & resetting query
  shinyjs::onclick("toggleAdvanced",shinyjs::toggle(id = "advanced", anim = TRUE))
  shinyjs::onclick("checkPrecond", shinyjs::toggle(id = "preConditions", anim = TRUE))
  shinyjs::onclick("resetForm", {shinyjs::reset(id="sidePanel"); 
    shinyjs::hideElement(id="preConditions",anim = TRUE, animType = "slide", time = 0.5);
    shinyjs::hideElement(id="advanced",anim = TRUE, animType = "slide", time = 0.5);
    updateVal(-1)})
  
  
  ################################################
  ################################################
  ####   START Shiny "reactive envirenment"   ####
  ################################################
  ################################################
  
  
  # server-side selectizeInput --> data is passed to the ui, 
  updateSelectizeInput(session, 'analyte', choices = analyteDF$lab_test, server = TRUE)
  updateSelectizeInput(session, 'selectpreConditions', choices = preCondDF$code_description, server = TRUE)
  
  
  ###############################
  ### FILTERING THE LABVALUES ###
  ###############################
  
  # Select the analyte
  selectedAnalyte <- reactive({
    
    req(auth$result)  # <---- depends on the result of the authentication step
    
    validate(need(input$analyte != "", "Error: No analyte is selected"))
    
    data_sql %>% filter(lab_test == as.character(input$analyte))
    
  })
  
  # The parameters from the analytes.csv
  analyteSettings <- reactive({
    
    analyteDF[which(analyteDF[,1]==as.character(input$analyte)),]
    
  })
  
  
  # if non-parametric the bootstrapping option is automatically selected
  
  observe({
    updateCheckboxInput(session, 'bootstrapCI', 
                        value = !(analyteSettings()$parametric | 
                                   analyteSettings()$lab_test == "C-reaktives Protein | mg/L"))
  })
  
  
  
  # Select data based on the chosen sex, age range and version control date
  selectedLabvalues <- reactive({
    
    validate(need(!is.na(input$sex), "Error: Please select at least one sex"))
    
    # Both male & female are chosen
    if(length(input$sex) > 1){
      
      validate(need(nrow(selectedAnalyte() %>% filter(between(age, input$range[1], input$range[2]))) != 0, 
                    "Error: Your age range does return any entries"))
      
      selectedAnalyte() %>%
        #filter(version_control <= as.Date(input$date)) %>%
        filter(between(age, input$range[1], input$range[2]))
      
    }else{
      
      validate(need(nrow(selectedAnalyte() %>% filter(between(age, input$range[1], input$range[2])) %>% filter(sex == input$sex)) != 0, 
                    "Error: Your age range does return any entries"))
      
      selectedAnalyte() %>%
        #filter(as.Date(version_control) <= as.Date(input$date)) %>%
        filter(between(age, input$range[1], input$range[2])) %>%
        filter(sex == input$sex)
    }
    
  })
  
  
  # filtering selectedLabvalues() if any preconditions have been specified
  preCondLabvalues <- reactive({
    cond <- input$selectpreConditions
    condCode <- NULL
    
    # Get the correct ICD10 code for filtering the results
    if(input$checkPrecond && !is.null(cond)){
      
      # creates a vector with all selected pre-Conditions
      for(i in 1:length(cond)){ condCode <- c(condCode,preCondDF$ICD10_code[which(preCondDF$code_description == cond[i])]) }
      
      validate(need(length(condCode) < 4, message="This protoype application only supports the selection of max. three existing preconditions."))
      
      if(length(condCode)==1){
        
        # CHECK IF IT WILL RETURN LENGTH(LABVALUES) != 0 FOR A SPECIFIC CONDITION TO CONTINUE
        validate(need(nrow(selectedLabvalues() %>% filter(diag01==condCode[1] | diag02==condCode[1] | diag03==condCode[1] | diag04==condCode[1] | diag05==condCode[1]) %>%
                             dplyr::select(lab_value)) != 0, message='Error: Given the one selected precondition, there is no data availible'))
        
        selectedLabvalues() %>% filter(diag01==condCode[1] | diag02==condCode[1] | diag03==condCode[1] | diag04==condCode[1] | diag05==condCode[1]) %>% 
          dplyr::select(lab_value) 
        
      }else if(length(condCode)==2){
        
        # CHECK IF IT WILL RETURN LENGTH(LABVALUES) != 0 FOR A SPECIFIC CONDITION TO CONTINUE
        validate(need(nrow(selectedLabvalues() %>% filter(diag01==condCode[1] | diag02==condCode[1] | diag03==condCode[1] | diag04==condCode[1] | diag05==condCode[1]) %>%
                             filter(diag01==condCode[2] | diag02==condCode[2] | diag03==condCode[2] | diag04==condCode[2] | diag05==condCode[2]) %>%
                             dplyr::select(lab_value)) != 0, message='Error: Given the two selected preconditions, there is no data availible'))
        
        selectedLabvalues() %>% filter(diag01==condCode[1] | diag02==condCode[1] | diag03==condCode[1] | diag04==condCode[1] | diag05==condCode[1]) %>%
          filter(diag01==condCode[2] | diag02==condCode[2] | diag03==condCode[2] | diag04==condCode[2] | diag05==condCode[2]) %>% 
          dplyr::select(lab_value)
        
      }else if(length(condCode)==3){
        
        # CHECK IF IT WILL RETURN LENGTH(LABVALUES) != 0 FOR A SPECIFIC CONDITION TO CONTINUE
        validate(need(nrow(selectedLabvalues() %>% filter(diag01==condCode[1] | diag02==condCode[1] | diag03==condCode[1] | diag04==condCode[1] | diag05==condCode[1]) %>%
                             filter(diag01==condCode[2] | diag02==condCode[2] | diag03==condCode[2] | diag04==condCode[2] | diag05==condCode[2]) %>%
                             filter(diag01==condCode[3] | diag02==condCode[3] | diag03==condCode[3] | diag04==condCode[3] | diag05==condCode[3]) %>%
                             dplyr::select(lab_value)) != 0, message='Error: Given the three selected preconditions, there is no data availible'))
        
        selectedLabvalues() %>% filter(diag01==condCode[1] | diag02==condCode[1] | diag03==condCode[1] | diag04==condCode[1] | diag05==condCode[1]) %>%
          filter(diag01==condCode[2] | diag02==condCode[2] | diag03==condCode[2] | diag04==condCode[2] | diag05==condCode[2]) %>%
          filter(diag01==condCode[3] | diag02==condCode[3] | diag03==condCode[3] | diag04==condCode[3] | diag05==condCode[3]) %>% 
          dplyr::select(lab_value)
      }
      
    }else{
      selectedLabvalues() %>%
        dplyr::select(lab_value)
    }
  })
  
  
  # Setting up the variable updateVal that controls the updating of the graphing 
  # The graph should only be visible after pressing the running analysis button
  # This creates an intuitive user experience in the GUI
  updateVal <- reactiveVal(-1)
  
  
  # Data is prepared for analysis
  finalLabvalues <- reactive({
    
    #validate(need(nrow(preCondLabvalues()) >= 120, "Error: Query returned to few results. Readjust your parameters!"))
    validate(need(input$analyte != "", "Error: No analyte is selected"))
    
    
    if(analyteSettings()$parametric=="TRUE"){
      preCondLabvalues()
      
    }else{
      preCondLabvalues()
    }
    
  })
  
  
  # While the parameters are changed, the graph output shuld not be visible
  observeEvent(finalLabvalues(), {
    updateVal(-1)
  })
  
  
  #################################################################
  ##                        ANALYSIS PART                        ##
  #################################################################
  
  
  #################################################################
  #                                                               #
  #  Parametric RV estimation                                     #
  #  ########################                                     #
  #                                                               #
  #  filtered Lab values ->  a) box cox transformation            #
  #                          b) remove Outliers                   #
  #                          c) RV estimation                     #
  #                          d) confidence interval               #
  #                                                               #
  #                                                               #
  #                                                               #
  #  Non-parametric RV estimation  (directly from bootstrap)      #
  #  ############################                                 #
  #                                                               #
  #  filtered Lab values ->  c) RV estimation                     #
  #                          d) confidence interval               #
  #                                                               #
  #                                                               #
  #################################################################
  
  
  # a) The box cox transformation 
  ###############################
  
  boxcoxValues <- reactiveValues( )
  
  boxCox <- reactive({
    
    # Prepare values
    x <- (finalLabvalues() %>% mutate(lab_value=lab_value+1) %>% dplyr::select(lab_value))$lab_value
    
    # Minimization function
    boxcox.nlm <- function(lambda){
      if (lambda == 0){
        # Minimize sd of geomean(x) * log(x)
        sd(exp(mean(log(x)))*log(x))
      }else{
        # Minimize sd of  x^lambda - 1 / lambda * geomean(x)^(lambda -1)
        sd(((x)^lambda-1)/(lambda*exp(mean(log(x)))^(lambda-1)))
      }
    }
    
    # Running the non-linear minimization
    stats::nlm(boxcox.nlm, p=-2)
    
  })
  
  # Observe values from the boxCox() minimazation
  observe({
    boxcoxValues$lambda = boxCox()$estimate
    boxcoxValues$sigma = boxCox()$minimum
    boxcoxValues$code = boxCox()$code
  })
  
  
  # Transform finalLabvalues() using the box cox transformation
  boxcoxLabvalues <- reactive({
    
    # Transformation using the determined lambda
    if(boxcoxValues$lambda != 0){
      finalLabvalues() %>% mutate(((. + 1)^boxcoxValues$lambda-1)/boxcoxValues$lambda) %>% dplyr::select(lab_value)
    }else if(boxcoxValues$lambda == 1){
      finalLabvalues() %>% dplyr::select(lab_value)
    }else{
      finalLabvalues() %>% mutate(log(. + 1)) %>% dplyr::select(lab_value)
    }
    
  })
  
  # b) Remove Outliers
  ####################
  
  # Range definded by Tukey's criterion
  outlierRange <- reactive({
    
    q.1 <- quantile(boxcoxLabvalues()$lab_value, probs = 0.25, na.rm = TRUE)
    q.3 <- quantile(boxcoxLabvalues()$lab_value, probs = 0.75, na.rm = TRUE)
    
    IQR <- stats::IQR(boxcoxLabvalues()$lab_value, na.rm = TRUE)
    
    c(q.1 - 1.5*IQR, q.3 + 1.5*IQR)
    
  })
  
  # Filter values based on Tukey
  removedOutliers <- reactive({
    
    boxcoxLabvalues() %>% filter(between(lab_value, outlierRange()[1], outlierRange()[2]))
    
  })
  
  # c ) Estimate the 95% reference interval
  #####################################
  
  referenceInt <- reactive({
    
    if(input$analyte == ""){return()}
    
    
    # Parametric approach
    if(analyteSettings()$parametric==TRUE){
      
      # Due to the max. 0.7% removal of valiable results from the outlier removal step
      # the 95% RI has to be adjusted to: 
      
      # 96.6697%
      
      # This means that the lower and upper values of the RI have to be adjusted:
      # 5% --> 4.550512%
      
      lower_limit <- 0.021652
      upper_limit <- 0.978348
      
      
      mean <- mean(removedOutliers()$lab_value,na.rm = TRUE)
      sd <- sd(removedOutliers()$lab_value,na.rm = TRUE) 
      
      lowerband <- qnorm(lower_limit, mean, sd)
      higherband <- qnorm(upper_limit, mean, sd)
      
      boxcox.backtransform(c(lowerband, higherband),boxcoxValues$lambda)
    
    #Non-parametric approach  
    }else if(analyteSettings()$parametric==FALSE){
      
      confidenceInt()[c(2,5)]
      
    }
    
  })
  
  # d) Calculate confidence interval
  ##################################
  
  # --> Bias corrected boostrapping (Author: Harald Witte)
  
  confidenceInt <- reactive({
    
    if (input$bootstrapCI == FALSE)
      return(c(0,0,0,0))
    
    
    if(analyteSettings()$parametric==TRUE){
      
      ####### Parametric boot-strapping 

      # The lower and upper values for the adjusted 95% reference interval:
      # (see referenceInt() for expl.)
      
      lower_limit <- 0.021652
      upper_limit <- 0.978348
      
      
      bootSamples <- replicate(n = 100,
                                expr = quantile(sample(removedOutliers()$lab_value,size=(250),replace = TRUE),
                                                probs = c(lower_limit,upper_limit)),
                                simplify = TRUE)
      
      ci_lower = quantile(bootSamples[1,], probs = c(0.05,0.95))
      
      ci_higher = quantile(bootSamples[2,], probs = c(0.05,0.95))
      
      bootstrapped_P <- c(ci_lower[1],0,ci_lower[2],ci_higher[1],0,ci_higher[2])

      #Backtransform from the boxcox space
      boxcox.backtransform(bootstrapped_P, boxcoxValues$lambda)
      

      
    }else if(analyteSettings()$parametric==FALSE){
      
      lower_limit <- 0.025
      upper_limit <- 0.975
      
      
      # Will return a vector like this
      # [1]   2.5%_lower_bound   [2]   2.5%_bootstrap  [3]   2.5%_upper_bound 
      # [4]  97.5%_lower_bound   [5]  97.5%_bootstrap  [6]  97.5%_upper_bound 
      
      
      boot.conf.int(data = finalLabvalues()$lab_value, stats_function = utils.stats.bootstrap,
                    
                    lower_limit = lower_limit,
                    upper_limit = upper_limit,
                    
                    num_rep = 120,
                    #core_usage = "multicore",
                    #num_cpus = 8,
                    alpha = 0.9,
                    random.seed = 42)
      
    }
    
  })
  
  
  ###############################
  # Parameters & values
  
  # Graphing parameters that require user input
  # For Tab (1)
  # Setup the reative value for the x_min & x_max for the ggplot output
  
  setGraphing <- reactiveVal( c(NULL,NULL) )
  
  observe({ setGraphing(c(round(min(finalLabvalues()),0),
                          round(max(finalLabvalues()),0))) })
  
  observeEvent(input$resetGraph, {
    setGraphing(c(round(min(finalLabvalues()),0),
                  round(max(finalLabvalues()),0)))
  })
  
  observeEvent(input$plot_brush, {
    setGraphing(c(round(input$plot_brush$xmin,0),round(input$plot_brush$xmax,0)))
  })
  
  # Skweness & Kurtosis (for Tab 2)
  
  skew_kurt <- reactive({
    symmetrie(data=(removedOutliers() %>% filter(is.finite(lab_value)))$lab_value,
              nrow=nrow(removedOutliers()))
  })
  
  
  # How much data is used for the analyis (for Tab 1)
  amountofData <- reactive({
    
    nrow(finalLabvalues())
    
  })
  
  
  #######################
  # Running the analysis
  # When the button is clicked, wrap the code in a call to `withBusyIndicatorServer()`
  observeEvent(input$update, {
    withBusyIndicatorServer("update", {
      if (nrow(finalLabvalues()) == 0) {
        stop("Your query does not return any values. Something went wrong!")
      }
    })
  })
  
  ###############################
  # Reset the input$update value
  
  observeEvent(input$update, {
    updateVal(input$update)
  })
  
  
  
  ##################
  # Output for GUI #
  ##################
  
  
  
  ### TAB 1 ####
  
  ###################
  # Query information
  
  output$amoutofdata <- renderText({ 
    if (updateVal() != input$update)
      return("<h4>Reference values</h4><br>Set parameters to your left and click \"Run analysis\"")
    
    c("<h4>Reference values</h4><br>Based on the given parameters, the query has returned <b>", amountofData(), 
      "</b>of<b>", nrow(selectedAnalyte()), "</b>entries.") 
  })
  
  #################
  # The main plot 
  
  output$plot <- renderPlot({
    if (updateVal() != input$update)
      return()
    
    validate(need(!is.na(input$analyte), "Please select a sex"))
    
    ciTRUE = input$bootstrapCI
    
    if (is.na(referenceInt()[1]) | is.na(referenceInt()[2])){
      ciTRUE = FALSE
    }
    
    xlim <- setGraphing()
    
    # Checks if xlimits have to be adjusted do to fit the upper edge of the calc. confidence interval
    if (input$bootstrapCI == TRUE & setGraphing()[1] > confidenceInt()[1] | input$bootstrapCI == TRUE & setGraphing()[2] < confidenceInt()[6]){
      xlim <- confidenceInt()[c(1,6)]
    }
    
    finalLabvalues()$lab_value %>%
      
      as_tibble() %>%
      
      shinyHistogram(., ana = input$analyte, xlimits = xlim, inputVal = input$numeric , 
                     refLower = referenceInt()[1], refUpper = referenceInt()[2],
                     ciTRUE = ciTRUE, ciLower=confidenceInt()[c(1,3)],ciUpper=confidenceInt()[c(4,6)]) 
  })
  
  
  #########
  # Calculated reference values and their confidence interval
  
  output$headerRV <- renderText({ 
    if (updateVal() != input$update)
      return()
    
    paste0("The reference interval (95%) is: ")
    
  })
  
  
  output$referencevalues <- renderText({ 
    if (updateVal() != input$update)
      return()
    
    #validate(need(setGraphing()[1] < referenceInt()[1] && referenceInt()[2] < setGraphing()[2], "Cannot zoom in any further"))
    
    if (is.na(referenceInt()[1]) | is.na(referenceInt()[2])) {
      return("The reference values could not be be estimated")
    }
    
    
    if (input$bootstrapCI) {
      paste0(round(referenceInt()[1],1)," (",paste(round(confidenceInt()[c(1,3)],2), collapse = "-"),") - ",
             round(referenceInt()[2],1)," (",paste(round(confidenceInt()[c(4,6)],2), collapse = "-"),")")
      
      # paste0(paste(round(referenceInt(),1), collapse = " - "), " | with confidence: (",
      #       paste(round(confidenceInt()[c(1,3)],2), collapse = " - "), ") and (",
      #       paste(round(confidenceInt()[c(4,6)],2), collapse = " - "), ")")
      
    } else{
      paste(round(referenceInt(),1), collapse = " - ")
    }
    
    
  })
  
  ###########################
  
  ### TAB 2 ####
  
  ## Text on top 
  
  output$boxcoxHeader <- renderText({
    if (updateVal() != input$update)
      return("Set parameters on your left to your liking and click \"Run analysis\"")
    
    if (analyteSettings()$parametric==TRUE){
      paste("<h4>Summary of the Box-Cox transformation</h4>")
    } else {
      paste("<h4>Non-parametric bootstrapping</h4><br> For this analyte, the reference values and their respective confidence interval are directly estimated
            from the empirical distribution by bias-corrected bootstrapping.")
    }
    
  })
  
  
  output$boxcoxText <- renderText({
    if (updateVal() != input$update | analyteSettings()$parametric==FALSE)
      return("")
    
    
    paste0("Transformation the empirical distribution with lambda: <b>", round(boxcoxValues$lambda,2), 
           "</b>.Values outside of the <b>Tukey range</b> are considered as outliers (yellow lines) and removed before continuing: ")
    
  })
  
  #################
  # Plots for Tab 2 
  
  output$boxcox1 <- renderPlot({
    if (updateVal() != input$update | analyteSettings()$parametric==FALSE)
      return()

    name <- strsplit(input$analyte, split = ' \\| ')
    
    q <- boxcoxLabvalues()$lab_value %>%
            as_tibble() %>%
            ggplot(aes(value)) +
      
            #geom_histogram(colour="black", fill="white")
            
            # Kernel density estimator
            geom_bar(aes(x =boxcoxLabvalues()$lab_value), stat = "density",show.legend = FALSE, fill = "#b2e0f9",alpha = 1) + #428bca
            
            # Setting the title and the xlab
            labs(title = paste0("Transformed distribution for: ", name[[1]][1]), " with the Tukey range") +
            labs(x = paste0("transformed Value [", name[[1]][2], "]"), y = "Density") +
          
            theme(legend.position = "none")

              
      
    
    # Adding vertical lines
    q + geom_vline(xintercept = mean(boxcoxLabvalues()$lab_value), color = "#428bca", linetype = 1) +
        geom_vline(xintercept = outlierRange(), color = "#ffa700", linetype = "F1")
    
    
    
  })
  
  output$skewkurt <- renderText({
    if (updateVal() != input$update | analyteSettings()$parametric==FALSE)
      return()
    
    paste0("The transformed distribution now presents a skewness of <b>", 
          round(skew_kurt()[1],2), "</b> and a kurtosis of <b>", round(skew_kurt()[2],2), "</b>.<br>
          An undelying gaussian distribution is now approximated (green density curve), 
          from which the 95% interval is calculated (red lines): ")
    
  })
  
  
  output$boxcox2 <- renderPlot({
    if (updateVal() != input$update | analyteSettings()$parametric==FALSE)
      return()
    
    name <- strsplit(input$analyte, split = ' \\| ')
    
    r <- removedOutliers()$lab_value %>%
      as_tibble() %>%
      ggplot(aes(value)) +
      
      # Kernel density estimator
      geom_bar(stat = "density",show.legend = FALSE, fill = "#b2e0f9") + #428bca
      
      # Setting the title and the xlab
      labs(title = paste0("Parametric estimation of reference values for ", name[[1]][1])) +
      labs(x = paste0("transformed Value [", name[[1]][2], "]"), y = "Density") +
      
      theme(legend.position = "none")
    
    # Transform estimates back to boxcox space
    if(boxcoxValues$lambda != 0){
      refInt <- ((referenceInt() + 1)^boxcoxValues$lambda-1)/boxcoxValues$lambda
    }else if(boxcoxValues$lambda == 1){
      refInt <- referenceInt()
    }else{
      refInt <- log(referenceInt() + 1)
    }
    
    
    # Adding vertical lines
    r + stat_function(fun = function(x) {dnorm(x, mean(removedOutliers()$lab_value), 
                                               sd(removedOutliers()$lab_value))}, color = "#4d8f8d") +
        geom_vline(xintercept = mean(removedOutliers()$lab_value), color = "#428bca", linetype = 1) +
        geom_vline(xintercept = refInt, color = "#db110d", linetype = "twodash")
    
    
  })
  
  

  
  
  #############################################################
  # Reporting function (with rmarkdown::knitr)
  
  output$report <- downloadHandler(
    
    # For PDF output, change this to "report.pdf" (This will likely not work in the browser, since there is no Latex engine)
    # filename = "report.html",
    filename = paste0(format.Date(today, "%Y-%m-%d__%H-%M"),"-",config.appName,"-report.html"),
    
    content = function(file) {
      # Copy the report file to a temporary directory before processing
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params1 <- list(analyte = input$analyte, 
                      inputVal = input$numeric,
                      refInt = referenceInt(), 
                      data = finalLabvalues(),
                      xlim = setGraphing(),
                      ciTRUE = input$bootstrapCI,
                      ci = confidenceInt()[c(1,3,4,6)])
      
      # Eval it in a child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file, params = params1)#, envir = new.env(parent = globalenv()))
    }
    
  )
  
})
