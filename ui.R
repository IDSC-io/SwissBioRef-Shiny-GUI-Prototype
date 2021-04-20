###################################################################
###################################################################
#######                                                     #######
#####                                                         #####
####                                                           ####
####    SwissBioRef - A tool for reference value estimation    ####
####                                                           ####
####                      UI source code                       ####
#####                                                         #####
#######                                                     #######
###################################################################
###################################################################


##################
# INITIALIZATION #
##################

# Sourcing external ressources
source("config.R", local=TRUE)
source("helpers.R", local = TRUE) # Load all the code needed to show feedback on a button click

# loading packages 
library(shiny)
library(shinyjs,lib = config.Rlib)
library(dplyr,lib = config.Rlib)
library(shinymanager, lib = config.Rlib)


# Setting variables
today <- Sys.Date()

# Separate JS code
jscode <- paste0("shinyjs.logoutWindow = function() { window.location.href = '",config.exitWebpage,"'; }")


###################
##  MAIN SCRIPT  ##
###################

shinyUI(fluidPage(
  
  # Include your own css for formatting
  inlineCSS(config.serverCSS),  
  
  # LOGIN: Authentication module
  auth_ui(
    id = "auth",
    
    # Customization of the authentification UI
    
    # add stuff on top of the user and pw field
    tags_top = 
      tags$div(tags$img(src="logoSPHN.jpeg", width = 83)),
    
    # add stuff on bottom of the user and pw field
    tags_bottom = tags$div(
      tags$p("Contact the ",
             tags$a(href = paste0("mailto:",config.emailSupport,"?Subject=SwissBioRef Tool ",config.appName),
                    target="_top", "developer")
      )
    ),
    
    # change auth ui background
    background  = "linear-gradient(rgb(106, 134, 164),rgb(212, 220, 223))"#, 
  ),
  
  # LOGOUT
  # Button for logout is added below in the side bar panel
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = jscode, functions = c("logoutWindow")),
  
  
  ##################################################################
  ### Building of the UI (-->compiled into HTML by shiny server) ###
  ##################################################################
  
  # Title and Top pannel
  titlePanel(tags$head(HTML("<title>SwissBioRef Tool - Calculate RV</title>"), style="visibility: hidden;")),
  div(img(src="logoSPHN.jpeg", height=73),style="float: right; padding: 5px; overflow: auto;"), #width: 200px; 
  h2(paste0("SwissBioRef Tool: Estimate personalized reference values")),
  h4(paste0("This is protoype GUI is strictly for non-diagnostic and non-therapeutic purposes.")),#br(),

  # Layout with a left sidebar panel and a main panel to the right
  sidebarLayout(
    
    #################
    # Sidebar panel #  --> setting parameters
    
    sidebarPanel(
      
      # To reset the query when the "reset form" button is pressed, this specified div named sidePanel is reset using shinyjs::reset(). 
      div(id="sidePanel",
          
          ### Input for the value
          numericInput("numeric", label = "Observed value", value = NULL, min = 0 ), #, width = "100px"
          
          ### server side selectize --> choices are feched from the server side (for setting the analyte)
          selectizeInput("analyte", label = "Select analyte",
                         choices = NULL),
          
          ### Checkbox for Sex
          checkboxGroupInput("sex", label = "Select sex",
                             choices = list("Female" = "F", "Male" = "M"), 
                             selected = NULL),
          
          ### Slider-Range for Age
          sliderInput("range", "Select age range",
                      min = 0, max = 100,
                      value = c(18,60)),
          
          
          div(id="AdvancedSettings",
              h5(strong("Additional settings")),
              
              ### Setting pre existing conditions
              checkboxInput("checkPrecond", label = "Predetermined condition(s)", value = FALSE),
              
          
              # This div only appears if the checkbox "checkPrecond" is pressed. This behaviour is directed by the shinyjs::toggle function.
              # Also affected by the shinyjs::reset() call of the div "sidePanel"
              shinyjs::hidden(
                div(id = "preConditions",
                    
                    # server side selectize --> choices are feched from the server side
                    selectizeInput("selectpreConditions",
                                   label = "Conditions (max. 3)",
                                   choices = NULL,
                                   multiple = TRUE
                    ))
              ),
              
              ### Setting for bootstrapping confidence interval for the RV
              checkboxInput("bootstrapCI", label = "Bootstrapping confidence interval", value = FALSE),
          ),
          
          ### Toggle advanced settings
          
          # The same as above with the "preCondition", the div "advanced" only appears if href "toggleAdvanced" is pressed. 
          # Also affected by the shinyjs::reset() call of the div "sidePanel"
          a(id = "toggleAdvanced", "Show/hide advanced settings", href = "#"),br(),#br(),
          
          shinyjs::hidden(
            div(id = "advanced",#br(),
                
                ### Range for date for the implementation of the version control
                h5(strong("Version control")),
                h5("Select the date of your query"),
                dateInput('date',label = NULL, value = Sys.Date(), format = "dd.mm.yyyy", min = as.Date("2020-01-01"), max = today, width = "100px"),
                
                ### Show permission for the user
                div(strong(a(id = "popupPermission", "Show user permissions", href = "#"))),
                div("A regular user will only have accessiblity to the adv. settings above, while
                a shinymanager could have more permissions (for e.g. user management). This is however not deployed in this prototype.")
            
            )),br(),
          
          
          ###  Running the analysis
          
          # This works with the updateVal value on the server side. Only when "Run analysis" is pressed are the calculations executed.
          withBusyIndicatorUI(actionButton("update", "Run analysis", style="align: center;", class = "btn-primary")),br(),
          
          
          ###   Reseting the query
          
          # shinyjs::reset of the div "sidePannel"
          actionButton("resetForm", "Reset query"),
          
          
          ###  Generating the report
          
          # This starts the Pandoc execution of the markdown process in the parent env
          h5(strong("Generate report")),
          h5("A HTML report of this query can be produced by clicking the button below."),
          downloadButton("report", "Generate report"),
          br(),
          

          ###   Ending the application
          
          # This is the button, that triggers ending the R session for this user. It shows the user a pop-up it the logout was done successfully.
          h5(strong("End session by logging out")),
          actionButton("logout", "Logout", style="align:center;")
      ),
      
      width = 4),
    
    
    # Main Panel 
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  #########
                  # TAB 1 #
                  #########
                  
                  tabPanel("RV calculation", br(), 
                           
                           div(id = "blank-container", 
                               htmlOutput(outputId = "amoutofdata"),br(),
                               style="margin: 15px; width: auto;"
                               
                           ),
                           
                           # Div for the graph ouput of the RV estimation.
                           div(id = "plot-container",
                               
                               # This div container holds both the graph and the the spinning wheel animation.
                               
                               # Accomplished by z-stacking of the different elements 
                               # --> controlled by the variable "config.serverCSS", 
                               
                               # config.serverCSS can be found in the config.R file
                               
                               plotOutput(outputId = "plot", 
                                          brush = brushOpts(id = "plot_brush", fill = "#9cf", stroke = "#036", opacity = 0.25,
                                                            delay = 1000, delayType = "debounce", clip = TRUE,
                                                            direction="x", resetOnNew = TRUE),
                                          height = "350px", width = "550px"),
                               
                               tags$img(src = "spinner.gif",id = "loading-spinner"),
                               
                               style="margin-right: 40px"
                               
                               #actionButton("resetGraph", label = "Reset graph"),br(),#br(),
                           ),

                           # This div holds all of the results of the analysis
                           div(id="data container",
                               
                               textOutput(outputId = "headerRV"),br(),
                               
                               div(id="RVandCI",
                                   
                                   verbatimTextOutput(outputId = "referencevalues"),
                                   style="margin-left: 25px; width: fit-content;"
                               ),
                                   
                                   
                               style="margin: 15px; width: fit-content;"
                           )
                           
                           
                  ),
                  
                  #########
                  # TAB 2 #
                  #########
                  
                  tabPanel("Statistics of your query", br(),
                           
                           div(id="data container2",
                               
                               htmlOutput(outputId = "boxcoxHeader"),br(),
                               htmlOutput(outputId = "boxcoxText"),
                               
                               style="margin: 15px; width: auto;"
                           ),
                           
                           div(id="plot container2",
                               
                               plotOutput(outputId = "boxcox1", height = "250px", width = "550px"),br(),
                               htmlOutput(outputId = "skewkurt"),br(),
                               plotOutput(outputId = "boxcox2", height = "250px", width = "550px"),
                               
                               tags$img(src = "spinner.gif",id = "loading-spinner"),
                               
                               style="margin-right: 40px;",
                               
                               
                               
                           )
                  )
      )
    )
  )
))
