

###########################
###########################
####    CONFIG FILE    ####   
###########################
###########################


# Basic settings
config.appName <- "prototype1_0"
config.onServer <- FALSE

# Support / Dev settings
config.createLogs <- TRUE
config.emailSupport <- "" # your email here : "name.surname@insel.ch"
config.exitWebpage <- "" # where the application will direct to after the Logout button is pressed

# Setting the file names
config.data <- "mockup_cohort_data.csv"
config.analytes <- "analytes.csv"
config.ICD10 <- "ICD10_codes.csv"

# Set home directory
if(config.onServer){
  config.HOME <- "" # path/to/the/main_folder of the user 
  config.Rlib <- "" # path/to/the/Rlib_folder of the user
  config.log <- paste0(config.HOME,"/",config.appName,"/log")
}else{
  config.Rlib <- .libPaths()[1]
}


###################################
#  Setting credentials for login  #
###################################

if(config.onServer){
  library(scrypt,lib=config.Rlib)
  library(shinymanager,lib=config.Rlib)
}

config.credentials <- data.frame(
  user = c("shiny"), #, "shinymanager"
  password = c(scrypt::hashPassword("shiny")), #, scrypt::hashPassword("shinymanager")
  is_hashed_password = TRUE,
  comment = c("regular shiny user"), #, "shiny admin"
  permission  = c("basic"), #, "advanced"
  stringsAsFactors = FALSE
)


# Shiny manager settings
shinymanager::set_labels(
  language = "en",
  "Please authenticate" = "SwissBioRef Login",
  "Username" = "Username",
  "Password" = "Password"
)

###################################################
# Inline CSS --> need to be imported to server.R ##
###################################################

config.serverCSS <- "
#body {
  background-color: rgb(238, 240, 242);
}
#plot-container {
  position: relative;
}
#loading-spinner {
  position: absolute;
  left: 300px;
  top: 50%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}

#plot.recalculating {
  z-index: -2;
}
"
