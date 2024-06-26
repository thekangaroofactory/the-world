
# -------------------------------------
# UI items section
# -------------------------------------

# -- Main map
map_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # map
  leafletOutput(ns("map"), height = 800)
  
}


# -------------------------------------
# Input items section
# -------------------------------------

# -- Search input form
search_Input <- function(id) {
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    
    # search input
    searchInput(ns("search"), label = "Search", value = ""))
  
}


# -- filter
country_INPUT <- function(id){
  
  # -- namespace
  ns <- NS(id)
  
  tagList(
    div(style="display: inline-block", actionButton(inputId = ns("filter_reset"), label = "Reset filter")),
    div(style="display: inline-block", selectizeInput(inputId = ns("filter_country"), 
                                                      label = "Country", 
                                                      choices = NULL,
                                                      options = list(placeholder = 'Please select an option below',
                                                                     onInitialize = I('function() { this.setValue(""); }'),
                                                                     create = TRUE))),
    div(style="display: inline-block", checkboxInput(inputId = ns("freeze_map"), label = icon("anchor"))))

}

