#
# XRD Peak Conversion calculator
#
# Author: r.y.penchev@gmail.com
# 
# Radiation: Cu (Copper) Ka1 and Kb1, W (Tungsten) Lalpha1 and Lalpha2
# - Multi peak input/output
# - New interface to streamline the analysis
# 

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)

# Definitions -------------------------------------------------------------
Cu_Ka1 <- 1.5406
Cu_Kb1 <- 1.3922
W_La1 <- 1.4763
W_La2 <- 1.4875 

# Function ----------------------------------------------------------------
xrd_conversion <- function(wave_length_1, wave_length_2, peak_vector) {
  d_spacing <- wave_length_1/(2*sin((peak_vector/2)*pi/180))
  peak <- 2*(180/pi)*(asin(wave_length_2/(2*d_spacing)))
  xrd_dfr <- data.frame(d_spacing, peak)
  return(xrd_dfr)
}


# HEADER
header <- dashboardHeader(title = "XRD Peak Conversion", titleWidth = 280, disable = FALSE)
# SIDEBAR
sidebar <- dashboardSidebar(disable = TRUE)
# BODY
body <- dashboardBody(
  fluidRow(
    box(width = 2, title = "Input", status = "primary",solidHeader = TRUE, collapsible = FALSE,
        awesomeRadio("rb", label = h3("Type of peak(s) to enter:"),
                     choices = list("Cu Ka1" = 1, "Cu Kb1, W La1, W La2" = 2), 
                     selected = 2, status = "danger"),  
        textInput("peak", "Enter peak(s) position*:", value = NULL),
        helpText("* Comma separated!"),
        actionButton(inputId = "calc", label = "Calculate")
        
    ),
    
    box(width = 10, title = "Output", status = "primary",solidHeader = TRUE, collapsible = FALSE,
        footer=HTML("Author: r.y.penchev@gmail.com."),
        DT::dataTableOutput("table"),
        br(),
        textOutput("text1")
    )
    
  )
) # end of BODY

# UI
ui <- dashboardPage(header, sidebar, body, skin = "red")

# SERVER
server <- function(input, output) {
  
  observeEvent(input$calc, {
    req(input$peak)
    peaks_str <- as.character(input$peak)
    peaks_list <- as.numeric(unlist(strsplit(peaks_str, split = ",")))
    n_peaks <- length(peaks_list)
    
    if (input$rb == 1) {
      xrd_dfr <- round(xrd_conversion(rep(Cu_Ka1, n_peaks*3), rep(c(Cu_Kb1, W_La1, W_La2), each = n_peaks), peaks_list), 2)
      
      df <- data.frame(Radiation_1 = rep("Cu Ka1"),
                                   Peak_1 = rep(peaks_list),
                                   Corresponds_To = rep("-->"),
                                   Peak_2 = xrd_dfr[, 2],
                                   d_Spacing = xrd_dfr[, 1],
                                   Radiation_2 = rep(c("Cu Kb1", "W La1", "W La2"), each = n_peaks)
      )
      
    } else {
      xrd_dfr <- round(xrd_conversion(rep(c(Cu_Kb1, W_La1, W_La2), each = n_peaks), rep(Cu_Ka1, n_peaks*3), peaks_list), 2)
      
      df <- data.frame(Radiation_1 = rep(c("Cu Kb1", "W La1", "W La2"), each = n_peaks),
                                   Peak_1 = rep(peaks_list),
                                   Corresponds_To = rep("-->"),
                                   Peak_2 = xrd_dfr[, 2],
                                   d_Spacing = xrd_dfr[, 1],
                                   Radiation_2 = rep("Cu Ka1")
      )   
    } # end of if
    
    output$table <- DT::renderDataTable({
      datatable(df,
                filter = 'none', 
                class="cell-border stripe", 
                rownames = TRUE,
                colnames = c("Radiation 1", "Peak 1", "Corresponds To", "Peak 2", "d-Spacing", "Radiation 2"),
                extensions = c('Buttons'),
                options = list(dom = "Bfrt", scrollX = TRUE,
                               columnDefs = list(list(className = 'dt-center', targets = 0:6)),
                               buttons = list("copy", list(extend = "collection",buttons = c("excel", "pdf"), text = "Save")), # end of buttons customization
                               pageLength=n_peaks*3, lengthChange = TRUE, autoWidth = FALSE, 
                               searchHighlight = TRUE) # end of options
      ) # end of DT datatable
    })
    
    
  }) # end observeEvent
  
  output$text1 <- renderText("Wavelengths used: Cu Ka1 = 1.5406; Cu Kb1 = 1.3922; W La1 = 1.4763; W La2 = 1.4875")
  
}

# Run the application
shinyApp(ui, server)




