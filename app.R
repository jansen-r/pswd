library(shiny)
library(DT)
# function ----------------------------------------------------------------
pswd_random <- function(pswd_length=8, include_numbers=T, include_special=T){
  
  number_vector <- 0:9
  special_characters <- unlist(strsplit("~!@#$%^&*(){}_+:<>?,./;'[]-=", ""))
  
  sample_vector <- c(letters, LETTERS)
  if(include_numbers) sample_vector <- c(sample_vector, number_vector) 
  if(include_special) sample_vector <- c(sample_vector, special_characters)
  
  pswd <- paste(sample(sample_vector, pswd_length, replace=T), collapse = '')
  return(pswd)
}


# ui ----------------------------------------------------------------------


ui <- fluidPage(
  titlePanel('Password Generator'),
  sidebarPanel(
    sliderInput('pswdnbr', 'Number of passwords', min = 1, max = 10, step = 1, value = 3),
    sliderInput('pswdlgth', 'Length of passwords', min = 4, max = 20, step = 1, value = c(6,10)),
    checkboxInput('numbers', 'Include numbers', value = T),
    checkboxInput('special', 'Include special character', value = T),
    actionButton('pswdgnr', 'Generate Passwords')
  ),
  mainPanel(
    DTOutput('pswdresults')
  )
)

server <- function(input, output, session) {
  pswdvalues <- reactiveValues(results = NULL)
  
  observeEvent(input$pswdgnr, {
    number_pswd <- input$pswdnbr
    pswd_lengths <- input$pswdlgth
    pswd_lengths <- pswd_lengths[2]:pswd_lengths[1]
    include_numbers <- input$numbers
    include_special <- input$special
    
    res <- data.frame(matrix(ncol = number_pswd, nrow = length(pswd_lengths)))
    
    for(i in 1:length(pswd_lengths)){
      for(j in 1:number_pswd){
        res[i,j] <- pswd_random(pswd_lengths[i], include_numbers, include_special)
      }  
    }
    colnames(res) <- paste('Password', 1:number_pswd)
    rownames(res) <- paste('Length', pswd_lengths)
    pswdvalues$results <- res
  })

  output$pswdresults <- DT::renderDataTable(datatable({
    if(!is.null(pswdvalues$results))
    pswdvalues$results 
  }
  ))
  
}

shinyApp(ui, server)

