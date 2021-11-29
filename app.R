library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(janitor)

#get current affiliate list
afflist_url <- "https://foodsforhealth.osu.edu/media/94"
afflist <- read_csv(url(afflist_url),
                    col_types = cols(.default = col_character())) %>%
  clean_names()


#get current publication list, convert all columns to characters so display properly
pub_list_url <- "https://foodsforhealth.osu.edu/media/93"
FFH_Publication_List <- read_csv(url(pub_list_url),
                                 col_types = cols(.default = col_character())) %>%
                        clean_names() %>%
                        filter(food_or_diet_related =="yes")

#pull list of food keywords
foods <- as.tibble(unique({
  unlist(
    str_split(FFH_Publication_List$food, ",[:space:]+")
  )
})) %>%
  drop_na()

#pull list of disease keywords
diseases <- as.tibble(unique({
  unlist(
    str_split(FFH_Publication_List$disease, ",[:space:]+")
  ) 
})) %>%
  drop_na() %>%
  add_case(value = " ", .before = 1)



#Define UI Environment
ui <- {fluidPage(
  
  #set the theme
  theme = bs_theme(
    bootswatch = "lux"
  ),
  
  #Application Title
  #titlePanel(div(img(src = "FFH Masthead_2JPEG.jpg", height = '90.99px', width = '400px'), "Publication Database")),
  headerPanel(""),
  
  fluidRow(column(4, img(src = "FFH Masthead_2JPEG.jpg", height = '90.99px', width = '400px', align = "left", offset = 2)),
           column(8, h1("Publication Database", align = "left", style="padding:38px;"))), 
  
  
  #Layout with inputs on sidebar, results in main page
  sidebarLayout(
    
    #Add input options to sidebar panel
    sidebarPanel(
      
      #Select researcher from list of names
      selectInput(inputId = "Name",
                  "Select Researcher",
                  choices = str_c(afflist$first_name, afflist$last_name, sep = " "),
                  multiple = TRUE),
      
      #Input term to search within article titles
      textInput(inputId = "TitleSearch",
                "Search Titles"),
      
      #Select Food or Diet
      selectInput(inputId = "FoodSearch",
                  "Foods/Nutrients/Diets",
                  choices = foods,
                  multiple = TRUE),
      
      #Select Disease
      selectInput(inputId = "DiseaseSearch",
                  "Diseases",
                  choices = diseases,
                  selected = "Select",
                  multiple = FALSE),
      
      #Add Reset Button
      actionButton(inputId = "ResetSearch",
                   "Reset Filter")
    ),
    
    #Define what outputs will show up on main panel
    mainPanel(
      #line of text showing number of results
      textOutput(outputId = "DIM"),
      
      #A table of publications
      tableOutput(outputId = "Publications")
    )
  )
)}

#Server Outputs
server <- function(input, output){
  #extract last name of researcher
  lastname <- reactive ({
    str_extract(input$Name, "(?<=\\s)[:alpha:]+")
  })
  
  #deliniate list of title search into character vector
  #ensure everything is lower case
  titles <- reactive({
    str_to_lower(unlist(str_split(input$TitleSearch,str_c(",\\s"))))
  })
  
  #result of food input
  food1 <- reactive({
    input$FoodSearch
  })
  
  #result of disease input
  disease1 <- reactive({
    disease1 <- if_else(input$DiseaseSearch==" ", "NoInput", input$DiseaseSearch)
    print(disease1)
  })
  
  #Search for words separated by commas
  #generate list of instances where a pattern match is TRUE
  Presents <- reactive({
    
    Presents <- tibble(.rows = length(FFH_Publication_List$title))
    
    for(z in 1:length(titles())){
      Present<- tibble(str_detect(str_to_lower(FFH_Publication_List$title), str_c(".*",titles()[z])))
      Presents[ , paste0("term", z)] <- as.numeric(Present[[1]])
      
    }
    
    Presents <- Presents %>%
      mutate(Match = rowSums(Presents) >= length(titles())) %>%
      select(last_col())
  })
  #generate final table
  List <- reactive({
    List <- FFH_Publication_List %>%
      mutate(Last = str_detect(author, str_c(".+", lastname()))) %>%
      mutate(FoodPresent = str_detect(food, str_c(".*", food1(), "[:punct:]?"))) %>%
      mutate(DiseasePresent = str_detect(disease, str_c(".*", disease1(), "[:punct:]?"))) %>%
      mutate(inTitle = Presents()) %>%
      filter(if(disease1() == "NoInput") Last == "TRUE" & inTitle=="TRUE" & FoodPresent=="TRUE" else Last == "TRUE" & inTitle=="TRUE" & FoodPresent=="TRUE" & DiseasePresent=="TRUE") %>%
      select(title,author,journal,year)
  })
  
  
  
  #render table output
  output$Publications <- renderTable(List())
  output$DIM <- renderText(paste("displaying", nrow(List()), "results"))
  
  #clear search button 
  observeEvent(input$ResetSearch, {
    updateSelectInput(inputId = "DiseaseSearch", selected = " ")
    updateSelectInput(inputId = "FoodSearch", selected = "NULL")
    updateTextInput(inputId = "TitleSearch",value = "")
    updateSelectInput(inputId = "Name", selected = "NULL")
  })
  
}

#need to find a way to reactively print the dimensions of the table to double check this is working right

#combine into app
shinyApp(ui = ui, server = server)
