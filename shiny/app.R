
# library(shiny)
library(ggmap)
library(osmdata)
library(RCurl)
# library(tidyverse)
# library(rsconnect)

# load data
map_text <- getURL("https://raw.githubusercontent.com/lalmaraz/toronto_climate_perceptions/main/outputs/data/map_data_full.csv")
map_data <- read.csv(text = map_text)
map_tiles <- get_map(getbb("Toronto"),maptype = "terrain")

map_data[map_data == 0] <- NA

ui <- fluidPage(
    titlePanel("Climate Perceptions Study: Demographics"),
    sidebarLayout(
        sidebarPanel("Select an item from the drop down.",
                     selectInput("dropdown", label = "Characteristics", choices = list("AGE" = list("18-34", "35-49", "50-64", "65+"),
                                                                                       "GENDER" = list("Male", "Female"),
                                                                                       "EDUCATION" = list("High school or less", 
                                                                                                          "Some community college", 
                                                                                                          "Completed community college",
                                                                                                          "Some university",
                                                                                                          "Completed undergraduate degree",
                                                                                                          "Post graduate/professional school"),
                                                                                       "INCOME" = list("Under $40,000",
                                                                                                       "$40,001 to $60,000",
                                                                                                       "$60,001 to $80,000",
                                                                                                       "$80,001 to $100,000",
                                                                                                       "$100,001 to $150,000",
                                                                                                       "More than $150,000"),
                                                                                       "CONCERNED ABOUT CLIMATE CHANGE" = list("Local Impacts: Yes", "Local Impacts: No", "Global Impacts: Yes", "Global Impacts: No"),
                                                                                       "INFORMED ABOUT CLIMATE CHANGE" = list("Yes", "No")))
        ),
        #mainPanel("Created with data from the City of Toronto's Open Data Portal.", plotOutput(outputId = "mapOut", width="100%")
        mainPanel(
            tabsetPanel(
                tabPanel("Map", plotOutput("mapOut", width="100%")), 
                tabPanel("About", verbatimTextOutput("about"))
        )
    )))

server <- function(input, output) {
    output$about <- renderText({
"This app is created with data from the City of Toronto's open data catalogue
using the R statistical programming language (R Core Team 2020). It uses shiny
(Chang et al. 2021), ggplot2 (Wickham 2016), ggmap (Kahle and Wickham 2013),
osmdata (Padgham et al. 2017) and RCurl (Temple Lang 2021).



References
City of Toronto. 2021. “Climate Perception Study.” https://open.toronto.ca/dataset/climate-perception-study/. 
Kahle, David, and Hadley Wickham. 2013. “Ggmap: Spatial Visualization with Ggplot2.” The R Journal 5 (1): 144–61. https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf. 
Lüdecke, Daniel, Mattan S. Ben-Shachar, Indrajeet Patil, Philip Waggoner, and Dominique Makowski. 2021. “Assessment, Testing and Comparison of Statistical Models Using r.” Journal of Open Source Software 6 (59): 3112. https://doi.org/10.31234/osf.io/vtq8f. 
Padgham, Mark, Bob Rudis, Robin Lovelace, and Maëlle Salmon. 2017. “Osmdata.” The Journal of Open Source Software 2 (14). https://doi.org/10.21105/joss.00305. 
Temple Lang, Duncan. 2021. RCurl: General Network (HTTP/FTP/...) Client Interface for r. https: //CRAN.R-project.org/package=RCurl. 
Wickham, Hadley. 2016. Ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. https: //ggplot2.tidyverse.org."
    })
    
    output$mapOut <- renderPlot({
        if(input$dropdown == "18-34"){choice <- map_data$age_group_0}
        else if (input$dropdown == "35-49"){choice <- map_data$age_group_1}
        else if (input$dropdown == "50-64"){choice <- map_data$age_group_2}
        else if (input$dropdown == "65+"){choice <- map_data$age_group_3}
        
        else if (input$dropdown == "Male"){choice <- map_data$gender_0}
        else if (input$dropdown == "Female"){choice <- map_data$gender_1}
        
        else if (input$dropdown == "High school or less"){choice <- map_data$education_0}
        else if (input$dropdown == "Some community college"){choice <- map_data$education_1}
        else if (input$dropdown == "Completed community college"){choice <- map_data$education_2}
        else if (input$dropdown == "Some university"){choice <- map_data$education_3}
        else if (input$dropdown == "Completed undergraduate degree"){choice <- map_data$education_4}
        else if (input$dropdown == "Post graduate/professional school"){choice <- map_data$education_5}
        
        else if (input$dropdown == "Under $40,000"){choice <- map_data$income_0}
        else if (input$dropdown == "$40,001 to $60,000"){choice <- map_data$income_1}
        else if (input$dropdown == "$60,001 to $80,000"){choice <- map_data$income_2}
        else if (input$dropdown == "$80,001 to $100,000"){choice <- map_data$income_3}
        else if (input$dropdown == "$100,001 to $150,000"){choice <- map_data$income_4}
        else if (input$dropdown == "More than $150,000"){choice <- map_data$income_5}
        
        else if (input$dropdown == "Local Impacts: Yes"){choice <- map_data$c_local_y}
        else if (input$dropdown == "Local Impacts: No"){choice <- map_data$c_local_n}
        else if (input$dropdown == "Global Impacts: Yes"){choice <- map_data$c_global_y}
        else if (input$dropdown == "Global Impacts: No"){choice <- map_data$c_global_n}
        else if (input$dropdown == "Yes"){choice <- map_data$informed_y}
        else if (input$dropdown == "No"){choice <- map_data$informed_n}
        
        ggmap(map_tiles)+
            geom_point(map_data, mapping = aes(x = longitude, y = latitude, size = choice))
    }
    )
}

shinyApp(ui = ui, server = server)


