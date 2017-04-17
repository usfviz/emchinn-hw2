# rm(list=ls())
# cat("\014")
# setwd('~/DataViz')


if(!require("dplyr")){  
  install.packages("dplyr")  
} 
library("dplyr")

if(!require("tidyr")){
  install.packages("tidyr")
}
library("tidyr")

if(!require("ggplot2")){
  install.packages("ggplot2")
}
library("ggplot2")

if(!require("shiny")){
  install.packages("shiny")
}
library("shiny")

if(!require("scales")){
  install.packages("scales")
}
library("scales")

if(!require("plyr")){
  install.packages("plyr")
}
library("plyr")

if(!require("reshape")){
  install.packages("reshape")
}
library("reshape")


##______________________Reading-in/Organizing Data_______________________##

# Clean DF function
fun_make_df <- function(df){
  df <- df %>% select(-X, -X2015, -X2016, -Indicator.Code, -Indicator.Name, -Country.Code)
  df<-df[!(df$Country.Name=="Not classified"), ] # delete row
  
  # rename columns
  num <- ncol(df) - 1
  year <- as.character(1960:2014) 
  colnames(df) <- c("Country", year)
  
  df <- df[rowSums(is.na(df))!= num, ] # delete rows with all NA's
  
  # replace NA's with mean of row
  ind <- which(is.na(df), arr.ind = TRUE) # indices of NA's
  df[ind] <- rowMeans(df[ , 2:ncol(df)],  na.rm = TRUE)[ind[ , 1]]
  df <- melt(df, id = "Country")
  return(df)
}


metadata <- read.csv("Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", comment.char="#",  na.strings=c("","NA"))
metadata <- metadata %>% select(TableName, Region)
colnames(metadata) <- c("Country", "Region")

data1 <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2.csv") # total population
d1 <- fun_make_df(data1)
colnames(d1) <- c("Country", "Year", "Population")

data2 <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", skip = 4) # life expectancy
d2 <- fun_make_df(data2)
colnames(d2) <- c("Country", "Year", "Life.Expectancy")

data3 <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", skip = 4) # fertility
d3 <- fun_make_df(data3)
colnames(d3) <- c("Country", "Year", "Fertility")

total_df <- merge(d1, d2, by=c("Country","Year"))
total_df <- merge(total_df, d3, by=c("Country","Year"))

total_df <- merge(x = total_df, y = metadata, by = "Country", all.x = TRUE)
total_df <- total_df %>% drop_na(Region)
total_df <- total_df[with(total_df, order(Country, Year)), ]



##______________________Shiny App_________________________##

# Checkbox Input Function
# inspired by http://stackoverflow.com/questions/41813960/how-to-make-the-checkboxgroupinput-color-coded-in-shiny
my_checkboxGroupInput <- function(variable, label, choices, selected, colors){
  choices_names <- choices
  if(length(names(choices))>0) my_names <- names(choices)
  div(id=variable, class="form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
      HTML(paste0('<label class="control-label" for="',variable,'">',label,'</label>')),
      div( class="shiny-options-group",
           HTML(paste0('<div class="checkbox" style="color:', colors,'">',
                       '<label>',
                       '<input type="checkbox" name="', variable, 
                       '" value="', choices, 
                       '"', ifelse(choices %in% selected, 'checked = "checked"', ''), 
                       '/>',
                       '<span>', choices_names,'</span>',
                       '</label>',
                       '</div>', collapse = " "))
      )
  )
}


my_names <- levels(total_df$Region)
my_selected <- c(NULL) # initialize checkbox selection to NULL
my_colors <-c("#3366cc", "#990099", "forestgreen", "#ff9900", "deeppink", "darkturquoise", "firebrick")


ui <- fluidPage(
  headerPanel("World Data Interactive Plot"),
  mainPanel(
    plotOutput("plot1", hover = hoverOpts(id = "plot_hover", delay = 100, delayType = "debounce")),
    
    sliderInput("year", "Year", 1960, 2014, 1960, step = NULL, round = FALSE,
                ticks = FALSE, width = "880px", sep = "", dragRange = TRUE,
                animate = animationOptions(interval = 1000, loop = FALSE)),
    uiOutput("hover_info")
  ), 
  sidebarPanel(
    sliderInput("pop", "Population", 1, 10, 1, step = NULL, ticks = FALSE, width = "400px"), width = 3,
    uiOutput("region")
  ) 
)


server <- function(input, output) {
  yearData <- reactive({
    # filter to the desired year
    total_df %>% filter(Year == input$year)
  })
  
  popData <- reactive({
    # population multiplier from slider input
    input$pop * 0.5
  })
  
  output$region <- renderUI(
    # checkboxGroupInput resulting from function above
    my_checkboxGroupInput("region", "Please Select from Regions Below:", choices = my_names,
                          selected = my_selected, colors = my_colors))
  
  data <- reactive({
    # filter to the desired region
    yearData() %>% filter(Region %in% input$region)
  })
  
  output$plot1 <- renderPlot({
    theme1 <- theme(
      axis.text = element_text(size=13),
      axis.title.x = element_text(size=15),
      axis.title.y = element_text(size=15),
      legend.position = "none",
      panel.grid.major = element_line(colour = "grey60"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(colour = "black", fill = NA, size = 1)
    )
    
    ggplot(data = data(), aes(x= Life.Expectancy, y= Fertility, colour = Region)) + 
      geom_point(aes(size= Population * popData()), alpha= 0.60) +
      geom_point(aes(size= Population * popData()), shape = 21) +
      scale_size_continuous(range = c(3, 10 * popData())) +
      labs(x= "\nLife Expectancy", y= "Fertility Rate\n") +
      scale_y_continuous(limits=c(0, 9), breaks = seq(0, 9, by= 1)) +
      scale_x_continuous(limits=c(15, 90), breaks = seq(10, 100, by= 10)) +
      scale_colour_manual(values = c("East Asia & Pacific"= "#3366cc", "Europe & Central Asia" = "#990099",
                                     "Latin America & Caribbean" = "forestgreen", "Middle East & North Africa" = "#ff9900",
                                     "North America" = "deeppink", "South Asia" = "darkturquoise",
                                     "Sub-Saharan Africa" = "firebrick")) +
      theme1
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(yearData(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.65); ",
                    "left:", left_px + 2 , "px; top:", top_px + 2, "px; padding: 2px 2px 0px 2px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Country: </b>", point$Country, "<br/>",
                    "<b> Fertility Rate: </b>", round(point$Fertility, 2), "<br/>",
                    "<b> Life Expectancy: </b>", round(point$Life.Expectancy, 2), "<br/>",
                    "<b> Population: </b>", point$Population, "<br/>")))
      ) 
  })
}

shinyApp(ui = ui, server = server)





