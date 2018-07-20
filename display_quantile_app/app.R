library(tidyverse)
library(lubridate)
library(waterData)
library(shiny)

# station and site
station = '02323500'   
stinfo  = siteInfo(station)

# Load data and dynamically check if data needs to be updated
dis <- read_rds("data/dis1.rds")
if (max(dis$dates) < (Sys.Date() - 5)) {
  sdate <- max(dis$dates) + 1
  newdis <- importDVs(staid = station, code = '00060', stat = '00003', sdate= as.character(sdate))
  dis <- bind_rows(dis, newdis) %>%
    distinct() # In case there's repeated rows due to code failure
  write_rds(dis, "data/dis1.rds")
}

# Data carpentries and create quantile data table
dis_noleap <- dis %>%
  filter(!(month(dates) == 2 & day(dates) == 29))

dis_quant <- dis_noleap %>%
  mutate(md = strftime(dates, format = "%m-%d")) %>%
  group_by(md) %>%
  summarise(quan0 = quantile(val, 0),
            quan25 = quantile(val, 0.25),
            quan50 = quantile(val, 0.50),
            quan75 = quantile(val, 0.75),
            quan100 = quantile(val, 1)) %>%
  gather("quantile", "val", -md)
dis_quant$quantile <- str_remove(dis_quant$quantile, "quan") %>%
  factor(levels = c("100", "75", "50", "25", "0"))

#### UI ####
ui <- fluidPage(
   
   titlePanel("Display discharge quantiles"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("yoi",
                     "Years:",
                     min = 1950,
                     max = year(Sys.Date()),
                     value = year(Sys.Date()),
                     step = 1)
      ),
      
      mainPanel(
         plotOutput("quantPlot")
      )
   )
)

#### SERVER ####
server <- function(input, output) {
   
   output$quantPlot <- renderPlot({
     dis_quant1 <- dis_quant %>%
       mutate(dates = paste(input$yoi, md, sep="-") %>% as.Date)
     
     dis_yoi <- dis_noleap %>%
       filter(year(dates) == input$yoi)
     
     ggplot(dis_yoi, aes(x=dates, y=val)) +
       geom_ribbon(data = dis_quant1, aes(x=dates, ymax=val, ymin=0, fill=quantile)) +
       geom_line()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

