# load packages
library(shiny)
library(ggthemes)
library(tidyverse)

# Communicable Disease data
disease_data <- readxl::read_xls("data/communicable_diseases.xls") %>% 
  janitor::clean_names() %>% 
  select(-flag_codes, -flags) %>% 
  filter(measure == "Incidence per 100 000 population") %>% 
  mutate(variable = factor(variable, 
                      levels = c("Incidence of pertussis", 
                                 "Incidence of hepatitis B", 
                                 "Incidence of measles", 
                                 "Acquired immunodeficiency syndrome (AIDS)"), 
                      labels = c("Pertussis", 
                                 "Hepatitis B", 
                                 "Measles", 
                                 "Acquired Immunodeficiency Syndrome (AIDS)")), 
         continent = case_when(country == "Korea" | country == "Japan" |
                                 country == "Costa Rica" | country == "Canada" |
                                 country == "Mexico" | country == "United States" |
                                 country == "Australia" | country == "New Zealand" |
                                 country == "Chile" | country == "Colombia" |
                                 country == "Israel" | country == "Türkiye"
                               ~ 'Non-Europe',
                               country == "Poland" | country == "Sweden" |
                                 country == "Finland" | country == "Romania" |
                                 country == "Greece" | country == "Slovak Republic" |
                                 country == "Slovenia" | country == "Lithuenia" |
                                 country == "Estonia" | country == "Hungary" |
                                 country == "Czech Republic" | country == "Croatia" | 
                                 country == "Latvia" | country == "Hungary"
                               ~ 'East Europe',
                               country == "Portugal" | country == "Spain" |
                                 country == "France" | country == "Belgium" |
                                 country == "Ireland" | country == "United Kingdom" |
                                 country == "Netherlands" | country == "Switzerland" |
                                 country == "Italy" | country == "Germany" |
                                 country == "Iceland" | country == "Denmark" |
                                 country == "Austria" | country == "Luxembourg" |
                                 country == "Norway"
                               ~ 'West Europe')) 
country_list <- unique(disease_data$country)

# Define UI 
ui <- fluidPage(
  # Title
  titlePanel('Communicable Diseases in OECD Countries'),
  # Sidebar  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      
      # Plot Tabset
      conditionalPanel(condition = "input.tabselected==1",
        helpText("Observe the trends of incidence of communicable diseases by year for any OECD country."),
        # country
        selectInput(inputId = "coun",
                    label = "Choose a country to display",
                    choices = c(country_list),
                    selected = "Australia"),
        # measure
        radioButtons(inputId = "var1",
                     label = "Choose a communicable disease",
                     choices = c("Pertussis",
                                 "Hepatitis B",
                                 "Measles",
                                 "Acquired Immunodeficiency Syndrome (AIDS)"),
                     selected = "Pertussis"),
        # show trendline
        checkboxInput(inputId = "box", 
                      label = "Display Trendline",
                      value = FALSE)
        ),
      conditionalPanel(condition = "input.tabselected==2",
                       helpText("Compare incidence of communicable diseases in OECD countries by region for any year."),
                       # country
                       selectInput(inputId = "eur",
                                   label = "Choose a region to display",
                                   choices = c("Non-Europe", 
                                               "East Europe",
                                               "West Europe"),
                                   selected = "Non-Europe"),
                       # year
                       selectInput(inputId = "year",
                                  label = "Choose a year to display",
                                  choices = c("2010", "2011", "2012", "2013", "2014", "2015", 
                                              "2016", "2017", "2018", "2019", "2020"), 
                                  selected = "2010"),
                       # measure
                       radioButtons(inputId = "var2",
                                    label = "Choose a communicable disease",
                                    choices = c("Pertussis",
                                                "Hepatitis B",
                                                "Measles",
                                                "Acquired Immunodeficiency Syndrome (AIDS)"),
                                    selected = "Pertussis")
      ),
      conditionalPanel(condition = "input.tabselected==3",
        helpText("Application Author: Louise Oh"),
        helpText("Last Updated: March 10, 2023"),   
        br(),
        helpText("This is a project description that outlines the core concept, insight, choice of widgets, and other information about the application.")
        )
      
    ),
    
    mainPanel(
      # Tabset Panels
      tabsetPanel(type = "tabs", id = "tabselected", selected = 1,
                  
                  # Conditional Tabsets
                  tabPanel("Cases by Year", value = 1, 
                           br(),
                           plotOutput("distPlot1"),
                           p("Data source: Communicable diseases in OECD countries by year - Health status. (2022). Retrieved February 14, 2023 from the",
                             a("OECD Hompage.",
                               href = "https://stats.oecd.org/Index.aspx?DatasetCode=HEALTH_STAT")),
                           p("Note: Missing data are not displayed on the graph.")),
                  tabPanel("Cases by Country", value = 2, 
                           br(),
                           plotOutput("distPlot2"),
                           p("Data source: Communicable diseases in OECD countries by year - Health status. (2022). Retrieved February 14, 2023 from the",
                             a("OECD Hompage.",
                               href = "https://stats.oecd.org/Index.aspx?DatasetCode=HEALTH_STAT")),
                           p("Note: Missing data are not displayed on the graph.")),
                  tabPanel("Project Description", value = 3, 
                           verbatimTextOutput("summary"),
                            br(),
                            h2("Core Concepts & Insights"),
                            p("This application aims to communicate the trend of communicable diseases by year and regions in OECD countries.  Using the first line graph, users can note the trend of each communicable disease by year for each country; this information can be supplemented with the second bar graph where users can easily note the trend of communicable disease trends by country per geographical region."),
                            p("A key insight using these graphs is the trend of pertussis cases in Oceana countries. The line graph in the first tab shows that incidence of pertussis in Australia increases from 2010 to 2011 and decreases in overall trend from 2012 to 2021. The graph for incidence of pertussis in New Zealand shows that cases of pertussis increase from 2010 to 2012 and decreases after 2012 overall. Users can easily make this observation using the trendline function. Next, users can supplement this information using the bar plot in the second tab. Observe incidence of pertussis in non-European countries. Pertussis seemed to be transmitted to the Oceania region starting in Australia in 2010, and to New Zealand in the next few years. In 2010, Australia starts with 158.1 cases per 100,000 population while New Zealand was at 20. In 2011, Australia reaches its peak with 173.5 cases and cases in New Zealand starts to grow as well. New Zealand reaches 133.8 cases per 100,000 population in 2012, becoming the highest country with pertussis cases that year. New Zealand and Australia tops the cases in non-European countries until 2018 when Colombia seems to have an outbreak of pertussis cases. Using these two graphs, users can infer a story about pertussis in the Oceania region over 2010 to 2021."),
                            h2("Choice of Widgets"),
                            h4("Tab 1: Line graph"),
                            p("The widgets allow the users to create customized graphs to focus on a specific country and communicable disease of interest. A drop-down menu is used to choose one country out of the 41 OECD countries. Radio buttons are used to select the type of communicable disease to display. An option to choose multiple diseases through check boxes was not included because of missing data for some diseases which may cause a misleading interpretation when comparing two or more type of diseases (e.g. there could be misinterpretation comparing incidence of pertussis and AIDS in Australia because the graph will show data for pertussis from 2010 to 2021 while that for AIDS only for 2010 to 2012). Users can choose to display a linear trendline for each graph to aid their interpretation of the larger trend over the years."),
                            h4("Tab 2: Bar graph"),
                            p("The widgets allow the users to compare incidence of communicable disease by geographical region for a specific year and disease type. A drop-down menu is used to choose between Non-Europe, East Europe, and West Europe regions. Similarly, a drop-down menu is used to choose a year between 2010 and 2021. Radio buttons are used to select the type of communicable disease to display, identically with the line graph in the first tab. These widgets were chosen based on lacking features to supplement the line graph in the first tab. Widgets with bar graphs were used instead of a map because there are only 41 countries and would be more areas on the map with missing data, which would make it difficult to compare the countries at first glance."),
                            h2("Other Information"),
                            p("The data was retrieved from Communicable diseases in OECD countries by year - Health status (2022). on February 14, 2023 from the",
                              a("OECD Hompage.",
                                href = "https://stats.oecd.org/Index.aspx?DatasetCode=HEALTH_STAT"),
                              "Missing data are not displayed on the graph. The colors used were taken from the OECD factbook color palette.")
                           ),
                  br(),
                  helpText("Application Author: Louise Oh"),
                  br()
                  )
      
        ))
) # close fluidpage


# Define server
server <- function(input, output, session) {
  
  # Display Plot 1
  output$distPlot1 <- renderPlot({
    # dataset filtered country and disease by input
    disease_input1 <- disease_data %>% 
      filter(country == input$coun & variable == input$var1) 
    # y-axis labels
    y_labs1 <- switch(input$var1,
                     "Pertussis" = "Incidence of Pertussis", 
                     "Hepatitis B" = "Incidence of Hepatitis B",
                     "Measles" = "Incidence of Measles",
                     "Acquired Immunodeficiency Syndrome (AIDS)" = "Incidence of Acquired Immunodeficiency Syndrome (AIDS)")
    # y-axis coordinates
    y_value1 <- unique(disease_input1$value)
    y_min1 <- min(y_value1)
    y_max1 <- max(y_value1)
    year1 <- unique(disease_input1$year)
    year_min1 <- min(year1)
    year_max1 <- max(year1)
    subtitle1 <- str_c(y_labs1, " (", year_min1, " - ", year_max1, ")")
    title1 <- str_c(input$coun, "\n")
    # line labels
    line_label <- disease_input1 %>% 
      select(year, value) %>% 
      arrange(year) %>%
      pivot_longer(cols = -year,
                   names_to = "category") %>% 
      mutate(labels = prettyNum(value, big.mar = ","))
    # create line graph
    ggplot(disease_input1, aes(x = year, y = value)) +
      geom_point(size = 2.5) +
      geom_line(size = 0.5) +
      stat_smooth(geom = 'line', alpha = 0.7, se = FALSE, linetype = as.integer(input$box),
                  method = lm, color = "#0057d9") +
      scale_x_continuous(breaks = seq(2010, 2021, 1),
                         labels = c("2010", "2011", "2012", "2013", "2014", "2015", 
                                    "2016", "2017", "2018", "2019", "2020", "2021"),
                         expand = c(0, 0.2)) +
      scale_y_continuous(limits = c(0, y_max1*1.06),
                         n.breaks = 5,
                         expand = c(0, 0)) +
      theme_classic() +
      labs(x = "Year",
           y = "Cases per 100,000 population",
           title = title1,
           subtitle = subtitle1) +
      theme(panel.grid.minor.x = element_blank(),
            plot.title = element_text(size = 23, vjust = 0),
            plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 13),
            axis.text = element_text(size = 10)) +
      geom_text(data = line_label, 
                aes(x = year, y = value, label = labels),
                vjust = -1, hjust = 0.5)
  })
  
  # Display Plot 2
  output$distPlot2 <- renderPlot({
    # dataset filtered country and disease by input
    disease_input2 <- disease_data %>% 
      filter(continent == input$eur & year == input$year & variable == input$var2) 
    # y-axis labels
    y_labs2 <- switch(input$var2,
                      "Pertussis" = "Incidence of Pertussis", 
                      "Hepatitis B" = "Incidence of Hepatitis B",
                      "Measles" = "Incidence of Measles",
                      "Acquired Immunodeficiency Syndrome (AIDS)" = "Incidence of Acquired Immunodeficiency Syndrome (AIDS)")
    title2 <- str_c(disease_input2$continent, "an Countries \n")
    subtitle2 <- str_c(y_labs2, " (", input$year, ")")
    # bar labels
    bar_label <- disease_input2 %>% 
      select(country, value) %>% 
      arrange(desc(value)) %>%
      pivot_longer(cols = -country,
                   names_to = "category") %>% 
      mutate(labels = prettyNum(value, big.mar = ","))
    y_max2 <- unique(bar_label$value) %>% 
      max()
    y_maxlim <- (y_max2*1.05)
    # create bar graph
    ggplot(disease_input2) +
      geom_col(aes(y = value, x = reorder(country, -value)),
               fill = "#00aae6", width = 0.8) +
      scale_y_continuous(expand = c(0, 0),
                         limit = c(0, y_maxlim)) +
      scale_x_discrete(guide = guide_axis(angle = 30)) +
      theme_classic() +
      theme(legend.position = "top",
            legend.direction = "horizontal",
            panel.grid.minor.x = element_blank(),
            plot.title = element_text(size = 23, vjust = 0),
            plot.subtitle = element_text(size = 18, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 13),
            axis.text.x = element_text(vjust = 1, size = 13),
            axis.text.y = element_text(size = 10)) +
      labs(y = "Cases per 100,000 population",
           x = NULL,
           title = title2,
           subtitle = subtitle2) +
      geom_text(data = bar_label,
                aes(y = value, x = country, label = labels),
                vjust = -0.3)
  })

} # close server


# Run the application 
shinyApp(ui = ui, server = server)



