# Load required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(viridis)
if(file.exists("manifest.json")) file.remove("manifest.json")
if(file.exists("app.R-manifest.json")) file.remove("app.R-manifest.json")

# Generate fresh manifest
library(rsconnect)
rsconnect::writeManifest(appPrimaryDoc = "app.R")


manifest <- jsonlite::read_json("manifest.json")
if(is.null(manifest$platform)) {
  manifest$platform <- as.character(getRversion())
  jsonlite::write_json(manifest, "manifest.json", pretty = TRUE, auto_unbox = TRUE)
}
cat(readLines("manifest.json"), sep = "\n")

rsconnect::setAccountInfo(name='rwcrf7-sanny0un0sowadh-wamik',
			  token='CA2CC17BFEED007C14D237DE2A1AF598',
			  secret='L+YkCe0dDNfUjs2uYd1KJp076brx6ByCKBNJY9E0')

# Load and preprocess data
fugitives <- read_csv("Fugitives.csv", show_col_types = FALSE)
countries <- read_csv("Countries.csv", show_col_types = FALSE)
regions <- read_csv("Regions.csv", show_col_types = FALSE)

# Data preprocessing
fugitives_clean <- fugitives %>%
  mutate(
    Status = case_when(
      grepl("free|at large|wanted", Status, ignore.case = TRUE) ~ "Free",
      grepl("captured|arrested|custody", Status, ignore.case = TRUE) ~ "Captured",
      grepl("deceased|dead", Status, ignore.case = TRUE) ~ "Deceased",
      TRUE ~ "Unknown"
    ),
    Status = factor(Status, levels = c("Free", "Captured", "Deceased", "Unknown")),
    Age_Group = cut(`Current Age (approx.)`, 
                    breaks = c(0, 30, 40, 50, 60, 100), 
                    labels = c("Under 30", "30-39", "40-49", "50-59", "60+"),
                    include.lowest = TRUE),
    Crime_Category = case_when(
      grepl("murder|homicide|kill", `Wanted for`, ignore.case = TRUE) ~ "Murder/Homicide",
      grepl("drug|narco|traffic", `Wanted for`, ignore.case = TRUE) ~ "Drug-related",
      grepl("fraud|money|financial|embezzle", `Wanted for`, ignore.case = TRUE) ~ "Financial Crimes",
      grepl("terror|explos", `Wanted for`, ignore.case = TRUE) ~ "Terrorism",
      grepl("kidnap|abduct", `Wanted for`, ignore.case = TRUE) ~ "Kidnapping",
      grepl("rape|sexual", `Wanted for`, ignore.case = TRUE) ~ "Sexual Crimes",
      grepl("organized crime|criminal org", `Wanted for`, ignore.case = TRUE) ~ "Organized Crime",
      TRUE ~ "Other"
    ),
    Year = as.numeric(`Year of Interpol operation`)
  )

# Define color palettes
status_colors <- c("Free" = "#e74c3c", "Captured" = "#27ae60", "Deceased" = "#7f8c8d", "Unknown" = "#f39c12")

# Calculate key metrics
total_fugitives <- nrow(fugitives)
free_fugitives <- sum(fugitives_clean$Status == "Free", na.rm = TRUE)
captured_fugitives <- sum(fugitives_clean$Status == "Captured", na.rm = TRUE)
capture_rate <- round((captured_fugitives / total_fugitives) * 100, 1)
avg_age <- round(mean(fugitives$`Current Age (approx.)`, na.rm = TRUE), 1)
countries_count <- countries %>% 
  filter(`National Fugitives` > 0 | `Possible Hosted Fugitives / Captured Fugitives` > 0) %>% 
  nrow()

# Country coordinates
country_coords <- data.frame(
  Country = c("United States", "Mexico", "Brazil", "Colombia", "Canada", 
              "Argentina", "Venezuela", "Peru", "Chile", "Ecuador",
              "Bolivia", "Paraguay", "Uruguay", "Guatemala", "Honduras",
              "El Salvador", "Nicaragua", "Costa Rica", "Panama", "Dominican Republic"),
  lat = c(37.0902, 23.6345, -14.2350, 4.5709, 56.1304, 
          -38.4161, 6.4238, -9.1900, -35.6751, -1.8312,
          -16.2902, -23.4425, -32.5228, 15.7835, 15.2000,
          13.7942, 12.8654, 9.7489, 8.5380, 18.7357),
  lng = c(-95.7129, -102.5528, -51.9253, -74.2973, -106.3468, 
          -63.6167, -66.5897, -75.0152, -71.5430, -78.1834,
          -63.5887, -58.4438, -55.7658, -90.2308, -86.2419,
          -88.8965, -85.2072, -83.7534, -80.7821, -70.1627),
  stringsAsFactors = FALSE
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "The Global Hunt: Mapping Interpol's Most Wanted"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("globe")),
      menuItem("Demographics", tabName = "demographics", icon = icon("user-secret")),
      menuItem("Search & Explore", tabName = "search", icon = icon("search"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #ecf0f1;
        }
        .small-box {
          border-radius: 5px;
        }
        .info-box {
          border-radius: 5px;
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("totalBox"),
          valueBoxOutput("freeBox"),
          valueBoxOutput("captureRateBox"),
          valueBoxOutput("countriesBox")
        ),
        fluidRow(
          box(
            title = "The Global Manhunt: Where Are They Hiding?",
            leafletOutput("worldMap", height = 400),
            width = 6,
            status = "primary"
          ),
          box(
            title = "Status Over Time: The Hunt Continues",
            plotlyOutput("statusTimeline", height = 400),
            width = 6,
            status = "primary"
          )
        ),
        fluidRow(
          box(
            title = "Crime Categories: What Are They Wanted For?",
            plotlyOutput("crimeDonut", height = 350),
            width = 6,
            status = "info"
          ),
          box(
            title = "Regional Overview",
            plotlyOutput("regionalBar", height = 350),
            width = 6,
            status = "info"
          )
        )
      ),
      
      # Demographics Tab
      tabItem(
        tabName = "demographics",
        fluidRow(
          box(
            title = "Age Distribution by Current Status",
            plotlyOutput("ageDistribution", height = 400),
            width = 6,
            status = "primary"
          ),
          box(
            title = "Gender Analysis",
            plotlyOutput("genderAnalysis", height = 400),
            width = 6,
            status = "primary"
          )
        ),
        fluidRow(
          box(
            title = "Top 15 Nationalities of Fugitives",
            plotlyOutput("nationalityChart", height = 400),
            width = 6,
            status = "warning"
          ),
          box(
            title = "Countries Hosting Fugitives",
            plotlyOutput("hostingCountries", height = 400),
            width = 6,
            status = "warning"
          )
        )
      ),
      
      # Search Tab
      tabItem(
        tabName = "search",
        fluidRow(
          box(
            title = "Search Filters",
            width = 3,
            status = "info",
            selectInput("status_filter", "Filter by Status:",
                       choices = c("All", levels(fugitives_clean$Status)),
                       selected = "All"),
            selectInput("nationality_filter", "Filter by Nationality:",
                       choices = c("All", sort(unique(fugitives_clean$Nationality[!is.na(fugitives_clean$Nationality)]))),
                       selected = "All"),
            selectInput("crime_filter", "Filter by Crime Type:",
                       choices = c("All", sort(unique(fugitives_clean$Crime_Category))),
                       selected = "All"),
            sliderInput("age_filter", "Age Range:",
                       min = floor(min(fugitives_clean$`Current Age (approx.)`, na.rm = TRUE)),
                       max = ceiling(max(fugitives_clean$`Current Age (approx.)`, na.rm = TRUE)),
                       value = c(20, 80),
                       step = 1),
            br(),
            h4("Summary Statistics"),
            uiOutput("summaryStats"),
            br(),
            div(
              style = "padding: 15px; background-color: #3498db; color: white; border-radius: 5px;",
              h4("About This Dashboard"),
              p("This dashboard visualizes data from Interpol's Red Notice database, tracking international fugitives from 2010-2014."),
              p(strong("Data Source:"), br(), "Interpol Red Notice Database")
            )
          ),
          box(
            title = "Interactive Fugitive Database",
            width = 9,
            status = "primary",
            DTOutput("fugitiveTable")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Value Boxes
  output$totalBox <- renderValueBox({
    valueBox(
      value = total_fugitives,
      subtitle = "Total Fugitives in Database",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  output$freeBox <- renderValueBox({
    valueBox(
      value = free_fugitives,
      subtitle = "Still on the Run",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  output$captureRateBox <- renderValueBox({
    valueBox(
      value = paste0(capture_rate, "%"),
      subtitle = "Successfully Captured",
      icon = icon("handcuffs"),
      color = "green"
    )
  })
  
  output$countriesBox <- renderValueBox({
    valueBox(
      value = countries_count,
      subtitle = "Countries with Fugitive Activity",
      icon = icon("map"),
      color = "light-blue"
    )
  })
  
  # World Map
  output$worldMap <- renderLeaflet({
    map_data <- countries %>%
      left_join(country_coords, by = "Country") %>%
      filter(!is.na(lat)) %>%
      mutate(
        Total_Activity = `National Fugitives` + `Possible Hosted Fugitives / Captured Fugitives`,
        popup_text = paste0(
          "<strong>", Country, "</strong><br>",
          "<b>National Fugitives:</b> ", `National Fugitives`, "<br>",
          "<b>Hosted/Captured:</b> ", `Possible Hosted Fugitives / Captured Fugitives`, "<br>",
          "<b>Total Activity:</b> ", Total_Activity
        )
      )
    
    pal <- colorNumeric(palette = "YlOrRd", domain = map_data$Total_Activity)
    
    leaflet(map_data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -70, lat = -15, zoom = 3) %>%
      addCircleMarkers(
        ~lng, ~lat,
        radius = ~sqrt(Total_Activity) * 3,
        popup = ~popup_text,
        color = ~pal(Total_Activity),
        fillColor = ~pal(Total_Activity),
        fillOpacity = 0.7,
        weight = 2
      ) %>%
      addLegend(
        "bottomright", 
        pal = pal,
        values = ~Total_Activity,
        title = "Fugitive<br>Activity",
        opacity = 1
      )
  })
  
  # Status Timeline
  output$statusTimeline <- renderPlotly({
    status_by_year <- fugitives_clean %>%
      filter(!is.na(Year)) %>%
      group_by(Year, Status) %>%
      summarise(Count = n(), .groups = "drop") %>%
      complete(Year, Status, fill = list(Count = 0))
    
    p <- ggplot(status_by_year, aes(x = factor(Year), y = Count, fill = Status)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = status_colors, name = "Current Status") +
      labs(
        title = "Fugitive Status by Interpol Operation Year",
        x = "Year of Interpol Operation", 
        y = "Number of Fugitives"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>% 
      layout(showlegend = TRUE, legend = list(orientation = "h", y = -0.2))
  })
  
  # Crime Donut Chart
  output$crimeDonut <- renderPlotly({
    crime_summary <- fugitives_clean %>%
      group_by(Crime_Category) %>%
      summarise(
        Count = n(),
        Percentage = round(n() / nrow(fugitives_clean) * 100, 1)
      ) %>%
      arrange(desc(Count))
    
    plot_ly(
      crime_summary, 
      labels = ~Crime_Category, 
      values = ~Count,
      text = ~paste(Count, "fugitives<br>", Percentage, "%"),
      type = 'pie',
      hole = 0.4,
      textposition = 'auto',
      textinfo = 'label+percent',
      marker = list(
        colors = viridis(nrow(crime_summary)),
        line = list(color = '#FFFFFF', width = 2)
      )
    ) %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = "v", x = 1, y = 0.5)
      )
  })
  
  # Regional Bar Chart
  output$regionalBar <- renderPlotly({
    regional_summary <- regions %>%
      mutate(
        Total = `National Fugitives` + `Wanted Fugitives`,
        Region = factor(Region, levels = Region[order(Total, decreasing = TRUE)])
      )
    
    p <- ggplot(regional_summary, aes(x = Region, y = Total)) +
      geom_col(fill = "#3498db", alpha = 0.8) +
      geom_text(aes(label = Total), hjust = -0.1, size = 3.5) +
      coord_flip() +
      labs(
        title = "Fugitive Activity by Region",
        x = "",
        y = "Number of Fugitives"
      ) +
      theme_minimal() +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(regional_summary$Total) * 1.1))
    
    ggplotly(p, tooltip = c("y"))
  })
  
  # Age Distribution
  output$ageDistribution <- renderPlotly({
    age_status <- fugitives_clean %>%
      filter(!is.na(Age_Group)) %>%
      group_by(Age_Group, Status) %>%
      summarise(Count = n(), .groups = "drop") %>%
      complete(Age_Group, Status, fill = list(Count = 0))
    
    p <- ggplot(age_status, aes(x = Age_Group, y = Count, fill = Status)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = status_colors, name = "Status") +
      labs(
        title = "Age Groups by Current Status",
        subtitle = paste("Average fugitive age:", avg_age, "years"),
        x = "Age Group", 
        y = "Number of Fugitives"
      ) +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(showlegend = TRUE)
  })
  
  # Gender Analysis
  output$genderAnalysis <- renderPlotly({
    gender_summary <- fugitives_clean %>%
      filter(!is.na(Sex)) %>%
      group_by(Sex, Status) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(Sex) %>%
      mutate(
        Total = sum(Count),
        Percentage = round(Count / Total * 100, 1)
      )
    
    p <- ggplot(gender_summary, aes(x = Sex, y = Count, fill = Status)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_fill_manual(values = status_colors, name = "Status") +
      scale_y_continuous(labels = scales::percent) +
      labs(
        title = "Gender Distribution by Status",
        x = "Gender", 
        y = "Percentage"
      ) +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(showlegend = TRUE)
  })
  
  # Nationality Chart
  output$nationalityChart <- renderPlotly({
    nationality_data <- fugitives_clean %>%
      filter(!is.na(Nationality)) %>%
      group_by(Nationality) %>%
      summarise(
        Total = n(),
        Free = sum(Status == "Free"),
        Captured = sum(Status == "Captured"),
        .groups = "drop"
      ) %>%
      arrange(desc(Total)) %>%
      head(15) %>%
      mutate(
        Nationality = factor(Nationality, levels = rev(Nationality)),
        Capture_Rate = round(Captured / Total * 100, 1)
      )
    
    p <- ggplot(nationality_data, aes(x = Nationality, y = Total)) +
      geom_col(aes(fill = Capture_Rate), alpha = 0.8) +
      scale_fill_gradient(low = "#e74c3c", high = "#27ae60", name = "Capture Rate (%)") +
      geom_text(aes(label = paste0(Total, " (", Capture_Rate, "%)")), 
                hjust = -0.1, size = 3) +
      coord_flip() +
      labs(
        title = "Top 15 Nationalities of Fugitives",
        x = "", 
        y = "Number of Fugitives"
      ) +
      theme_minimal() +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(nationality_data$Total) * 1.25))
    
    ggplotly(p)
  })
  
  # Hosting Countries
  output$hostingCountries <- renderPlotly({
    hosting_data <- countries %>%
      filter(`Possible Hosted Fugitives / Captured Fugitives` > 0) %>%
      arrange(desc(`Possible Hosted Fugitives / Captured Fugitives`)) %>%
      head(10) %>%
      mutate(Country = factor(Country, levels = rev(Country)))
    
    p <- ggplot(hosting_data, aes(x = Country, y = `Possible Hosted Fugitives / Captured Fugitives`)) +
      geom_segment(aes(x = Country, xend = Country, y = 0, yend = `Possible Hosted Fugitives / Captured Fugitives`),
                   color = "gray70", size = 1) +
      geom_point(size = 4, color = "#e74c3c") +
      geom_text(aes(label = `Possible Hosted Fugitives / Captured Fugitives`), 
                hjust = -0.5, size = 3.5) +
      coord_flip() +
      labs(
        title = "Top 10 Countries Hosting/Capturing Fugitives",
        x = "", 
        y = "Number of Fugitives"
      ) +
      theme_minimal() +
      scale_y_continuous(expand = c(0, 0), limits = c(0, max(hosting_data$`Possible Hosted Fugitives / Captured Fugitives`) * 1.2))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    data <- fugitives_clean
    
    if (input$status_filter != "All") {
      data <- data %>% filter(Status == input$status_filter)
    }
    
    if (input$nationality_filter != "All") {
      data <- data %>% filter(Nationality == input$nationality_filter)
    }
    
    if (input$crime_filter != "All") {
      data <- data %>% filter(Crime_Category == input$crime_filter)
    }
    
    data <- data %>% 
      filter(`Current Age (approx.)` >= input$age_filter[1],
             `Current Age (approx.)` <= input$age_filter[2])
    
    data
  })
  
  # Summary Statistics
  output$summaryStats <- renderUI({
    data <- filtered_data()
    
    HTML(paste0(
      '<div style="padding: 10px; background-color: #f8f9fa; border-radius: 5px;">',
      '<p><strong>Total Fugitives:</strong> ', nrow(data), '</p>',
      '<p><strong>At Large:</strong> ', sum(data$Status == "Free", na.rm = TRUE), '</p>',
      '<p><strong>Captured:</strong> ', sum(data$Status == "Captured", na.rm = TRUE), '</p>',
      '<p><strong>Average Age:</strong> ', round(mean(data$`Current Age (approx.)`, na.rm = TRUE), 1), ' years</p>',
      '</div>'
    ))
  })
  
  # Fugitive Table
  output$fugitiveTable <- renderDT({
    data_to_show <- filtered_data() %>%
      select(
        Fugitive, 
        Nationality, 
        `Wanted for`,
        Crime_Category,
        Status, 
        `Current Age (approx.)`,
        Sex,
        `Country believed to be in / Country of capture`,
        `Year of Interpol operation`
      ) %>%
      rename(
        Name = Fugitive,
        Crime = `Wanted for`,
        `Crime Type` = Crime_Category,
        Age = `Current Age (approx.)`,
        Gender = Sex,
        `Location/Capture Country` = `Country believed to be in / Country of capture`,
        `Operation Year` = `Year of Interpol operation`
      )
    
    datatable(
      data_to_show,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        columnDefs = list(
          list(width = '150px', targets = 0),
          list(width = '200px', targets = 2)
        )
      ),
      filter = 'top',
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      formatStyle('Status',
                  backgroundColor = styleEqual(
                    c("Free", "Captured", "Deceased", "Unknown"),
                    c("#ffcccc", "#ccffcc", "#e6e6e6", "#fff3cd")
                  ),
                  fontWeight = 'bold')
  })
}

# Run the app
shinyApp(ui = ui, server = server)
