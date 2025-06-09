# The Global Hunt: Mapping Interpol's Most Wanted

## 📊 Data Visualization Assignment 3 - MATH2237/MATH2270

An interactive Shiny dashboard visualizing Interpol's Red Notice database, tracking international fugitives from 2010-2014. This project tells a compelling story about global law enforcement efforts through data visualization.

### 🔗 Live Demo
[View Dashboard on Posit Connect](https://connect.posit.cloud/sannyunsowadhwamik/content/019755c5-7e6b-1738-34d8-c1d9cfa1acf5)

---

## 📋 Table of Contents
- [Overview](#overview)
- [Features](#features)
- [Data Source](#data-source)
- [Technologies Used](#technologies-used)
- [Installation](#installation)
- [Running Locally](#running-locally)
- [Deployment](#deployment)
- [Project Structure](#project-structure)
- [Key Insights](#key-insights)
- [Assignment Details](#assignment-details)
- [Author](#author)

---

## 🎯 Overview

This dashboard provides an interactive exploration of Interpol's most wanted fugitives, offering insights into:
- Geographic distribution of fugitive activity
- Crime categories and trends
- Capture success rates by region
- Demographic analysis of international criminals
- Real-time filtering and search capabilities

The visualization aims to make complex international crime data accessible and understandable to a general audience while maintaining analytical depth for researchers and law enforcement professionals.

---

## ✨ Features

### 📍 **Overview Tab**
- **Interactive World Map**: Visualizes fugitive locations using Leaflet
- **Key Metrics**: Total fugitives, capture rate, countries involved
- **Timeline Analysis**: Shows capture success over time
- **Crime Distribution**: Donut chart of crime categories

### 👥 **Demographics Tab**
- **Age Distribution**: Analysis by status (Free, Captured, Deceased)
- **Gender Analysis**: Percentage breakdown by capture status
- **Nationality Rankings**: Top 15 nationalities with capture rates
- **Host Countries**: Countries most likely to harbor fugitives

### 🔍 **Search & Explore Tab**
- **Dynamic Filtering**: Filter by status, nationality, crime type, and age
- **Interactive Data Table**: Searchable and sortable with export options
- **Real-time Statistics**: Updates based on filter selections
- **Export Functionality**: Download data in CSV, Excel, or PDF formats

---

## 📊 Data Source

**Interpol Red Notice Database (2010-2014)**
- Source: [Interpol Operation Infra-Red]([http://www.interpol.int/Crime-areas/Fugitive-investigations/Operation-Infra-Red/](https://github.com/ali-ce/datasets/tree/master/Interpol-Most-Wanted))
- Contains data on 95 international fugitives
- Includes nationality, crime details, status, and location information

### Data Files:
- `Fugitives.csv`: Main dataset with fugitive details
- `Countries.csv`: Country-level aggregated statistics
- `Regions.csv`: Regional summary data
- `README.md`: Methodology and data collection notes

---

## 🛠️ Technologies Used

- **R** (v4.2.0+)
- **Shiny** - Interactive web application framework
- **shinydashboard** - Dashboard layout and UI components
- **tidyverse** - Data manipulation and analysis
- **plotly** - Interactive visualizations
- **leaflet** - Interactive maps
- **DT** - Interactive data tables
- **viridis** - Color palettes

---

## 💻 Installation

### Prerequisites
- R (version 4.0.0 or higher)
- RStudio (recommended) or VS Code with R extension

### Clone the Repository
```bash
git clone https://github.com/Sanny-Un-Sowadh-Wamik/Data_Vis_Assignment-3.git
cd Data_Vis_Assignment-3
```

### Install Required Packages
```r
# Install all required packages
install.packages(c(
  "shiny",
  "shinydashboard",
  "tidyverse",
  "plotly",
  "DT",
  "leaflet",
  "viridis"
))
```

---

## 🚀 Running Locally

### Method 1: Using RStudio
1. Open `app.R` in RStudio
2. Click "Run App" button
3. The dashboard will open in your default browser

### Method 2: Using R Console
```r
# Set working directory
setwd("path/to/Data_Vis_Assignment-3")

# Run the app
shiny::runApp("app.R", launch.browser = TRUE)
```

### Method 3: Using VS Code
```r
# In VS Code terminal
shiny::runApp(host = "127.0.0.1", port = 5000)
```

The app will be available at `http://localhost:5000`

---

## 🌐 Deployment

### Posit Connect Cloud
```r
library(rsconnect)

# Deploy to Posit Connect
rsconnect::deployApp(
  appDir = ".",
  appName = "interpol-fugitives-dashboard",
  server = "connect.posit.cloud"
)
```

### Alternative Platforms
- **shinyapps.io**: Free hosting for Shiny apps
- **Posit Cloud**: Integrated development and hosting
- **RPubs**: For static R Markdown documents only

---

## 📁 Project Structure

```
Data_Vis_Assignment-3/
│
├── app.R                    # Main Shiny application
├── Fugitives.csv           # Primary dataset
├── Countries.csv           # Country aggregations
├── Regions.csv             # Regional summaries
├── manifest.json           # Deployment configuration
├── README.md               # This file
└── .gitignore             # Git ignore file
```

---

## 📈 Key Insights

Based on the data analysis:

1. **Capture Rate**: Approximately 45% of tracked fugitives have been successfully captured
2. **Crime Distribution**: Drug-related crimes constitute the largest category
3. **Regional Patterns**: Latin America shows the highest fugitive activity
4. **Demographics**: Average fugitive age is 42 years, with males comprising 90% of the database
5. **Hide-out Patterns**: Certain countries appear repeatedly as likely hiding locations

---

## 📚 Assignment Details

**Course**: MATH2237/MATH2270 - Data Visualization  
**Assignment**: Assessment 3 - Storytelling with Open Data  
**Weight**: 40% of final grade  
**Due Date**: Wednesday 11th June 2025, 23:59 AEST  

### Learning Outcomes Demonstrated:
- ✅ Identified target audience and visualization goals
- ✅ Applied storytelling techniques with interactive features
- ✅ Addressed ethical considerations in crime data visualization
- ✅ Integrated best practices from data visualization research
- ✅ Conceptualized multiple visualization strategies
- ✅ Successfully sourced, prepared, and deployed open data

---

## 👤 Author

**Sanny Un Sowadh Wamik**  
RMIT University  
Bachelor of Data Science  

📧 Contact: [s4130359@student.rmit.edu.au]  
🔗 GitHub: [@Sanny-Un-Sowadh-Wamik]([https://github.com/Sanny-Un-Sowadh-Wamik](https://github.com/Sanny-Un-Sowadh-Wamik/Data_Vis_Assignment-3))

---

## 📄 License

This project is submitted as part of academic coursework at RMIT University. The visualizations and code are created for educational purposes.

Data Source: Interpol Red Notice Database (Public Domain)

---

## 🙏 Acknowledgments

- Course Coordinator and Teaching Staff of MATH2237/MATH2270
- Interpol for making the Red Notice data publicly available
- R and Shiny communities for excellent documentation and packages

---

*Last Updated: June 2025*
