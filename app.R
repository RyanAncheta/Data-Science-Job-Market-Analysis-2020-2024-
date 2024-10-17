# Load necessary libraries
library(data.table)
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(httr)
library(data.table)
library(countrycode)  # For converting country codes to names
library(maps)  # For world map data
library(viridis)
library(corrplot)
library(patchwork)
library(tidyr)
library(caret)


# Define the API URL and fetch the dataset
url <- "https://www.kaggle.com/api/v1/datasets/download/saurabhbadole/latest-data-science-job-salaries-2024"
resp <- GET(url, write_disk("data_science_salaries_2024.zip", overwrite = TRUE))

# Check if the download was successful
if (resp$status_code == 200) {
  print("Download successful")
  unzip("data_science_salaries_2024.zip", exdir = "data")
  data <- fread("data/DataScience_salaries_2024.csv")  # Load the dataset
} else {
  stop("Download failed. Please check the API or your Kaggle credentials.")
}

# Filter the top 8 regions
top_8_regions <- c("QA", "IL", "PR", "US", "NZ", "CA", "EG", "SA")

# Convert country codes to country names for the map
location_data <- data %>%
  mutate(country_name = countrycode(company_location, origin = 'iso2c', destination = 'country.name')) %>%
  group_by(country_name) %>%
  summarise(count = n()) %>%
  filter(!is.na(country_name))

# World map data
location_data <- data %>%
  mutate(country_name = ifelse(company_location == "US", "USA", 
                               countrycode(company_location, origin = 'iso2c', destination = 'country.name'))) %>%
  group_by(country_name) %>%
  summarise(count = n()) %>%
  filter(!is.na(country_name))  # Remove any rows with NA values

# Join with world map data
world_map <- map_data("world")

# Merge the world map data with your company location data
map_data_with_companies <- world_map %>%
  left_join(location_data, by = c("region" = "country_name"))

# Define UI for the app with tabs
ui <- fluidPage(
  
  # App title
  titlePanel("Data Science Job Market Analysis"),
  
  tags$head(
    tags$style(HTML("
      body {
        background-color: #282847;  /* Black background */
        color: #FFFFFF;  /* White text */
      }
      .navbar-default .navbar-nav > li > a {
        color: #FFFFFF !important; /* White text for navbar links */
      }
      .navbar-default .navbar-brand {
        color: #FFFFFF !important; /* White text for the brand */
      }
      .tab-content {
        background-color: #282847;  /* Black background for tabs */
        color: #FFFFFF;  /* White text for tab content */
      }
      .well {
        background-color: #333333;  /* Dark gray background for sidebars */
        color: #FFFFFF;  /* White text */
      }
      h3, h4 {
        color: #FFFFFF; /* White headings */
      }
      .table {
        color: #FFFFFF;  /* White text for table */
        background-color: #333333;  /* Dark gray table background */
      }
    "))
  ),
  
  
  # Create a dashboard-like layout using Bootstrap grid system
  tabsetPanel(
    tabPanel("Home",
             fluidRow(
               column(1, h4("Choose Visualization"),
                      radioButtons("viz_choice", "Select:",
                                   choices = c("Average Salary by Year" = "salary_year",
                                               "Top 10 Frequencies" = "freq_plot",
                                               "Correlation Plot" = "correlation_plot",
                                               "Box Plots" = "box_plots",
                                               "Summary Statistics" = "summary_stats"),
                                   selected = "salary_year")  # Default visualization
               ),
               column(5, h4("Company Location Map"), plotOutput("mapPlot", height = "380px")),
               column(6, h4("Visualization"), uiOutput("mainContent"))  # Use uiOutput to render plot or table
             ),
             h4("Dataset Snapshot"),
             tableOutput("dataPreview")
    ),
    
    tabPanel("Chronological Changes",
             sidebarLayout(
               sidebarPanel(
                 h3("Chronological Analysis"), width = 2
               ),
               mainPanel(
                 h3("Chronological Changes in the Post-COVID Era"),
                 
                 # Add navigation tabs to tell the story
                 tabsetPanel(
                   # Sub-tab 1: Overall Job Trends across years
                   tabPanel("Job Trends",
                            h4("Job Postings Over Time (2022 to 2024)"),
                            plotOutput("jobTrendPlot")
                   ),
                   
                   # Sub-tab 2: Top 10 Jobs Increased in 2023
                   tabPanel("Top 10 Jobs Increased in 2023",
                            h4("Top 10 Jobs with the Highest Increase in Job Postings (2022 to 2023)"),
                            plotOutput("top_increase_jobs_plot")
                   ),
                   
                   # Sub-tab 3: Changes in 2024 for Top 2023 Jobs
                   tabPanel("Changes in 2024",
                            h4("Change in Job Postings for Top 2023 Jobs (2023 vs 2024)"),
                            plotOutput("change_2024_jobs_plot")
                   ),
                   
                   # Sub-tab 4: Comparison of Job Postings for Top 10 Jobs (2023 vs 2024)
                   tabPanel("Top Jobs Comparison (2023 vs 2024)",
                            h4("Comparison of Job Postings for Top 10 Jobs (2023 vs 2024)"),
                            plotOutput("jobComparisonPlot")
                   )
                 )
               )
             )
    ),
    
    
    # Tab 2: Current Work Nature Trends with Side Navigation Tabs
    tabPanel("Work Nature Trends",
             sidebarLayout(
               sidebarPanel(
                 h3("Work Nature Trends"), width = 3,
                 # Navigation Tabs (placed in sidebar)
                 tabsetPanel(id = "selected_trend_tab", # Add an input ID to track selected tab
                             tabPanel("Salary by Experience Level"),
                             tabPanel("Salary by Employment Type"),
                             tabPanel("Salary by Work Delivery Type"),
                             tabPanel("Salary by Remote Ratio and Employment Type"),
                             tabPanel("Salary by Remote Ratio and Experience Level"),
                             tabPanel("Salary by Remote Ratio and Experience Level (US)") # Update this tab name
                 )
               ),
               mainPanel(
                 # Output based on selected tab
                 uiOutput("work_nature_output")
               )
             )
    ),
    
    # Tab 3: Regional Opportunities
    tabPanel("Regional Opportunities",
             sidebarLayout(
               sidebarPanel(
                 h3("Regional Opportunities"), width = 2
               ),
               mainPanel(
                 tabsetPanel(
                   # Sub-tab 1: Top 8 Regions by Average Salary
                   tabPanel("Top 8 Regions by Salary",
                            h3("Top 8 Regions with Highest Average Salaries"),
                            plotOutput("top_regions_salary_plot")
                   ),
                   
                   # Sub-tab 2: Top Jobs by Employee Count
                   tabPanel("Top Jobs by Employee Count",
                            h3("Top 5 Jobs with Highest Employees in Each Top 8 Region"),
                            plotOutput("top_jobs_per_region_plot")
                   ),
                   
                   # Sub-tab 3: Top Jobs by Year and Region
                   tabPanel("Top Jobs by Year and Region",
                            h3("Top 5 Jobs in Each Top Region (2023 & 2024)"),
                            plotOutput("top_jobs_per_region_year_plot")
                   ),
                   
                   # Sub-tab 4: US and CA Top Jobs
                   tabPanel("Top Jobs in US and Canada",
                            h3("Top 5 Jobs in US and CA with Highest Average Salaries (2023 & 2024)"),
                            plotOutput("us_ca_jobs_plot")
                   )
                 )
               )
             )
    ),
    tabPanel("Market Prospects by Job Role",
             sidebarLayout(
               sidebarPanel(
                 h3("Market Prospects Story"), width = 3
               ),
               mainPanel(
                 # Add navigation tabs for different analyses in this story
                 tabsetPanel(
                   # Sub-tab 1: Top 10 Job Roles by Average Salary
                   tabPanel("Top 10 Job Roles by Average Salary",
                            h4("Top 10 Highest Paying Job Roles in Data Science"),
                            plotOutput("top10JobRolesSalaryPlot")
                   ),
                   
                   # Sub-tab 2: Job Opportunities and Average Salary (UPDATED)
                   tabPanel("Job Opportunities and Average Salary",  # Update tab name
                            h4("Job Roles with Highest Opportunities and Average Salary (USD)"),
                            plotOutput("jobOpportunitiesPlot")  # Reuse the existing output plot ID
                   ),
                   
                   # Sub-tab 3: Experience Level and Company Size
                   tabPanel("Experience & Company Size",
                            h4("Experience Level and Company Size for Top 5 Data Science Job Roles"),
                            plotOutput("jobRequirementsPlot", height = "500px")
                   )
                 )
               )
             )
    ),
    tabPanel("Ensuring Data Fairness and Unbiased",
             sidebarLayout(
               sidebarPanel(
                 h3("Data Fairness and Unbiased Analysis"), width = 3
               ),
               mainPanel(
                 tabsetPanel(
                   # Sub-tab 1: Remote Work Opportunities by Region
                   tabPanel("Remote Work Opportunities",
                            h4("Remote Work Opportunities by Region and Remote Ratio"),
                            plotOutput("remoteWorkPlot", height = "600px")
                   ),
                   
                   # Sub-tab 2: Average Salaries by Company Location
                   tabPanel("Average Salaries by Location",
                            h4("Average Salaries for Data Science Jobs by Company Location"),
                            plotOutput("salaryLocationPlot", height = "600px")
                   ),
                   
                   # Sub-tab 3: Company Location Concentration
                   tabPanel("Company Location Map",
                            h4("Concentration of Company Locations Worldwide"),
                            plotOutput("companyLocationMap")
                   ),
                   
                   # Sub-tab 4: Salary Distribution
                   tabPanel("Salary Distribution",
                            h4("Salary Distribution in the Dataset"),
                            plotOutput("salaryDistributionPlot")
                   )
                 )
               )
             )
    ),
    tabPanel("Clustering Data Science Job Roles in the US",
             sidebarLayout(
               sidebarPanel(
                 h3("Clustering Analysis"), width = 3
               ),
               mainPanel(
                 tabsetPanel(
                   # Sub-tab 1: K-means Clustering Plot
                   tabPanel("K-means Clustering Plot",
                            h4("K-means Clustering with 4 Clusters (US Data)"),
                            plotOutput("clusterPlot")
                   ),
                   
                   # Sub-tab 2: Top 3 Job Roles by Cluster
                   tabPanel("Top 3 Job Roles by Cluster",
                            h4("Top 3 Job Roles in Each Cluster"),
                            plotOutput("topJobsByClusterPlot")
                   )
                 )
               )
             )
    )
  )
)

# Define server logic required to create the plots
server <- function(input, output) {
  
  output$dataPreview <- renderTable({
    head(data, 5)  # Display the first 5 rows of the dataset
  })
  
  # Company location map
  output$mapPlot <- renderPlot({
    ggplot(map_data_with_companies, aes(long, lat, group = group)) +
      geom_polygon(aes(fill = count), color = "white") +  # Fill the regions based on company counts
      scale_fill_viridis_c(option = "C", na.value = "gray90", name = "Company Count") +  # Use viridis color scale
      labs(title = "Concentration of Company Locations Worldwide",
           x = "Longitude",
           y = "Latitude") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "lightblue"),  # Set ocean color
            panel.grid = element_blank())
  })
  
  # Reactive main plot or content based on selection
  output$mainContent <- renderUI({
    if (input$viz_choice == "summary_stats") {
      tableOutput("summaryStats")  # Display the summary stats table
    } else {
      plotOutput("mainPlot")  # Display the plot
    }
  })
  
  # Reactive main plot based on selection
  output$mainPlot <- renderPlot({
    if (input$viz_choice == "salary_year") {
      avg_salary_by_year <- data %>%
        group_by(work_year) %>%
        summarise(Average_Salary_USD = mean(salary_in_usd, na.rm = TRUE))
      
      ggplot(avg_salary_by_year, aes(x = work_year, y = Average_Salary_USD)) +
        geom_line(group = 1, color = "blue") +
        geom_point(color = "red", size = 3) +
        labs(title = "Average Salary vs. Work Year", x = "Work Year", y = "Average Salary in USD") +
        theme_minimal()
      
    } else if (input$viz_choice == "freq_plot") {
      combined_freq_df <- bind_rows(
        calculate_top_frequencies_df(data, "experience_level"),
        calculate_top_frequencies_df(data, "employment_type"),
        calculate_top_frequencies_df(data, "job_title"),
        calculate_top_frequencies_df(data, "employee_residence"),
        calculate_top_frequencies_df(data, "company_location"),
        calculate_top_frequencies_df(data, "remote_ratio")
      )
      
      ggplot(combined_freq_df, aes(x = reorder(Category, -Count), y = Count, fill = Variable)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_wrap(~ Variable, scales = "free", ncol = 2) +
        labs(title = "Top 10 Frequencies of Different Variables", x = "Category", y = "Count") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_brewer(palette = "Set2")
      
    } else if (input$viz_choice == "correlation_plot") {
      data_encoded <- data[, lapply(.SD, function(x) {
        if (is.factor(x) || is.character(x)) as.integer(as.factor(x)) else x
      })]
      
      cor_matrix <- cor(data_encoded, use = "complete.obs")
      corrplot(cor_matrix, method = "circle")
      
    } else if (input$viz_choice == "box_plots") {
      plot_work_year <- ggplot(data_encoded, aes(x = "", y = work_year)) +
        geom_boxplot(fill = "lightblue", color = "black") +
        labs(title = "Work Year", y = "Value") +
        theme_minimal()
      
      plot_experience_level <- ggplot(data_encoded, aes(x = "", y = experience_level)) +
        geom_boxplot(fill = "lightgreen", color = "black") +
        labs(title = "Experience Level", y = "Value") +
        theme_minimal()
      
      plot_employment_type <- ggplot(data_encoded, aes(x = "", y = employment_type)) +
        geom_boxplot(fill = "lightcoral", color = "black") +
        labs(title = "Employment Type", y = "Value") +
        theme_minimal()
      
      plot_salary <- ggplot(data_encoded, aes(x = "", y = salary)) +
        geom_boxplot(fill = "lightyellow", color = "black") +
        scale_y_continuous(labels = comma) +
        labs(title = "Salary", y = "Value") +
        theme_minimal()
      
      plot_salary_usd <- ggplot(data_encoded, aes(x = "", y = salary_in_usd)) +
        geom_boxplot(fill = "lightgray", color = "black") +
        scale_y_continuous(labels = comma) +
        labs(title = "Salary in USD", y = "Value") +
        theme_minimal()
      
      plot_remote_ratio <- ggplot(data_encoded, aes(x = "", y = remote_ratio)) +
        geom_boxplot(fill = "lightsteelblue", color = "black") +
        labs(title = "Remote Ratio", y = "Value") +
        theme_minimal()
      
      plot_company_size <- ggplot(data_encoded, aes(x = "", y = company_size)) +
        geom_boxplot(fill = "lightsalmon", color = "black") +
        labs(title = "Company Size", y = "Value") +
        theme_minimal()
      
      # Combine all box plots into one layout
      combined_plot <- plot_work_year + plot_experience_level + plot_employment_type +
        plot_salary + plot_salary_usd + plot_remote_ratio + plot_company_size +
        plot_layout(ncol = 3)  # Adjust the layout to fit into 3 columns
      
      combined_plot
    }
  })
  
  # Summary statistics when selected
  output$summaryStats <- renderTable({
    data %>%
      summarise(
        `Average Salary (USD)` = mean(salary_in_usd, na.rm = TRUE),
        `Median Salary (USD)` = median(salary_in_usd, na.rm = TRUE),
        `Min Salary (USD)` = min(salary_in_usd, na.rm = TRUE),
        `Max Salary (USD)` = max(salary_in_usd, na.rm = TRUE),
        `Total Job Entries` = n()
      )
  })
  
  # Reactive expression to filter the data based on inputs
  filtered_data <- reactive({
    df <- data
    
    # Filter by year
    df <- df %>% filter(work_year == input$year_filter)
    
    return(df)
  })
  
  # Output based on selected tab for Work Nature Trends
  output$work_nature_output <- renderUI({
    switch(input$selected_trend_tab,  # Use the selected tab ID to display the correct plot
           "Salary by Experience Level" = plotOutput("salary_by_experience_plot"),
           "Salary by Employment Type" = plotOutput("salary_by_employment_type_plot"),
           "Salary by Work Delivery Type" = plotOutput("salary_by_remote_ratio_plot"),
           "Salary by Remote Ratio and Employment Type" = plotOutput("salary_by_remote_and_employment_plot"),
           "Salary by Remote Ratio and Experience Level" = plotOutput("salary_by_remote_and_experience_plot"),
           "Salary by Remote Ratio and Experience Level (US)" = plotOutput("salary_by_remote_and_experience_us_plot")
    )
  })
  
  # Plot for Salary by Experience Level
  output$salary_by_experience_plot <- renderPlot({
    salary_by_experience <- data %>%
      group_by(experience_level) %>%
      summarise(avg_salary_usd = mean(salary_in_usd, na.rm = TRUE)) %>%
      arrange(desc(avg_salary_usd))
    
    ggplot(salary_by_experience, aes(x = experience_level, y = avg_salary_usd, fill = experience_level)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Salary by Experience Level in Data Science",
           x = "Experience Level",
           y = "Average Salary (USD)")
  })
  
  # Plot for Salary by Employment Type
  output$salary_by_employment_type_plot <- renderPlot({
    salary_by_employment_type <- data %>%
      group_by(employment_type) %>%
      summarise(avg_salary_usd = mean(salary_in_usd, na.rm = TRUE)) %>%
      arrange(desc(avg_salary_usd))
    
    ggplot(salary_by_employment_type, aes(x = employment_type, y = avg_salary_usd, fill = employment_type)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Salary by Employment Type in Data Science",
           x = "Employment Type",
           y = "Average Salary (USD)")
  })
  
  # Plot for Salary by Remote Ratio
  output$salary_by_remote_ratio_plot <- renderPlot({
    salary_by_remote_ratio <- data %>%
      group_by(remote_ratio) %>%
      summarise(avg_salary_usd = median(salary_in_usd, na.rm = TRUE)) %>%
      arrange(desc(avg_salary_usd))
    
    ggplot(salary_by_remote_ratio, aes(x = factor(remote_ratio), y = avg_salary_usd, fill = factor(remote_ratio))) +
      geom_bar(stat = "identity") +
      labs(title = "Average Salary by Work Delivery Type (Remote vs In-Person)",
           x = "Remote Ratio",
           y = "Average Salary (USD)")
  })
  
  # Plot for Salary by Remote Ratio and Employment Type
  output$salary_by_remote_and_employment_plot <- renderPlot({
    salary_by_remote_and_employment <- data %>%
      group_by(remote_ratio, employment_type) %>%
      summarise(
        avg_salary_usd = mean(salary_in_usd, na.rm = TRUE),
        employee_count = n()
      )
    
    ggplot(salary_by_remote_and_employment, aes(x = factor(remote_ratio), y = avg_salary_usd, fill = employment_type)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = employee_count), vjust = -0.5, position = position_dodge(0.9)) +  # Add employee count labels
      labs(title = "Average Salary by Remote Ratio and Employment Type",
           x = "Remote Ratio",
           y = "Average Salary (USD)",
           fill = "Employment Type") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)
  })
  
  # Plot for Salary by Remote Ratio and Experience Level
  output$salary_by_remote_and_experience_plot <- renderPlot({
    salary_by_remote_and_experience <- data %>%
      group_by(remote_ratio, experience_level) %>%
      summarise(
        avg_salary_usd = mean(salary_in_usd, na.rm = TRUE),
        employee_count = n()
      )
    
    ggplot(salary_by_remote_and_experience, aes(x = factor(remote_ratio), y = avg_salary_usd, fill = experience_level)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = employee_count), vjust = -0.5, position = position_dodge(0.9)) +  # Add employee count labels
      labs(title = "Average Salary by Remote Ratio and Experience Level",
           x = "Remote Ratio",
           y = "Average Salary (USD)",
           fill = "Experience Level") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)
  })
  
  # Plot for Salary by Remote Ratio and Experience Level (US)
  output$salary_by_remote_and_experience_us_plot <- renderPlot({  # Unique ID for US plot
    salary_by_remote_and_experience_us <- data %>%
      filter(company_location == "US") %>%  # Filter to show only US data
      group_by(remote_ratio, experience_level) %>%
      summarise(
        avg_salary_usd = mean(salary_in_usd, na.rm = TRUE),
        employee_count = n()
      )
    
    ggplot(salary_by_remote_and_experience_us, aes(x = factor(remote_ratio), y = avg_salary_usd, fill = experience_level)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_text(aes(label = employee_count), vjust = -0.5, position = position_dodge(0.9)) +  # Add count labels
      labs(title = "Average Salary by Remote Ratio and Experience Level (US)",
           x = "Remote Ratio",
           y = "Average Salary (USD)",
           fill = "Experience Level") +
      theme_minimal() +
      scale_y_continuous(labels = scales::comma)
  })
  
  # Step 1: Data Preparation for Chronological Job Trends
  output$jobTrendPlot <- renderPlot({
    job_trend <- data %>%
      group_by(work_year) %>%
      summarise(job_count = n())
    
    ggplot(job_trend, aes(x = work_year, y = job_count)) +
      geom_line(color = "blue") +
      geom_point() +
      labs(title = "Chronological Changes in Data Science Job Postings",
           x = "Year",
           y = "Number of Job Postings") +
      theme_minimal()
  })
  
  # Step 2: Data Preparation for job postings increase from 2022 to 2023
  job_trend_by_title <- data %>%
    group_by(work_year, job_title) %>%
    summarise(job_count = n()) %>%
    filter(work_year %in% c(2022, 2023, 2024))  # Focus on 2022, 2023, and 2024
  
  # Step 3: Spread the data to calculate differences
  job_trend_wide <- job_trend_by_title %>%
    spread(work_year, job_count, fill = 0) %>%
    rename(job_count_2022 = `2022`, job_count_2023 = `2023`, job_count_2024 = `2024`)
  
  # Step 4: Calculate the increase from 2022 to 2023
  job_trend_wide <- job_trend_wide %>%
    mutate(increase_2023 = job_count_2023 - job_count_2022) %>%
    filter(increase_2023 > 0)  # Focus on jobs that increased
  
  # Output: Top 10 job titles with highest increase from 2022 to 2023
  output$top_increase_jobs_plot <- renderPlot({
    top_increase_jobs_2023 <- job_trend_wide %>%
      arrange(desc(increase_2023)) %>%
      head(10)  # Top 10 jobs with highest increase
    
    ggplot(top_increase_jobs_2023, aes(x = reorder(job_title, -increase_2023), y = increase_2023)) +
      geom_bar(stat = "identity", fill = "green") +
      coord_flip() +
      labs(title = "Top 10 Job Titles with Highest Increase in Job Postings (2022 vs 2023)",
           x = "Job Title",
           y = "Increase in Job Postings") +
      theme_minimal()
  })
  
  # Step 5: Calculate the change in 2024 for these top jobs from 2023
  job_trend_wide <- job_trend_wide %>%
    mutate(change_2024 = job_count_2024 - job_count_2023)
  
  # Output: Visualize the change in 2024 for the top jobs from 2023
  output$change_2024_jobs_plot <- renderPlot({
    ggplot(job_trend_wide %>% filter(job_title %in% top_increase_jobs_2023$job_title),
           aes(x = reorder(job_title, -change_2024), y = change_2024, fill = change_2024 > 0)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("red", "green"), labels = c("Decrease", "Increase")) +
      coord_flip() +
      labs(title = "Change in Job Postings for Top 2023 Jobs (2023 vs 2024)",
           x = "Job Title",
           y = "Change in Job Postings",
           fill = "Change Status") +
      theme_minimal()
  })
  
  #New plot for comparison of job postings between 2023 and 2024
  output$jobComparisonPlot <- renderPlot({
    # Filter the data to only include 2023 and 2024
    job_trend_long <- job_trend_wide %>%
      filter(job_title %in% top_increase_jobs_2023$job_title) %>%
      gather(key = "year", value = "job_count", job_count_2023, job_count_2024)  # Only keep 2023 and 2024
    
    # Create the combined plot
    ggplot(job_trend_long, aes(x = reorder(job_title, -job_count), y = job_count, fill = year)) +
      geom_bar(stat = "identity", position = "dodge", aes(fill = year)) +
      geom_text(aes(label = job_count), position = position_dodge(0.9), vjust = -0.3, size = 3.5) +
      scale_fill_manual(values = c("job_count_2023" = "green", "job_count_2024" = "red"),
                        labels = c("2023", "2024")) +
      coord_flip() +
      labs(title = "Comparison of Job Postings for Top 10 Jobs (2023 vs 2024)",
           subtitle = "Green: Increase in 2023 | Red: Decrease in 2024",
           x = "Job Title",
           y = "Number of Job Postings",
           fill = "Year") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5),  # Center the title
            plot.subtitle = element_text(hjust = 0.5)) 
  })
  
  # Plot for Top 8 Regions by Average Salary
  output$top_regions_salary_plot <- renderPlot({
    regional_salaries_by_company <- data %>%
      group_by(company_location) %>%
      summarise(avg_salary_usd = mean(salary_in_usd, na.rm = TRUE)) %>%
      arrange(desc(avg_salary_usd)) %>%
      head(10)
    
    ggplot(regional_salaries_by_company, aes(x = reorder(company_location, -avg_salary_usd), y = avg_salary_usd)) +
      geom_bar(stat = "identity", fill = "blue") +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(title = "Top 8 Regions with Highest Average Salaries for Data Science Jobs",
           x = "Company Location",
           y = "Average Salary (USD)")
  })
  
  # Plot for Top Jobs by Employee Count in Each Region
  output$top_jobs_per_region_plot <- renderPlot({
    top_5_data <- data %>%
      filter(company_location %in% top_8_regions)
    
    top_jobs_by_region <- top_5_data %>%
      group_by(company_location, job_title) %>%
      summarise(employee_count = n()) %>%
      arrange(company_location, desc(employee_count))
    
    top_5_jobs_per_region <- top_jobs_by_region %>%
      group_by(company_location) %>%
      slice_max(employee_count, n = 5)
    
    ggplot(top_5_jobs_per_region, aes(x = reorder(job_title, -employee_count), y = employee_count, fill = company_location)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ company_location, scales = "free") +
      coord_flip() +
      labs(title = "Top 5 Jobs with Highest Employees in Each Top 8 Region",
           x = "Job Title",
           y = "Employee Count")
  })
  
  # Plot for Top Jobs by Year and Region (2023 & 2024)
  output$top_jobs_per_region_year_plot <- renderPlot({
    data_2023_2024 <- data %>%
      filter(work_year %in% c(2023, 2024))
    
    top_5_data_2023_2024 <- data_2023_2024 %>%
      filter(company_location %in% top_8_regions)
    
    top_jobs_by_region_year <- top_5_data_2023_2024 %>%
      group_by(company_location, job_title, work_year) %>%
      summarise(employee_count = n()) %>%
      arrange(company_location, work_year, desc(employee_count))
    
    top_5_jobs_per_region_year <- top_jobs_by_region_year %>%
      group_by(company_location, work_year) %>%
      slice_max(employee_count, n = 5)
    
    ggplot(top_5_jobs_per_region_year, aes(x = reorder(job_title, -employee_count), y = employee_count, fill = company_location)) +
      geom_bar(stat = "identity") +
      facet_grid(work_year ~ company_location, scales = "free") +
      coord_flip() +
      labs(title = "Top 5 Jobs with Highest Employees in Each Top 8 Region (2023 & 2024)",
           x = "Job Title",
           y = "Employee Count")
  })
  
  # Plot for US and CA Top Jobs by Salary
  output$us_ca_jobs_plot <- renderPlot({
    us_ca_data <- data %>%
      filter(work_year %in% c(2023, 2024)) %>%
      filter(company_location %in% c("US", "CA"))
    
    top_jobs_by_employee_count <- us_ca_data %>%
      group_by(company_location, job_title, work_year) %>%
      summarise(employee_count = n(), avg_salary_usd = mean(salary_in_usd, na.rm = TRUE)) %>%
      arrange(company_location, work_year, desc(employee_count))
    
    top_5_jobs_per_region <- top_jobs_by_employee_count %>%
      group_by(company_location, work_year) %>%
      slice_max(employee_count, n = 5)
    
    ggplot(top_5_jobs_per_region, aes(x = reorder(job_title, -avg_salary_usd), y = avg_salary_usd, fill = company_location)) +
      geom_bar(stat = "identity") +
      facet_grid(work_year ~ company_location, scales = "free") +
      coord_flip() +
      labs(title = "Top 5 Jobs in US and CA with Highest Average Salaries (2023 & 2024)",
           x = "Job Title",
           y = "Average Salary (USD)")
  })
  # Data for top 10 job roles by average salary
  output$top10JobRolesSalaryPlot <- renderPlot({
    job_role_summary <- data %>%
      group_by(job_title) %>%
      summarise(average_salary = mean(salary_in_usd, na.rm = TRUE)) %>%
      arrange(desc(average_salary))
    
    top_job_roles <- head(job_role_summary, 10)
    
    ggplot(top_job_roles, aes(x = reorder(job_title, average_salary), y = average_salary)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() + 
      scale_y_continuous(labels = scales::comma) +
      labs(title = "Top 10 Job Roles by Average Salary in Data Science", 
           x = "Job Role", 
           y = "Average Salary (USD)") +
      theme_minimal()
  })
  
  standardize_job_title <- function(job_title) {
    job_title <- tolower(job_title)  # Convert to lowercase
    if (grepl("data scientist|data science", job_title)) {
      return("Data Scientist")
    } else if (grepl("machine learning|ml", job_title)) {
      return("Machine Learning Engineer")
    } else if (grepl("data engineer|etl|cloud engineer", job_title)) {
      return("Data Engineer")
    } else if (grepl("business intelligence|bi", job_title)) {
      return("Business Intelligence")
    } else if (grepl("ai|artificial intelligence", job_title)) {
      return("AI Engineer")
    } else if (grepl("data analyst|analytics", job_title)) {
      return("Data Analyst")
    } else {
      return("Other")
    }
  }
  
  # Apply the standardization function to the job titles
  data$standardized_job_title <- sapply(data$job_title, standardize_job_title)
  
  # Group by standardized job title, calculate the average salary in USD, and count the number of occurrences of each job title
  job_analysis <- data %>%
    group_by(standardized_job_title) %>%
    summarise(avg_salary_usd = mean(salary_in_usd, na.rm = TRUE),
              ncount = n()) %>%
    arrange(desc(ncount))
  
  # Render the new plot in Sub-tab 2
  output$jobOpportunitiesPlot <- renderPlot({
    ggplot(job_analysis, aes(x = ncount, y = reorder(standardized_job_title, ncount))) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = paste("Avg Salary: $", round(avg_salary_usd, 0))), 
                hjust = ifelse(job_analysis$ncount < 1000, -0.1, 1.1),  # Ensure correct reference
                color = "black", size = 4) +  # Adjust text size and hjust based on ncount
      labs(title = "Job Roles with Highest Opportunities and Average Salary (USD)", 
           x = "Number of Job Opportunities", y = "Job Role") +
      xlim(0, max(job_analysis$ncount) * 1.1) +  # Add some padding to the x-axis
      theme_minimal()
  })
  
  # Data for experience level and company size for the top 5 job roles
  output$jobRequirementsPlot <- renderPlot({
    job_requirements_by_company_size <- data %>%
      group_by(standardized_job_title, experience_level, company_size) %>%
      summarise(job_count = n()) %>%
      arrange(desc(job_count))
    
    # Plot the experience level and company size for each standardized job role
    ggplot(job_requirements_by_company_size, aes(x = experience_level, y = job_count, fill = company_size)) +
      geom_bar(stat = "identity", position = "dodge") +
      facet_wrap(~ standardized_job_title) +
      labs(title = "Experience Level and Company Size for Standardized Job Roles",
           x = "Experience Level",
           y = "Number of Job Opportunities",
           fill = "Company Size") +
      theme_minimal()
  })
  
  # Remote Work Opportunities by Region Plot
  output$remoteWorkPlot <- renderPlot({
    remote_work_analysis <- data %>%
      group_by(employee_residence, remote_ratio) %>%
      summarise(job_count = n()) %>%
      arrange(desc(job_count))
    
    ggplot(remote_work_analysis, aes(x = reorder(employee_residence, job_count), y = job_count)) +
      geom_point(aes(color = as.factor(remote_ratio)), size = 3) +
      geom_segment(aes(x = reorder(employee_residence, job_count), xend = reorder(employee_residence, job_count), 
                       y = 0, yend = job_count, color = as.factor(remote_ratio)), size = 1) +
      coord_flip() +
      facet_wrap(~remote_ratio, scales = "free_x") +
      labs(title = "Remote Work Opportunities by Region and Remote Ratio",
           x = "Region",
           y = "Number of Jobs",
           color = "Remote Ratio") +
      theme_minimal() 
      
  })
  
  # Average Salaries by Company Location Plot
  output$salaryLocationPlot <- renderPlot({
    regional_salaries_by_company <- data %>%
      group_by(company_location) %>%
      summarise(avg_salary_usd = mean(salary_in_usd, na.rm = TRUE)) %>%
      arrange(desc(avg_salary_usd))
    
    ggplot(regional_salaries_by_company, aes(x = reorder(company_location, -avg_salary_usd), y = avg_salary_usd)) +
      geom_bar(stat = "identity", fill = "blue") +
      coord_flip() + 
      scale_y_continuous(labels = comma) +
      labs(title = "Average Salaries for Data Science Jobs by Company Location",
           x = "Company Location",
           y = "Average Salary (USD)") + 
      theme_minimal() 
      
  })
  
  # Company Location Concentration Map Plot
  output$companyLocationMap <- renderPlot({
    ggplot(map_data_with_companies, aes(long, lat, group = group)) +
      geom_polygon(aes(fill = count), color = "white") +  # Fill the regions based on company counts
      scale_fill_viridis_c(option = "C", na.value = "gray90", name = "Company Count") +  # Use viridis color scale
      labs(title = "Concentration of Company Locations Worldwide",
           x = "Longitude",
           y = "Latitude") +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "lightblue"),  # Set ocean color
            panel.grid = element_blank())
  })
  
  # Salary Distribution Plot
  output$salaryDistributionPlot <- renderPlot({
    ggplot(data, aes(x = salary_in_usd)) +
      geom_histogram(binwidth = 10000, fill = "lightblue", color = "black") +
      labs(title = "Salary Distribution in the Dataset",
           x = "Salary (USD)",
           y = "Frequency") + 
      scale_x_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  # Clustering analysis and plots for US data
 # Reactive expression for filtered data
filtered_data <- reactive({
  data %>%
    filter(company_location == "US") %>%
    select(salary_in_usd, experience_level, company_size, remote_ratio, job_title) %>%
    mutate(
      experience_level = as.numeric(as.factor(experience_level)),
      company_size = as.numeric(as.factor(company_size))
    )
})

# Render the clustering plot
output$clusterPlot <- renderPlot({
  # Access the reactive filtered_data()
  filtered_data_val <- filtered_data()
  
  # Check if filtered_data is non-empty
  if (nrow(filtered_data_val) == 0) {
    stop("No data available for clustering")
  }
  
  # Normalize the filtered data
  preprocess_params_us <- preProcess(filtered_data_val[, c("salary_in_usd", "experience_level", "company_size", "remote_ratio")], method = c("center", "scale"))
  filtered_data_scaled <- predict(preprocess_params_us, filtered_data_val[, c("salary_in_usd", "experience_level", "company_size", "remote_ratio")])
  
  # Perform k-means clustering
  set.seed(123)
  kmeans_model <- kmeans(filtered_data_scaled, centers = 4, nstart = 10)
  
  # Add the cluster labels to the original unscaled data
  filtered_data_val$cluster <- as.factor(kmeans_model$cluster)
  
  # Relabel clusters based on characteristics (salary, experience level, job titles)
  filtered_data_val <- filtered_data_val %>%
    mutate(cluster_label = recode(cluster,  # Ensure recoding is happening on the correct variable
                                  '1' = "Mid-Level Roles",
                                  '2' = "Entry-Level Roles",
                                  '3' = "Senior and Executive Roles",
                                  '4' = "High-Paying Executive Roles"))
  
  # Check and print whether the cluster_label column has been created
  if ("cluster_label" %in% names(filtered_data_val)) {
    print("cluster_label column created successfully")
  } else {
    print("Error: `cluster_label` column not found. Check mutation and recode.")
    stop("Error: `cluster_label` column not found. Ensure it is created properly.")
  }
  

  # Plot the clusters using the unscaled salary values
  ggplot(filtered_data_val, aes(x = salary_in_usd, y = experience_level, color = cluster_label)) +
    geom_point() +
    labs(title = "K-means Clustering with 4 Clusters (US Data)",
         x = "Salary (USD)",
         y = "Experience Level",
         color = "Cluster Label") + 
    scale_x_continuous(labels = scales::comma) +  # Format salary with commas
    theme_minimal()
})

# Render the top jobs by cluster plot
output$topJobsByClusterPlot <- renderPlot({
  filtered_data_val <- filtered_data()
  
  # Check if filtered_data is non-empty
  if (nrow(filtered_data_val) == 0) {
    stop("No data available for clustering")
  }
  
  # Normalize the filtered data
  preprocess_params_us <- preProcess(filtered_data_val[, c("salary_in_usd", "experience_level", "company_size", "remote_ratio")], method = c("center", "scale"))
  filtered_data_scaled <- predict(preprocess_params_us, filtered_data_val[, c("salary_in_usd", "experience_level", "company_size", "remote_ratio")])
  
  # Perform k-means clustering
  set.seed(123)
  kmeans_model <- kmeans(filtered_data_scaled, centers = 4, nstart = 10)
  
  # Add the cluster labels to the original unscaled data
  filtered_data_val$cluster <- as.factor(kmeans_model$cluster)
  
  # Relabel clusters based on characteristics (salary, experience level, job titles)
  filtered_data_val <- filtered_data_val %>%
    mutate(cluster_label = recode(cluster,  # Ensure recoding is happening on the correct variable
                                  '1' = "Mid-Level Roles",
                                  '2' = "Entry-Level Roles",
                                  '3' = "Senior and Executive Roles",
                                  '4' = "High-Paying Executive Roles"))
  
  # Check and print whether the cluster_label column has been created
  if ("cluster_label" %in% names(filtered_data_val)) {
    print("cluster_label column created successfully")
  } else {
    print("Error: `cluster_label` column not found. Check mutation and recode.")
    stop("Error: `cluster_label` column not found. Ensure it is created properly.")
  }
  
  # Group by cluster and job title with new cluster labels
  job_title_by_cluster_with_labels <- filtered_data_val %>%
    group_by(cluster_label, job_title) %>%
    summarise(job_count = n()) %>%
    arrange(desc(job_count))

  # Select the top 3 job roles for each cluster
  top_3_jobs_by_cluster <- job_title_by_cluster_with_labels %>%
    group_by(cluster_label) %>%
    slice_max(order_by = job_count, n = 3)

  # Plot the top 3 job roles for each cluster
  ggplot(top_3_jobs_by_cluster, aes(x = reorder(job_title, job_count), y = job_count, fill = cluster_label)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    facet_wrap(~cluster_label, scales = "free_y") +
    labs(title = "Top 3 Job Roles in US for Each Cluster",
         x = "Job Title",
         y = "Job Count") +
    theme_minimal()
})

  
}

# Run the application
shinyApp(ui = ui, server = server)

           
           