# Author: UW Population Health Initiative
# Purpose: This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Date Last Updated: February 06 2023

# Load packages
library(vctrs)
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(formattable)
library(tidyr)
library(leaflet)
library(plotly)
library(readxl)
library(shinycssloaders)
library(shinyWidgets)
library(shinyhelper)
library(rintrojs)
library(shinyBS)
library(here)
library(shinyjs)
library(tools)
library(ggplot2)

# load datasets that are used throughout the dashboard
vaccine_trends <- readRDS("data/aim_1/New/01_vaccine_trends.RDS")
sdi <- readRDS("data/aim_1/New/02_sdi.RDS")
sdi$sdi[sdi$year_id == '2020'] <- NA

disease_trends <- readRDS("data/aim_1/New/05_disease_trends.RDS")
codebook <- read.csv("data/aim_2/vaccine_index_variable_codebook_for_website.csv")

# data on disease burdens
merged_data_for_visuals <- readRDS("data/aim_1/New/06_merged_data_for_visuals.RDS")
merged_data_for_visuals$sdi[merged_data_for_visuals$year_id == '2020'] <- NA

vaccine_preventable_diseases <- read_excel("data/aim_1/vaccine_preventable_diseases.xlsx")

#available disease data for cause name
merged_data_for_vac_dis <- dplyr::left_join(vaccine_preventable_diseases,disease_trends, "cause_name", "cause_name")

# available vaccine data with description and cause name
preventable_vac_trend <- vaccine_trends

# Load the index results
index_results <- readRDS("data/aim_2/19_index_results_third_version.RDS") #from 11/03/2022
index_results$sdi[index_results$year == '2020'] <- NA

# merge untransformed data with index results
untransformed_data <- readRDS("data/17_merged_dataset_third_version.RDS")

# label table to use when trying to present more details on data
label_table <- read_xlsx("data/aim_1/label_table.xlsx")

sdi_dup <- sdi
colnames(sdi_dup)[2] <- "location"
colnames(sdi_dup)[4] <- "year"

# drop certain rows from sdi_dup since "Georgia" is duplicated
sdi_dup <- sdi_dup %>% filter(level=="3")

merged_data_for_vacii_sdi <- dplyr::left_join(index_results,sdi_dup[,-c("sdi")], by=c("location","year"))
#add iso_code for vaccination coverage map
merged_vaccine_trends <- merge(vaccine_trends, codebook[-1, c("location", "iso_code")], by.x = "location_name", by.y = "location")

# ui portion of R shiny app -----------------------------------------------

body <- navbarPage(
  
  # Creates a head container #####
  tags$head(),

  # Necessary for JavaScript in R Shiny ####
  # introjsUI(),
  
  # website title #####
  # title = div(img(src="/assets/images/UW_PopulationHealthInit_print_wt.png", width = "250px", height ="29px")
  #             # ,
  #             # toupper("VIP Index Project")
  #             ),
  
  # Visualizations Tab #####
  tabPanel("Visualizations",
           
           # This creates the sidebar panel #####
           sidebarPanel(h4("Vaccination Improvement Potential (VIP) Index Ranking Table"),
                        
                        # slider selector for year input
                        sliderInput("year", "Year", value = 2019, min = 1990, max = 2019, step=1, sep = "", animate = TRUE),
             
                        
                        fluidRow(
                          # This is the buttons which select the SDI Group Category
                          column(8, radioButtons("sdi_group_present", "2019 SDI Group", choices = c("All"="all", "Low" ="low", "Medium" = "medium", "High" = "high"), inline = TRUE)),
                                 
                                 
                          # Information box: explains what the SDI is
                          column(2, dropMenu(circleButton(label = "What is SDI?","What is SDI?", inputId='sdiinfo',icon = icon('info')),
                                  h4(strong('SDI')),
                                  h5('The Socio-demographic Index (SDI) is a summary measure that identifies where countries sit on the spectrum of development.'),
                                  placement = "bottom", arrow = TRUE),
                                  style='text-align:center',
                                  class = 'rightAlign')),
             
                        # Region drop down: so user can select one of several GBD Regions 
                        selectInput("region_table", "Regions", selected = NULL, choices = c("All",unique(index_results$region))),
                        
                        # This is the html formatting for the table output
                        tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #92c9e8 !important;}')), 
                        
                        # this is the table which shows all of the countries and their Index result
                        DT::dataTableOutput("table")
                        
                        ), # end of the sidebar panel
           
           # This is the main section of the website to the right of the sidebar panel consisting of five tabs #####
           mainPanel(
             
             # # This is formatting for the overall main panel of the dashboard #####
             # div(class="outer", 
             #   
             #   # Creates a panel whose contents are absolutely positioned #####
             #   absolutePanel(
             #     fixed=FALSE,
             #     width = "100%",
             #     draggable = FALSE,
             #     height = "100%",
             #     tags$style(
             #       type="text/css",
             #       ".shiny-output-error { visibility: hidden; }",
             #       ".shiny-output-error:before { visibility: hidden; }"),
                 
                 # this is the container for the five tabs #####
                 tabsetPanel(id = "t1",
                             
                             # This is the first panel that plots the index results and components #####
                             tabPanel("Vaccination Improvement",
                                      value="t_sdi",
                                      fluidRow(column(6, " ", style='padding:10px;')), 
                                      fluidRow(column(width = 9,
                                                      # This list the year that is selected on the left sidebar and determines what is plotted
                                                      h4(strong(textOutput("yeartitle")),style='text-align:left')),
                                               column(width = 3,
                                                      # This button allows the user to switch between visualizing the Index Result and the Index Components
                                                      div(switchInput(inputId = "view", onLabel = "Show Map", offLabel = "Show Table", value = FALSE, labelWidth = "50px")))),
                                                            
                                      # This is the top section of the tab containing the different selection options               
                                      conditionalPanel("!input.view", 
                                                       fluidRow(column(12, " ", style='padding:3px;')), 
                                                       
                                                       # This is the selection button to choose between Results and Components
                                                       fluidRow(column(10, radioButtons(inputId = "show", label = NULL,
                                                                                        choices = c("Plot VIP Index Results","Plot Index Components"),
                                                                                        selected = "Plot VIP Index Results", inline = TRUE)), 
                                                                # THis is the Information button explaining what the VIP Index is
                                                                column(2,dropMenu(circleButton(label = "", inputId = 'info', icon = icon('info')),
                                                                                h4(strong('Index Mapper')),
                                                                                h5('The Vaccine Improvement Potential (VIP) Index
                                                                                   assesses the potential of a country to improve its immunization rates.'),
                                                                                h5("A higher number (or darker shading on the map) indicates a better score."),
                                                                                placement = "bottom",
                                                                                arrow = TRUE))),
                                                       
                                                       # This is a section break I believe
                                                       fluidRow(column(12, " ", style='padding:2px')),
                                                       
                                                       # This is the FIRST section that appears depending on whether Index Result or Components are plotted
                                                       conditionalPanel("input.show == 'Plot VIP Index Results'",
                                                                        
                                                                        fluidRow(column(12, " ", style='padding:4px;')),
                                                                        
                                                                        # This section shows the index results map in the top half of the page
                                                                        fluidRow(column(12,
                                                                                        plotlyOutput("index_map", height = "43vh")), column(1,"")),
                                                                        fluidRow(column(width = 12, 
                                                                                        "Select locations in left VIP Index Ranking Table for comparison",
                                                                                        style='font-family:Avenir, Helvetica;font-size:30;text-align:center')),
                                                                        # This is a section break I believe
                                                                        fluidRow(column(10,''), 
                                                                                 column(2, dropMenu(circleButton(label = "", inputId='info2', icon = icon('info')),
                                                                                                    h4(strong('Time Series of Vaccination Improvement Potential (VIP) Index')),
                                                                                                    h5('The index value is estimated yearly between 1990 and 2019, in order to track improvement or change over time. '),
                                                                                                    placement = "bottom", arrow = TRUE))),
                                                                        
                                                                        # This is the trendline that appears on the bottom half of the page
                                                                        fluidRow(column(12, 
                                                                                        plotlyOutput("index_trend_plot_multi", height = "43vh"))), 
                                                                        column(1,"", style='padding: 1em 0;')), 
                                                       
                                                       # This is a section break I believe
                                                       fluidRow(column(12, " ", style='padding:2px;')),
                                                                     
                                                       # This is the SECOND tab that appears depending on whether Index Result or Components are selected
                                                       conditionalPanel("input.show == 'Plot Index Components'",
                                                                        fluidRow(column(5, 
                                                                                        selectInput("indicators", "Indicator:",
                                                                                                    choices=c("Socio-demographic Index",
                                                                                                              "Total Health Spending",
                                                                                                              "Government Health Spending",
                                                                                                              "Development Assistance for Health",
                                                                                                              "Healthcare Access and Quality Index",
                                                                                                              "Corruption Perception Index",
                                                                                                              "Skilled Attendants at Birth",
                                                                                                              "Immigrant Population",
                                                                                                              "Urbanicity",
                                                                                                              "Agree Vaccines are Safe",
                                                                                                              "Agree Vaccines are Important", 
                                                                                                              "Agree Vaccines are Effective",
                                                                                                              "Trust in Government"), width = "515px"))),
                                                                        
                                                                        # This section shows the indicator map on the top half of the page              
                                                                        fluidRow(column(11, plotlyOutput("new_indicator_map",height = "43vh")), column(1,"")),
                                                                        
                                                                        column(width = 12, "Select location in left VIP Index Ranking Table for comparison",
                                                                               style='font-family:Avenir, Helvetica;font-size:30;text-align:center;padding:2px; padding-bottom: 20px'),
                                                                               
                                                                        # This plots the time series plot of the components on the bottom half of the page
                                                                        fluidRow(column(11, plotlyOutput("indicator_trend_plot_multi", height = "27vh")), 
                                                                                 column(1,"")))
                                                       ),
                                                                             
                                                    
                                      # This is the table that shows the component values that are used in calculating the index
                                      conditionalPanel("input.view",
                                                       fluidRow(column(11,
                                                                       DT::dataTableOutput("indextable")), column(1,"")))
                                      ),
                             
                             # This is the second tab which shows the vaccination trends #####
                             tabPanel("Vaccination Trends", 
                                      value = "t_vac",
                                      fluidRow(column(width = 11,
                                                      h4(strong(htmlOutput("content_vac"))))),
                                      fluidRow(column(width = 10, "Select location in left VIP Ranking table", style='font-family:Avenir, Helvetica;font-size:30;text-align:left'),
                                               column(2, dropMenu(circleButton(label = "", inputId='info3',icon = icon('info')), h4(strong('Vaccination Trends')),
                                                                  h5('Recommended vaccinations and the date they began to be used can differ between countries. This visual shows how vaccination coverage has changed over time in a particular location.'),
                                                                  placement = "bottom",
                                                                  arrow = TRUE))),
                                      fluidRow(column(8, " ", style='padding:10px;')),
                                      radioButtons("vaccine_plot","Plot type:", choices = c("Time Series of Vaccine Coverage" ="line_trend","Single Year Vaccine Coverage"="bar_plot", "Vaccine Coverage Map" = "vaccine_map"),inline = TRUE),
                                      
                                      conditionalPanel("input.vaccine_plot == 'vaccine_map'",
                                                       fluidRow(column(5,selectInput("vaccine_drop", "Vaccine:",
                                                                                     choices=c("BCG", "DTP1", "DTP3", "HepB3", "Hib3", "MCV1", "MCV2", "PCV3", "Pol3", "RCV1", "RotaC"), 
                                                                                     width = "200px")))),
                                      
                                      fluidRow(column(11,plotlyOutput("all_vaccine_plot",height = "50vh")),column(1,""))
                                      
                                      ),
                             
                             # This is the third panel which shows the mortality and disability trends #####
                             tabPanel("Mortality & Disability Trends",
                                      value = "d_vac", 
                                      fluidRow(column(width = 11,h4(strong(htmlOutput("content_dis"))))),
                                      fluidRow(column(width = 10, "Select location in left VIP Index Ranking Table",
                                                      style='font-family:Avenir, Helvetica;font-size:30;text-align:left'),
                                               column(2,
                                                      dropMenu(
                                                        circleButton(label = "", inputId='info4',icon = icon('info')),
                                                        h4(strong('Mortality and Disability Trends')),
                                                        h5('This graphic shows how the number of deaths or number of people infected have changed between the years 1990 to 2019. The diseases or disabilities presented on this graph can all be prevented with a vaccine administered at the appropriate time.'),
                                                        placement = "bottom",
                                                        arrow = TRUE))),
                                      radioButtons("disease_estimate","Choose y-axis:", choices = c("Number Value"="number_val","Percent Value" ="percent_val","Rate Value" = "rate_val"),inline = TRUE),
                                      
                                      plotlyOutput("all_disease_plot", height = "35vh"),
                                      fluidRow(column(10, " ", style='padding:30px;'), column(2, dropMenu(
                                                        circleButton(label = "", inputId='info5',icon = icon('info')),
                                                        h4(strong('Times Series of Number of Years Lived with Disability')),
                                                        h5('Can also be considered total years lived with less than ideal health. This measure takes into account prevelance and duration of illnesses.'),
                                                        placement = "bottom",
                                                        arrow = TRUE))),
                                      plotlyOutput("all_disability_plot", height = "35vh")
                                      
                                      ),
                            
                             # This is the fourth panel: on vaccines and corresponding diseases #####                                           
                             tabPanel(paste0("Vaccination & Corresponding Disease Trends"),value="vac_dis_tab",
                                      fluidRow(column(12,h4(strong(htmlOutput("content_vac_dis"))))),
                                      fluidRow(column(10,selectInput("vaccinations", "Vaccination:",choices=NULL)),
                                               column(2,
                                                      dropMenu(
                                                        circleButton(label = "", inputId='info6',icon = icon('info')),
                                                        h4(strong('Vaccination and Disease Trends')),
                                                        h5('This graphic compares vaccination rates and the death rate from the diseases that vaccines help prevent.'),
                                                        placement = "bottom",
                                                        arrow = TRUE))),
                                      fluidRow(column(12,plotlyOutput("selected_vac_dis_plot",height = "40vh"))),
                                      fluidRow(column(3,
                                                      h4("BCG"), helpText("Bacillus Calmette-Guerin"),
                                                      DT::dataTableOutput("BCGtable")),
                                               column(3,
                                                      h4("DTP1 & DTP3"),
                                                      helpText("Diphtheria, tetanus, pertussis"),
                                                      DT::dataTableOutput("DTPtable")),
                                               column(2,
                                                      h4("HepB3"),
                                                      helpText("Hepatitis B"),
                                                      DT::dataTableOutput("HepB3table")), 
                                               column(2,
                                                      h4("MCV1 & MCV2"),
                                                      helpText("Measles"),
                                                      DT::dataTableOutput("MCVtable")),
                                               column(1,
                                                      h4("RotaC"),
                                                      helpText("Rotavirus"),
                                                      DT::dataTableOutput("RotaCtable")))
                                      ),
                              
                             
                             # This is the fifth panel: to download the data used in the project #####                                      
                             tabPanel("Data Explorer",
                                      fluidRow(column(5, " ", style='padding:5px;')),
                                      fluidRow(column(9, radioButtons("dataset","Choose Dataset", choices = c("Improvement Index"= "improvement index","Vaccine Trends" = "vaccine trends","Disease Trends" = "disease trends","Data Dictionary" = "data dictionary"),inline = TRUE)),
                                               column(3, style = "margin-top: 10px;",div(downloadButton("download","Download Raw Data"),style='float:right'))),
                                      fluidRow(column(width = 9, "Download custom datasets from the variables that were used to create visuals on the previous tabs. ",
                                                      style='font-family:Avenir, Helvetica;font-size:30;text-align:left')),
                                      fluidRow(column(12,DT::dataTableOutput("alldatatable") %>% withSpinner(color="#4b2e83"))))
                             
                             ) # end of the container
                 # ) # end of the absolute panel
               # ) # end of the division class
             ) # end of the main panel
           ), # end of the visualizations tab
  
  
  # About Tab #####
  tabPanel("About", 
           tags$div(id = "aboutPage",
                   # Background information
                    tags$h4(strong("Background"), id = "aboutHeader"),
                                     
                                     tags$p("Vaccinations are lauded as one of the top public health interventions, 
                                            yet most countries and regions of the world can still greatly improve the rates at which 
                                            they protect their citizens - and particularly children - from vaccine-preventable diseases."),
                                     
                                     tags$p("The factors that drive vaccination coverage are many and can differ between countries, 
                                            so the University of Washington", 
                                            tags$a("Population Health Initiative",
                                                   href = "https://www.washington.edu/populationhealth/",
                                                   class="aboutLinks"), 
                                            'has created a composite measure - 
                                            the "Vaccination Improvement Potential" (VIP) index - that summarizes the relationship between 
                                            increased immunization coverage and a key factors influencing immunization.'),
                                     tags$p("The VIP Index, which is available on this site, organizes data regarding key indicators and
                                            trends into a single index for the first time ever. The comprehensive nature of this index 
                                            allows key stakeholders to make easy comparison between locations and across time to 
                                            better target efforts to improve and/or maintain vaccination coverage levels in a 
                                            country or region."),
                                     tags$p("Please contact the Population Health Initiative via email at", 
                                            tags$a("pophlth@uw.edu", 
                                                   href = "mailto:pophlth@uw.edu",
                                                   class="aboutLinks"),
                                            "with questions regarding this site."),
                                     
                                     # Funder information
                                     tags$h4(strong("Funder"), id = "aboutHeader"),
                                     tags$p("This study was funded by a research grant from the Investigator-Initiated Studies Program of
                                                        Merck Sharp & Dohme Corp (MISP Reference Number 60343).
                                                        The findings and opinions expressed in this report are those
                                                        of the authors and do not necessarily represent those of Merck
                                                        Sharp & Dohme Corp."),
                                     
                                     # Link to final report
                                     tags$h4(strong("Final Report"), id = "aboutHeader"),
                                     tags$p("Download our Final Report", 
                                            downloadLink("finalStudyReport",
                                                         label = "here.",
                                                         class = "aboutLinks")))
                                            
                                            # tags$a("here.",
                                            #        href="wwww/Final _Study_Report_Feb0623.pdf",
                                            #        label = "here.",
                                            #        # download=NA,
                                            #        class = "aboutLinks")))
                                            
                                            # a(href="downloadme.csv", "Download CSV", download=NA, target="_blank")
                                            # downloadLink(final_study_report, 
                                            #              label = "here", 
                                            #              class = "aboutLinks")
                                            # a(href="downloadme.csv", "Download CSV", download=NA, target="_blank")
                                            
                                            # tags$a("here.", 
                                                   # href = "https://www.washington.edu/populationhealth/",
                                                   # class="aboutLinks")))
           ) # end of the About tab
  ) # end of the navigation tab

# server portion of R shiny app -------------------------------------------


server <- function(input, output, session) {
  
  # subsets the sdi data for plotting *I THINK*
  year <- reactive({
    merged_data_for_vacii_sdi[merged_data_for_vacii_sdi$year == input$year,]
  })
  
  # creates a reactive title showing which year was plotted
  output$yeartitle <- renderText({ 
    paste0("Year ", input$year)
  })
  
  # Dynamically sets the slider input with the ...
  observeEvent(year(),{
    choices = sort(unique(merged_data_for_vacii_sdi$year))
    
    updateSliderInput(session,'year', 
                      value=unique(year()$year),
                      min = min(as.numeric(choices)), 
                      max = max(as.numeric(choices)), 
                      step = 1)
  })
  
  # 
  index_year_input <- reactive({
    req(input$index_year_input)
    index_results[index_results$year == input$index_year_input,]
  })
  
  #Vaccination and Disease Trend Tab
  #update select vaccine
  observeEvent(preventable_vac_trend,{
    vacdata = filter(preventable_vac_trend, gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
    disdata = filter(merged_data_for_vac_dis, gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
    merged = dplyr::inner_join(vacdata,disdata,"vaccine_name","vaccine_name")
    choices = sort(unique(merged$vaccine_name))
    updateSelectInput(session,'vaccinations', choices = choices)
  })
  
  dis_data_for_selected_vac <- reactive({
    filter(merged_data_for_vac_dis, merged_data_for_vac_dis$vaccine_name==input$vaccinations)
  })
  
  selected_dis_vac_data <- reactive({
    return(
      list(
        selected_vac_data = filter(preventable_vac_trend, vaccine_trends$vaccine_name==input$vaccinations),
        dis_data_for_selected_vac = filter(merged_data_for_vac_dis, merged_data_for_vac_dis$vaccine_name==input$vaccinations)
      ))
  })
  
  sdi_group_present <- reactive({
    req(input$sdi_group_present)
    if (input$sdi_group_present == "all"){
      all_sdi_group <- year() 
    }
    else{
      filter(year(), sdi_group_present == input$sdi_group_present)
    }
  })
  
  observeEvent(sdi_group_present(),{
    updateSelectInput(session, 'region_table',choices = c("All",unique(sdi_group_present()$region)))
  })
  
  
  sdi_group_present_comp <- reactive({
    req(input$sdi_group_present_comp)
    if (input$sdi_group_present_comp == "all"){
      all_sdi_group <- merged_data_for_vacii_sdi
    }
    else{
      filter(merged_data_for_vacii_sdi, sdi_group_present== input$sdi_group_present_comp)
    }
  })
  
  observeEvent(sdi_group_present_comp(),{
    updateSelectInput(session, 'region', selected = 'Western Sub-Saharan Africa', choices = sdi_group_present_comp()$region)
  })
  
  regionselected <- reactive({
    req(input$region)
    filter(sdi_group_present_comp(),region == input$region)
  })
  
  observeEvent(regionselected(),{
    updateSelectInput(session, 'my_multi', selected = 'Nigeria', choices = regionselected()$location)
  })

  
  # Create a Ranking Table for the sideBar #####
  output$table <- DT::renderDataTable({
    
    # load required inputs
    req(input$year)
    req(input$sdi_group_present)
    req(input$region_table)
    
    # # select the data that will be featured in the ranking table
    sdi_rank_table <- merged_data_for_vacii_sdi
    
    # filter by region if that is modified
    if (input$region_table!="All"){
      sdi_rank_table <- sdi_rank_table %>% filter(region == input$region_table)
    }
    
    # filter by SDI group if that is selected
    if (input$sdi_group_present!="all"){
      sdi_rank_table <- sdi_rank_table %>% filter(sdi_group_present == input$sdi_group_present)
    }
    
    # filter out data by year
    sdi_rank_table <- sdi_rank_table %>% filter(year==input$year)
    
    # subset the data that will be used in the ranking table
    sdi_rank_table <- sdi_rank_table[,c("location","result","sdi_group_present")]
    
    # Create a rank variable
    sdi_rank_table$rank <- NA
    
    # rank the results in the dataset
    sdi_rank_table$rank = dense_rank(desc(sdi_rank_table$result))
    
    # subset the columns that will be visualized
    sdi_rank_table <- sdi_rank_table[,c("rank","location","result","sdi_group_present")]
    
    # round the result to three decimal places
    sdi_rank_table$result = round(sdi_rank_table$result,2)
    
    # Rename the columns that will be visualized
    colnames(sdi_rank_table) <- c('Rank','Location','VIP Index', "2019 SDI Group")
    
    # Re name the title case
    sdi_rank_table$`2019 SDI Group` <- toTitleCase(sdi_rank_table$`2019 SDI Group`)
    
    # Order the countries according to rank
    sdi_rank_table<-sdi_rank_table[order(sdi_rank_table$Rank),]
    
    # Creates a custom formatter for true-false values
    true_false_formatter <-
      formatter("span",
                style = x ~ formattable::style(
                  font.weight = "bold",
                  color = ifelse(x == "High", "forestgreen", ifelse(x == "Low", "red", "black"))
                ))
    
    # format the ranking table for display
    formattable(sdi_rank_table,
                list(
                  # a colored bar with length proportional to value
                  'Vaccination Improvement Index' = color_tile("white", "#569eca"),
        
                  # use custom formatter for TRUE/FALSE values
                  '2019 SDI Group' = true_false_formatter)
                ) %>%
      as.datatable(rownames = FALSE, 
                   selection = list(mode = 'multiple',target="cell", selected = matrix(c(0, 1), nrow = 1,ncol = 2)), 
                   options = list(paging = FALSE,
                                  scrollY = '400px', 
                                  scrollY=TRUE, 
                                  autoWidth = FALSE,
                                  ordering = FALSE, 
                                  pageLength=1000))
  })
  
  # Create a time series plot that shows the index results over time #####
  output$index_trend_plot_multi <- renderPlotly({
    
    # set default countries according to which SDI Groups we want plotted
    if (input$sdi_group_present == 'medium'){
      country = 'Uruguay'
    } else if (input$sdi_group_present == 'low'){
      country = 'Eswatini'
    } else{
      country = 'United States of America'
    }
    
    # filter the data according to the country selected
    index_trend_data <- filter(index_results, location == country)
    
    # Create the figure showing the tred in the data 
    fig_a <- plot_ly(index_trend_data, x = ~year) %>% 
      add_trace(y=~result,type='scatter', name = country, mode = 'lines', line = list(color = 'rgba(49,130,189, 1)',width=2)) %>% 
      layout(autosize = T,
            title = paste0("Time Series of VIP Index"), 
            showlegend = TRUE,
            xaxis = list(title = "Year", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
            yaxis = list(title = "VIP Index", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE, range = c(-0.02, 1)))
    
    fig_a
  })
  
  # Create a time series plotly graph that shows the component values over time #####
  output$new_indicator_map <- renderPlotly({
    
    req(input$year)
    req(input$indicators)
    
    # keep columns of interest
    untf_data_wide <- untransformed_data
    
    untf_data_long <- untf_data_wide %>% 
      pivot_longer(
        cols = !c(
          location, year, gbd_location_id, iso_code, iso_num_code, region, 
          dah_eligible),
        names_to = "variable",
        values_to = "value")
    
    # load labels from label table for graphics
    plot_labels <- label_table %>% select(variable, label, unit)
    
    # merge labels onto the data
    untf_data_long <- merge(untf_data_long, plot_labels, by="variable")

    # filter the data --manually first
    # untf_data_subset <- untf_data_long %>% filter(year == 2018       & label == "Total Health Spending")
    untf_data_subset <- untf_data_long %>% filter(year == input$year & label == input$indicators)
    
    # drop na values
    untf_data_final <- untf_data_subset %>% filter(!is.na(value))

    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      showland = TRUE,
      showcountries = TRUE,
      resolution = 150,
      countrycolor = toRGB("white"),
      landcolor = toRGB("grey85"),
      projection = list(scale=1.2))
    
    l <- list(color = toRGB("white"), width = 0.5)
    
    fig <- plot_ly(data = untf_data_final)
    
    fig <- fig %>% 
      add_trace(
        type="choropleth",
        locations = ~iso_code, 
        z = ~value, 
        color = ~value, 
        colors="Purples", 
        text = ~paste0(location),
        marker = list(line = l)) %>%
      colorbar(
        title = paste0(unique(untf_data_final$unit))) %>% #### Improvement Indicator
      layout(
        autosize = T,
        title = paste0(input$indicators, " Map, in ", input$year),
        mapbox=list(
          style="carto-positron",
          center = list(lon = -90, lat = 80)),
        geo = g)
    
    fig
  })
  
  # Create an map that shows the index results for a given year #####
  output$index_map <- renderPlotly({
    
    req(input$year)
    req(input$region_table)
    req(input$sdi_group_present)
      
    # new_map_data <- index_results %>% filter(year==2019)
    new_map_data <- merged_data_for_vacii_sdi %>% filter(year==input$year)
    
    # filter by region
    if (input$region_table!="All"){
      new_map_data <- new_map_data %>% filter(region == input$region_table)
    }
    
    # filter by sdi group
    if (input$sdi_group_present!="all"){
      new_map_data <- new_map_data %>% filter(sdi_group_present == input$sdi_group_present)
    }
      
    new_map_data$result <- round(new_map_data$result, 2)
    
    new_map_data$hover <- with(new_map_data, 
                                 paste(location, '<br>','<br>',
                                       "Development Assistance Per Total Health Spending Categorical: ",round(dah_per_cap_ppp_mean,2),'<br>',
                                       "Socio-demographic Index: ", round(sdi,2),'<br>',
                                       "Total Health Spending per Person: ", round(the_per_cap_mean,2),'<br>',
                                       "Government Health Spending per Total Health Spending:", round(ghes_per_the_mean,2),'<br>',
                                       "Development Assistance for Health per capita:", round(dah_per_cap_ppp_mean,2),'<br>',
                                       "Healthcare Access and Quality Index: ", round(haqi,2),'<br>',
                                       "Corruption Perception Index:", round(cpi,2),'<br>',
                                       "Skilled Attendants at Birth: ", round(perc_skill_attend,2),'<br>',
                                       "Immigrant Population:", round(imm_pop_perc,2),'<br>',
                                       "Urbanicity:", round(perc_urban,2),
                                       "Agreement Vaccines are Safe", round(mean_agree_vac_safe,2),'<br>',
                                       "Agreement Vaccines are Important", round(mean_agree_vac_important,2),'<br>',
                                       "Agreement Vaccines are Effective: ", round(mean_agree_vac_effective,2), '<br>',
                                       "Trust in Government", round(gov_trust, 2), '<br>')
                                 )
      
      # not sure why these are here if not necessary
      # height <- 1500
      # units <- "px"

      # light grey boundaries
      l <- list(color = toRGB("white"), width = 0.5)

      # specify map projection/options
      g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,showland = TRUE,
        showcountries = TRUE,
        resolution = 150,
        countrycolor = toRGB("white"),
        landcolor = toRGB("grey85"),
        projection = list(scale=1.2))

      fig <- plot_ly(new_map_data)
      fig <- fig %>%
        add_trace(
          z = ~result, 
          color = ~result, 
          type = 'choropleth',
          text = ~hover, 
          locations = ~iso_code, 
          colors="Purples",
          marker = list(line = l))%>%
        colorbar(title = 'VIP Index')%>%
        layout(autosize = T,
               title = paste0("Map of VIP Index in ", input$year),
               mapbox=list(style="carto-positron",
                           center = list(lon = -90, lat = 80)),
               geo = g)
      fig
      })

  # Create the time series plot of the indicator components #####
  output$indicator_trend_plot_multi <- renderPlotly({
    
      if (input$sdi_group_present == 'medium'){
        country <- 'Uruguay'
      } else if (input$sdi_group_present == 'low'){
        country <- 'Eswatini'
      } else {
        country = 'United States of America'
      }

      indicator_trend_data <- filter(index_results, location == country)
      
      fig_a <- plot_ly(indicator_trend_data, x = ~year)

      if (input$indicators == "Socio-demographic Index"){
        titles = paste0("Time Series of SDI")
        ytitles = "Socio-demographic Index"
        fig_a <- fig_a  %>% add_trace(y=~sdi,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      } else if (input$indicators == "Total Health Spending"){
        titles=paste0("Time Series of Total Health Spending per Person")
        ytitles = "Total Health Spending per Person"
        fig_a <- fig_a  %>% add_trace(y=~the_per_cap_mean,type='scatter', name =~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      } else if (input$indicators == "Government Health Spending"){
        titles=paste0("Time Series of Government Health Spending per Total Health Spending")
        ytitles = "Government Health Spending \n per Total Health Spending"
        fig_a <- fig_a  %>% add_trace(y=~ghes_per_the_mean,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      } else if (input$indicators == "Development Assistance for Health"){
        titles=paste0("Time Series of Development Assistance per Person")
        ytitles = "Development Assistance per Person"
        fig_a <- fig_a  %>% add_trace(y=~dah_per_cap_ppp_mean,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      } else if (input$indicators == "Healthcare Access and Quality Index"){
        titles=paste0("Time Series of HAQI")
        ytitles = "HAQI"
        fig_a <- fig_a  %>% add_trace(y=~haqi,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      } else if (input$indicators == "Corruption Perception Index"){
        titles=paste0("Time Series of Corruption Perception Index")
        ytitles = "Corruption Perception Index"
        fig_a <- fig_a  %>% add_trace(y=~cpi,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      } else if (input$indicators == "Skilled Attendants at Birth"){
        titles=paste0("Time Series of Skilled Attendants at Birth")
        ytitles = "Skilled Attendants at Birth"
        fig_a <- fig_a  %>% add_trace(y=~perc_skill_attend,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      } else if (input$indicators == "Immigrant Population"){
        titles=paste0("Time Series of Immigrant Population (%)")
        ytitles = "Immigrant Population (%)"
        fig_a <- fig_a  %>% add_trace(y=~imm_pop_perc,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      } else if(input$indicators == "Urbanicity"){
        titles=paste0("Time Series of Urbanicity (%)")
        ytitles = "Urbanicity (%)"
        fig_a <- fig_a  %>% add_trace(y=~perc_urban,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      } else if(input$indicators == "Agree Vaccines are Safe"){
        titles=paste0("Time Series of Agreement Vaccines are Safe")
        ytitles = "Agree Vaccines are Safe (%)"
        fig_a <- fig_a %>% 
          add_trace(y=~mean_agree_vac_safe, 
                    type='scatter', 
                    name = ~unique(location), 
                    mode = 'lines', 
                    line = list(color = 'rgb(106, 90, 205)', width=2))
      } else if (input$indicators == "Agree Vaccines are Important"){
        titles=paste0("Time Series of Agreement Vaccines are Important")
        ytitles = "Agree Vaccines are Important (%)"
        fig_a <- fig_a %>% 
          add_trace(y=~mean_agree_vac_important, 
                    type='scatter', 
                    name = ~unique(location), 
                    mode = 'lines', 
                    line = list(color = 'rgb(106, 90, 205)', width=2))
      } else if (input$indicators == "Agree Vaccines are Effective"){
        titles=paste0("Time Series of Agreement Vaccines are Effective")
        ytitles = "Agree Vaccines are Effective (%)"
        fig_a <- fig_a %>% add_trace(y=~mean_agree_vac_effective, type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)', width=2))
      } else if(input$indicators == "Trust in Government"){
        titles=paste0("Time Series of Trust in Government")
        ytitles = "Trust in Government"
        fig_a <- fig_a %>% add_trace(y=~gov_trust, type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)', width=2))
      }
      
      fig_a <- fig_a %>%
        layout( autosize = T,
                title = titles,
                showlegend = TRUE,
                xaxis = list(title = "Year", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = ytitles, showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE, range=c(0,1)))
      fig_a
    })

  # Create indexTable which shows the components that are used in calculating the index results
  output$indextable = DT::renderDataTable({
    
    # load the data that will be included in the ranking table
    index_rank_table <- index_results
    
    # filter index results according to year and region being plotted
    # This needs to be updated to automatically update based on the input
    index_rank_table <- index_rank_table %>% filter(year==2019)
    
    # subset columns
    index_rank_table <- index_rank_table[,-c("year","gbd_location_id","iso_code","iso_num_code")]
    
    # create a ranking variable
    index_rank_table$rank <- NA
      
    # assign a rank value to each value in the data
    index_rank_table$rank = dense_rank(desc(index_rank_table$result))
      
    # not sure if this is necessary yet
    #index_rank_table <- index_rank_table[,c("rank","location_name","sdi","sdi_group_present")]
      
    index_rank_table<-index_rank_table[order(index_rank_table$rank),]
    
    # round the variables in each of the values
    index_rank_table$result <- round(index_rank_table$result, 2)
    index_rank_table$the_per_cap_mean <- round(index_rank_table$the_per_cap_mean, 2)
    index_rank_table$ghes_per_the_mean <- round(index_rank_table$ghes_per_the_mean, 2)
    index_rank_table$dah_per_cap_ppp_mean <- round(index_rank_table$dah_per_cap_ppp_mean, 2)
    index_rank_table$haqi <- round(index_rank_table$haqi, 2)
    index_rank_table$cpi <- round(index_rank_table$cpi, 2)
    index_rank_table$perc_skill_attend <- round(index_rank_table$perc_skill_attend, 2)
    index_rank_table$imm_pop_perc <- round(index_rank_table$imm_pop_perc, 2)
    index_rank_table$perc_urban <- round(index_rank_table$perc_urban, 2)
    index_rank_table$mean_agree_vac_safe <- round(index_rank_table$mean_agree_vac_safe, 2)
    index_rank_table$mean_agree_vac_important <- round(index_rank_table$mean_agree_vac_important, 2)
    index_rank_table$mean_agree_vac_effective <- round(index_rank_table$mean_agree_vac_effective, 2)
    index_rank_table$gov_trust <- round(index_rank_table$gov_trust, 2)

    # Create pretty column names
    index_rank_table <- rename(index_rank_table,
                               "Location" = "location",
                               "Region" = "region",
                               
                               "Eligibility to Receive DAH" = "dah_eligible",
                               "Socio-demographic Index" = "sdi",
                               "Total Health Spending per Person" = "the_per_cap_mean",
                               "Government Health Spending per Total Health Spending" = "ghes_per_the_mean",
                               "Development Assistance per Person" = "dah_per_cap_ppp_mean",
                               
                               "HAQI" = "haqi",
                               "Corruption Perception Index" = "cpi",
                                  
                               "Skilled Attendants at Birth" = "perc_skill_attend", 
                               "Immigrant Population" = "imm_pop_perc",
                               
                               "Urbanicity" = "perc_urban",
                               "Improvement Index" = "result",
                                  
                               # "location_id" = "location_id",
                                  
                               # "level" = "level",
                                  
                               # "2019 SDI Group" = "sdi_group_present",
                                  
                               "Rank" = "rank",
                               "Agree Vaccines are Safe" = "mean_agree_vac_safe",
                               "Agree Vaccines are Important" = "mean_agree_vac_important", 
                               "Agree Vaccines are Effective" = "mean_agree_vac_effective",
                               "Trust in Government" = "gov_trust")
    
      # Re-order the columns of the table
      index_rank_table <- index_rank_table[, c("Rank", "Location", "Improvement Index", "Eligibility to Receive DAH", "Socio-demographic Index",
                                               "Total Health Spending per Person", "Government Health Spending per Total Health Spending",
                                               "Development Assistance per Person", "HAQI", "Corruption Perception Index", "Skilled Attendants at Birth",
                                               "Immigrant Population", "Urbanicity", 
                                               "Agree Vaccines are Safe", "Agree Vaccines are Important", "Agree Vaccines are Effective",
                                               "Trust in Government")]
      
      # Create custom colors that will be used in formatting the table
      customGreen0 = "#DeF7E9"
      customGreen = "#71CA97"

      # Creat a custom True False format
      true_false_formatter <-
        formatter( "span",
                   style = x ~ formattable::style(
                     font.weight = "bold",
                     color = ifelse(x == TRUE, "forestgreen", ifelse(x == FALSE, "red", "black"))
                   ))

      # Create a table that will be formatted according to value of the indicator and components
      formattable(
        index_rank_table,
        list(
          ## a coloured bar with length proportional to value
          'Improvement Index' = color_tile("white", "#569eca"),
          "Socio-demographic Index" = color_tile("white", "pink"),
          "Eligibility to Receive DAH" = true_false_formatter,
          "Total Health Spending per Person"= color_tile(customGreen0, customGreen),
          "Government Health Spending per Total Health Spending"= color_tile(customGreen0, customGreen),
          "HAQI"= color_tile(customGreen0, customGreen),
          "Development Assistance per Person"=color_tile(customGreen0, customGreen),
          "Corruption Perception Index"= color_tile(customGreen0, customGreen),
          "Skilled Attendants at Birth"= color_tile(customGreen0, customGreen),
          "Immigrant Population" = color_tile(customGreen0, customGreen),
          "Urbanicity" = color_tile(customGreen0, customGreen),
          "Agree Vaccines are Safe" = color_tile(customGreen0, customGreen),
          "Agree Vaccines are Important" = color_tile(customGreen0, customGreen),
          "Agree Vaccines are Effective" = color_tile(customGreen0, customGreen),
          "Trust in Government" = color_tile(customGreen0, customGreen)
        )
      )%>%
        as.datatable(rownames = FALSE,
                     options = list(paging = FALSE,
                                    scrollY = '520px',
                                    scrollY=TRUE,
                                    scrollX=TRUE,
                                    #searching = FALSE,
                                    autoWidth = FALSE,
                                    ordering = FALSE,
                                    pageLength=1000)
        )
    })
  
  # Create output showing which location is getting plotted *I THINK*
  output$content_vac <- renderText("United States of America")
  output$content_dis <- renderText("United States of America")
  output$content_vac_dis <- renderText("United States of America")
  
  
  # dynamically determine which data relating to vaccines will get plotted 
  vaccine_map_data <- reactive({
    req(input$vaccine_drop)
    vaccine_map_df <-as.data.frame.matrix(merged_vaccine_trends)
  })
  
  # Create the vaccination trends plots
  output$all_vaccine_plot <- renderPlotly({
    vac_plotdata <- filter(vaccine_trends,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
    if (input$vaccine_plot == "line_trend"){
      fig_a <- plot_ly(vac_plotdata, x = ~year_id,y=~prop_val, color = ~vaccine_name)%>%
        add_lines()
      
      fig_a <- fig_a %>% 
        layout(autosize = T,
               title ="Time Series of Vaccination Coverage",  showlegend = T,
               xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
               yaxis = list(title = "Vaccination Coverage (%)",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_a
      
    }
    else if (input$vaccine_plot == "bar_plot"){
      single_year_vac_plotdata <- filter(vac_plotdata,year_id == input$year)
      fig1 <- plot_ly(x = ~single_year_vac_plotdata$prop_val, y = ~reorder(single_year_vac_plotdata$vaccine_name, single_year_vac_plotdata$prop_val), name = single_year_vac_plotdata$vaccine_name,
                      type = 'bar', orientation = 'h',
                      color = single_year_vac_plotdata$vaccine_name)
      #marker = list(color = 'rgba(50, 171, 96, 0.6)',
      #line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) 
      fig1 <- fig1 %>% layout( autosize = T,
                               title = paste0("Vaccination Coverage in ", input$year),
                               yaxis = list(title = "Vaccine",showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                               xaxis = list(title = "Vaccination Coverage (%)", zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
      fig1 <- fig1 %>% add_annotations(xref = 'x1', yref = 'y',
                                       x = single_year_vac_plotdata$prop_val * 1 + 0.05,  y = single_year_vac_plotdata$vaccine_name,
                                       text = paste(round(single_year_vac_plotdata$prop_val*100, 2), '%'),
                                       font = list(family = 'Arial', size = 12, color = 'rgba(0, 0, 0, 1)'),
                                       showarrow = FALSE)
    }
    else if (input$vaccine_plot == "vaccine_map"){
      single_year_vac_plotdata <- filter(vaccine_map_data(),year_id == input$year)
      single_year_vac_plotdata <- filter(single_year_vac_plotdata,vaccine_name == input$vaccine_drop)
      # single_year_vac_plotdata$hover <- with(single_year_vac_plotdata, paste(location_name))
      # light grey boundaries
      l <- list(color = toRGB("white"), width = 0.5)
      
      # specify map projection/options
      g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,showland = TRUE,showcountries = TRUE,
        resolution = 150,
        countrycolor = toRGB("white"),
        landcolor = toRGB("grey85"),
        projection = list(scale=1.2))
      
      fig1 <- plot_ly(single_year_vac_plotdata)
        
        fig1 <- fig1 %>% 
          add_trace(
            z = ~prop_val, color = ~prop_val, type = 'choropleth', locations = ~iso_code, colors="Blues", 
            text = ~paste0(location_name),#paste0(with(single_year_vac_plotdata, paste(location_name))),
            marker = list(line = l)) %>%
          colorbar(title = paste0("Vaccination \nCoverage (%)")) %>% 
          layout(
            # autosize = T,
            title = paste0(input$year, " ", input$vaccine_drop, " Coverage Map"),
            mapbox=list(
              style="carto-positron",
              center = list(lon = -90, lat = 80)
              ),
            geo = g)
      # }
    }
  })

  # Create the mortality and disability plots
  output$all_disease_plot <- renderPlotly({
    # print("input")
    # print(input$disease_estimate)
    disease_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
    # print("disease_plotdata")
    # print(disease_plotdata)
    if (input$disease_estimate == "number_val"){
      fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~round(deaths_number_val,8), color = ~cause_name)%>%
        add_lines()
      title = "Time Series of Deaths, Disease or Disability Number"
      y_title = "Number of Deaths \n in Population"
    }
    else if (input$disease_estimate == "percent_val"){
      fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~round(deaths_percent_val,8), color = ~cause_name)%>%
        add_lines()
      title = "Time Series of Deaths, Disease or Disability Percent"
      y_title="Particular Cause \n Death/All Causes Death"
    }
    else{
      fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~round(deaths_rate_val,8), color = ~cause_name)%>%
        add_lines()
      title = "Time Series of Deaths, Disease or Disability Rate"
      y_title="Deaths per 100,000 Population"
    }
    fig_dis <- fig_dis %>% 
      layout( autosize = T,
              title =title,  showlegend = T,
              xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
              yaxis = list(title =y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE,type= "log",tickfont = list(size = 1)))
    fig_dis
  })
  
  # create the disability plots
  output$all_disability_plot <- renderPlotly({
    disability_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
    if (input$disease_estimate == "number_val"){
      fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_number_val, color = ~cause_name)%>%
        add_lines()
      title = "Time Series of Number of Years Lived with Disability"
      y_title = "Years Lived with Burden"
    }
    else if (input$disease_estimate == "percent_val"){
      fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_percent_val, color = ~cause_name)%>%
        add_lines()
      title = "Time Series of Proportion of Years Lived with Disability in Population"
      y_title="YLDs for Particular \n Cause/YLDs for All Causes"
    }
    else{
      fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_rate_val, color = ~cause_name)%>%
        add_lines()
      title = "Time Series of Years Lived with Disability per 100,000 Population"
      y_title="YLDs per 100,000 Population"
    }
    fig_dis <- fig_dis %>% 
      layout( autosize = T,
              title =title,  showlegend = T,
              xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
              yaxis = list(title = y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE,type= "log",tickfont = list(size = 1)))
    fig_dis
  })
  
  # Dynamicaly select the disability plots to render
  observeEvent(selected_dis_vac_data(),{
    output$selected_vac_dis_plot <- renderPlotly({
      selected_vac_plotdata <- filter(selected_dis_vac_data()$selected_vac_data,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
      selected_dis_plotdata <- filter(selected_dis_vac_data()$dis_data_for_selected_vac,gsub(" ", "", location_name) == gsub(" ", "", "United States of America"))
      # print(selected_dis_plotdata)
      merged_selected_plotdata <- dplyr::left_join(selected_vac_plotdata,selected_dis_plotdata, "year_id", "year_id")
      # print("complete")
      print(merged_selected_plotdata)
      fig <- plot_ly()
      # Add traces
      fig <- plot_ly(merged_selected_plotdata)
      fig <- fig %>% add_trace(x= ~year_id, y = ~round(deaths_rate_val,8), type = 'scatter', mode = 'lines+makers', color = ~cause_name) 
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "<b> Vaccine</b> Coverage (%)")
      
      fig <- fig %>% add_trace(x =  ~year_id, y = ~prop_val, type = 'scatter',name = ~vaccine_name.x,yaxis = "y2", mode = 'lines',line = list(color = 'rgba(49,130,189, 1)', width = 4)) 
      # Set figure title, x and y-axes titles
      fig <- fig %>% layout(
        autosize = T,
        title = list(text="Vaccine & Corresponding Disease Trend", x=0.25),
        xaxis = list(title="Year"),
        yaxis = list(title= "<b> Deaths</b> per 100,000 Population"),
        yaxis2 = ay,
        legend = list(x = 3000, y = 1.2)
      )%>%
        layout(xaxis = list(
          zerolinecolor = '#ffff',
          zerolinewidth = 2,
          gridcolor = 'ffff'),
          yaxis = list(
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff')
        )
      
      fig
    })
    })
  
  
  list_all <- reactiveVal()
  observeEvent(input$table_cell_clicked,{
    
    info = input$table_cell_clicked
    if (is.null(info$value) || info$col != 1) return()
    updateTabsetPanel(session, 't1', selected = 'Vaccination Trends')
    if(input$t1 == "t_sdi"){
      updateTabsetPanel(session, 't1', selected = 'Vaccination Trends') 
    }
    else if(input$t1 == "t_vac"){
      updateTabsetPanel(session, 't1', selected = 'Disease Trends') 
    }
    
    vacdata = filter(preventable_vac_trend, gsub(" ", "", location_name) == gsub(" ", "", info$value))
    disdata = filter(merged_data_for_vac_dis, gsub(" ", "", location_name) == gsub(" ", "", info$value))
    merged = dplyr::inner_join(vacdata,disdata,"vaccine_name","vaccine_name")
    choices = sort(unique(merged$vaccine_name))
    updateSelectInput(session,'vaccinations', choices = choices)
    
    output$content_vac <- renderText(info$value)
    output$content_dis <- renderText(info$value)
    output$content_vac_dis <- renderText(info$value)
    
    if (gsub(" ", "", info$value) %in% list_all()){
      lst = list_all()
      list_all(lst[lst!=gsub(" ", "", info$value)])
    }
    else{
      list_all(append(list_all(), gsub(" ", "", info$value)))
    }
    
    output$index_trend_plot_multi <- renderPlotly({
      index_trend_data <- index_results %>% 
        filter(gsub(" ", "",location) %in% gsub(" ", "",list_all()))
      
      fig_a <- plot_ly(index_trend_data, x = ~year)
      fig_a <- fig_a  %>% add_trace(y=~result,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      fig_a <- fig_a %>% 
        layout( autosize = T,
                title = paste0("Time Series of Vaccine Improvement Index"), 
                showlegend = TRUE,
                xaxis = list(title = "Year", showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE, list(range=c(0,1))),
                yaxis = list(title = "Vaccine Improvement Potential Index",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_a
    })
    
    output$indicator_trend_plot_multi <- renderPlotly({
      indicator_trend_data <- index_results %>% 
        filter(gsub(" ", "",location) %in% gsub(" ", "",list_all()))
      fig_a <- plot_ly(indicator_trend_data, x = ~year)
      
      if (input$indicators == "Socio-demographic Index"){
        titles = paste0("Time Series of SDI")
        ytitles = "Socio-demographic Index"
        fig_a <- fig_a  %>% add_trace(y=~sdi,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      else if (input$indicators == "Eligibility to Receive DAH"){
        titles = paste0("Time Series of Eligibility to Receive DAH")
        ytitles = "Eligibility to Receive DAH"
        fig_a <- fig_a  %>% add_trace(y=~dah_eligible,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      else if (input$indicators == "Total Health Spending per Person"){
        titles=paste0("Time Series of Total Health Spending per Person")
        ytitles = "Total Health Spending per Person"
        fig_a <- fig_a  %>% add_trace(y=~the_per_cap_mean,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      else if (input$indicators == "Government Health Spending per Total Health Spending"){
        titles=paste0("Time Series of Government Health Spending per Total Health Spending")
        ytitles = "Government Health Spending \n per Total Health Spending"
        fig_a <- fig_a  %>% add_trace(y=~ghes_per_the_mean,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      else if (input$indicators == "HAQI"){
        titles=paste0("Time Series of HAQI")
        ytitles = "HAQI"
        fig_a <- fig_a  %>% add_trace(y=~haqi,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
        
      }
      else if (input$indicators == "Corruption Perception Index"){
        titles=paste0("Time Series of Corruption Perception Index")
        ytitles = "Corruption Perception Index"
        fig_a <- fig_a  %>% add_trace(y=~cpi,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
        
      }
      else if (input$indicators == "Skilled Attendants at Birth"){
        titles=paste0("Time Series of Skilled Attendants at Birth")
        ytitles = "Skilled Attendants at Birth"
        fig_a <- fig_a  %>% add_trace(y=~perc_skill_attend,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
        
      }
      else if (input$indicators == "Immigrant Population (%)"){
        titles=paste0("Time Series of Immigrant Population (%)")
        ytitles = "Immigrant Population (%)"
        fig_a <- fig_a  %>% add_trace(y=~imm_pop_perc,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
        
      }
      else if (input$indicators == "Urbanicity (%)"){
        titles=paste0("Time Series of Urbanicity (%)")
        ytitles = "Urbanicity (%)"
        fig_a <- fig_a  %>% add_trace(y=~perc_urban,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      else if (input$indicators == "Agree Vaccines are Safe"){
        titles=paste0("Time Series of Agreement Vaccines are Safe")
        ytitles = "Agree Vaccines are Safe"
        fig_a <- fig_a  %>% add_trace(y=~mean_agree_vac_safe,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
        
      }
      else if (input$indicators == "Agree Vaccines are Important"){
        titles=paste0("Time Series of Agreement Vaccines are Important")
        ytitles = "Agree Vaccines are Important"
        fig_a <- fig_a  %>% add_trace(y=~mean_agree_vac_important,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      else{
        titles=paste0("Time Series of Agree Vaccines are Effectivel")
        ytitles = "Agree Vaccines are Effective"
        fig_a <- fig_a  %>% add_trace(y=~mean_agree_vac_effective,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2)
      }
      
      fig_a <- fig_a %>% 
        layout( autosize = T,
                title = paste0(titles),
                showlegend = TRUE,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = ytitles, showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_a
      
    })
    
    output$indicator_trend_plot <- renderPlotly({
      indicator_trend_data <- filter(index_results,gsub(" ", "",location) == gsub(" ", "", info$value))
      fig_a <- plot_ly(indicator_trend_data, x = ~year)
      
      if (input$indicators == "Socio-demographic Index"){
        left_text = round(indicator_trend_data$sdi[1],3)
        right_text =round(indicator_trend_data$sdi[30],3)
        left_y = indicator_trend_data$sdi[1]+0.01
        right_y = indicator_trend_data$sdi[30]+0.02
        titles = paste0("Time Series of SDI in United States of America")
        ytitles = "Socio-demographic Index"
        fig_a <- fig_a  %>% add_trace(y=~sdi,type='scatter', name = "SDI", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(sdi[1], sdi[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }      else if (input$indicators == "Eligibility to Receive DAH"){
        left_text = " "
        right_text =" "
        left_y = as.numeric(indicator_trend_data$dah_eligible[1])+0.01
        right_y = as.numeric(indicator_trend_data$dah_eligible[30])+0.02
        titles = paste0("Time Series of Eligibility to Receive DAH")
        ytitles = "Eligibility to Receive DAH"
        fig_a <- fig_a  %>% add_trace(y=~dah_eligible,type='scatter', name = "Development Assistance Per Total Health Spending Categorical", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(dah_eligible[1], dah_eligible[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
      }
      else if (input$indicators == "Total Health Spending per Person"){
        left_text = round(indicator_trend_data$the_per_cap_mean[1],3)
        right_text =round(indicator_trend_data$the_per_cap_mean[30],3)
        left_y = indicator_trend_data$the_per_cap_mean[1]+0.01
        right_y = indicator_trend_data$the_per_cap_mean[30]+0.02
        titles=paste0("Time Series of Total Health Spending per Person")
        ytitles = "Total Health Spending \n per Person"
        fig_a <- fig_a  %>% add_trace(y=~the_per_cap_mean,type='scatter', name = "Total Health Spending per Person", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(the_per_cap_mean[1], the_per_cap_mean[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Government Health Spending per Total Health Spending"){
        left_text = round(indicator_trend_data$ghes_per_the_mean[1],3)
        right_text =round(indicator_trend_data$ghes_per_the_mean[30],3)
        left_y = indicator_trend_data$ghes_per_the_mean[1]+0.01
        right_y = indicator_trend_data$ghes_per_the_mean[30]+0.02
        titles=paste0("Time Series of Government Health Spending per Total Health Spending")
        ytitles = "Government Health Spending \n per Total Health Spending"
        fig_a <- fig_a  %>% add_trace(y=~ghes_per_the_mean,type='scatter', name = "Government Health Spending per Total Health Spending", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(ghes_per_the_mean[1], ghes_per_the_mean[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "HAQI"){
        left_text = round(indicator_trend_data$haqi[1],3)
        right_text =round(indicator_trend_data$haqi[30],3)
        left_y = indicator_trend_data$haqi[1]+0.01
        right_y = indicator_trend_data$haqi[30]+0.02
        titles=paste0("Time Series of HAQI")
        ytitles = "HAQI"
        fig_a <- fig_a  %>% add_trace(y=~haqi,type='scatter', name = "HAQI", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(haqi[1], haqi[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Corruption Perception Index"){
        left_text = round(indicator_trend_data$cpi[1],3)
        right_text =round(indicator_trend_data$cpi[30],3)
        left_y = indicator_trend_data$cpi[1]+0.01
        right_y = indicator_trend_data$cpi[30]+0.02
        titles=paste0("Time Series of Corruption Perception Index")
        ytitles = "Corruption Perception Index"
        fig_a <- fig_a  %>% add_trace(y=~cpi,type='scatter', name = "Corruption Perception Inde", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(cpi[1], cpi[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Skilled Attendants at Birth"){
        left_text = round(indicator_trend_data$perc_skill_attend[1],3)
        right_text =round(indicator_trend_data$perc_skill_attend[30],3)
        left_y = indicator_trend_data$perc_skill_attend[1]+0.01
        right_y = indicator_trend_data$perc_skill_attend[30]+0.02
        titles=paste0("Time Series of Skilled Attendants at Birth")
        ytitles = "Skilled Attendants at Birth"
        fig_a <- fig_a  %>% add_trace(y=~perc_skill_attend,type='scatter', name = "Skilled Attendants at Birth", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(perc_skill_attend[1], perc_skill_attend[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Immigrant Population (%)"){
        left_text = round(indicator_trend_data$imm_pop_perc[1],3)
        right_text =round(indicator_trend_data$imm_pop_perc[30],3)
        left_y = indicator_trend_data$imm_pop_perc[1]+0.01
        right_y = indicator_trend_data$imm_pop_perc[30]+0.02
        titles=paste0("Time Series of Immigrant Population (%)")
        ytitles = "Immigrant Population (%)"
        fig_a <- fig_a  %>% add_trace(y=~imm_pop_perc,type='scatter', name = "Immigrant Population (%)", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(imm_pop_perc[1], imm_pop_perc[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Urbanicity (%)"){
        left_text = round(indicator_trend_data$perc_urban[1],3)
        right_text =round(indicator_trend_data$perc_urban[30],3)
        left_y = indicator_trend_data$perc_urban[1]+0.01
        right_y = indicator_trend_data$perc_urban[30]+0.02
        titles=paste0("Time Series of Urbanicity (%)")
        ytitles = "Urbanicity (%)"
        fig_a <- fig_a  %>% add_trace(y=~perc_urban,type='scatter', name = "Urbanicity (%)", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(perc_urban[1], perc_urban[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Agree Vaccines are Safe"){
        left_text = round(indicator_trend_data$mean_agree_vac_safe[1],3)
        right_text =round(indicator_trend_data$mean_agree_vac_safe[30],3)
        left_y = indicator_trend_data$mean_agree_vac_safe[1]+0.01
        right_y = indicator_trend_data$mean_agree_vac_safe[30]+0.02
        titles=paste0("Time Series of Agreement Vaccines are Safe")
        ytitles = "Agree Vaccines are Safe"
        fig_a <- fig_a  %>% add_trace(y=~mean_agree_vac_safe,type='scatter', name = "Agree Vaccines are Safe", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(mean_agree_vac_safe[1], mean_agree_vac_safe[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else if (input$indicators == "Agree Vaccines are Important"){
        left_text = round(indicator_trend_data$mean_agree_vac_important[1],3)
        right_text =round(indicator_trend_data$mean_agree_vac_important[30],3)
        left_y = indicator_trend_data$mean_agree_vac_important[1]+0.01
        right_y = indicator_trend_data$mean_agree_vac_important[30]+0.02
        titles=paste0("Time Series of Agreement Vaccines are Important")
        ytitles = "Agree Vaccines are Important"
        fig_a <- fig_a  %>% add_trace(y=~mean_agree_vac_important,type='scatter', name = "Agree Vaccines are Important", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(mean_agree_vac_important[1], mean_agree_vac_important[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      else{
        left_text = round(indicator_trend_data$mean_agree_vac_effective[1],3)
        right_text =round(indicator_trend_data$mean_agree_vac_effective[30],3)
        left_y = indicator_trend_data$mean_agree_vac_effective[1]+0.01
        right_y = indicator_trend_data$mean_agree_vac_effective[30]+0.02
        titles=paste0("Time Series of Agreement Vaccines are Effective")
        ytitles = "Agree Vaccines are Effective"
        fig_a <- fig_a  %>% add_trace(y=~mean_agree_vac_effective,type='scatter', name = "Agree Vaccines are Effective", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(mean_agree_vac_effective[1], mean_agree_vac_effective[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
        
      }
      
      vii_left <- list(
        xref = 'paper',
        yref = 'y',
        x = 0.01,
        y = left_y,
        xanchor = 'middle',
        yanchor = 'center',
        text = ~left_text,
        font = list(family = 'Arial',
                    size = 16,
                    color = 'rgba(67,67,67,1)'),
        showarrow = FALSE)
      
      vii_right <- list(
        xref = 'paper',
        yref = 'y',
        x = 0.97,
        y = right_y,
        xanchor = 'middle',
        yanchor = 'center',
        text = ~right_text,
        font = list(family = 'Arial',
                    size = 16,
                    color = 'rgba(67,67,67,1)'),
        showarrow = FALSE)
      
      fig_a <- fig_a %>% 
        layout( autosize = T,
                title = paste0(titles, " in ",info$value),
                showlegend = FALSE,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = ytitles,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_a <- fig_a %>% layout(annotations = vii_left) 
      fig_a <- fig_a %>% layout(annotations = vii_right) 
      fig_a
    })
    
    output$all_vaccine_plot <- renderPlotly({
      vac_plotdata <- filter(vaccine_trends,gsub(" ", "", location_name) == gsub(" ", "", info$value))
      if (input$vaccine_plot == "line_trend"){
        fig_a <- plot_ly(vac_plotdata, x = ~year_id,y=~prop_val, color = ~vaccine_name)%>%
          add_lines()
        fig_a <- fig_a %>% 
          layout( autosize = T,
                  title ="Time Series of Vaccination Coverage",  showlegend = T,
                  xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                  yaxis = list(title = "Vaccination Coverage (%)",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
        fig_a
      }
      else{
        single_year_vac_plotdata <- filter(vac_plotdata,year_id == input$year)
        fig1 <- plot_ly(x = ~single_year_vac_plotdata$prop_val, y = ~reorder(single_year_vac_plotdata$vaccine_name, single_year_vac_plotdata$prop_val), name = single_year_vac_plotdata$vaccine_name,
                        type = 'bar', orientation = 'h',color = single_year_vac_plotdata$vaccine_name)
 
        fig1 <- fig1 %>% layout( autosize = T,
                                 title = paste0("Vaccination Coverage in ", input$year),
                                 yaxis = list(title = "Vaccine",showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85)),
                                 xaxis = list(title = "Vaccination Coverage (%)", zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) 
        fig1 <- fig1 %>% add_annotations(xref = 'x1', yref = 'y',
                                         x = single_year_vac_plotdata$prop_val * 1+ 0.05,  y = single_year_vac_plotdata$vaccine_name,
                                         text = paste(round(single_year_vac_plotdata$prop_val*100, 2), '%'),
                                         font = list(family = 'Arial', size = 12, color = 'rgb(0,0,0,1)'),
                                         showarrow = FALSE)
      }
    })
    output$all_disease_plot <- renderPlotly({
      disease_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", info$value))
      if (input$disease_estimate == "number_val"){
        fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~round(deaths_number_val,8), color = ~cause_name)%>%
          add_lines()
        title = "Time Series of Deaths, Disease or Disability Number"
        y_title = "Number of Deaths \n in Population"
      }
      else if (input$disease_estimate == "percent_val"){
        fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~round(deaths_percent_val,8), color = ~cause_name)%>%
          add_lines()
        title = "Time Series of Deaths, Disease or Disability Percent"
        y_title="Deaths for a Particular \n Cause/Deaths from All Causes"
      }
      else{
        fig_dis <- plot_ly(disease_plotdata, x = ~year_id,y= ~round(deaths_rate_val,8), color = ~cause_name)%>%
          add_lines()
        title = "Time Series of Deaths, Disease or Disability Rate"
        y_title="Deaths per 100,000 Population"
      }
      fig_dis <- fig_dis %>% 
        layout( autosize = T,
                title =title,  showlegend = T,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE,type= "log"))
      fig_dis
    })
    
    output$all_disability_plot <- renderPlotly({
      disability_plotdata <- filter(disease_trends,gsub(" ", "", location_name) == gsub(" ", "", info$value))
      if (input$disease_estimate == "number_val"){
        fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_number_val, color = ~cause_name)%>%
          add_lines()
        title = "Time Series of Years Lived with Disability in Population"
        y_title = "Years Lived with \n Disability in Population"
      }
      else if (input$disease_estimate == "percent_val"){
        fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_percent_val, color = ~cause_name)%>%
          add_lines()
        title = "Time Series of Proportion of Years Lived with Disability in Population" 
        y_title="YLDs for Particular \n Cause/YLDs for All Causes" 
      }
      else{
        fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_rate_val, color = ~cause_name)%>%
          add_lines()
        title = "Time Series of Years Lived with Disability per 100,000 Population"
        y_title="YLDs per 100,000 Population"
      }
      fig_disa <- plot_ly(disability_plotdata, x = ~year_id,y=~ylds_number_val, color = ~cause_name)%>%
        add_lines()
      fig_disa <- fig_disa %>% 
        layout( autosize = T,
                title =title,  showlegend = T,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE,type= "log"))
      fig_disa
    })
    
    observeEvent(selected_dis_vac_data(),{
      output$selected_vac_dis_plot <- renderPlotly({
        selected_vac_plotdata <- filter(selected_dis_vac_data()$selected_vac_data,gsub(" ", "", location_name) == gsub(" ", "", info$value))
        selected_dis_plotdata <- filter(selected_dis_vac_data()$dis_data_for_selected_vac,gsub(" ", "", location_name) == gsub(" ", "", info$value))
        merged_selected_plotdata <- dplyr::left_join(selected_vac_plotdata,selected_dis_plotdata, "year_id", "year_id")
        # print(selected_dis_plotdata)
        fig <- plot_ly()
        # Add traces
        fig <- plot_ly(merged_selected_plotdata)
        fig <- fig %>% add_trace(x= ~year_id, y = ~round(deaths_rate_val,8), type = 'scatter', mode = 'lines+makers', color = ~cause_name) 
        ay <- list(
          overlaying = "y",
          side = "right",
          title = "<b> Vaccine</b> Coverage (%)")
        
        fig <- fig %>% add_trace(x =  ~year_id, y = ~prop_val, type = 'scatter',name = ~vaccine_name.x,yaxis = "y2", mode = 'lines',line = list(color = 'rgba(49,130,189, 1)', width = 4)) 
        # Set figure title, x and y-axes titles
        fig <- fig %>% layout(
          autosize = T,
          title = list(text="Vaccine & Corresponding Disease Trend", x=0.25), 
          xaxis = list(title="Year"),
          yaxis = list(title="<b> Deaths</b> per 100,000 Population"),
          yaxis2 = ay,
          legend = list(x = 3000, y = 1.2)
        )%>%
          layout(xaxis = list(
            zerolinecolor = '#ffff',
            zerolinewidth = 2,
            gridcolor = 'ffff'),
            yaxis = list(
              zerolinecolor = '#ffff',
              zerolinewidth = 2,
              gridcolor = 'ffff')
          )
        
        fig
      })
    })
  })
  
  output$vac_name <- renderText({
  input$vaccinations
  print(input$vaccinations)
  })

  output$vac_description <- renderText({
   unique(filter(vaccine_preventable_diseases,vaccine_name == input$vaccinations)$vaccine_description)
  })
  
  output$vac_dis <- renderText({ 
    unique(filter(vaccine_preventable_diseases,vaccine_name == input$vaccinations)$cause_name)
  })
  
  output$BCGtable = DT::renderDataTable({
    bcg_table <- vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "BCG",][,c("cause_name")]
    formattable(
      bcg_table
    ) %>%
      as.datatable(rownames = FALSE,  colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE)
      )
  })
  
  
  # Create DTP Table for Vaccination and Corresponding Disease Trends Tab
  output$DTPtable <- DT::renderDataTable({
    table <- vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "DTP1",][,c("cause_name")]
    formattable(
      table) %>% 
      as.datatable(rownames = FALSE, colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE))
  })
  
  # Create HEPB3 Table for Vaccination and Corresponding Disease Trends Tab
  output$HepB3table = DT::renderDataTable({
    table <- vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "HepB3",][,c("cause_name")]
    formattable(
      table
    ) %>%
      as.datatable(rownames = FALSE,  colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE))
  })
  
  # Create MCV Table for Vaccination and Corresponding Disease Trends Tab
  output$MCVtable = DT::renderDataTable({
    table <- unique(vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "MCV1" | vaccine_preventable_diseases$vaccine_name == "MCV2",][,c("cause_name")])
    formattable(
      table
    ) %>%
      as.datatable(rownames = FALSE,  colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE))
  })
  
  # Create RotaC Table for Vaccination and Corresponding Disease Trends Tab
  output$RotaCtable = DT::renderDataTable({
    table <- vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "RotaC",][,c("cause_name")]
    formattable(
      table
    ) %>%
      as.datatable(rownames = FALSE,  colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE))
  })
  
  # Define the data that will be downloaded in the data Explorer Tab
  dataexplorer <- reactive({
    
    # require the input specifying which data will be downloaded
    req(input$dataset)
    
    if(input$dataset == "vaccine trends"){
      dataexplorer <- vaccine_trends
    } else if (input$dataset == "disease trends"){
      dataexplorer <- disease_trends
    } else if (input$dataset == "data dictionary"){
      dataexplorer <- codebook
    } else{
      dataexplorer <- index_results
    }
  })
  
  # Create the table that will preview the data to be downloaded in the data explorer
  output$alldatatable <- DT::renderDataTable({
    
    # load the data that was previously defined
    data <- dataexplorer()
    
    # define the x and pl variables
    if (input$dataset == "vaccine trends"){
      # define dataset
      x <- data %>% dplyr::select(-c("location_id"))
      
      # define page length
      pl <- 17
      
      } else if (input$dataset == "disease trends"){
      
        #define data
        x <- data %>% dplyr::select(-c("location_id","cause_id"))
      
        # define page length
        pl <- 11
      
        } else if (input$dataset == 'data dictionary'){
      
          # define data
          x <-data 
          
          # define page length
          pl <- 8
    
          } else {
            # define data
            x <- data 
            
            # define page length
            pl = 11
            
            # rename the columns in the dataset
            x <-  rename(x,
                         "Location" = "location",
                         "Year" = "year",
                         "Region" = "region",
                         "GBD Location ID" = "gbd_location_id",
                         "ISO Code" = "iso_code",
                         "ISO Number Code" = "iso_num_code",
                         "Eligibility to Receive DAH" = "dah_eligible",
                         "Socio-demographic Index" = "sdi",
                         "Total Health Spending per Person" = "the_per_cap_mean",
                         "Government Health Spending per Total Health Spending" = "ghes_per_the_mean",
                         "Development Assistance per Person" = "dah_per_cap_ppp_mean",
                         "HAQI" = "haqi",
                         "Corruption Perception Index" = "cpi",
                         "Skilled Attendants at Birth" = "perc_skill_attend",
                         "Immigrant Population" = "imm_pop_perc",
                         "Urbanicity" = "perc_urban",
                         "Improvement Index" = "result",
                         "Agree Vaccines are Safe" = "mean_agree_vac_safe",
                         "Agree Vaccines are Important" = "mean_agree_vac_important",
                         "Agree Vaccines are Effective" = "mean_agree_vac_effective",
                         "Government Trust" = "gov_trust")
            
            }
    
    
    
    # Format the table that will be previewed in the visualizer before downloading
    formattable(x) %>%
      as.datatable(rownames = FALSE,
                   options = list(paging = TRUE,
                                  searching = TRUE,
                                  scrollX=TRUE, 
                                  ordering = TRUE,
                                  dom = '<lf<t>p>',
                                  pageLength=pl,
                                  lengthChange = FALSE))
    }
    )
  

  # Download handler which allows users to download dataset of their choosing ####
output$download <- downloadHandler(
    filename =  paste0(input$dataset,".csv",sep=""),
    content = function(fname){
      write.csv(dataexplorer(), fname)
    }
  )
  
  # download handler which will make the final report available 
  output$finalStudyReport <- downloadHandler(
    filename = "Final-Report_MISP-60343.pdf",
    content = function(file){
      file.copy("www/Final-Report_MISP-60343.pdf", file)
    }
  )
}

# this merges the ui above with the html index file ######
body <- htmlTemplate(
  filename = "www/index.html",
  what_if_ui = body
)

shinyApp(body, server)


