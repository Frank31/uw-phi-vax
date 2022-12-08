# Date Last Updated: December 1 2022
# Purpose: This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


##### Set working directory
# setwd("C:/Users/sarah/Downloads/Work/Dashboard")


# source("01_packages.R", local = TRUE) #install/loads necessary packages

# data_dir <- here("Data") #define data folder directory

# source("02_alter_data.R", local = TRUE) #script to edit data frames
# source("03_ui.R", local = TRUE)
# source("04_server.R", local = TRUE)

#Install Packages

# Function to check if you have a package installed and installs if you don't
inst.pkg <- function(package_name) {
  if (package_name %in% rownames(installed.packages()) == FALSE) # check to see if the package is already installed
  {install.packages(package_name)} # install package if missing
  if (package_name %in% rownames(.packages()) == FALSE) # check to see if the package is loaded
  {library(package_name, character.only = TRUE)} # load package into library
}

r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

inst.pkg("shiny")
inst.pkg("shinythemes")
inst.pkg("DT")
inst.pkg("dplyr")
inst.pkg("formattable")
inst.pkg("tidyr")
inst.pkg("leaflet")
inst.pkg("plotly")
inst.pkg("readxl")
inst.pkg("shinycssloaders")
inst.pkg("shinyWidgets")
inst.pkg("shinyhelper")
inst.pkg("rintrojs")
inst.pkg("shinyBS")
inst.pkg("here")
inst.pkg("shinyjs")
inst.pkg("tools")

data_dir <- here("Data")
# data_dir <- "C:/Users/frc2/Documents/uw-phi-vax/dashboard/Data/"

vaccine_trends <- readRDS(here(data_dir, "aim_1/New/01_vaccine_trends.RDS"))
sdi <- readRDS(here(data_dir, "aim_1/New/02_sdi.RDS"))
sdi$sdi[sdi$year_id == '2020'] <- NA

#raw_extracted_dhs <- readRDS("aim_1/New/03_raw_extracted_dhs.RDS")
#prepped_dhs_for_mov <- readRDS("aim_1/New/04_prepped_dhs_for_mov.RDS")
disease_trends <- readRDS(here(data_dir, "aim_1/New/05_disease_trends.RDS"))
codebook <- read.csv(here(data_dir, "aim_2/vaccine_index_variable_codebook_for_website.csv"))
merged_data_for_visuals <- readRDS(here(data_dir,"aim_1/New/06_merged_data_for_visuals.RDS"))
merged_data_for_visuals$sdi[merged_data_for_visuals$year_id == '2020'] <- NA
# merged_data_for_visuals$sdi_group_in_2019 <- toTitleCase(merged_data_for_visuals$sdi_group_in_2019)

vaccine_preventable_diseases <- read_excel(here(data_dir, "aim_1/vaccine_preventable_diseases.xlsx"))

#available disease data for cause name
merged_data_for_vac_dis <- dplyr::left_join(vaccine_preventable_diseases,disease_trends, "cause_name", "cause_name")
# available vaccine data with description and cause name
preventable_vac_trend <- vaccine_trends

#aim_2
#index_results <- readRDS("aim_2/10_index_results.RDS")
# index_results <- readRDS(here(data_dir,"aim_2/11_index_results.RDS"))
index_results <- readRDS(here(data_dir,"aim_2/19_index_results_third_version.RDS")) #from 11/03/2022
index_results$sdi[index_results$year == '2020'] <- NA

sdi_dup <- sdi
colnames(sdi_dup)[2] <- "location"
colnames(sdi_dup)[4] <- "year"
merged_data_for_vacii_sdi <- dplyr::left_join(index_results,sdi_dup[,-c("sdi")], by=c("location","year"))


# ui portion of R shiny app -----------------------------------------------

body <- navbarPage(tags$head(includeCSS(here(data_dir, "Style/navbarpage_style.css"))),
                   introjsUI(),
                   theme = shinytheme("flatly"), collapsible = TRUE,
                   title = div(img(src="https://uw-s3-cdn.s3.us-west-2.amazonaws.com/wp-content/uploads/sites/98/2014/09/07214416/W-Logo_White.png",width= "73px", height="45px"),  strong(toupper("Global Vaccine Index Dashboard"))),
                   tabPanel("Visualizations",
                            sidebarPanel(
                              h4(strong("Vaccination Improvement Potential (VIP) Index Ranking Table")),
                              sliderInput("year", "Year", value = 2019, min = 1990, max = 2019, step=1, sep = "", 
                                          animate = TRUE),
                              fluidRow(column(8,radioButtons("sdi_group_present","2019 SDI Group", choices = c("All"="all","Low" ="low","Medium" = "medium","High" = "high"),inline = TRUE)),
                                       column(2,
                                              #tags$i
                                              # class = "glyphicon glyphicon-info-sign", 
                                              # style = "color:#2c3e50;;",
                                              # title = "The Socio-demographic Index (SDI) is a summary measure that identifies where countries sit on the spectrum of development."
                                              #))),
                                              dropMenu(
                                                circleButton(label = "What is SDI?","What is SDI?", inputId='sdiinfo',icon = icon('info')),
                                                h4(strong('SDI')),
                                                h5('The Socio-demographic Index (SDI) is a summary measure that identifies where countries sit on the spectrum of development.'),
                                                placement = "bottom",
                                                arrow = TRUE),
                                              style='text-align:center',
                                              class = 'rightAlign')),
                              selectInput("region_table", "Regions", selected = NULL, choices = c("All",unique(index_results$region))),
                              tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #92c9e8 !important;}')),
                              DT::dataTableOutput("table")),
                            mainPanel(
                              div(class="outer",absolutePanel(fixed=FALSE, width = "100%", 
                                                              draggable = FALSE, height = "100%",
                                                              tags$style(type="text/css",
                                                                         ".shiny-output-error { visibility: hidden; }",
                                                                         ".shiny-output-error:before { visibility: hidden; }"
                                                              ),
                                                              tabsetPanel(id = "t1",
                                                                          tabPanel("Vaccination Improvement",value="t_sdi",
                                                                                   fluidRow(column(6, " ", style='padding:10px;')),
                                                                                   fluidRow(column(width = 9,h4(strong(textOutput("yeartitle")),style='text-align:left')),
                                                                                            column(width = 3,div(switchInput(
                                                                                              inputId = "view",
                                                                                              # label = "Display",
                                                                                              onLabel = "Show Map",
                                                                                              offLabel = "Show Table",
                                                                                              value = FALSE,
                                                                                              labelWidth = "50px")))
                                                                                            # column(width = 2, div(actionButton(inputId = "view", label = "Table"),
                                                                                            #                       actionButton(inputId = "map", label = "Map")))
                                                                                   ),
                                                                                   conditionalPanel("!input.view", 
                                                                                                    fluidRow(column(12, " ", style='padding:3px;')),
                                                                                                    fluidRow(column(10, radioButtons(inputId = "show",label = NULL,
                                                                                                                                     choices = c("Vaccination Improvement Potential (VIP) Index","VIP Index Component"),
                                                                                                                                     selected = "Vaccination Improvement Potential (VIP) Index",
                                                                                                                                     inline = TRUE)), 
                                                                                                             column(2,
                                                                                                                    dropMenu(
                                                                                                                      circleButton(label = "", inputId='info',icon = icon('info')),
                                                                                                                      h4(strong('Index Mapper')),
                                                                                                                      h5('The Vaccine Improvement Index assesses a country’s potential to improve its immunization rates.'),
                                                                                                                      h5("A higher number (or darker shading on the map) indicates a better score."),
                                                                                                                      placement = "bottom",
                                                                                                                      arrow = TRUE))),
                                                                                                    conditionalPanel("input.show == 'Vaccination Improvement Potential (VIP) Index'",
                                                                                                                     fluidRow(column(12, " ", style='padding:3px;')),
                                                                                                                     fluidRow(column(12,plotlyOutput("index_map",height = "40vh")),column(1,"")),
                                                                                                                     fluidRow(column(width = 12, "Select locations in left VIP Index Ranking Table for comparison",
                                                                                                                                     style='font-family:Avenir, Helvetica;font-size:30;text-align:center')),
                                                                                                                     fluidRow(column(10,''),column(2,
                                                                                                                                                   dropMenu(
                                                                                                                                                     circleButton(label = "", inputId='info2',icon = icon('info')),
                                                                                                                                                     h4(strong('Time Series of Vaccination Improvement Potential (VIP) Index')),
                                                                                                                                                     h5('The index value is estimated yearly between 1990 and 2019, in order to track improvement or change over time. '),
                                                                                                                                                     placement = "bottom",
                                                                                                                                                     arrow = TRUE))),
                                                                                                                     fluidRow(column(12, plotlyOutput("index_trend_plot_multi",height = "30vh"))),column(1,"")),
                                                                                                    fluidRow(column(12, " ", style='padding:2px;')),
                                                                                                    conditionalPanel("input.show == 'VIP Index Component'",
                                                                                                                     #fluidRow(column(4,selectInput("indicators", "Indicator:",choices=c("SDI","Development Assistance Per Total Health Spending Categorical","Total Health Spending per Person","Government Health Spending per Total Health Spending",
                                                                                                                     #                                                                  "HAQI","Corruption Perception Index","Skilled Attendants at Birth","Immigrant Population (%)","Urbanicity (%)","Agreement Vaccines are Safe",
                                                                                                                     #                                                                 "Agreement Vaccines are Important","Agreement Vaccines are Effective"),width = "500px")),
                                                                                                                     fluidRow(column(5,selectInput("indicators", "Indicator:",choices=c("Socio-demographic Index","Eligibility to Receive DAH","Total Health Spending per Person","Government Health Spending per Total Health Spending",
                                                                                                                                                                                        "Development Assistance per Person","HAQI","Corruption Perception Index","Skilled Attendants at Birth","Immigrant Population (%)","Urbanicity (%)"),width = "500px"))),
                                                                                                                     fluidRow(column(11,plotlyOutput("indicator_map",height = "43vh")),column(1,"")),
                                                                                                                     column(width = 12, "Select location in left VIP Index Ranking Table for comparison",
                                                                                                                            style='font-family:Avenir, Helvetica;font-size:30;text-align:center;padding:2px; padding-bottom: 20px'),
                                                                                                                     fluidRow(column(11, plotlyOutput("indicator_trend_plot_multi",height = "27vh")),column(1,""))
                                                                                                    )),
                                                                                   conditionalPanel("input.view",
                                                                                                    fluidRow(column(11, DT::dataTableOutput("indextable")),column(1,"")))),
                                                                          tabPanel("Vaccination Trends", value = "t_vac",
                                                                                   fluidRow(column(width = 11,h4(strong(htmlOutput("content_vac"))))),
                                                                                   fluidRow(column(width = 10, "Select location in left VIP Ranking table",
                                                                                                   style='font-family:Avenir, Helvetica;font-size:30;text-align:left'),
                                                                                            column(2,
                                                                                                   dropMenu(
                                                                                                     circleButton(label = "", inputId='info3',icon = icon('info')),
                                                                                                     h4(strong('Vaccination Trends')),
                                                                                                     h5('Recommended vaccinations and the date they began to be used can differ between countries. This visual shows how vaccination coverage has changed over time in a particular location.'),
                                                                                                     placement = "bottom",
                                                                                                     arrow = TRUE))),
                                                                                   fluidRow(column(8, " ", style='padding:10px;')),
                                                                                   radioButtons("vaccine_plot","Plot type:", choices = c("Time Series of Vaccine Coverage" ="line_trend","Single Year Vaccine Coverage"="bar_plot"),inline = TRUE),
                                                                                   fluidRow(column(11,plotlyOutput("all_vaccine_plot",height = "50vh")),column(1,""))),
                                                                          tabPanel("Mortality & Disability Trends",value = "d_vac",
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
                                                                                   fluidRow(column(10, " ", style='padding:30px;'),
                                                                                            column(2,
                                                                                                   dropMenu(
                                                                                                     circleButton(label = "", inputId='info5',icon = icon('info')),
                                                                                                     h4(strong('Times Series of Number of Years Lived with Disability')),
                                                                                                     h5('Can also be considered total years lived with less than ideal health. This measure takes into account prevelance and duration of illnesses.'),
                                                                                                     placement = "bottom",
                                                                                                     arrow = TRUE))),
                                                                                   plotlyOutput("all_disability_plot", height = "35vh")),
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
                                                                                   fluidRow(
                                                                                     column(3,
                                                                                            h4("BCG"),
                                                                                            helpText("Bacillus Calmette–Guérin"),
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
                                                                                            DT::dataTableOutput("RotaCtable"))
                                                                                   )),
                                                                          tabPanel("Data Explorer",
                                                                                   fluidRow(column(5, " ", style='padding:5px;')),
                                                                                   fluidRow(column(9, radioButtons("dataset","Choose Dataset", choices = c("Improvement Index"= "improvement index","Vaccine Trends" = "vaccine trends","Disease Trends" = "disease trends","Data Dictionary" = "data dictionary"),inline = TRUE)),
                                                                                            column(3, style = "margin-top: 10px;",div(downloadButton("download","Download Raw Data"),style='float:right'))),
                                                                                   fluidRow(column(width = 9, "Download custom datasets from the variables that were used to create visuals on the previous tabs. ",
                                                                                                   style='font-family:Avenir, Helvetica;font-size:30;text-align:left')),
                                                                                   fluidRow(column(12,DT::dataTableOutput("alldatatable") %>% withSpinner(color="#4b2e83")))
                                                                          )
                                                              ),
                                                              tags$head(tags$style('ul.nav.nav-tabs { overflow-y: hidden; display: flex;}')
                                                              ), 
                              ))))
                   ,
                   tabPanel("Reports",
                            mainPanel(
                              tabsetPanel(tabPanel("Community Leaders",
                                                   tags$div(id = "reportsPage", 
                                                            h4(id = "reportsHeader","The Role of Community Leaders to Reduce Vaccine Hesitancy"),
                                                            h5(id = "reportsText", "Vaccine hesitancy is a growing public health concern defined by an individual’s refusal or delayed acceptance of a readily available vaccine. Vaccine hesitancy has many contributing causes including but not limited to, historical, political, and socio-cultural factors; addressing hesitancy is a key factor in successfully increasing vaccination coverage globally Engaging and educating trusted community leaders has been explored as a strategy to reduce vaccine hesitancy and increase a community’s vaccine uptake1. The WHO has created a framework called “Human-centered design for tailoring immunization programs”, in which they stress community engagement as a key pillar for building trust between healthcare systems and individuals 2. The COVID-19 pandemic has served as a large-scale experiment in quickly vaccinating communities against SARS-Cov-2. This surge for vaccination required the development of strategies to reduce vaccine hesitancy around the globe, particularly in underserved and vulnerable communities. Two organizations in particular, Breakthrough Action – a partnership led by Johns Hopkins Center for Communication Programs, and The Partnership for Healthy Cities supported by Bloomberg Philanthropies in partnership with the World Health Organization (WHO) and Vital Strategies have sponsored programs in cities and communities across the globe with the goal of educating and engaging community leaders to decrease vaccine hesitancy. The Partnership for Healthy Cities has supported community lead programs in 18 cities across the world with a focus on educating local leaders on the importance of vaccination against COVID-19 3. To illustrate this further a few campaigns associated with Breakthrough Action and the Partnership for Healthy Cities will be highlighted."),
                                                            img(src="Community_Leaders_Picture1.jpg", width= "700px"),
                                                            h6("Credit: Vital Strategies"),
                                                            h4(id = "reportsHeader", "Buenos Aires, Argentina: The peer-to-peer “Butterfly Effect” project vaccinates people experiencing homelessness"),
                                                            h5(id = "reportsText", "In Buenos Aires a campaign was created to increase vaccine coverage of the unhoused population within the city. The unhoused due to various socioeconomic and health factors have high levels of mistrust towards health systems and healthcare personnel. To address this, the city formed a group of 44 formerly unhoused people to develop an outreach program based on one-to-one communication outreach strategies. The campaign successfully vaccinated 10,000 people over the course of 4 months and provided new insights into future outreach programs. The researchers found that reducing barriers to getting vaccinated such as the need for completing online forms or showing ID led to a greater vaccine uptake among the unhoused population 4. See the link below for the full story from Vital Strategies blog."),
                                                            h4(id = "reportsHeader", "Engaging Religious Leaders to Boost COVID-19 Vaccination"),
                                                            img(src="Community_Leaders_Picture2.jpg", width= "700px"),
                                                            h6("Malawi's Bishop Martin Mtumbuka. Credit: Lusayo Banda"),
                                                            h5(id = "reportsText", "Johns Hopkins Center for Communications Programs (CCP) has worked alongside religious leaders in Africa to combat vaccine misinformation that has proliferated throughout many communities. Johns Hopkins CCP has worked in Malawi, Liberia, and Nigeria to engage and educate faith leaders in local communities. In Malawi 144 leaders were given education sessions on COVID-19 prevention and vaccination. This training helps faith leaders create campaigns to increase vaccine demand in their local communities. In Nigeria, a two-day forum was hosted in which state and local health officials discussed common rumors and myths surrounding the vaccines and gave leaders talking points they could bring back with them to better address concerns surrounding vaccine safety.  In Liberia a vaccine ambassador program was established to train a range of influential community leaders to be educators on COVID-19 prevention and vaccination. The ambassadors have gone on to host community engagement events and talk on local radio stations 5."),
                                                            h4(id = "reportsHeader", "Battling Covid-19 Vaccine Misinformation Through Targeted Community Dialogues"),
                                                            img(src="Community_Leaders_Picture3.png", width= "700px"),
                                                            h6("Credit: Breakthrough ACTION"),
                                                            h5(id = "reportsText", "In Guinea, Breakthrough Action partnered with the National Agency for Health Security of Guinea to create programs to combat misinformation and rumors surrounding the pandemic and vaccines. Community dialogue sessions took in place in which Public Health Officials were able to gain feedback from local leaders about their community’s special needs and conditions regarding vaccine hesitancy while also equipping leaders with tools to combat these hesitancies. These sessions were well received and many leaders felt that they had been given the resources needed to better educate their communities about the nature of the pandemic as well as the importance of getting vaccinated 6."),
                                                            h4(id = "reportsHeader", "Key Lessons"),
                                                            h5(id = "reportsText", "These campaigns addressing COVID-19 prevention and vaccine hesitancy in local communities can provide more broadly applicable lessons for combatting hesitancy and increasing vaccine dosage demand in communities. Understanding and using existent community structures and leaders can enable Public Health Officials to make efficient usage of resources and to better understand the material reality of the on the ground conditions inside these communities.  Community engagement meetings can serve as a way for Public Health Officials to continuously monitor and understand trends in public opinion and test out new ways of engaging with the population on these issues while receiving real time feedback. Training community leaders to combat myths surrounding vaccines can be an effective strategy for reaching community members with an already trusted voice. It has long been a mistake of Western Public Health Agencies to not engage enough with community leaders and instead take a paternalistic approach of global health that minimizes that opportunity for local voices to have a say in how Global Health funds are being utilized. Both programs seek to change this old paradigm by participating in active conversations with communities and their leaders, enabling them to take a more community-based approach to Public Health programs. Equipping members of underserved communities to become advocates in their own communities can create a “social butterfly effect” which spreads vaccination talking points throughout the community, creating an organic and eventually self-perpetuating community-based conversation about vaccination. With vaccine hesitancy on the rise in many parts of the globe it is paramount that leaders in Global Health take note of these successful campaigns to increase vaccine demand in underserved communities across the global and they we seek to apply these lessons going into the future."),
                                                            h4(id = "reportsHeader", "References"),
                                                            h5("1.	Dubé, E. et al. Vaccine hesitancy. Hum Vaccin Immunother 9, 1763–1773 (2013).",
                                                               tags$br(), tags$br(),
                                                               "2.	World Health Organization & Fund (UNICEF), U. N. C. Human-centred design for tailoring immunization programmes. (World Health Organization, 2022).",
                                                               tags$br(), tags$br(),
                                                               "3.	The Partnership for Healthy Cities supports COVID-19 Vaccine Outreach Efforts of 18 Cities in Africa, Asia, and Latin America. https://www.who.int/news/item/06-05-2021-the-partnership-for-health-cities-supports-covid-19-vaccine-outreach-efforts-of-18-cities-in-africa-asia-and-latin-america.",
                                                               tags$br(), tags$br(),
                                                               "4.	Trusted Voices for COVID-19: Engaging community leaders to overcome vaccine hesitancy. Vital Strategies https://www.vitalstrategies.org/vital-stories-trusted-voices-for-covid-19-engaging-community-leaders-to-overcome-vaccine-hesitancy/.",
                                                               tags$br(), tags$br(),
                                                               "5.	Engaging Religious Leaders to Boost COVID-19 Vaccination. Johns Hopkins Center for Communication Programs https://ccp.jhu.edu/2022/05/02/religious-leaders-covid/ (2022).",
                                                               tags$br(), tags$br(),
                                                               "6.	meeichild. Battling COVID-19 Vaccine Misinformation Through Targeted Community Dialogues. Breakthrough ACTION and RESEARCH https://breakthroughactionandresearch.org/battling-covid-19-vaccine-misinformation-through-targeted-community-dialogues/ (2022)."
                                                            )
                                                   )
                              ),
                              tabPanel("Canada",
                                       tags$div(id = "reportsPage",
                                                h2(id = "reportsHeader", "Canada"),
                                                h5(id = "reportsText", "Canada is one of the top 28 richest nations and boasts one of the best healthcare systems that in the world. Despite this, its childhood vaccination coverage is only around 84% which is far behind other developed countries such as the UK and the USA. (1) It was also found that approximately 20% of the population is vaccine apprehensive, and approximately 5% of the populace holds anti-vaccination sentiments. (2)", 
                                                   tags$br(), tags$br(), "High rates of immunization are critical for preventing the spread of many infectious diseases. The immunizations for children that are recommended include the DPTP-Hib, Rotavirus, Pneumococcal, Meningococcal, MMR, Varicella, Hepatitis B, dTap, and HPV vaccines. (3)"),
                                                h4(id = "reportsHeader", "Challenges in vaccine coverage"),
                                                h5(id = "reportsText", "Low vaccine coverage rates in Canada may be ascribed to parents' reluctance to vaccinate their children due to a lack of clarity and misinformation. An interesting pattern observed is that parents with high levels of education, namely those with postgraduate degrees, frequently lack faith in the efficacy of vaccinations and are worried more about potential for  adverse reactions, such as anaphylaxis or paralysis caused by vaccines. On the other hand, parents of children with education levels below secondary school frequently know little about immunization programs and worry about their potential side effects. (4) Another factor that contributed to incomplete immunization (children not receiving  all of the recommended doses) was the parents' marital status. The children of single parents often did not receive the recommended number of doses of vaccines. (5) It's also crucial to recognize that philosophical and religious opposition to immunizations still exists and contributes to around 10% of vaccine hesitancy. (6)",
                                                   tags$br(), tags$br(), "A significant problem is frequently posed by the heterogeneity of immunization programs in each of Canada's territories. People frequently have questions about whether or not the neighborhood hospitals or clinics provide free vaccinations. Additionally, a lot of individuals are worried about the potential out-of-pocket cost and are often uninformed of the free immunization program that the Canadian government provides.", 
                                                   tags$br(), tags$br(), "Finally, the COVID outbreak in 2019 also contributed to a decline in immunization rates by forcing a temporary suspension of immunization campaigns. (7) However, in the years 2020 and 2021, this was improved by giving infants shorter appointments and by adhering to guidelines including wearing PPE kits and screening for COVID-19."),
                                                h4(id = "reportsHeader", "Ways to improve vaccine coverage: "),
                                                h5(id = "reportsText", "According to one study, Canadians most frequently regarded the sources of information given by their healthcare practitioners as reliable. (8) As a result, regular health checkups should include educating patients on the benefits of vaccination in order to foster a positive attitude toward vaccination. Another study discovered that pharmacists could be trained to educate people about vaccination, its side effects, and how to obtain them. (8) Raising awareness about the actual negative effects of vaccines aids in the prevention of the spread of false information about vaccines, as uninformed people frequently believe that the vaccine failed to work and caused their symptoms.",
                                                   tags$br(), tags$br(), "Additionally, in the era of infodemics on social media, it would also be beneficial if videos, reels, and podcasts of pediatricians talking about vaccinations are promoted more on social media, so people could have access to high-quality unbiased information.",
                                                   tags$br(), tags$br(), "As healthcare professionals, it would beneficial if a set of dedicated staff could handle immunization requests. This would cut down on hospital wait times, which are a major obstacle for parents. The same group of nurses should also be in charge of sending out reminders through messages and calls. It has been observed that this tailored immunization delivery has increased immunization rates. (10) The significant issue of incomplete immunization among children could be addressed by extending the hours of local clinics. In addition, it would be helpful if the health system made better attempts to inform parents about the next date for immunizations of their children and ensured that the parents followed the schedule.",
                                                   tags$br(), tags$br(), "When parents refuse vaccination for their children due to religious beliefs, it is critical that healthcare workers respect those beliefs. However, healthcare professionals could inform them with respect of the significance of immunizations. Vaccination coverage is also high in cultures that promote the collective benefits of vaccinations. It would be extremely beneficial for health care professionals to educate parents on the significance of herd immunity, explaining that by immunizing their children, they are protecting immunocompromised children. Thereby inspiring them to grow a strong sense of social responsibility.",
                                                   tags$br(), tags$br(), "Thus, in order to increase vaccination coverage for childhood vaccines and close immunity gaps, prosocial nudges, awareness about the importance of vaccination, increased access, and promotion of reliable sources of vaccination information, would be required. (11) Listed below are few trusted sources about vaccination provided by the Canadian government:",
                                                   tags$br(), tags$ul(tags$li(tags$a("https://caringforkids.cps.ca/handouts/immunization/vaccination_and_your_child", target = "_blank", href = "https://caringforkids.cps.ca/handouts/immunization/vaccination_and_your_child")),
                                                                      tags$li(tags$a("https://immunize.ca/", target = "_blank", href = "https://immunize.ca/")),
                                                                      tags$li(tags$a("https://www.canada.ca/en/public-health/services/diseases/coronavirus-disease-covid-19/vaccines.html", target = "_blank", href = "https://www.canada.ca/en/public-health/services/diseases/coronavirus-disease-covid-19/vaccines.html")),
                                                   )),
                                                h4(id = "reportsHeader", "References"),
                                                h5("1. UNICEF Office of Research, Innocenti Report Card 11 (2013) Child well-being in rich countries: A comparative overview. <www.unicef-irc.org/publications/pdf/rc11_eng.pdf> ",
                                                   tags$br(), tags$br(),
                                                   "2. Stratoberdha D, Gobis B, Ziemczonek A, Yuen J, Giang A, Zed PJ. Barriers to adult vaccination in Canada: A qualitative systematic review. Canadian Pharmacists Journal / Revue des Pharmaciens du Canada. June 2022. doi:10.1177/17151635221090212 ",
                                                   tags$br(), tags$br(),
                                                   "https://immunize.ca/",
                                                   tags$br(), tags$br(),
                                                   "4. Carpiano RM, Polonijo AN, Gilbert N, Cantin L, Dubé E. Socioeconomic status differences in parental immunization attitudes and child immunization in Canada: Findings from the 2013 Childhood National Immunization Coverage Survey (CNICS). Prev Med. 2019; 123:278-287. doi: 10.1016/j.ypmed.2019.03.033",
                                                   tags$br(), tags$br(),
                                                   "5. Boulianne N, Deceuninck G, Duval B, Lavoie F, Dionne M, Carsley J, Valiquette L, Rochette L, De Serres G. Why are some children incompletely vaccinated at the age of 2? Can J Public Health 2003; 94:218-23; PMID:12790498",
                                                   tags$br(), tags$br(),
                                                   "6. Gilbert NL, Gilmour H, Wilson SE, Cantin L. Determinants of non-vaccination and incomplete vaccination in Canadian toddlers. Hum Vaccin Immunother. 2017;13(6):1-7. doi:10.1080/21645515.2016.1277847",
                                                   tags$br(), tags$br(),
                                                   "7.MacDonald SE, Paudel YR, Kiely M, et al. Impact of the COVID-19 pandemic on vaccine coverage for early childhood vaccines in Alberta, Canada: a population-based retrospective cohort study. BMJ Open.m2022;12(1):e055968. Published 2022 Jan 25. doi:10.1136/bmjopen-2021-055968 ",
                                                   tags$br(), tags$br(),
                                                   "8.  Brenner RA, Simons-Morton BG, Bhaskar B, et al. Prevalence and   predictors of immunization inner-city infants: a birth cohort study. Pediatrics. 2001;108(3):661–670.",
                                                   tags$br(), tags$br(),
                                                   "9. Lisenby KM, Patel KN, Uichanco MT. The role of pharmacists in addressing vaccine hesitancy and the measles outbreak. J Pharm Pract. 2021;34(1):127-32. ",
                                                   tags$br(), tags$br(),
                                                   "10. Zimmerman RK, Nowalk MP, Raymund M, et al. Tailored interventions to increase influenza vaccination in neighborhood health centers serving the disadvantaged. Am J Pub Health. 2003;93(10):1699–1705", 
                                                   tags$br(), tags$br(),
                                                   "11. Mah CL, Guttmann A, McGeer A, Krahn M, Deber RB. Compulsory school-entry vaccination laws and exemptions: who is opting out in ontario and why does it matter? Healthc Policy. 2010;5(4):37-46.",  
                                                   tags$br(), tags$br(),
                                                   "12. Scheifele DW, Halperin SA, Bettinger JA. Childhood immunization rates in Canada are too low: UNICEF. Paediatr Child Health. 2014;19(5):237-238. doi:10.1093/pch/19.5.237",
                                                   tags$br(), tags$br(),
                                                   "13. Betsch, C., Böhm, R., Korn, L. et al. On the benefits of explaining herd immunity in vaccine advocacy. Nat Hum Behav 1, 0056 (2017). https://doi.org/10.1038/s41562-017-0056")  
                                       )
                              ),
                              tabPanel("Document 1",
                                       # tags$iframe(embed(src="Canada Childhood Vaccination FInal.pdf", width= "100%", height="400px"))
                                       tags$iframe(style = "height:400px; width:100%; scrolling=yes", src = "Canada_Childhood_Vaccination_Final.pdf")
                              )
                              )
                            )),
                   tabPanel("Sample Report",
                            sidebarPanel(
                              h4(strong("Sample Report: for Nigeria")),
                              fluidRow(column(6, " ", style='padding:70px;')),
                              fluidRow(column(12,h5(strong(textOutput("report_nig_title"))))),
                              fluidRow(column(6, " ", style='padding:10px;')),
                              fluidRow(column(12,textOutput("report_nig_body"))),
                              fluidRow(column(6, " ", style='padding:90px;')),
                              tags$head(tags$style("{color: #0060bf; font-size: 20px;}")),
                              downloadButton(
                                outputId = "report",
                                label = "Download Report"
                              ),
                              width = 4),
                            mainPanel(
                              tabsetPanel(id="t2",
                                          tabPanel("Comparison",value = "comp_index",
                                                   fluidRow(column(8, " ", style='padding:20px;')),
                                                   fluidRow(column(4,radioButtons("sdi_group_present_comp","2019 SDI Group", choices = c("All"="all","Low" ="low","Medium" = "medium","High" = "high"),selected="low",inline = TRUE)),
                                                            column(4,selectInput("region", "Regions", choices = unique(index_results$region))),
                                                            column(4,selectInput(
                                                              inputId = "my_multi",
                                                              label = "Search Country :",
                                                              choices = unique(merged_data_for_vacii_sdi$location),
                                                              selected = "Nigeria",
                                                              selectize = TRUE,
                                                              multiple=TRUE))),
                                                   fluidRow(column(8, " ", style='padding:20px;')),
                                                   fluidRow(column(12, plotlyOutput("index_trend_plot_com",height = "40vh"))),column(1,"")),
                                          tabPanel("Vaccination Trends",value = "report_vt",
                                                   fluidRow(column(width = 11,h4(strong("Nigeria")))),
                                                   fluidRow(column(8, " ", style='padding:10px;')),
                                                   fluidRow(column(11,plotlyOutput("nigeria_vaccine_plot",height = "55vh")),column(1,""))),
                                          tabPanel("Mortality and Disability Trends",value = "report_md",
                                                   fluidRow(column(width = 11,h4(strong("Nigeria")))),
                                                   fluidRow(column(8, " ", style='padding:10px;')),
                                                   fluidRow(column(12,plotlyOutput("nigeria_disability_plot", height = "45vh")))),
                                          tabPanel("Vaccination and Corresponding Diseases Trends",value = "report_vcdt",
                                                   fluidRow(column(width = 11,h4(strong("Nigeria")))),
                                                   fluidRow(column(width = 11,h5(strong("Selected Vaccination: MCV1")))),
                                                   fluidRow(column(8, " ", style='padding:10px;')),
                                                   fluidRow(column(12,plotlyOutput("selected_nigeria_vac_dis_plot", height = "50vh")))
                                          )))),
                   tabPanel("About This Site", 
                            tags$div(id = "aboutPage",
                                     tags$h4(strong("Background"), id = "aboutHeader"),
                                     tags$h6(id = "aboutText", "Improving routine vaccination coverage is an important global health goal. Reaching and maintaining high vaccination coverage helps avoid death and illness from many preventable diseases, especially in children.",
                                             tags$br(),tags$br(),"The factors that drive vaccination coverage are many and can differ between countries. The Global Vaccine Index Project is an attempt to organize data on several of these factors into a single index that allows a quick comparison between locations and across time. This project builds on prior research that identified factors that contribute to higher vaccination rates in multiple low and middle income countries.",
                                             tags$br(),tags$br(),"As part of the Global Vaccine Index Project, this website offers data and resources to people interested in learning more about vaccination trends, vaccine-preventable diseases, and information on the importance of vaccines to promote health and well-being in communities around the world. "),
                                     tags$br(),tags$br(),
                                     tags$h4(strong("Website Last Updated"), id = "aboutHeader"),
                                     h6(id = "aboutText",paste0(Sys.Date())),
                                     tags$br(),tags$br(),
                                     tags$h4(strong("Data Last Updated"), id = "aboutHeader"),
                                     h6(id ="aboutText", paste0("2022-11-03")),
                                     tags$br(),tags$br(),
                                     tags$h4(strong("Sources"), id = "aboutHeader"),
                                     h4(id = "aboutText", "1. Global Burden of Disease Collaborative Network.",
                                        tags$a("Global Burden of Disease Study 2020, Release 1 (GBD 2020 R1) Routine Childhood Vaccination Coverage 1980-2019 [Internet]",target="_blank", href="https://ghdx.healthdata.org/record/ihme-data/gbd-2020-routine-childhood-vaccination-coverage-1980-2019"),
                                        "Institute for Health Metrics and Evaluation (IHME); 2021 [cited 2021 Jul 21].",
                                        tags$br(), tags$br(),
                                        "2. Barau I, Zubairu M, Mwanza MN, Seaman VY. Improving polio vaccination coverage in Nigeria through the use of geographic information system technology. J Infect Dis. 2014 Nov 1;210 Suppl 1:S102-110.",
                                        tags$br(), tags$br(),
                                        "3. National Primary Health Care Development Agency.",
                                        tags$a("Nigeria: National Routine Immunization Strategic Plan 2013-2015 | NITAG RESOURCE CENTER [Internet].",target="_blank",href="https://www.jhsph.edu/research/centers-and-institutes/ivac/resources/Nigeria-NRISP-Technical-Policy.pdf"),
                                        "2013 [cited 2021 Sep 22]",
                                        tags$br(), tags$br(),
                                        "4. Oku A, Oyo-Ita A, Glenton C, Fretheim A, Ames H, Muloliwa A, et al. Communication strategies to promote the uptake of childhood vaccination in Nigeria: a systematic map. Glob Health Action. 2016 Feb 12;9(1).",
                                        tags$br(), tags$br(),
                                        "5. Ado JM, Etsano A, Shuaib F, Damisa E, Mkanda P, Gasasira A, et al. Progress toward poliomyelitis eradication in Nigeria. J Infect Dis. 2014 Nov 1;210 Suppl 1:S40-49.",
                                        tags$br(), tags$br(),
                                        "6. Gunnala R, Ogbuanu IU, Adegoke OJ, Scobie HM, Uba BV, Wannemuehler KA, et al. Routine Vaccination Coverage in Northern Nigeria: Results from 40 District-Level Cluster Surveys, 2014-2015. Borrow R, editor. PLOS ONE. 2016 Dec 9;11(12):e0167835.",
                                        tags$br(), tags$br(),
                                        "7. Oleribe O, Kumar V, Awosika-Olumo A, Taylor SD.",
                                        tags$a("Individual and socioeconomic factors associated with childhood immunization coverage in Nigeria. Pan Afr Med J [Internet].",target="_blank",href="https://panafrican-med-journal.com/content/article/26/220/full/"),
                                        "2017 [cited 2021 Sep 27];26.")
                            )
                   )
)



# server portion of R shiny app -------------------------------------------


server <- function(input, output, session) {
  # print(input)
  year <- reactive({
    merged_data_for_vacii_sdi[merged_data_for_vacii_sdi$year == input$year,]
  })
  
  output$yeartitle <- renderText({ 
    paste0("Year ", input$year)
  })
  
  observeEvent(year(),{
    choices = sort(unique(merged_data_for_vacii_sdi$year))
    updateSliderInput(session,'year', value=unique(year()$year),
                      min = min(as.numeric(choices)), max = max(as.numeric(choices)), step = 1)
  })
  
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
  
  regionstable <- reactive({
    req(input$region_table)
    if (input$region_table =='All'){
      sdi_group_present()
    }
    else{
      filter(sdi_group_present(),region == input$region_table)
    }
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
  
  
  output$table = DT::renderDataTable({
    sdi_rank_table<-regionstable()[,c("location","result","sdi_group_present")]
    # print("sorting")
    # print(sdi_rank_table)
    sdi_rank_table$rank <- NA
    sdi_rank_table$rank = dense_rank(desc(sdi_rank_table$result))
    sdi_rank_table <- sdi_rank_table[,c("rank","location","result","sdi_group_present")]
    sdi_rank_table$result = round(sdi_rank_table$result,3)
    colnames(sdi_rank_table) <- c('Rank','Location','VIP Index', "2019 SDI Group")
    sdi_rank_table$`2019 SDI Group` <- toTitleCase(sdi_rank_table$`2019 SDI Group`)
    sdi_rank_table<-sdi_rank_table[order(sdi_rank_table$Rank),]
    true_false_formatter <-
      formatter("span",
                style = x ~ formattable::style(
                  font.weight = "bold",
                  color = ifelse(x == "High", "forestgreen", ifelse(x == "Low", "red", "black"))
                ))
    
    formattable(
      sdi_rank_table,
      list(
        ## a coloured bar with length proportional to value
        'Vaccination Improvement Index' = color_tile("white", "#569eca"),
        #'SDI' = color_tile("white", "pink"),
        ## use custom formatter for TRUE/FALSE values
        '2019 SDI Group' = true_false_formatter
      )
    ) %>%
      as.datatable(rownames = FALSE, 
                   selection = list(mode = 'multiple',target="cell", selected = matrix(c(0, 1), nrow = 1,ncol = 2)), 
                   options = list(paging = FALSE,
                                  scrollY = '400px', 
                                  scrollY=TRUE, 
                                  autoWidth = FALSE,
                                  ordering = FALSE,
                                  #dom = 'Bfrtip',
                                  pageLength=1000)
      )
  })
  
  output$index_trend_plot_multi <- renderPlotly({
    if (input$sdi_group_present == 'medium'){
      country = 'Uruguay'
    }
    else if (input$sdi_group_present == 'low'){
      country = 'Eswatini'
    }
    else{
      country = 'United States of America'
    }
    index_trend_data <- filter(index_results,location == country)
    fig_a <- plot_ly(index_trend_data, x = ~year)
    fig_a <- fig_a  %>% add_trace(y=~result,type='scatter', name = country, mode = 'lines', line = list(color = 'rgba(49,130,189, 1)',width=2))
    fig_a <- fig_a %>% 
      layout( autosize = T,
              title = paste0("Time Series of VIP Index"), 
              showlegend = TRUE,
              xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
              yaxis = list(title = "VIP Index",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
    fig_a
  })
  
  observeEvent(regionstable(),{
    # print("map data")
    map_data <- regionstable()
    # print(map_data)
    
    indicatorsdata <- reactive({
      req(input$indicators)
      indicators <-as.data.frame.matrix(map_data[,-c("gbd_location_id","iso_num_code")])
    })
    
    observeEvent(indicatorsdata(),{
      output$indicator_map <- renderPlotly({ #### Improvement Indicator
        height  = 1500
        units="px"
        
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
        
        fig <- plot_ly(indicatorsdata())
        if (input$indicators == "Socio-demographic Index"){
          fig <- fig %>% 
            add_trace(
              z = ~sdi, color = ~sdi, type = 'choropleth', locations = ~iso_code, colors="Purples", 
              text = ~paste0(location),
              marker = list(line = l)) %>%
            colorbar(title = paste0("Socio-demographic \n Index"))
        }
        # resultindex-1
        #else if (input$indicators == "Development Assistance Per Total Health Spending Categorical"){
        # fig <- fig %>% 
        #  add_trace(
        #   z = ~dah_per_the_mean_cat, color = ~dah_per_the_mean_cat, type = 'choropleth', locations = ~iso_code, colors="Purples", 
        #  text = ~paste0(location),
        # marker = list(line = l))
        #}
        else if (input$indicators == "Eligibility to Receive DAH"){
          colfunc <- colorRampPalette(c("#A2A2A1FF", "#4b2e83"))
          fig <- fig %>% 
            add_trace(
              z = ~as.numeric(indicatorsdata()$dah_eligible), type = 'choropleth',locations = ~iso_code, colors=colfunc(2),
              text = ~paste0(location),
              marker = list(line = l))%>% 
            colorbar(title = paste0("Eligibility to Receive DAH",'<br>',"(1: True, 0: False)"),brks=c(0,1),labels=c("True",' ',"False"))
        }
        else if (input$indicators == "Total Health Spending per Person"){
          colfunc <- colorRampPalette(c("#C7C5E7", "#4b2e83"))
          fig <- fig %>% 
            add_trace(
              z = ~the_per_cap_mean, color = ~the_per_cap_mean, type = 'choropleth', locations = ~iso_code, colors=colfunc(10),
              text = ~paste0(location),
              marker = list(line = l)) %>%
            colorbar(title = paste0("Mean Spending \n per Person"))
        }
        else if (input$indicators == "Government Health Spending per Total Health Spending"){
          fig <- fig %>% 
            add_trace(
              z = ~ghes_per_the_mean, color = ~ghes_per_the_mean, type = 'choropleth', locations = ~iso_code,  colors="Purples", 
              text = ~paste0(location),
              marker = list(line = l)) %>%
            colorbar(title = paste0("Mean Government \n Spending"))
        }
        #add resultindex2
        else if (input$indicators == "Development Assistance per Person"){
          colfunc <- colorRampPalette(c("#C7C5E7", "#4b2e83"))
          fig <- fig %>% 
            add_trace(
              z = ~dah_per_cap_ppp_mean, color = ~dah_per_cap_ppp_mean, type = 'choropleth', locations = ~iso_code, colors=colfunc(10),
              text = ~paste0(location),
              marker = list(line = l)) %>%
            colorbar(title = paste0("Mean Development \n Assistance per \n Person"))
        }
        
        else if (input$indicators == "HAQI"){
          fig <- fig %>% 
            add_trace(
              z = ~haqi, color = ~haqi, type = 'choropleth', locations = ~iso_code, colors="Purples", 
              text = ~paste0(location),
              marker = list(line = l)) %>%
            colorbar(title = paste0("HAQI"))
        }
        else if (input$indicators == "Corruption Perception Index"){
          fig <- fig %>% 
            add_trace(
              z = ~cpi, color = ~cpi, type = 'choropleth', locations = ~iso_code, colors="Purples",
              text = ~paste0(location),
              marker = list(line = l)) %>%
            colorbar(title = paste0("Corruption \n Perception Index"))
        }
        else if (input$indicators == "Skilled Attendants at Birth"){
          fig <- fig %>% 
            add_trace(
              z = ~perc_skill_attend, color = ~perc_skill_attend, type = 'choropleth', locations = ~iso_code, colors="Purples", 
              text = ~paste0(location),
              marker = list(line = l)) %>%
            colorbar(title = paste0("Skilled \n Attendants at \n Birth"))
          
        }
        else if (input$indicators == "Immigrant Population (%)"){
          fig <- fig %>% 
            add_trace(
              z = ~imm_pop_perc, color = ~imm_pop_perc, type = 'choropleth', locations = ~iso_code, colors="Purples", 
              text = ~paste0(location),
              marker = list(line = l)) %>%
            colorbar(title = paste0("Immigrant \n Population (%)"))
        }
        else if (input$indicators == "Urbanicity (%)"){
          fig <- fig %>% 
            add_trace(
              z = ~perc_urban, color = ~perc_urban, type = 'choropleth', locations = ~iso_code, colors="Purples", 
              text = ~paste0(location),
              marker = list(line = l)) %>%
            colorbar(title = paste0("Urbanicity (%)"))
        }
        
        fig <- fig%>% #### Improvement Indicator
          layout(
            autosize = T,
            title = paste0(input$year," Global VIP Index Component Mapper"),
            mapbox=list(
              style="carto-positron",
              center = list(lon = -90, lat = 80)),
            geo = g)
      })
    })
    
    map_data$hover <- with(map_data, paste(location, '<br>','<br>',
                                           ###########################add 2
                                           "Eligibility to Receive DAH: " ,dah_eligible,'<br>',
                                           #"Development Assistance Per Total Health Spending Categorical: ",round(dah_per_the_mean_cat,3),'<br>',
                                           ##########################
                                           
                                           "Socio-demographic Index: ", round(sdi,3),'<br>',
                                           "Total Health Spending per Personn: ",round(the_per_cap_mean,3),'<br>',
                                           "Government Health Spending per Total Health Spending: ",round(ghes_per_the_mean,3),'<br>',
                                           
                                           #####################
                                           "Development Assistance per Person: ", round(dah_per_cap_ppp_mean,3),'<br>',
                                           ######################
                                           
                                           "HAQI: ",round(haqi,3),'<br>',
                                           "Corruption Perception Index:", round(cpi,3),'<br>',
                                           "Skilled Attendants at Birth: ",round(perc_skill_attend,3),'<br>',
                                           "Immigrant Population (%): ",round(imm_pop_perc,3),'<br>',
                                           "Urbanicity (%)",round(perc_urban,3)
                                           #"Agreement Vaccines are Safe",round(mean_agree_vac_safe,3),'<br>',
                                           #"Agreement Vaccines are Important",round(mean_agree_vac_important,3),'<br>',
                                           #"Agreement Vaccines are Effective: ",round(mean_agree_vac_effective,3)
    ))
    output$index_map <- renderPlotly({
      height  = 1500
      units="px"
      
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
      
      
      fig <- plot_ly(map_data)
      fig <- fig %>% 
        add_trace(
          z = ~result, color = ~result, type = 'choropleth',
          text = ~hover, locations = ~iso_code, colors="Blues", 
          marker = list(line = l))%>%
        colorbar(title = 'VIP Index')%>% 
        layout(
          autosize = T,
          title = paste0(input$year," Global VIP Index Mapper"),
          mapbox=list(
            style="carto-positron",
            center = list(lon = -90, lat = 80)),
          geo = g)
    })
    ##############test##########t##########t##########t##########t
    reportindextrendplot<-reactive(
      fig_a <- plot_ly(index_results %>%  filter(location %in% input$my_multi), x = ~year) %>% 
        add_trace(y=~result,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2) %>% 
        layout( autosize = T,
                title = paste0("Time Series of Vaccine Improvement Index"), 
                showlegend = TRUE,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = "Vaccine Improvement Index",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
    )
    
    ##############test##########t##########t##########t##########t
    output$index_trend_plot_com <- renderPlotly({
      reportindextrendplot()
    })
    
    output$indicator_trend_plot_multi <- renderPlotly({
      if (input$sdi_group_present == 'medium'){
        country = 'Uruguay'
      }
      else if (input$sdi_group_present == 'low'){
        country = 'Eswatini'
      }
      else{
        country = 'United States of America'
      }
      
      indicator_trend_data <- filter(index_results,location == country)
      fig_a <- plot_ly(indicator_trend_data, x = ~year)
      
      if (input$indicators == "Socio-demographic Index"){
        titles = paste0("Time Series of SDI")
        ytitles = "Socio-demographic Index"
        fig_a <- fig_a  %>% add_trace(y=~sdi,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if (input$indicators == "Eligibility to Receive DAH"){
        titles = paste0("Time Series of Eligibility to Receive DAH")
        ytitles = "Eligibility to Receive DAH"
        fig_a <- fig_a  %>% add_trace(y=~dah_eligible,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if (input$indicators == "Total Health Spending per Person"){
        titles=paste0("Time Series of Total Health Spending per Person")
        ytitles = "Total Health Spending per Person"
        fig_a <- fig_a  %>% add_trace(y=~the_per_cap_mean,type='scatter', name =~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if (input$indicators == "Government Health Spending per Total Health Spending"){
        titles=paste0("Time Series of Government Health Spending per Total Health Spending")
        ytitles = "Government Health Spending \n per Total Health Spending"
        fig_a <- fig_a  %>% add_trace(y=~ghes_per_the_mean,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        
      }
      else if (input$indicators == "Development Assistance per Person"){
        titles=paste0("Time Series of Development Assistance per Person")
        ytitles = "Development Assistance per Person"
        fig_a <- fig_a  %>% add_trace(y=~dah_per_cap_ppp_mean,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if (input$indicators == "HAQI"){
        titles=paste0("Time Series of HAQI")
        ytitles = "HAQI"
        fig_a <- fig_a  %>% add_trace(y=~haqi,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
        
      }
      else if (input$indicators == "Corruption Perception Index"){
        titles=paste0("Time Series of Corruption Perception Index")
        ytitles = "Corruption Perception Index"
        fig_a <- fig_a  %>% add_trace(y=~cpi,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if (input$indicators == "Skilled Attendants at Birth"){
        titles=paste0("Time Series of Skilled Attendants at Birth")
        ytitles = "Skilled Attendants at Birth"
        fig_a <- fig_a  %>% add_trace(y=~perc_skill_attend,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if (input$indicators == "Immigrant Population (%)"){
        titles=paste0("Time Series of Immigrant Population (%)")
        ytitles = "Immigrant Population (%)"
        fig_a <- fig_a  %>% add_trace(y=~imm_pop_perc,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      else if(input$indicators == "Urbanicity (%)"){
        titles=paste0("Time Series of Urbanicity (%)")
        ytitles = "Urbanicity (%)"
        fig_a <- fig_a  %>% add_trace(y=~perc_urban,type='scatter', name = ~unique(location), mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      }
      fig_a <- fig_a %>% 
        layout( autosize = T,
                title = titles, 
                showlegend = TRUE,
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = ytitles,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
      fig_a
    })
    
    
    output$indextable = DT::renderDataTable({
      # print("table")
      index_rank_table<-regionstable()[,-c("year","gbd_location_id","iso_code","iso_num_code")]
      index_rank_table$rank <- NA
      index_rank_table$rank = dense_rank(desc(index_rank_table$result))
      #index_rank_table <- index_rank_table[,c("rank","location_name","sdi","sdi_group_present")]
      index_rank_table<-index_rank_table[order(index_rank_table$rank),]
      # print(colnames(index_rank_table))
      # round_columns <- c("result", "sdi", "the_per_cap_mean", )
      index_rank_table$result <- round(index_rank_table$result, 3)
      index_rank_table$the_per_cap_mean <- round(index_rank_table$the_per_cap_mean, 3)
      index_rank_table$ghes_per_the_mean <- round(index_rank_table$ghes_per_the_mean, 3)
      index_rank_table$dah_per_cap_ppp_mean <- round(index_rank_table$dah_per_cap_ppp_mean, 3)
      index_rank_table$haqi <- round(index_rank_table$haqi, 3)
      index_rank_table$perc_skill_attend <- round(index_rank_table$perc_skill_attend, 3)
      index_rank_table$imm_pop_perc <- round(index_rank_table$imm_pop_perc, 3)
      index_rank_table$perc_urban <- round(index_rank_table$perc_urban, 3)
      index_rank_table$mean_agree_vac_safe <- round(index_rank_table$mean_agree_vac_safe, 3)
      index_rank_table$mean_agree_vac_important <- round(index_rank_table$mean_agree_vac_important, 3)
      index_rank_table$mean_agree_vac_effective <- round(index_rank_table$mean_agree_vac_effective, 3)
      
      index_rank_table <-  rename(index_rank_table, 
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
                                  "Immigrant Population (%)" = "imm_pop_perc",
                                  "Urbanicity (%)" = "perc_urban", 
                                  "Improvement Index" = "result", 
                                  "location_id" = "location_id", 
                                  "level" = "level", 
                                  "2019 SDI Group" = "sdi_group_present", 
                                  "Rank" = "rank",
                                  "Agree Vaccines are Safe" = "mean_agree_vac_safe",
                                  "Agree Vaccines are Important" = "mean_agree_vac_important",
                                  "Agree Vaccines are Effective" = "mean_agree_vac_effective",
                                  "Government Trust" = "gov_trust"
      )
      # table_column_order <- c("result", "location", "cpi", "gbd_location_id", "iso_code", "iso_num_code")
      # index_rank_table <- index_rank_table[,c(17,1,13,3,4,5,6,7,8,9,10,11,12)]
      index_rank_table <- index_rank_table[, c("Rank", "Location", "Improvement Index", "Eligibility to Receive DAH", "Socio-demographic Index",
                                               "Total Health Spending per Person", "Government Health Spending per Total Health Spending",
                                               "Development Assistance per Person", "HAQI", "Corruption Perception Index", "Skilled Attendants at Birth",
                                               "Immigrant Population (%)", "Urbanicity (%)", "Agree Vaccines are Safe", "Agree Vaccines are Important", "Agree Vaccines are Effective")]     
      # print(colnames(index_rank_table))
      customGreen0 = "#DeF7E9"
      customGreen = "#71CA97"
      
      true_false_formatter <-
        formatter( "span",
                   style = x ~ formattable::style(
                     font.weight = "bold",
                     color = ifelse(x == TRUE, "forestgreen", ifelse(x == FALSE, "red", "black"))
                   ))
      
      formattable(
        index_rank_table,
        list(
          ## a coloured bar with length proportional to value
          'Improvement Index' = color_tile("white", "#569eca"),
          "Socio-demographic Index" = color_tile("white", "pink"),
          "Eligibility to Receive DAH" = true_false_formatter,
          #"Development Assistance Per Total Health Spending Categorical" = color_tile(customGreen0, customGreen),
          "Total Health Spending per Person"= color_tile(customGreen0, customGreen),
          "Government Health Spending per Total Health Spending"= color_tile(customGreen0, customGreen),
          "HAQI"= color_tile(customGreen0, customGreen),
          "Development Assistance per Person"=color_tile(customGreen0, customGreen),
          "Corruption Perception Index"= color_tile(customGreen0, customGreen),
          "Skilled Attendants at Birth"= color_tile(customGreen0, customGreen),
          "Immigrant Population (%)"= color_tile(customGreen0, customGreen),
          "Urbanicity (%)"= color_tile(customGreen0, customGreen),
          "Agree Vaccines are Safe"= color_tile(customGreen0, customGreen),
          "Agree Vaccines are Important"= color_tile(customGreen0, customGreen),
          "Agree Vaccines are Effective"= color_tile(customGreen0, customGreen)
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
  })
  
  
  # output$indextable = DT::renderDataTable({
  #   index_rank_table<-index_year_input()[,-c("year","gbd_location_id","iso_code","iso_num_code")]
  #   index_rank_table$rank <- NA
  #   index_rank_table$rank = dense_rank(desc(index_rank_table$result))
  #   index_rank_table<-index_rank_table[order(index_rank_table$rank),]
  #   # print("Index_ranktable")
  #   # print(index_rank_table)
  #   
  #   colnames(index_rank_table) = c("Location", "Region","Eligibility to Receive DAH","Socio-demographic Index","Total Health Spending per Person",
  #                                  "Government Health Spending per Total Health Spending","Development Assistance per Person","HAQI","Corruption Perception Index","Skilled Attendants at Birth","Immigrant Population (%)",
  #                                  "Urbanicity (%)","Improvement Index","location_id","level",
  #                                  "2019 SDI Group","Rank")
  #   index_rank_table <- index_rank_table[,c(17,1,13,2,3,4,5,6,7,8,9,10,11)]
  #   
  #   customGreen0 = "#DeF7E9"
  #   customGreen = "#71CA97"
  #   
  #   true_false_formatter <-
  #     formatter("span",
  #               style = x ~ formattable::style(
  #                 font.weight = "bold",
  #                 color = ifelse(x == TRUE, "forestgreen", ifelse(x == FALSE, "red", "black"))
  #               ))
  #   
  #   formattable(
  #     index_rank_table,
  #     list(
  #       ## a coloured bar with length proportional to value
  #       'Improvement Index' = color_tile("white", "#569eca"),
  #       "Socio-demographic Index" = color_tile("white", "pink"),
  #       "Eligibility to receive DAH" = true_false_formatter,
  #       "Total Health Spending per Person"= color_tile(customGreen0, customGreen),
  #       "Government Health Spending per Total Health Spending"= color_tile(customGreen0, customGreen),
  #       "HAQI"= color_tile(customGreen0, customGreen),
  #       "Development Assistance per Person"=color_tile(customGreen0, customGreen),
  #       "Corruption Perception Index"= color_tile(customGreen0, customGreen),
  #       "Skilled Attendants at Birth"= color_tile(customGreen0, customGreen),
  #       "Immigrant Population (%)"= color_tile(customGreen0, customGreen),
  #       "Urbanicity (%)"= color_tile(customGreen0, customGreen)
  #     )
  #   ) %>%
  #     as.datatable(rownames = FALSE, 
  #                  options = list(paging = FALSE,
  #                                 scrollY = '500px', 
  #                                 scrollY=TRUE, 
  #                                 scrollX=TRUE, 
  #                                 #searching = FALSE,
  #                                 autoWidth = FALSE,
  #                                 ordering = FALSE,
  #                                 pageLength=1000)
  #     )
  # })
  
  output$content_vac <- renderText("United States of America")
  output$content_dis <- renderText("United States of America")
  output$content_vac_dis <- renderText("United States of America")
  
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
    else{
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
  })
  
  output$nigeria_vaccine_plot <- renderPlotly({
    vac_plotdata <- filter(vaccine_trends,gsub(" ", "", location_name) == gsub(" ", "", "Nigeria"))
    fig_a <- plot_ly(vac_plotdata, x = ~year_id,y=~prop_val, color = ~vaccine_name)%>%
      add_lines()
    
    fig_a <- fig_a %>% 
      layout(autosize = T,
             title ="Time Series of Vaccination Coverage",  showlegend = T,
             xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
             yaxis = list(title = "Vaccination Coverage (%)",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
    fig_a
  })
  
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
  
  output$nigeria_disability_plot <- renderPlotly({
    disability_plotdata <- filter(disease_trends,location_name == "Nigeria")
    fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_number_val, color = ~cause_name)%>%
      add_lines()
    title = "Time Series of Number of Years Lived in Disability in Population"
    y_title = "Years Lived with \n Disability in Population"
    fig_dis <- fig_dis %>% 
      layout( autosize = T,
              title =title,  showlegend = T,
              xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
              yaxis = list(title =y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE,type= "log"))
    fig_dis
  })
  
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
    
    
    output$selected_nigeria_vac_dis_plot <- renderPlotly({
      print("preventable_vac_trend")
      selected_vac_data = filter(preventable_vac_trend, vaccine_trends$vaccine_name=="MCV1")
      selected_vac_plotdata <- filter(selected_vac_data,location_name == "Nigeria")
      dis_data_for_selected_vac = filter(merged_data_for_vac_dis, merged_data_for_vac_dis$vaccine_name=="MCV1")
      selected_dis_plotdata <- filter(dis_data_for_selected_vac,location_name == "Nigeria")
      merged_selected_plotdata <- dplyr::left_join(selected_vac_plotdata,selected_dis_plotdata, "year_id", "year_id")
      fig <- plot_ly()
      # Add traces
      fig <- plot_ly(merged_selected_plotdata)
      fig <- fig %>% add_trace(x= ~year_id, y = ~round(deaths_rate_val,8), type = 'scatter', mode = 'lines+makers', color = ~cause_name) 
      ay <- list(
        overlaying = "y",
        side = "right",
        title = "<b> Vaccine</b> coverage (%)")
      
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
  
  # output$pdfview <- renderUI({
  #   pdf("www/Canada Childhood Vaccination FInal.pdf")
  #   tags$iframe(style="height:600px; width:100%", src="Canada Childhood Vaccination FInal.pdf")
  #   })
  # output$pdf_viewer <- renderUI( tags$iframe(src = input$pdf_selection, height = 550) ) 
  
  observe({
    if (input$t2 == "comp_index"){
      output$report_nig_title<- renderText("Comparison with other locations of similar geography or SDI")
      output$report_nig_body<- renderText("The construction of the Vaccine Improvement Index is based, in-part, on research on individual and socioeconomic factors associated with vaccine coverage in Nigeria. Prior research has found that mother’s age, education, and wealth as significantly related to immunization coverage after adjusting for other factors. In addition, the child’s birth order, family size, and place of delivery (home, public, or private facility) were related to vaccination coverage as well (1).")
    }
    else if(input$t2 == "report_vt"){
      output$report_nig_title<- renderText("Vaccination Trends")
      output$report_nig_body<- renderText("Between 2014 and 2019, Nigeria saw greater-than average improvements in seven routine vaccinations (out of 11 measured) (2). The progress demonstrated in this period contrasts to many years of stalled and even worsening vaccine coverage previously. Between 2005 and 2009, barriers to vaccination included structural issues including lack of security and armed conflict (3), supply chain and service delivery issues (4), and cultural and religious beliefs affecting vaccine hesitancy (5).")
    }
    else if(input$t2 == "report_md"){
      output$report_nig_title<- renderText("Mortality and Disability Trends")
      output$report_nig_body<- renderText("Several vaccine-preventable diseases present a large burden on the population in Nigeria. For instance, by 2013 Nigeria was one of three countries in the world with endemic polio; yet, Nigeria also struggled with declining polio vaccine coverage (6).")
    }
    else{
      output$report_nig_title<- renderText("Relationship between Vaccines and Corresponding Diseases")
      output$report_nig_body<- renderText("Greater attention to polio and other vaccine-preventable diseases led to both improved vaccination coverage and decreases in the number of deaths from diseases like measles. Revised national strategic plans for polio and routine immunizations (2013-2015) have also allowed the country to implement additional evidence-based interventions and plans for routine immunization (3–5,7).")
    }
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
                xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                yaxis = list(title = "Vaccine Improvement Index",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
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
        
      }
      #else if (input$indicators == "Development Assistance Per Total Health Spending Categorical"){
      # left_text = round(indicator_trend_data$dah_per_the_mean_cat[1],3)
      # right_text =round(indicator_trend_data$dah_per_the_mean_cat[30],3)
      # left_y = indicator_trend_data$dah_per_the_mean_cat[1]+0.01
      # right_y = indicator_trend_data$dah_per_the_mean_cat[30]+0.02
      # titles = paste0("Time Series of Development Assistance Per Total Health Spending Categorical")
      # ytitles = "Development Assistance Per Total Health Spending Categorical"
      # fig_a <- fig_a  %>% add_trace(y=~dah_per_the_mean_cat,type='scatter', name = "Development Assistance Per Total Health Spending Categorical", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
      # fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(dah_per_the_mean_cat[1], dah_per_the_mean_cat[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
      #}
      else if (input$indicators == "Eligibility to Receive DAH"){
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
        #marker = list(color = 'rgba(50, 171, 96, 0.6)',
        #line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) 
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
        print(selected_dis_plotdata)
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
  
  #output$vac_name <- renderText({ 
  #input$vaccinations
  #print(input$vaccinations)
  #})
  
  #output$vac_description <- renderText({ 
  #  unique(filter(vaccine_preventable_diseases,vaccine_name == input$vaccinations)$vaccine_description)
  #})
  
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
  
  
  output$DTPtable = DT::renderDataTable({
    table <- vaccine_preventable_diseases[vaccine_preventable_diseases$vaccine_name == "DTP1",][,c("cause_name")]
    formattable(
      table
    ) %>%
      as.datatable(rownames = FALSE, colnames = NULL,
                   options = list(paging = FALSE,
                                  dom = 't',
                                  ordering = FALSE))
  })
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
  
  dataexplorer <- reactive({
    req(input$dataset)
    if(input$dataset == "vaccine trends"){
      dataexplorer <- vaccine_trends
    }
    else if (input$dataset == "disease trends"){
      dataexplorer<-disease_trends
    }
    else if (input$dataset == "data dictionary"){
      dataexplorer<-codebook
    }
    else{
      dataexplorer <- index_results
    }
  })
  
  
  output$alldatatable = DT::renderDataTable({
    data<-dataexplorer()
    if(input$dataset == "vaccine trends"){
      x<-data %>%
        dplyr::select(-c("location_id"))
      pl=17
    }
    else if (input$dataset == "disease trends"){
      x<-data %>%
        dplyr::select(-c("location_id","cause_id"))
      pl=11
    }
    else if(input$dataset == 'data dictionary'){
      x<-data 
      pl=8
    }
    else{
      x<-data 
      pl=11
    }
    print(colnames(x))
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
                 "Immigrant Population (%)" = "imm_pop_perc",
                 "Urbanicity (%)" = "perc_urban",
                 "Improvement Index" = "result",
                 "Agree Vaccines are Safe" = "mean_agree_vac_safe",
                 "Agree Vaccines are Important" = "mean_agree_vac_important",
                 "Agree Vaccines are Effective" = "mean_agree_vac_effective",
                 "Government Trust" = "gov_trust"
    )
    
    
    formattable(
      x
    ) %>%
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
  output$download <- downloadHandler(
    filename =  paste0(input$dataset,".csv",sep=""),
    content = function(fname){
      write.csv(dataexplorer(), fname)
    }
  )
  
  #*****
  #* Download Report
  output$report <- downloadHandler(
    filename =   paste0("Nigeria-sample-report_", Sys.Date(), ".pdf"),
    content = function(file) {
      rmarkdown::render("report.Rmd",
                        output_file = file, 
                        params = list(
                          data = index_results,
                          muilti = input$my_multi,
                          vactrend = vaccine_trends,
                          dic_trend = disease_trends,
                          preventable_vac_trend = preventable_vac_trend,
                          merged_data_for_vac_dis = merged_data_for_vac_dis
                        ),
                        envir = new.env(parent = globalenv()))
    }
  )
}


shinyApp(body, server)



