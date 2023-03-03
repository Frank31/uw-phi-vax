# # ARchive of code that is no longer needed for the R Shiny Dashboard
# 
# #
# # Find out more about building applications with Shiny here:
# #
# #    http://shiny.rstudio.com/
# #
# 
# 
# ##### Set working directory
# # setwd("C:/Users/sarah/Downloads/Work/Dashboard")
# 
# 
# # source("01_packages.R", local = TRUE) #install/loads necessary packages
# 
# # data_dir <- here("Data") #define data folder directory
# 
# # source("02_alter_data.R", local = TRUE) #script to edit data frames
# # source("03_ui.R", local = TRUE)
# # source("04_server.R", local = TRUE)
# 
# # Function to check if you have a package installed and installs if you don't
# # inst.pkg <- function(package_name) {
# #   if (package_name %in% rownames(installed.packages()) == FALSE) # check to see if the package is already installed
# #   {install.packages(package_name)} # install package if missing
# #   if (package_name %in% rownames(.packages()) == FALSE) # check to see if the package is loaded
# #   {library(package_name, character.only = TRUE)} # load package into library
# # }
# 
# # r = getOption("repos")
# # r["CRAN"] = "http://cran.us.r-project.org"
# # options(repos = r)
# 
# # install.packages("vctrs") #must have version >= 5.0
# # packageVersion("vctrs")
# # inst.pkg("shiny")
# # inst.pkg("shinythemes")
# # inst.pkg("DT")
# # inst.pkg("dplyr")
# # inst.pkg("formattable")
# # inst.pkg("tidyr")
# # inst.pkg("leaflet")
# # inst.pkg("plotly")
# # inst.pkg("readxl")
# # inst.pkg("shinycssloaders")
# # inst.pkg("shinyWidgets")
# # inst.pkg("shinyhelper")
# # inst.pkg("rintrojs")
# # inst.pkg("shinyBS")
# # inst.pkg("here")
# # inst.pkg("shinyjs")
# # inst.pkg("tools")
# # inst.pkg("ggplot2")
# 
# tabPanel("Case Studies",
#          mainPanel(id = "info.page",
#                    tabsetPanel(tabPanel("For Community Leaders",
#                                         tags$div(id = "reportsPage", 
#                                                  h4(id = "reportsHeader","The Role of Community Leaders to Reduce Vaccine Hesitancy"),
#                                                  h5(id = "reportsText", "Vaccine hesitancy is a growing public health concern defined by an individualâs refusal or delayed acceptance of a readily available vaccine. Vaccine hesitancy has many contributing causes including but not limited to, historical, political, and socio-cultural factors; addressing hesitancy is a key factor in successfully increasing vaccination coverage globally Engaging and educating trusted community leaders has been explored as a strategy to reduce vaccine hesitancy and increase a communityâs vaccine uptake1. The WHO has created a framework called âHuman-centered design for tailoring immunization programsâ, in which they stress community engagement as a key pillar for building trust between healthcare systems and individuals 2. The COVID-19 pandemic has served as a large-scale experiment in quickly vaccinating communities against SARS-Cov-2. This surge for vaccination required the development of strategies to reduce vaccine hesitancy around the globe, particularly in underserved and vulnerable communities. Two organizations in particular, Breakthrough Action â a partnership led by Johns Hopkins Center for Communication Programs, and The Partnership for Healthy Cities supported by Bloomberg Philanthropies in partnership with the World Health Organization (WHO) and Vital Strategies have sponsored programs in cities and communities across the globe with the goal of educating and engaging community leaders to decrease vaccine hesitancy. The Partnership for Healthy Cities has supported community lead programs in 18 cities across the world with a focus on educating local leaders on the importance of vaccination against COVID-19 3. To illustrate this further a few campaigns associated with Breakthrough Action and the Partnership for Healthy Cities will be highlighted."),
#                                                  img(src="Community_Leaders_Picture1.jpg", width= "700px"),
#                                                  h6("Credit: Vital Strategies"),
#                                                  h4(id = "reportsHeader", "Buenos Aires, Argentina: The peer-to-peer âButterfly Effectâ project vaccinates people experiencing homelessness"),
#                                                  h5(id = "reportsText", "In Buenos Aires a campaign was created to increase vaccine coverage of the unhoused population within the city. The unhoused due to various socioeconomic and health factors have high levels of mistrust towards health systems and healthcare personnel. To address this, the city formed a group of 44 formerly unhoused people to develop an outreach program based on one-to-one communication outreach strategies. The campaign successfully vaccinated 10,000 people over the course of 4 months and provided new insights into future outreach programs. The researchers found that reducing barriers to getting vaccinated such as the need for completing online forms or showing ID led to a greater vaccine uptake among the unhoused population 4. See the link below for the full story from Vital Strategies blog."),
#                                                  h4(id = "reportsHeader", "Engaging Religious Leaders to Boost COVID-19 Vaccination"),
#                                                  img(src="Community_Leaders_Picture2.jpg", width= "700px"),
#                                                  h6("Malawi's Bishop Martin Mtumbuka. Credit: Lusayo Banda"),
#                                                  h5(id = "reportsText", "Johns Hopkins Center for Communications Programs (CCP) has worked alongside religious leaders in Africa to combat vaccine misinformation that has proliferated throughout many communities. Johns Hopkins CCP has worked in Malawi, Liberia, and Nigeria to engage and educate faith leaders in local communities. In Malawi 144 leaders were given education sessions on COVID-19 prevention and vaccination. This training helps faith leaders create campaigns to increase vaccine demand in their local communities. In Nigeria, a two-day forum was hosted in which state and local health officials discussed common rumors and myths surrounding the vaccines and gave leaders talking points they could bring back with them to better address concerns surrounding vaccine safety.  In Liberia a vaccine ambassador program was established to train a range of influential community leaders to be educators on COVID-19 prevention and vaccination. The ambassadors have gone on to host community engagement events and talk on local radio stations 5."),
#                                                  h4(id = "reportsHeader", "Battling Covid-19 Vaccine Misinformation Through Targeted Community Dialogues"),
#                                                  img(src="Community_Leaders_Picture3.png", width= "700px"),
#                                                  h6("Credit: Breakthrough ACTION"),
#                                                  h5(id = "reportsText", "In Guinea, Breakthrough Action partnered with the National Agency for Health Security of Guinea to create programs to combat misinformation and rumors surrounding the pandemic and vaccines. Community dialogue sessions took in place in which Public Health Officials were able to gain feedback from local leaders about their communityâs special needs and conditions regarding vaccine hesitancy while also equipping leaders with tools to combat these hesitancies. These sessions were well received and many leaders felt that they had been given the resources needed to better educate their communities about the nature of the pandemic as well as the importance of getting vaccinated 6."),
#                                                  h4(id = "reportsHeader", "Key Lessons"),
#                                                  h5(id = "reportsText", "These campaigns addressing COVID-19 prevention and vaccine hesitancy in local communities can provide more broadly applicable lessons for combatting hesitancy and increasing vaccine dosage demand in communities. Understanding and using existent community structures and leaders can enable Public Health Officials to make efficient usage of resources and to better understand the material reality of the on the ground conditions inside these communities.  Community engagement meetings can serve as a way for Public Health Officials to continuously monitor and understand trends in public opinion and test out new ways of engaging with the population on these issues while receiving real time feedback. Training community leaders to combat myths surrounding vaccines can be an effective strategy for reaching community members with an already trusted voice. It has long been a mistake of Western Public Health Agencies to not engage enough with community leaders and instead take a paternalistic approach of global health that minimizes that opportunity for local voices to have a say in how Global Health funds are being utilized. Both programs seek to change this old paradigm by participating in active conversations with communities and their leaders, enabling them to take a more community-based approach to Public Health programs. Equipping members of underserved communities to become advocates in their own communities can create a âsocial butterfly effectâ which spreads vaccination talking points throughout the community, creating an organic and eventually self-perpetuating community-based conversation about vaccination. With vaccine hesitancy on the rise in many parts of the globe it is paramount that leaders in Global Health take note of these successful campaigns to increase vaccine demand in underserved communities across the global and they we seek to apply these lessons going into the future."),
#                                                  h4(id = "reportsHeader", "References"),
#                                                  h5("1.	DubÃ©, E. et al. Vaccine hesitancy. Hum Vaccin Immunother 9, 1763â1773 (2013).",
#                                                     tags$br(), tags$br(),
#                                                     "2.	World Health Organization & Fund (UNICEF), U. N. C. Human-centred design for tailoring immunization programmes. (World Health Organization, 2022).",
#                                                     tags$br(), tags$br(),
#                                                     "3.	The Partnership for Healthy Cities supports COVID-19 Vaccine Outreach Efforts of 18 Cities in Africa, Asia, and Latin America. https://www.who.int/news/item/06-05-2021-the-partnership-for-health-cities-supports-covid-19-vaccine-outreach-efforts-of-18-cities-in-africa-asia-and-latin-america.",
#                                                     tags$br(), tags$br(),
#                                                     "4.	Trusted Voices for COVID-19: Engaging community leaders to overcome vaccine hesitancy. Vital Strategies https://www.vitalstrategies.org/vital-stories-trusted-voices-for-covid-19-engaging-community-leaders-to-overcome-vaccine-hesitancy/.",
#                                                     tags$br(), tags$br(),
#                                                     "5.	Engaging Religious Leaders to Boost COVID-19 Vaccination. Johns Hopkins Center for Communication Programs https://ccp.jhu.edu/2022/05/02/religious-leaders-covid/ (2022).",
#                                                     tags$br(), tags$br(),
#                                                     "6.	meeichild. Battling COVID-19 Vaccine Misinformation Through Targeted Community Dialogues. Breakthrough ACTION and RESEARCH https://breakthroughactionandresearch.org/battling-covid-19-vaccine-misinformation-through-targeted-community-dialogues/ (2022)."
#                                                  )
#                                         )
#                    ),
#                    tabPanel("For Health Care Providers in High Income Countries",
#                             tags$div(id = "reportsPage",
#                                      h2(id = "reportsHeader", "Canada"),
#                                      h5(id = "reportsText", "Canada is one of the top 28 richest nations and boasts one of the best healthcare systems that in the world. Despite this, its childhood vaccination coverage is only around 84% which is far behind other developed countries such as the UK and the USA. (1) It was also found that approximately 20% of the population is vaccine apprehensive, and approximately 5% of the populace holds anti-vaccination sentiments. (2)", 
#                                         tags$br(), tags$br(), "High rates of immunization are critical for preventing the spread of many infectious diseases. The immunizations for children that are recommended include the DPTP-Hib, Rotavirus, Pneumococcal, Meningococcal, MMR, Varicella, Hepatitis B, dTap, and HPV vaccines. (3)"),
#                                      h4(id = "reportsHeader", "Challenges in vaccine coverage"),
#                                      h5(id = "reportsText", "Low vaccine coverage rates in Canada may be ascribed to parents' reluctance to vaccinate their children due to a lack of clarity and misinformation. An interesting pattern observed is that parents with high levels of education, namely those with postgraduate degrees, frequently lack faith in the efficacy of vaccinations and are worried more about potential for  adverse reactions, such as anaphylaxis or paralysis caused by vaccines. On the other hand, parents of children with education levels below secondary school frequently know little about immunization programs and worry about their potential side effects. (4) Another factor that contributed to incomplete immunization (children not receiving  all of the recommended doses) was the parents' marital status. The children of single parents often did not receive the recommended number of doses of vaccines. (5) It's also crucial to recognize that philosophical and religious opposition to immunizations still exists and contributes to around 10% of vaccine hesitancy. (6)",
#                                         tags$br(), tags$br(), "A significant problem is frequently posed by the heterogeneity of immunization programs in each of Canada's territories. People frequently have questions about whether or not the neighborhood hospitals or clinics provide free vaccinations. Additionally, a lot of individuals are worried about the potential out-of-pocket cost and are often uninformed of the free immunization program that the Canadian government provides.", 
#                                         tags$br(), tags$br(), "Finally, the COVID outbreak in 2019 also contributed to a decline in immunization rates by forcing a temporary suspension of immunization campaigns. (7) However, in the years 2020 and 2021, this was improved by giving infants shorter appointments and by adhering to guidelines including wearing PPE kits and screening for COVID-19."),
#                                      h4(id = "reportsHeader", "Ways to improve vaccine coverage: "),
#                                      h5(id = "reportsText", "According to one study, Canadians most frequently regarded the sources of information given by their healthcare practitioners as reliable. (8) As a result, regular health checkups should include educating patients on the benefits of vaccination in order to foster a positive attitude toward vaccination. Another study discovered that pharmacists could be trained to educate people about vaccination, its side effects, and how to obtain them. (8) Raising awareness about the actual negative effects of vaccines aids in the prevention of the spread of false information about vaccines, as uninformed people frequently believe that the vaccine failed to work and caused their symptoms.",
#                                         tags$br(), tags$br(), "Additionally, in the era of infodemics on social media, it would also be beneficial if videos, reels, and podcasts of pediatricians talking about vaccinations are promoted more on social media, so people could have access to high-quality unbiased information.",
#                                         tags$br(), tags$br(), "As healthcare professionals, it would beneficial if a set of dedicated staff could handle immunization requests. This would cut down on hospital wait times, which are a major obstacle for parents. The same group of nurses should also be in charge of sending out reminders through messages and calls. It has been observed that this tailored immunization delivery has increased immunization rates. (10) The significant issue of incomplete immunization among children could be addressed by extending the hours of local clinics. In addition, it would be helpful if the health system made better attempts to inform parents about the next date for immunizations of their children and ensured that the parents followed the schedule.",
#                                         tags$br(), tags$br(), "When parents refuse vaccination for their children due to religious beliefs, it is critical that healthcare workers respect those beliefs. However, healthcare professionals could inform them with respect of the significance of immunizations. Vaccination coverage is also high in cultures that promote the collective benefits of vaccinations. It would be extremely beneficial for health care professionals to educate parents on the significance of herd immunity, explaining that by immunizing their children, they are protecting immunocompromised children. Thereby inspiring them to grow a strong sense of social responsibility.",
#                                         tags$br(), tags$br(), "Thus, in order to increase vaccination coverage for childhood vaccines and close immunity gaps, prosocial nudges, awareness about the importance of vaccination, increased access, and promotion of reliable sources of vaccination information, would be required. (11) Listed below are few trusted sources about vaccination provided by the Canadian government:",
#                                         tags$br(), tags$ul(tags$li(tags$a("https://caringforkids.cps.ca/handouts/immunization/vaccination_and_your_child", target = "_blank", href = "https://caringforkids.cps.ca/handouts/immunization/vaccination_and_your_child")),
#                                                            tags$li(tags$a("https://immunize.ca/", target = "_blank", href = "https://immunize.ca/")),
#                                                            tags$li(tags$a("https://www.canada.ca/en/public-health/services/diseases/coronavirus-disease-covid-19/vaccines.html", target = "_blank", href = "https://www.canada.ca/en/public-health/services/diseases/coronavirus-disease-covid-19/vaccines.html")),
#                                         )),
#                                      h4(id = "reportsHeader", "References"),
#                                      h5("1. UNICEF Office of Research, Innocenti Report Card 11 (2013) Child well-being in rich countries: A comparative overview. <www.unicef-irc.org/publications/pdf/rc11_eng.pdf> ",
#                                         tags$br(), tags$br(),
#                                         "2. Stratoberdha D, Gobis B, Ziemczonek A, Yuen J, Giang A, Zed PJ. Barriers to adult vaccination in Canada: A qualitative systematic review. Canadian Pharmacists Journal / Revue des Pharmaciens du Canada. June 2022. doi:10.1177/17151635221090212 ",
#                                         tags$br(), tags$br(),
#                                         "https://immunize.ca/",
#                                         tags$br(), tags$br(),
#                                         "4. Carpiano RM, Polonijo AN, Gilbert N, Cantin L, DubÃ© E. Socioeconomic status differences in parental immunization attitudes and child immunization in Canada: Findings from the 2013 Childhood National Immunization Coverage Survey (CNICS). Prev Med. 2019; 123:278-287. doi: 10.1016/j.ypmed.2019.03.033",
#                                         tags$br(), tags$br(),
#                                         "5. Boulianne N, Deceuninck G, Duval B, Lavoie F, Dionne M, Carsley J, Valiquette L, Rochette L, De Serres G. Why are some children incompletely vaccinated at the age of 2? Can J Public Health 2003; 94:218-23; PMID:12790498",
#                                         tags$br(), tags$br(),
#                                         "6. Gilbert NL, Gilmour H, Wilson SE, Cantin L. Determinants of non-vaccination and incomplete vaccination in Canadian toddlers. Hum Vaccin Immunother. 2017;13(6):1-7. doi:10.1080/21645515.2016.1277847",
#                                         tags$br(), tags$br(),
#                                         "7.MacDonald SE, Paudel YR, Kiely M, et al. Impact of the COVID-19 pandemic on vaccine coverage for early childhood vaccines in Alberta, Canada: a population-based retrospective cohort study. BMJ Open.m2022;12(1):e055968. Published 2022 Jan 25. doi:10.1136/bmjopen-2021-055968 ",
#                                         tags$br(), tags$br(),
#                                         "8.  Brenner RA, Simons-Morton BG, Bhaskar B, et al. Prevalence and   predictors of immunization inner-city infants: a birth cohort study. Pediatrics. 2001;108(3):661â670.",
#                                         tags$br(), tags$br(),
#                                         "9. Lisenby KM, Patel KN, Uichanco MT. The role of pharmacists in addressing vaccine hesitancy and the measles outbreak. J Pharm Pract. 2021;34(1):127-32. ",
#                                         tags$br(), tags$br(),
#                                         "10. Zimmerman RK, Nowalk MP, Raymund M, et al. Tailored interventions to increase influenza vaccination in neighborhood health centers serving the disadvantaged. Am J Pub Health. 2003;93(10):1699â1705", 
#                                         tags$br(), tags$br(),
#                                         "11. Mah CL, Guttmann A, McGeer A, Krahn M, Deber RB. Compulsory school-entry vaccination laws and exemptions: who is opting out in ontario and why does it matter? Healthc Policy. 2010;5(4):37-46.",  
#                                         tags$br(), tags$br(),
#                                         "12. Scheifele DW, Halperin SA, Bettinger JA. Childhood immunization rates in Canada are too low: UNICEF. Paediatr Child Health. 2014;19(5):237-238. doi:10.1093/pch/19.5.237",
#                                         tags$br(), tags$br(),
#                                         "13. Betsch, C., BÃ¶hm, R., Korn, L. et al. On the benefits of explaining herd immunity in vaccine advocacy. Nat Hum Behav 1, 0056 (2017). https://doi.org/10.1038/s41562-017-0056")  
#                             )
#                    ),
#                    # tabPanel("For Parents and Families",
#                    #          # tags$iframe(embed(src="Canada_Childhood_Vaccination_Final.pdf", width= "100%", height="400px"))
#                    #          tags$iframe(style = "height:1000px; width:100%; scrolling=yes", src = "Canada_Childhood_Vaccination_Final.pdf")
#                    # )
#                    )
#          )),
# 
# # Sample Report #####
# tabPanel("Sample Report",
#          sidebarPanel(
#            h4(strong("Sample Report: for Nigeria")),
#            fluidRow(column(6, " ", style='padding:70px;')),
#            fluidRow(column(12,h5(strong(textOutput("report_nig_title"))))),
#            fluidRow(column(6, " ", style='padding:10px;')),
#            fluidRow(column(12,textOutput("report_nig_body"))),
#            fluidRow(column(6, " ", style='padding:90px;')),
#            tags$head(tags$style("{color: #0060bf; font-size: 20px;}")),
#            downloadButton(
#              outputId = "report",
#              label = "Download Report"
#            ),
#            width = 4),
#          mainPanel(
#            tabsetPanel(id="t2",
#                        tabPanel("Comparison",value = "comp_index",
#                                 fluidRow(column(8, " ", style='padding:20px;')),
#                                 fluidRow(column(4,radioButtons("sdi_group_present_comp","2019 SDI Group", choices = c("All"="all","Low" ="low","Medium" = "medium","High" = "high"),selected="low",inline = TRUE)),
#                                          column(4,selectInput("region", "Regions", choices = unique(index_results$region))),
#                                          column(4,selectInput(
#                                            inputId = "my_multi",
#                                            label = "Search Country :",
#                                            choices = unique(merged_data_for_vacii_sdi$location),
#                                            selected = "Nigeria",
#                                            selectize = TRUE,
#                                            multiple=TRUE))),
#                                 fluidRow(column(8, " ", style='padding:20px;')),
#                                 fluidRow(column(12, plotlyOutput("index_trend_plot_com",height = "40vh"))),column(1,"")),
#                        tabPanel("Vaccination Trends",value = "report_vt",
#                                 fluidRow(column(width = 11,h4(strong("Nigeria")))),
#                                 fluidRow(column(8, " ", style='padding:10px;')),
#                                 fluidRow(column(11,plotlyOutput("nigeria_vaccine_plot",height = "55vh")),column(1,""))),
#                        tabPanel("Mortality and Disability Trends",value = "report_md",
#                                 fluidRow(column(width = 11,h4(strong("Nigeria")))),
#                                 fluidRow(column(8, " ", style='padding:10px;')),
#                                 fluidRow(column(12,plotlyOutput("nigeria_disability_plot", height = "45vh")))),
#                        tabPanel("Vaccination and Corresponding Diseases Trends",value = "report_vcdt",
#                                 fluidRow(column(width = 11,h4(strong("Nigeria")))),
#                                 fluidRow(column(width = 11,h5(strong("Selected Vaccination: MCV1")))),
#                                 fluidRow(column(8, " ", style='padding:10px;')),
#                                 fluidRow(column(12,plotlyOutput("selected_nigeria_vac_dis_plot", height = "50vh")))
#                        )))),
# 
# #   
# #   ##############test##########t##########t##########t##########t
# 
# # output$index_trend_plot_com <- renderPlotly({
# #   reportindextrendplot()
# # })
# 
# # output$nigeria_vaccine_plot <- renderPlotly({
# #   vac_plotdata <- filter(vaccine_trends,gsub(" ", "", location_name) == gsub(" ", "", "Nigeria"))
# #   fig_a <- plot_ly(vac_plotdata, x = ~year_id,y=~prop_val, color = ~vaccine_name)%>%
# #     add_lines()
# #   
# #   fig_a <- fig_a %>% 
# #     layout(autosize = T,
# #            title ="Time Series of Vaccination Coverage",  showlegend = T,
# #            xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
# #            yaxis = list(title = "Vaccination Coverage (%)",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
# #   fig_a
# # })
# 
# 
# #else if (input$indicators == "Development Assistance Per Total Health Spending Categorical"){
# # left_text = round(indicator_trend_data$dah_per_the_mean_cat[1],3)
# # right_text =round(indicator_trend_data$dah_per_the_mean_cat[30],3)
# # left_y = indicator_trend_data$dah_per_the_mean_cat[1]+0.01
# # right_y = indicator_trend_data$dah_per_the_mean_cat[30]+0.02
# # titles = paste0("Time Series of Development Assistance Per Total Health Spending Categorical")
# # ytitles = "Development Assistance Per Total Health Spending Categorical"
# # fig_a <- fig_a  %>% add_trace(y=~dah_per_the_mean_cat,type='scatter', name = "Development Assistance Per Total Health Spending Categorical", mode = 'lines', line = list(color = 'rgb(106, 90, 205)',width=2))
# # fig_a <- fig_a %>% add_trace(x = ~c(year[1], year[30]), y = ~c(dah_per_the_mean_cat[1], dah_per_the_mean_cat[30]), type = 'scatter', mode = 'markers', marker = list(color = 'rgb(106, 90, 205)', size = 10))
# #}
# 
# #marker = list(color = 'rgba(50, 171, 96, 0.6)',
# #line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1)))
# 
# # output$report <- downloadHandler(
# #   filename =   paste0("Nigeria-sample-report_", Sys.Date(), ".pdf"),
# #   content = function(file) {
# #     rmarkdown::render("report.Rmd",
# #                       output_file = file, 
# #                       params = list(
# #                         data = index_results,
# #                         muilti = input$my_multi,
# #                         vactrend = vaccine_trends,
# #                         dic_trend = disease_trends,
# #                         preventable_vac_trend = preventable_vac_trend,
# #                         merged_data_for_vac_dis = merged_data_for_vac_dis
# #                       ),
# #                       envir = new.env(parent = globalenv()))
# #   }
# # )
# # output$selected_nigeria_vac_dis_plot <- renderPlotly({
# #   # print("preventable_vac_trend")
# #   selected_vac_data = filter(preventable_vac_trend, vaccine_trends$vaccine_name=="MCV1")
# #   selected_vac_plotdata <- filter(selected_vac_data,location_name == "Nigeria")
# #   dis_data_for_selected_vac = filter(merged_data_for_vac_dis, merged_data_for_vac_dis$vaccine_name=="MCV1")
# #   selected_dis_plotdata <- filter(dis_data_for_selected_vac,location_name == "Nigeria")
# #   merged_selected_plotdata <- dplyr::left_join(selected_vac_plotdata,selected_dis_plotdata, "year_id", "year_id")
# #   fig <- plot_ly()
# #   # Add traces
# #   fig <- plot_ly(merged_selected_plotdata)
# #   fig <- fig %>% add_trace(x= ~year_id, y = ~round(deaths_rate_val,8), type = 'scatter', mode = 'lines+makers', color = ~cause_name)
# #   ay <- list(
# #     overlaying = "y",
# #     side = "right",
# #     title = "<b> Vaccine</b> coverage (%)")
# # 
# #   fig <- fig %>% add_trace(x =  ~year_id, y = ~prop_val, type = 'scatter',name = ~vaccine_name.x,yaxis = "y2", mode = 'lines',line = list(color = 'rgba(49,130,189, 1)', width = 4))
# #   # Set figure title, x and y-axes titles
# #   fig <- fig %>% layout(
# #     autosize = T,
# #     title = list(text="Vaccine & Corresponding Disease Trend", x=0.25),
# #     xaxis = list(title="Year"),
# #     yaxis = list(title= "<b> Deaths</b> per 100,000 Population"),
# #     yaxis2 = ay,
# #     legend = list(x = 3000, y = 1.2)
# #   )%>%
# #     layout(xaxis = list(
# #       zerolinecolor = '#ffff',
# #       zerolinewidth = 2,
# #       gridcolor = 'ffff'),
# #       yaxis = list(
# #         zerolinecolor = '#ffff',
# #         zerolinewidth = 2,
# #         gridcolor = 'ffff')
# #     )
# # 
# #   fig
# # })
# 
# # output$pdfview <- renderUI({
# #   pdf("www/Canada Childhood Vaccination FInal.pdf")
# #   tags$iframe(style="height:600px; width:100%", src="Canada Childhood Vaccination FInal.pdf")
# #   })
# # output$pdf_viewer <- renderUI( tags$iframe(src = input$pdf_selection, height = 550) )
# 
# # output$nigeria_disability_plot <- renderPlotly({
# #   disability_plotdata <- filter(disease_trends,location_name == "Nigeria")
# #   fig_dis <- plot_ly(disability_plotdata, x = ~year_id,y= ~ylds_number_val, color = ~cause_name)%>%
# #     add_lines()
# #   title = "Time Series of Number of Years Lived in Disability in Population"
# #   y_title = "Years Lived with \n Disability in Population"
# #   fig_dis <- fig_dis %>% 
# #     layout( autosize = T,
# #             title =title,  showlegend = T,
# #             xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
# #             yaxis = list(title =y_title,showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE,type= "log"))
# #   fig_dis
# # })
# 
# #   observeEvent(indicatorsdata(),{
# # 
# #     output$indicator_map <- renderPlotly({ #### Improvement Indicator
# #       height  = 1500
# #       units="px"
# # 
# #       # light grey boundaries
# #       l <- list(color = toRGB("white"), width = 0.5)
# # 
# #       # specify map projection/options
# #       g <- list(
# #         showframe = FALSE,
# #         showcoastlines = FALSE,showland = TRUE,showcountries = TRUE,
# #         resolution = 150,
# #         countrycolor = toRGB("white"),
# #         landcolor = toRGB("grey85"),
# #         projection = list(scale=1.2))
# # 
# #       fig <- plot_ly(indicatorsdata())
# # 
# # if (input$indicators == "Socio-demographic Index"){
# #   fig <- fig %>%
# #     add_trace(
# #       z = ~sdi, color = ~sdi, type = 'choropleth', locations = ~iso_code, colors="Purples",
# #       text = ~paste0(location),
# #       marker = list(line = l)) %>%
# #     colorbar(title = paste0("Socio-demographic \n Index"))
# # } else if (input$indicators == "Eligibility to Receive DAH"){
# #   colfunc <- colorRampPalette(c("#A2A2A1FF", "#4b2e83"))
# #   fig <- fig %>%
# #     add_trace(
# #       z = ~as.numeric(indicatorsdata()$dah_eligible), type = 'choropleth',locations = ~iso_code, colors=colfunc(2),
# #       text = ~paste0(location),
# #       marker = list(line = l))%>%
# #     colorbar(title = paste0("Eligibility to Receive DAH",'<br>',"(1: True, 0: False)"),brks=c(0,1),labels=c("True",' ',"False"))
# # } else if (input$indicators == "Total Health Spending per Person"){
# #   colfunc <- colorRampPalette(c("#C7C5E7", "#4b2e83"))
# #   fig <- fig %>%
# #     add_trace(
# #       z = ~the_per_cap_mean, color = ~the_per_cap_mean, type = 'choropleth', locations = ~iso_code, colors=colfunc(10),
# #       text = ~paste0(location),
# #       marker = list(line = l)) %>%
# #     colorbar(title = paste0("Mean Spending \n per Person"))
# # } else if (input$indicators == "Government Health Spending per Total Health Spending"){
# #   fig <- fig %>%
# #     add_trace(
# #       z = ~ghes_per_the_mean, color = ~ghes_per_the_mean, type = 'choropleth', locations = ~iso_code,  colors="Purples",
# #       text = ~paste0(location),
# #       marker = list(line = l)) %>%
# #     colorbar(title = paste0("Mean Government \n Spending"))
# # } else if (input$indicators == "Development Assistance per Person"){
# #   colfunc <- colorRampPalette(c("#C7C5E7", "#4b2e83"))
# #   fig <- fig %>%
# #     add_trace(
# #       z = ~dah_per_cap_ppp_mean, color = ~dah_per_cap_ppp_mean, type = 'choropleth', locations = ~iso_code, colors=colfunc(10),
# #       text = ~paste0(location),
# #       marker = list(line = l)) %>%
# #     colorbar(title = paste0("Mean Development \n Assistance per \n Person"))
# # } else if (input$indicators == "HAQI"){
# #   fig <- fig %>%
# #     add_trace(
# #       z = ~haqi, color = ~haqi, type = 'choropleth', locations = ~iso_code, colors="Purples",
# #       text = ~paste0(location),
# #       marker = list(line = l)) %>%
# #     colorbar(title = paste0("HAQI"))
# # } else if (input$indicators == "Corruption Perception Index"){
# #   fig <- fig %>%
# #     add_trace(
# #       z = ~cpi, color = ~cpi, type = 'choropleth', locations = ~iso_code, colors="Purples",
# #       text = ~paste0(location),
# #       marker = list(line = l)) %>%
# #     colorbar(title = paste0("Corruption \n Perception Index"))
# # } else if (input$indicators == "Skilled Attendants at Birth"){
# #   fig <- fig %>%
# #     add_trace(
# #       z = ~perc_skill_attend, color = ~perc_skill_attend, type = 'choropleth', locations = ~iso_code, colors="Purples",
# #       text = ~paste0(location),
# #       marker = list(line = l)) %>%
# #     colorbar(title = paste0("Skilled \n Attendants at \n Birth"))
# # 
# # } else if (input$indicators == "Immigrant Population (%)"){
# #   fig <- fig %>%
# #     add_trace(
# #       z = ~imm_pop_perc, color = ~imm_pop_perc, type = 'choropleth', locations = ~iso_code, colors="Purples",
# #       text = ~paste0(location),
# #       marker = list(line = l)) %>%
# #     colorbar(title = paste0("Immigrant \n Population (%)"))
# # } else if (input$indicators == "Urbanicity (%)"){
# #   fig <- fig %>%
# #     add_trace(
# #       z = ~perc_urban_untrnsf, color = ~perc_urban_untrnsf, type = 'choropleth', locations = ~iso_code, colors="Purples",
# #       text = ~paste0(location),
# #       marker = list(line = l)) %>%
# #     colorbar(title = paste0("Urbanicity (%)"))
# # }
# #       })
# #     })
# # observeEvent(regionstable(),{
# #   # print("map data")
# #   map_data <- regionstable()
#   # print(map_data)
# # indicatorsdata <- reactive({
# #   req(input$indicators)
# #   indicators <-as.data.frame.matrix(map_data[,-c("gbd_location_id","iso_num_code")])
# # })
# 
# # reportindextrendplot<-reactive(
# #   fig_a <- plot_ly(index_results %>%  filter(location %in% input$my_multi), x = ~year) %>%
# #     add_trace(y=~result,type='scatter', mode = 'lines', name = ~location, color = ~location,width=2) %>%
# #     layout( autosize = T,
# #             title = paste0("Time Series of Vaccine Improvement Index"),
# #             showlegend = TRUE,
# #             xaxis = list(title = "Year",showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
# #             yaxis = list(title = "Vaccine Improvement Index",showgrid = FALSE, zeroline = TRUE, showticklabels = TRUE))
# # )
# 
# # # reactivally set the regionstable data that will determine what gets ploted in the map
# # regionstable <- reactive({
# #   req(input$region_table)
# #   if (input$region_table =='All'){
# #     sdi_group_present()
# #   }
# #   else{
# #     filter(sdi_group_present(),region == input$region_table)
# #   }
# # })
# 
# # observe({
# #   if (input$t2 == "comp_index"){
# #     output$report_nig_title<- renderText("Comparison with other locations of similar geography or SDI")
# #     output$report_nig_body<- renderText("The construction of the Vaccine Improvement Index is based, in-part, on research on individual and socioeconomic factors associated with vaccine coverage in Nigeria. Prior research has found that motherâs age, education, and wealth as significantly related to immunization coverage after adjusting for other factors. In addition, the childâs birth order, family size, and place of delivery (home, public, or private facility) were related to vaccination coverage as well (1).")
# #   }
# #   else if(input$t2 == "report_vt"){
# #     output$report_nig_title<- renderText("Vaccination Trends")
# #     output$report_nig_body<- renderText("Between 2014 and 2019, Nigeria saw greater-than average improvements in seven routine vaccinations (out of 11 measured) (2). The progress demonstrated in this period contrasts to many years of stalled and even worsening vaccine coverage previously. Between 2005 and 2009, barriers to vaccination included structural issues including lack of security and armed conflict (3), supply chain and service delivery issues (4), and cultural and religious beliefs affecting vaccine hesitancy (5).")
# #   }
# #   else if(input$t2 == "report_md"){
# #     output$report_nig_title<- renderText("Mortality and Disability Trends")
# #     output$report_nig_body<- renderText("Several vaccine-preventable diseases present a large burden on the population in Nigeria. For instance, by 2013 Nigeria was one of three countries in the world with endemic polio; yet, Nigeria also struggled with declining polio vaccine coverage (6).")
# #   }
# #   else{
# #     output$report_nig_title<- renderText("Relationship between Vaccines and Corresponding Diseases")
# #     output$report_nig_body<- renderText("Greater attention to polio and other vaccine-preventable diseases led to both improved vaccination coverage and decreases in the number of deaths from diseases like measles. Revised national strategic plans for polio and routine immunizations (2013-2015) have also allowed the country to implement additional evidence-based interventions and plans for routine immunization (3â5,7).")
# #   }
# # })