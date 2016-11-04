#########################
## Shiny App UI Script ##
#########################

## Load libraries
library(leaflet)
library(markdown)
library(metricsgraphics)
library(plotly)

shinyUI(
  
  ## Title
  navbarPage(HTML("<b>Safety First: Understanding Vehicular Fatalities</b>"),
             ## "About" Tab
             tabPanel("About",
                      sidebarLayout(
                        sidebarPanel(
                          HTML("Original data sources: <br> 
                               - <a href = http://www-fars.nhtsa.dot.gov/Main/index.aspx> 
                               National Highway Traffic Safety Administration FARS Data</a>
                               <br>
                               - <a href = https://crashstats.nhtsa.dot.gov/#/DocumentTypeList/4> 
                               FARS Manual</a>
                               <br><br><br><br><br><br>
                               <hr>
                               Application developed by:
                               <a href=mailto:jim.crozier@ibm.com?>Jim Crozier</a>
                               <br>
                               Additional contributions by: 
                               <a href = mailto:rafi.kurlansik@ibm.com?>Rafi Kurlansik</a>
                               "),
                          img(src = 'http://www.keithbrooks.com/images/ibmlogo.png', 
                              align = "center",
                              height = 125,
                              width = 250)
                          ),
                        mainPanel(
                          h4("Background"),
                          HTML("Producers and consumers of automobiles have strong incentives
                               to prefer safer vehicles. As horrific and tragic as traffic accidents
                               are for the obvious reasons, they also impact a manufacturer's image
                               in the marketplace. Auto manufacturers strive to be the safest car on
                               the road to avoid loss of life and limb, and are keenly aware of the
                               impact negative publicity can have on business. 
                               <br><br>
                               The Safety First application was designed to explore the following question:
                               <br><br>
                               <b> In the event of a crash, which factors are associated with fatality?</b>
                               <br><br>
                               In this application, discovery takes place in three stages.  First, visually
                               explore the data.  Next, take a closer look at the distribution and correlation of
                               each variable under 'Variable Analysis'.  Lastly, you can configure and evaluate your 
                               model under the 'Modeling' tab.  The authors have included their results in the 
                               Modeling section under 'Findings', and encourage users to share what they have
                               found.
                               <br><br>
                               "),
                          h4("FARS Data"),
                          HTML("The Fatality Analysis Reporting System (FARS) data from 1975-2013 is the primary
                               source for this analysis.  FARS data is represented in a series of tables with
                               fields covering road conditions, driver details, vehicle specifications, the 
                               nature of the crash, and more.  <i>It is important to note that only crashes resulting
                               in a fatality are in the data set.</i>  As such, this analysis does not 
                               take all automotive collisions into consideration.  Findings are representative
                               of accidents with fatalities only.
                               <br><br>
                               See the sidebar for original sources and documentation.
                               <br><br>"),
                          h4("Powered by Apache Spark & Sparklyr"),
                          HTML("This app is launched from RStudio Server on IBM's Data Science Experience (DSX).  
                               DSX connects to a Spark service, allowing data transformations on the body of historical data 
                               with the sparklyr API.")
                          
                        )
                      )
                        
             ),             
             ## "Visualization" Tab
             navbarMenu("Visualization",
                        tabPanel("Map",
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Choropleth Map: County Deaths per 10,000 Residents (1999-2013)"),
                                     HTML("<hr>
                                          Colors correspond to the rate of fatalities
                                          in a county given its population.  Darker reds indicate higher
                                          incidence of fatalities.  Counties without data are left transparent.
                                          <br><br>
                                          Clicking on a county will present a pop-up with details on crash frequency.
                                          <br><br>
                                          While exploring the map, notice that rural
                                          areas appear to consistently possess more fatalities per capita 
                                          compared to cities.")
                                     ),
                                   mainPanel(
                                     leafletOutput("leafPlot"))
                                     )
                        ), ## tabPanel
                        
                        ## "Maker Comparison" Tab
                        tabPanel("Maker Comparison", 
                                 sidebarLayout(
                                      sidebarPanel(
                                        h4("Manufacturer Comparison"),
                                        HTML("<hr>
                                             Month over month breakdown of the number of fatalities 
                                             associated with each manufacturer in 2014. 
                                             <br><br>
                                             Absolute and normalized values are represented.
                                             <br><br>
                                             These plots clearly illustrate the importance
                                             of scaling data before drawing conclusions.")
                                        ),
                                      mainPanel(
                                            h4("Fatalities Over Time, by Maker"),
                                            plotlyOutput("deathbyvendor"), 
                                            h4("Fatalities Over Time, by Maker (normalized)"),
                                            plotlyOutput("deathbyvendor_norm")
                                      )
                                  )
                                             
                        ) ## tabPanel
                        ), ## navbarMenu
             
                   ## "Variable Analysis" Tab
                   navbarMenu("Variable Analysis",
                  
                   tabPanel("Distribution, Correlation Plots",
                            sidebarLayout(
                              sidebarPanel(
                                h4("Likelihood and Distribution Plots"),
                                HTML("<hr>
                                     Use the plots to explore relationships in the data:
                                     <br><br>
                                     <b>Likelihood of Fatality:</b> The percentage of people associated
                                      with a sub-category who died.
                                     <br><br>
                                     <b>Number of Observations:</b> Count of observations for each
                                     sub-category
                                     <br><br>
                                     <b>Mosaic Plot:</b> Compare sub-categories and their relationship to 
                                     a fatality (represented here as 'depvar'). A '1' represents a fatality, '0'
                                     a survivor. To learn more see
                                     <a href = https://en.wikipedia.org/wiki/Mosaic_plot> here</a>.
                                     <br><br>"),
                                selectInput("dataset", "Select a Variable to Explore:",
                                            choices = c("Manufacturer" = 'maker',
                                                        "Fatality In Vehicle" = 'depvar',
                                                        "Drunk Driver Involved" = 'dr_drink',
                                                        "Speeding Involved" = 'speeding',
                                                        "Driver in Previous Accident" = 'prev_acc',
                                                        "Driver with Previous Suspension" = 'prev_sus',
                                                        "Driver with Previous DWI" = 'prev_dwi',
                                                        "Driver with Previous Speeding Violation" = 'prev_spd',
                                                        "Roadtype" = 'roadtype',
                                                        "Relation to Junction" = 'reljct1',
                                                        "Lighting Conditions" = 'lighting',
                                                        "Weather Conditions" = 'weathercond',
                                                        "Number of Fatalities" = 'fatals',
                                                        "Number of Drunk Drivers" = 'drunk_dr',
                                                        "State" = 'st',
                                                        "Night" = 'night',
                                                        "Month" = 'month',
                                                        "Hour" = 'hour'),
                                            selected = 'maker')
                                                        
                              ),
                              mainPanel(
                                splitLayout(cellWidths = c("50%", "50%"),  
                                            plotlyOutput("depvarcorr"),
                                            plotlyOutput("vardensity")),
                                h4("Mosaic Plot"),
                                plotOutput("mosaicPlot")
                              )
                            )
                   ), ## tabPanel
                   
                   ## "Summary Stats" Tab
                   tabPanel("Category Summary Table",
                            sidebarLayout(
                              sidebarPanel(
                                h4("Category Summaries:"),
                                HTML("<b>Variable_Name:</b> field in dataset
                                     <br>
                                     <b>Value:</b> specific category in field
                                     <br>
                                     <b>Count:</b> number of records with category in dataset
                                     <br>
                                     <b>Percent_Total:</b> percent of records in field
                                     <br>
                                     <b>Percent_Fatal:</b> percent of fatalities possessing category
                                     <br>
                                     <hr>"),
                                 checkboxGroupInput("varsinmodel_sums", "Select a variable to view its summary:",
                                                    choices = c("Manufacturer" = 'maker',
                                                                "Fatality In Vehicle" = 'depvar',
                                                                "Drunk Driver Involved" = 'dr_drink',
                                                                "Speeding Involved" = 'speeding',
                                                                "Travel Speed" = 'trav_sp',
                                                                "Driver in Previous Accident" = 'prev_acc',
                                                                "Driver with Previous Suspension" = 'prev_sus',
                                                                "Driver with Previous DWI" = 'prev_dwi',
                                                                "Driver with Previous Speeding Violation" = 'prev_spd',
                                                                "Roadtype" = 'roadtype',
                                                                "Relation to Junction" = 'reljct1',
                                                                "Lighting Conditions" = 'lighting',
                                                                "Weather Conditions" = 'weathercond',
                                                                "Number of Fatalities" = 'fatals',
                                                                "Number of Drunk Drivers" = 'drunk_dr',
                                                                "State" = 'st',
                                                                "Night" = 'night',
                                                                "Month" = 'month',
                                                                "Hour" = 'hour'),
                                                    selected = c('roadtype','dr_drink'),
                                                   inline = F
                                 )
                              ),
                              mainPanel(
                                DT::dataTableOutput("summary"))
                            )
                            
                   ) ##tabPanel
             ), ## navbarMenu
             
             ## "Modeling" Tab
             navbarMenu("Modeling",
             tabPanel("Build & Evaluate Model",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Model and Feature Selection:"),
                          HTML("<hr>"),
                          selectInput("model", 
                                      label = "Model type:",
                                      choices = c('Linear Regression','Logistic Regression'),
                                      selected = 'Linear Regression'),
                          checkboxInput("holdout", "Test"),
                          checkboxGroupInput("varsinmodel", "Variables to include:",
                                             choices = c("Manufacturer" = 'maker',
                                                         "Vehicle Body Type" = 'body_typ',
                                                         "Vehicle Model Year" = 'mod_year',
                                                         "Fatality In Vehicle" = 'depvar',
                                                         "Drunk Driver Involved" = 'dr_drink',
                                                         "Speeding Involved" = 'speeding',
                                                         "Travel Speed" = 'trav_sp',
                                                         "Driver Height" = 'dr_hgt',
                                                         "Driver Weight" = 'dr_wgt',
                                                         "Driver in Previous Accident" = 'prev_acc',
                                                         "Driver with Previous Suspension" = 'prev_sus',
                                                         "Driver with Previous DWI" = 'prev_dwi',
                                                         "Driver with Previous Speeding Violation" = 'prev_spd',
                                                         "Roadtype" = 'roadtype',
                                                         "Relation to Junction" = 'reljct1',
                                                         "Intersection Type" = 'typ_int',
                                                         "Lighting Conditions" = 'lighting',
                                                         "Weather Conditions" = 'weathercond',
                                                         "Persons In Crash" = 'permvit',
                                                         "Number of Fatalities" = 'fatals',
                                                         "Number of Drunk Drivers" = 'drunk_dr',
                                                         "Night" = 'night',
                                                         "Hour of Day" = 'hour',
                                                         "Day of Week" = 'day_week',
                                                         "Day of Month" = 'day',
                                                         "Month" = 'month',
                                                         "State" = 'st',
                                                         "County" = 'county'),
                                             selected = c('roadtype','lighting','weathercond','dr_drink')
                          )
                        ),
                        mainPanel(
                          tabsetPanel(type = "tabs", 
                                      tabPanel("Guide", 
                                               h4("Input"),
                                               HTML("Configure the type and features of your model in the sidebar.  Once constructed,
                                                    you can test your model with data held out from modeling using the 'Test' checkbox.
                                                    <br><br>"),
                                               h4("Evaluation"),
                                               HTML("Check on the performance of your model using the 
                                                    three plots under 'Model Scores'.  
                                                    Predictive power is determined by Decile Prediction Accuracy
                                                    and ROC Curve.  Confidence intervals for coefficients are visualized
                                                    in the Variable Effects plot.  
                                                    <br><br>
                                                    <b>Decile Prediction Accuracy:</b> Visualizing model predictive power for each
                                                    decile of the population.  Useful to understand both true positive and true 
                                                    negative accuracies.  Constructed by taking the whole population,
                                                    scoring them, sorting by score, and taking the average of every decile.
                                                    The horizontal line is the average likelihood of fatality for the entire 
                                                    population.  <i>Power</i> is the top decile likelihood divided by the average
                                                    likelihood.  The higher the power, the greater the accuracy.
                                                    <br><br>
                                                    <b>ROC Curve:</b> Receiver Operating Characteristic curve, used to measure 
                                                    true positive and true negative rates compared to a random guess.  
                                                    More information available 
                                                    <a href = https://en.wikipedia.org/wiki/Receiver_operating_characteristic>
                                                    here</a>.
                                                    <br><br>
                                                    <b>Variable Effects:</b> Plot of coefficients along with their respective
                                                    confidence intervals.  Variables whose confidence intervals pass through zero - the
                                                    vertical line - are considered not statistically significant.
                                                    <br><br>
                                                    To view more detailed information about a model configuration, see the
                                                    Coefficient Summary tab.")), 
                                      tabPanel("Model Scores",   
                                               h4("Model Score"),
                                               fluidRow(
                                                 splitLayout(cellWidths = c("50%", "50%"),
                                                             plotOutput("modelscore"),
                                                             plotOutput("modelscore_ROC"))
                                               ), h4("Variable Effects"), plotOutput("plotCoefs")
                                              ), 
                                      tabPanel("Coefficient Summary",  
                                               h4("Model Summary"), verbatimTextOutput("modelsummary"))
                                    
                          )
                        ) ## mainPanel
                      ) ## sidebarLayout
             ), ## tabPanel
             
             tabPanel("Findings",
                      h4("Discussion"),
                      HTML("Intuitively, one would think the most likely factors associated with fatalities in crashes would be:
                            <br><br>
                           <ul>
                           <li>Speed</li>
                           <li>Driver</li>
                           <li>Environmental Conditions</li>
                           <li>Vehicle Make/Model</li>
                           </ul>
                           <br>
                           In this instance it would appear that our intuition is somewhat correct, though there are certainly some
                           surprising findings.  
                           <br><br>
                           Using only data from 2014, there appears to be a statistically significant negative
                           relationship between speeding, alcohol consumption, and fatalities.  Some manufacturers are seemingly safer
                           than others - Dodge and Ford being safer, while Honda being less so.  Less obvious is the relationship
                           between environmental conditions and fatality.  One would think that rain or poor light could be significant 
                           factors, but in this data that is not the case.  Perhaps most surprising is the difference between rural
                           and urban travel.
                           <br><br>
                           Accidents that occur on rural roads are much more likely to result in a fatality than those that occur in
                           cities.  This statistical relationship is observable in the heat map for US counties, but it is not 
                           immediately clear what the underlying cause may be.  Are average speeds higher on rural roads?  Are there
                           more head on collisions?  These are some of the questions that beg further investigation.
                           <br><br>
                           <h4>Conclusion</h4>
                           As confirmed 
                           <a href = https://en.wikipedia.org/wiki/Traffic_collision#Causes>by other studies</a>, there is a clear
                           relationship between vehicular fatalities and the factors mentioned above.  <i>For manufacturers to continue
                           improving the safety profile of their automobiles, more data needs to be collected and the correlation 
                           between rural roads and fatalities needs to be better understood.</i>")
             )
             
             ) ## navbarMenu
             
  ) ## NavbarPage
) ## Shiny UI

