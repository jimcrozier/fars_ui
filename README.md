# FARS_UI
Shiny app using decades of publicly available fatal crash data (FARS) to understand factors in fatal accidents.

To run app, download and open /ui/server.R in RStudio and hit the Run App button (alternatively, load shiny (library(shiny)) and runApp("ROUTE../ui.R"). 

Notice that data is currently sampled in the UI, we will work towards pushing the analysis to spark and retaining only the necessary information for the model in the UI layer. 
