#############################
## Shiny App Server Script ##
#############################
## Make sure the following are installed:
## install.packages('shiny')
## install.packages('leaflet')
## install.packages('DT')
## install.packages('ROCR')
## install.packages('noncensus')
## install.packages('plotly')
## install.packages('metricsgraphics')
## install.packages("coefplot")
## Load packages
library(shiny)
library(datasets)
library(dplyr)
library(ROCR)
library(RColorBrewer)
library(ROCR)
library(ggplot2, quietly=TRUE)
library(reshape2)
library(noncensus)
library(vcd)
library(plotly)
library(coefplot)
#library(sparklyr)
# library(sp)   # do we need this here to?

## Load functions: clean-cats.R, create-buckets.R, decile-plot.R, roc-curve.R, model.R, summaries.R
# source("./../R/functions.R")
source("./../R/clean-cats.R")
source("./../R/create-buckets.R")
source("./../R/decile-plot.R")
source("./../R/roc-curve.R")
source("./../R/model.R")
source("./../R/summaries.R")


#############################
## INGEST AND PREPARE DATA ##
#############################
# mod_dat1 <- readRDS("./data/mod_dat_all_1.rds")
# mod_dat2 <- readRDS("./data/mod_dat_all_2.rds")
# mod_dat3 <- readRDS("./data/mod_dat_all_3.rds")
# mod_dat = rbind(mod_dat1,mod_dat2, mod_dat3)
# mod_dat = mod_dat %>% filter(!is.na(fatality_ind))
# samp_crashdata = sample_n(mod_dat, 100000)
# saveRDS(samp_crashdata, file= "./data/samp_data_100k.rds")
map_dat <- readRDS("./../data/mapdata.rds")

## Initialize Spark Context and push data to Spark
#sc <- spark_connect(master = "local")

# note - server.R has been moved 
samp_data = readRDS("./../data/samp_data_100k.rds")
sc_crashdata = sample_n(samp_data, 10000)
#sc_crashdata <- copy_to(sc, mod_dat, 'sc_crashdata', overwrite = T)
#tbl_cache(sc, 'sc_crashdata')

## Number of deaths for each manufacturer by year, key is year + maker
maker_fatals <- group_by(sc_crashdata, year, maker) %>% 
  summarise(sumfatal = sum(deaths)) %>%
  arrange(desc(sumfatal)) #%>%
  #collect() %>% 
  #na.omit()

## Percent of total
pct_maker <- summarize(group_by(sc_crashdata, maker), pct_rows = (n()/nrow(sc_crashdata)) * 100) %>% 
  na.omit() 
pct_maker$pct_rows <- round(pct_maker$pct_rows, 2)  ## Round the values for clarity

## Normalize, filter makes with less than 2% of records
maker_fatals <- merge(x = maker_fatals, y = pct_maker, by ="maker", all.x = T) %>% 
  mutate(norm_fatal = sumfatal/pct_rows) %>%
  filter(pct_rows > 2, year > 1980) 

## Make copy of dataframe
makers_fatals2 <- maker_fatals

## Data for Shiny App
dataIn <- sc_crashdata
colnames(dataIn)[40] <-"depvar"         # replacing 'fatality_ind' with 'depvar' (as in dependent variable)

shinyServer(function(input, output) {
  
  ###################
  ## SUMMARY TABLE ##
  ###################
  
  ## Create summary statistics: 
  output$summary <- DT::renderDataTable({
    summaries_fn = function(input,df){
      sums = data.frame(varname = input, dplyr::summarize(group_by_(df, as.name(paste(input))),
                                                          n = n(), 
                                                          pct_n = n()/NROW(df),
                                                          avg = mean(as.numeric(depvar))))
      names(sums) = c("Variable_Name", "Value", "Count", "Percent_Total", "Percent_Fatal")
      sums$Value = as.character(sums$Value)
      sums$Percent_Total <- round(sums$Percent_Total, 2)
      sums$Percent_Fatal <- round(sums$Percent_Fatal, 2)
      return(sums)
    }
    mod_dat= dataIn
    
    runnames = input$varsinmodel_sums
    out = list()
    for (i in 1:NROW(runnames)){
      out[[i]] = summaries_fn(runnames[i], mod_dat)
    }
    
    out = do.call("rbind",out)
    
    DT::datatable(
      out, options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      )
    )
  })
  
  ######################
  ## MAKER COMPARISON ##
  ######################
  
  ## Create death by vendor visuals: 
  output$deathbyvendor <- renderPlotly({
    ggplot(data = maker_fatals,
           aes(x = year, y = sumfatal, color = maker)) + 
      geom_line(size = 2) + 
      labs(title = "Automotive Deaths by Manufacturer, 1980-2013",
           x = "Year", 
           y = "Number of Deaths",
           color = "") +
      theme_minimal()
    
  })
  output$deathbyvendor_norm <- renderPlotly({
    ggplot(data = maker_fatals, aes(x = year, y = norm_fatal, color = maker)) + 
      geom_line(size = 2) +
      labs(title = "Automotive Deaths by Manufacturer, 1980-2013 (Normalized)",
           x = "Year", 
           y = "Number of Deaths",
           color = "") +
      theme_minimal()
  })
  
  ###############################
  ## MODEL SUMMARY AND SCORING ##
  ###############################
  
  # note - do we have to redefine the same model in each of these methods? 
  # Why don't we define model in the outer scope, I doubt model_fn is inexpensive
  
  ## Create model summary read out: 
  output$modelsummary <- renderPrint({
    model <- model_fn(dataIn = dataIn,
                      varsinmodel = input$varsinmodel,
                      todummies = FALSE,
                      holdout = input$holdout,
                      model_in=input$model)$model
    summary(model)
    
  })
  
  ## Plot coeficients of the model, see code for credit on chart: 
  output$plotCoefs <- renderPlot({
    model <- model_fn(dataIn = dataIn,
                      varsinmodel = input$varsinmodel,
                      todummies = FALSE,
                      holdout = input$holdout,
                      model_in = input$model)$model
    # plotCoef(model)
    coefplot(model, title = "", xlab = "", ylab="")
  })
  
  
      output$frame <- renderUI({
    my_test <- tags$iframe(src="map.html", height=600, width=700)
  })
  
  
  ## Create model score plot, this needs to be documented: 
  output$modelscore <- renderPlot({
    
    pred <- model_fn(dataIn = dataIn,
                     varsinmodel = input$varsinmodel,
                     todummies = FALSE,
                     holdout = input$holdout,
                     model_in = input$model)$pred
   # withProgress(message = 'Making plot', value = 0, {
   # sc_crashdata2 = sc_crashdata %>% select_("fatality_ind",input$varsinmodel) %>% na.omit()
   #fit = ml_linear_regression(sc_crashdata2, response = "fatality_ind", features = c(input$varsinmodel))

   #pred = predict(fit, sc_crashdata2)
    #})
    
    # Comments on the next two lines (naming confusion)
    # 1. We're creating a new column named 'quartile', which is actually deciles. That's kind of misleading
    # 2. Then on the next line, we're creating a column in predout called 'pred' as in "prediction", which is the mean of the
    #    dependent variable, which is what literally happened, not the prediction. 
    pred <- pred %>% dplyr::mutate(quartile = ntile(pred, 10))
    predout <- dplyr::summarise( dplyr::group_by(pred, quartile), pred = mean(as.numeric(depvar))) # should 'depvar' be replaced with 'pred'?
    decileAccuracyPlot(predout)
    #axis(1,1:10, at = 1:10)
  })
  
  ## Create ROC curve for model scoring 
  output$modelscore_ROC <- renderPlot({
    
    model <- model_fn(dataIn = dataIn,
                      varsinmodel = input$varsinmodel,
                      todummies = FALSE,
                      holdout = input$holdout,
                      model_in = input$model)$model
    
    test_dat <- model_fn(dataIn = dataIn,
                         varsinmodel = input$varsinmodel,
                         todummies = FALSE,
                         holdout = input$holdout,
                         model_in = input$model)$test_dat
    
    if(input$model == "rf"){
      pr <- predict(model, test_dat, type = "prob")[,2]
    } else if (input$model == "nb") {
      pr <- predict(model, test_dat, type = "raw")[,2]
    } else {
      pr <- as.numeric(predict(model, test_dat))
    }
    
    pred <- prediction(pr, test_dat$depvar)
    
    roccurve(pred)
  })
  
  #################
  ## LEAFLET MAP ##
  #################
  output$leafPlot <- renderLeaflet({
    ## Popup
    county_popup <- paste("<strong>County:</strong>", 
                          map_dat@data$name, 
                          "<br><strong>State:</strong>",
                          map_dat@data$state_name,
                          "<br><strong>County Population: </strong>", 
                          map_dat@data$population,
                          "<br><strong>Total Fatalities: </strong>", 
                          map_dat@data$total,
                          "<br><strong>Deaths per 10,000:</strong>",
                          map_dat@data$scaled)
    ## Color palette
    pal <- colorQuantile("YlOrRd", NULL, n = 10, na.color = "transparent")
    ## Chloropleth map with scaled fatality rates for each county, data permitting
    leaflet(data = map_dat) %>% 
      setView(lng = -98.583, lat = 39.833, zoom = 3) %>%
      addTiles() %>% 
      addPolygons(fillColor = ~pal(scaled), 
                  fillOpacity = 0.6,
                  weight = 0.5,
                  popup = county_popup) %>%
      addLegend("bottomright", pal = pal, values = ~scaled,
                title = "Fatalities per 10,000 (Percentile)",
                opacity = 0.7) 
  })
  
  #######################
  ## VARIABLE ANALYSIS ##
  #######################
  
  ## Create dependent variable correlations for variable analysis top chart 
  output$depvarcorr <- renderPlotly({
    sums = summaries(input$dataset, 
                     df = dataIn, 
                     todummies = FALSE,
                     todummies3 = FALSE,
                     runnames = input$varsinmodel)
    
    ggplot(data = sums,
           aes(x = sums$value, y = sums$avg)) +
      geom_bar(stat = 'identity', fill = 'steelblue') +
      labs(x = "Variable Categories", 
           y = "Pct Fatalities in Category", 
           title = paste("Likelihood of Fatality Given:", input$dataset)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
            axis.text.y = element_text(size = 8)) 
  })
  
  ## Create variable density chart for variable analysis top chart 
  output$vardensity <- renderPlotly({
    sums = summaries(input$dataset, 
                     df = dataIn,
                     todummies = FALSE,
                     todummies3 = FALSE,
                     runnames = input$varsinmodel)
    
    ggplot(data = sums,
           aes(x = sums$value, y = sums$n, text = paste("Category: ", sums$value))) +
      geom_bar(stat = 'identity', fill = 'indianred') +
      labs(x = "Category", 
           y = "Count", 
           title = paste("No. of Observations for:", input$dataset)) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
            axis.text.y = element_text(size = 8)) 

    #plot(dataIn[,"depvar"],dataIn[,input$dataset], main = "depvar corr")
  })
  
  ## Create mosaic plots for variable analysis
  ## Note: These are hard coded because the package isn't robust
  output$mosaicPlot <- renderPlot({
    df = dataIn
    names(df) = make.names(names(df), unique=TRUE)
    
    if(input$dataset=='night') mosaic(depvar~`night`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(night = 5))) 
    if(input$dataset=='permvit') mosaic(depvar~`permvit`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(permvit = 2))) 
    if(input$dataset=='month') mosaic(depvar~`month`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(month = 2))) 
    if(input$dataset=='hour') mosaic(depvar~`hour`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(hour = 2))) 
    if(input$dataset=='reljct1') mosaic(depvar~`reljct1`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(reljct1 = 3))) 
    if(input$dataset=='fatals') mosaic(depvar~`fatals`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(fatals = 3))) 
    if(input$dataset=='drunk_dr') mosaic(depvar~`drunk_dr`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(drunk_dr = 2))) 
    if(input$dataset=='st') mosaic(depvar~`st`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(st = 3))) 
    if(input$dataset=='roadtype') mosaic(depvar~`roadtype`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(roadtype = 5))) 
    if(input$dataset=='lighting') mosaic(depvar~`lighting`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(lighting = 5))) 
    if(input$dataset=='weathercond') mosaic(depvar~`weathercond`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(5))) 
    if(input$dataset=='dr_drink') mosaic(depvar~`dr_drink`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(dr_drink = 2))) 
    if(input$dataset=='maker') mosaic(depvar~`maker`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(maker = 3))) 
    if(input$dataset=='speeding') mosaic(depvar~`speeding`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(speeding = 3))) 
    if(input$dataset=='trav_sp') mosaic(depvar~`trav_sp`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(trav_sp = 3))) 
    if(input$dataset=='prev_acc') mosaic(depvar~`prev_acc`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(prev_acc = 3))) 
    if(input$dataset=='prev_sus') mosaic(depvar~`prev_sus`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(prev_sus = 3))) 
    if(input$dataset=='prev_dwi') mosaic(depvar~`prev_dwi`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(prev_dwi = 3))) 
    if(input$dataset=='prev_spd') mosaic(depvar~`prev_spd`, data = df,shade=T, colorize = T, gp = gpar(fill=matrix(c('steelblue',gray.colors(10)[1], 'red',gray.colors(10)[1]), 2, 2)), labeling_args = list(abbreviate_labs = c(prev_spd = 3))) 
  }, width = 800, height = 600)
})

