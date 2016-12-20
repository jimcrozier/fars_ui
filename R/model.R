#' Create a model
#'
#' Makes a model based on data passed in, with vars that are selected in the application.
#' You can retrieve a particular attribute from the returned list with '$', i.e. list$mod_dat
#' 
#'
#' @param dataIn data passed into the function
#' @param varsinmodel a list of variables to be included in the model
#' @param todummies a boolean, true if you would like to turn the data into dummy variables
#' @param holdout a boolean, true if the user would like to do holdout sampling (use train and test data)
#'
#' @return a list with everything you need to know about the model
#'
#' @examples
#' model <- model_fn(dataIn = dataIn,
#'                   varsinmodel = c("lighting", "roadtype"), # varsinmodel user selected in the ui
#'                   todummies = FALSE,                       
#'                   holdout = FALSE,                         # holdout user selected in the ui
#'                   model_in="Linear Regression")$model      # model_in user selected in the ui
#'
#' @export
model_fn = function(dataIn, varsinmodel, todummies, holdout, model_in){
  # NOTES FOR US
  #   - anywhere it says 'REVIEW THIS', it means I've suggested a change
  #   - the biggest change I want to make is the todummies if clause, I don't think it's done correctly
  #
  

  dataIn = data.frame(dataIn, stringsAsFactors = FALSE)
  runnames = varsinmodel
  
  # we're modeling depvar using all the variables in runnames
  mod_dat = dataIn[,c("depvar", runnames )]
  
  # note - in dr_drink, there are 0's and 1's (as expected), but also 9's. You guys know what they mean?
  #        they get thrown out by clean_levels
  
  # run clean_levels on each column
  for (i in 1:NROW(runnames)){
    # should we prevent the user from being able to choose fatality index as a feature?
    # if they do choose it, what's the point of the model
    if(!runnames[i] %in% c("depvar")) {
      mod_dat[,runnames[i]] = clean_levels(mod_dat[,runnames[i]],10)
    }
  }
  
  # REVIEW THIS
  if(todummies){
    for (i in 1:NROW(runnames)){
      if(!runnames[i] %in% c("Gender","Age-range")) {
        # not sure I understand this next line. What does it mean for "Dusk" to be greater than 1?
        # If we're taking a column with 7 or so unique values and replacing them with just
        # 0's and 1's, we're losing information.
        #
        # Here's what I think should happen - 
        # If column 'lighting' had 3 values - 'Dusk', 'Daylight' and 'Dawn', we should split it 
        # up into three boolean columns, (i.e. 'is_Dusk', 'is_Dawn', 'is_Daylight').
        # 
        # If that is the goal, I'm sure there is a package so we don't have to do it manually.
        mod_dat[,runnames[i]] = ifelse(mod_dat[,runnames[i]]>1,1,0)
      }
    }
  }
  
  if(holdout){
    # get random indices for 75% of the data, then split the data into 
    train_dati <- base::sample(nrow(mod_dat), 0.75*nrow(mod_dat))
    
    # this works, but why isn't mod_dat overwritten on the first line?
    mod_dat = mod_dat[train_dati,]
    test_dat = mod_dat[-train_dati,]
  } else {
    train_dati = 1:nrow(mod_dat)    # is this line necessary? train_dati is an unused local var
    test_dat = mod_dat
  }
  
  # make the model
  # the user currently only has the option for linear and logistic regression
  if(model_in=="Linear Regression") model = lm(depvar~., data = mod_dat)   # ~. means all vars not already mentioned
  if(model_in=="Naive Bayes") model = naiveBayes(as.factor(depvar)~., data = mod_dat)
  if(model_in=="Logistic Regression") model = glm(depvar~., data = mod_dat,family=binomial(link='logit'))
  if(model_in=="Random Forest"){
    
    names(mod_dat) = c("depvar", letters[1:(NCOL(mod_dat)-1)])
    names(test_dat) = c("depvar", letters[1:(NCOL(mod_dat)-1)])
    model <- randomForest::randomForest(as.factor(depvar) ~ .,
                                        data=mod_dat,
                                        ntree=500,
                                        mtry=3,
                                        importance=TRUE,
                                        na.action=randomForest::na.roughfix,
                                        replace=FALSE)
  }
  
  # now score the model
  # REVIEW THIS -> because we set 'test_dat = mod_dat' if holdout is false, this can be shortened a bit
  if(holdout){
    # for(vari in 1:NROW(varsinmodel)){
    #    id <- which(!(test_dat[,varsinmodel[vari]] %in% levels(mod_dat[,varsinmodel[vari]])))
    #    test_dat[id,] <- NA
    # }

    pred = data.frame(depvar= test_dat$depvar, pred = predict(model, newdata = test_dat))
  } else {

        # for(vari in 1:NROW(varsinmodel)){
        #    id <- which(!(mod_dat[,varsinmodel[vari]] %in% levels(mod_dat[,varsinmodel[vari]])))
        #    mod_dat[id,] <- NA
        # }

    pred = data.frame(depvar= mod_dat$depvar, pred = predict(model, newdata = mod_dat))
  }
  
  data_in_train = data.frame(mod_dat, pred = predict(model, newdata =  mod_dat))
  data_in_val = data.frame(test_dat,pred = predict(model, newdata = test_dat))
  return(list(pred=pred, model=model, test_dat = test_dat, mod_dat=mod_dat, data_in_train= data_in_train, data_in_val=data_in_val))
}

#' Make clean levels
#'
#' The 'levels' are the set of values within some vector or factor in R. 
#' This function takes in a vector, and any level that has less than or equal 
#' to n instances (where n = threshold),  the function replaces the value with 
#' 'unknown'. Note that it also turns whatever data type that enters into a string.
#'
#' @param level_vec the input vector
#' @param threshold how many instances a level must have to not get replaced by 'unkown'
#'
#' @return the input vector, but with the altered levels.
#'
#' @examples
#' a <- c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4)
#' clean_levels(a, 2)
#' # returns the vector ["1", "1", "1", "1", "2, "2", "2", "unknown", "unknown", "unknown"]
#'
#' @export
clean_levels = function(level_vec,threshold){
  tab = table(level_vec) > threshold
  dftab = data.frame(tab)
  id = !level_vec %in% row.names(dftab[tab,])
  level_vec[id] = 'unknown'
  return(level_vec)
}