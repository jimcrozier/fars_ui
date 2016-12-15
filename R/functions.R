
###################################################################################
## This script contains functions to be called in data prep and in the Shiny App ##
## It contains the following functions:                                          ##
## 1. clean_cats()                                                               ##
## 2. plotCoef(), plotCoefBase()                                                 ##
## 3. model_fn()                                                                 ##
## 4. create_buckets()                                                           ##
## 5. decileAccuracyPlot()                                                       ##
## 6. roccurve()                                                                 ##
## 7. summaries()                                                                ##
###################################################################################

##################
## clean_cats() ##
##################
# This function returns a vector that is the the categories (presumably)
# that exist in the input vector 'input_nm' ("input name", probably), but only if they're above a 
# certain threshold. If not, they're thrown into the "Other" category.
clean_cats = function(df, input_nm,threshold){
  # creates a new column in character form of input_nm
  df$cat = as.character(df[,input_nm])
  
  # replaces NA with "Unkown for all rows in df
  df$cat[is.na(df$cat)] = "Unknown"
  
  # df = group_by(df, cat)
  # Then create a new column in df that is percent_of_n / total rows
  df <- df %>% 
    group_by(cat) %>% 
    mutate(pct_n = n()/NROW(df))
  
  # creates new column called 'cat2' that is the same as 
  # df$cat, UNLESS df$pct_n is above threshold, then df$cat2
  # for that row is "Other"
  df$cat2 = ifelse(df$pct_n>threshold, df$cat, "Other")
  
  return(df$cat2)
}

################################
## plotCoef(), plotCoefBase() ##
################################

# is plotCoefBase() used anywhere? I read that Jared Lander said it does the same thing, except just without ggplot.
# I'm going to try to replace this with the 

## Copyright (c) 2011, under the Simplified BSD License.
## For more information on FreeBSD see: http://www.opensource.aorg/licenses/bsd-license.php
## All rights reserved.


#  - used around line 160ish of server.R
plotCoef <- function(model,Main="",YLab="",XLab="",labelDirec=2,CEX=.8,LWD68=1,LWD95=0,vertColor="grey",vertLWD=1,vertType=2,Color="blue",Intercept=TRUE,Interaction="\xD7",ShortenFactors=TRUE)
  ## Written by Jared P. Lander, www.jaredlander.com
  ## Latest draft:  10/13/2010
{
  if(!"package:ggplot2" %in% search())
    # check if ggplot2 is loaded
  {
    if(!"ggplot2" %in% installed.packages())
      # see if it is even installed
      # if not, stop the function and ask the user to install it
    {
      cat("Please install ggplot2 before running this function\n")
      stop()
    }
    
    require(ggplot2)	# load the package
    cat("ggplot2 was loaded\n")
  }
  
  modelCoef <- model$coef		# get the coefficients
  
  ## this section grabs the standard deviations, it must be done differently depending on the type of model (lm or glm)
  if(model$call[1] == "lm()")
  {
    modelSE <- summary(model)$coefficients[, 2]		## gets standard error from summary()
    #modelSE <- sqrt(diag(summary(model)$cov))
  }
  else if(model$call[1] == "glm()")
  {
    modelSE <- sqrt(diag(summary(model)$cov.scaled))
  }
  
  modelCI <- data.frame(Low95=modelCoef - 2*modelSE,High95=modelCoef + 2*modelSE,Low68=modelCoef - modelSE,High68=modelCoef + modelSE,Coef=modelCoef) # build a data.frame of the confidence bounds and original coefficients
  modelCI$Name <- rownames(modelCI)	## grab the coefficient names into the data.frame
  
  if(ShortenFactors)
  {
    varNames <- names(model$model)[-1]	# grabs the variable names
    varFrame <- data.frame(varNames)	# starts a data.frame with those names
    varFrame$factor <- ifelse(attr(model$terms,"dataClasses")[-1]=="factor",1,0)	# denote each variable as a factor or not
    modelNames <- names(modelCoef)		# get the coefficient names
    modelNames <- sub("factor\\([a-zA-Z0-9]*\\)","",modelNames)	# strip the word "factor" and the included variable from any factors, only works for the kind like "factor(...)"
    for(i in 1:nrow(varFrame))
      # go through and strip the remaining factors
    {
      if(varFrame[i,2] == 1)
        # if that variable is a factor
      {
        modelNames <- gsub(varFrame[i,1],"",modelNames)	# strip out the variable name
      }
    }
    modelCI$Name <- modelNames	# change the names of the coefficients to these new names
  }
  
  modelCI$Name <- gsub("\\*|\\:x",Interaction,modelCI$Name,perl=T)
  
  if(Intercept == FALSE)
  {
    modelCI <- modelCI[-which(modelCI$Name=="Intercept"),]
  }
  
  modelMelt <- melt(modelCI,id="Name")	# melt the frame to make it easier to work with ggplot
  modelMelt95 <- modelMelt[modelMelt$variable=="Low95" | modelMelt$variable=="High95",]	# pull out the 95% CI
  modelMelt68 <- modelMelt[modelMelt$variable=="Low68" | modelMelt$variable=="High68",]	# pull out the 68% CI
  qplot(Coef,Name,data=modelCI,main=Main,xlab=XLab,ylab=YLab) + geom_vline(xintercept=0,colour=vertColor,linetype=vertType,lwd=vertLWD) + geom_line(aes(x=value),data=modelMelt95,colour=Color,lwd=LWD95) + geom_line(aes(x=value),data=modelMelt68,colour=Color,lwd=LWD68) + geom_point(colour=Color) # the actual plotting
}

plotCoefBase <- function(model,Main="",YLab="",XLab="",labelDirec=2,CEX=.8,LWD68=3,LWD95=1,Interaction="\xD7",Color="blue",vertType=2,vertColor="grey",ShortenFactors=TRUE)
  ## Written by Jared P. Lander, www.jaredlander.com
  ## Latest draft:  10/13/2010
{
  modelCoef <- model$coef		# get the coefficients
  numCoef <- length(modelCoef)	# find out how many coefficients there are in the model
  
  ## this section grabs the standard deviations, it must be done differently depending on the type of model (lm or glm)
  if(model$call[1] == "lm()")
  {
    modelSE <- sqrt(diag(summary(model)$cov))
  }
  else if(model$call[1] == "glm()")
  {
    modelSE <- sqrt(diag(summary(model)$cov.scaled))
  }
  
  modelCI <- data.frame(Low95=modelCoef - 2*modelSE,High95=modelCoef + 2*modelSE,Low68=modelCoef - modelSE,High68=modelCoef + modelSE)	# build a data.frame of the confidence bounds and original coefficients
  
  plot(modelCoef,1:numCoef,type="n",col=Color,xlim=c(floor(min(modelCI$Low95)),ceiling(max(modelCI$High95))),yaxt="n",ylab=YLab,xlab=XLab,main=Main)	# plot the (blank) graph for the coefficients leaving room for the 95% CIs, with no vertical axis
  abline(v=0,lty=vertType,col=vertColor)	# add in a vertical line at 0 for a sense of scale
  
  for(i in 1:numCoef)
    # plot the CI lines for each coefficient
  {
    lines(c(modelCI$Low95[i],modelCI$High95[i]),c(i,i),col=Color,lwd=LWD95)
    lines(c(modelCI$Low68[i],modelCI$High68[i]),c(i,i),col=Color,lwd=LWD68)
  }
  
  points(modelCoef,1:numCoef,col=Color,pch=16,cex=CEX)	# put in the coefficient points on top
  
  modelNames <- names(modelCoef)	# get the names of the coefficients
  
  if(ShortenFactors)
  {
    varNames <- names(model$model)[-1]	# grabs the variable names
    varFrame <- data.frame(varNames)	# starts a data.frame with those names
    varFrame$factor <- ifelse(attr(model$terms,"dataClasses")[-1]=="factor",1,0)	# denote each variable as a factor or not
    modelNames <- sub("factor\\([a-zA-Z0-9]*\\)","",modelNames)	# strip the word "factor" and the included variable from any factors, only works for the kind like "factor(...)"
    for(i in 1:nrow(varFrame))
      # go through and strip the remaining factors
    {
      if(varFrame[i,2] == 1)
        # if that variable is a factor
      {
        modelNames <- gsub(varFrame[i,1],"",modelNames)	# strip out the variable name
      }
    }
  }
  
  modelNames <- gsub("\\*|\\:x",Interaction,modelNames,perl=T)
  
  axis(side=2,labels=modelNames,at=1:numCoef,las=labelDirec,cex.axis=CEX)		# plot the vertical axis with the names of the coefficients
}

clean_levels = function(level_vec,threshold){
  tab = table(level_vec) > threshold
  dftab = data.frame(tab)
  id = !level_vec %in% row.names(dftab[tab,])
  level_vec[id] = 'unknown'
  return(level_vec)
}

################
## model_fn() ##
################

# model_in = model type, from {"Linear Regression", "Naive Bayes", "Logistic Regression", "Random Forest"}
model_fn = function(dataIn, varsinmodel,todummies,holdout,model_in){
  dataIn = data.frame(dataIn, stringsAsFactors = FALSE)
  runnames = varsinmodel
  mod_dat = dataIn[,c("depvar", runnames )]
  
        for (i in 1:NROW(runnames)){
      if(!runnames[i] %in% c("depvar")) mod_dat[,runnames[i]] = clean_levels(mod_dat[,runnames[i]],10)
      }
  
  
  if(todummies){
    for (i in 1:NROW(runnames)){
      if(!runnames[i] %in% c("Gender","Age-range")) mod_dat[,runnames[i]] = ifelse(mod_dat[,runnames[i]]>1,1,0)
    }
  }
  
  if(holdout){
    train_dati <- base::sample(nrow(mod_dat), 0.75*nrow(mod_dat))
    mod_dat = mod_dat[train_dati,]
    test_dat = mod_dat[-train_dati,]
  } else {
    train_dati = 1:nrow(mod_dat)
    test_dat = mod_dat
  }
  
  if(model_in=="Linear Regression") model = lm(depvar~., data = mod_dat)
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

######################
## create_buckets() ##
######################

create_buckets = function(input_vec,cuts=5){
  out = as.factor(cut(input_vec,quantile(input_vec,probs = seq(0, 1, 1/5)),labels=F))
  return(out)
}

test1 <- function(type) {
  switch(type,
         "1" = "lowest",
         "2" = "low",
         "3" = "mid",
         "4" = "high",
         "5" = "highest")
}

##########################
## decileAccuracyPlot() ##
##########################
decileAccuracyPlot = function(predout){
  predout =  predout %>% filter(!is.na(predout$pred))
  barplot(predout$pred, col = "steelblue",
          main = paste("Decile Predicition Accuracy \n Power*: ", round(predout$pred[10]/mean(predout$pred,na.rm=T),2)),
          xlab = "Decile \n *Power is top decile/average",names.arg =1:NROW(predout), ylab= "Likelihood of Death | Accident")
  abline(h=mean(predout$pred,na.rm=T))
}

################
## roccurve() ##
################
roccurve = function(pred){
  pe <- performance(pred, "tpr", "fpr")
  au <- performance(pred, "auc")@y.values[[1]]
  pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
  p <- ggplot(pd, aes(x=fpr, y=tpr))
  p <- p + geom_line(colour="red")
  p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
  p <- p + ggtitle("ROC Curve, Target: Fatality Indicator")
  p <- p + theme(plot.title=element_text(size=10))
  p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
  p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                    label=paste("AUC =", round(au, 2)))
  print(p)
}  
#################
## summaries() ##
#################
  summaries = function(input_nm, df, todummies, todummies3, runnames){
    #runnames = input$varsinmodel
    #if(todummies | todummies3){
    #  for (i in 1:NROW(runnames)){
    #    if(!runnames[i] %in% c("Gender","Age-range")) df[,runnames[i]] = ifelse(df[,runnames[i]]>1,1,0)
    #  }
    # }
    df = df %>% select_("depvar", input_nm)
       df[,input_nm]= clean_levels(df[,input_nm],50)
    

    sums = data.frame(varname = input_nm, 
                      dplyr::summarize(group_by_(df, as.name(paste(input_nm))),
                                       n = n(), 
                                       pct_n = n()/NROW(df),
                                       avg = mean(depvar)))
    names(sums) = c("varname", "value","n", "pct_n", "avg_fatality")
    #sums$pct_n = sums$n/NROW(df)
    sums$value = as.character(sums$value)
    return(sums)
  }


