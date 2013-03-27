source("https://raw.github.com/PirateGrunt/MRMR/master/NAIC.R")
source("https://raw.github.com/PirateGrunt/MRMR/master/NAIC.R")
source("https://raw.github.com/PirateGrunt/CASE-Spring-2013/master/NAIC Data Munging.R")

dfPPAuto = GetNAICData()
dfPPAuto = AdjustNAICData(dfPPAuto)

dfWC = GetNAICData("wkcomp_pos.csv")
dfWC = AdjustNAICData(dfWC)

dfCAuto = GetNAICData("comauto_pos.csv")
dfCAuto = AdjustNAICData(dfCAuto)

dfGL = GetNAICData("othliab_pos.csv")
dfGL = AdjustNAICData(dfGL)

GetUpperTriangle = function(df)
{
  df = subset(df, DevelopmentYear <= 1997)
  
  return (df)
}

GetWeights = function(df, WhichColumn, delta = 0)
{
  weights = df[, WhichColumn]
  weights = 1 / weights
  weights = weights ^ delta
  
  weights[is.na(weights)] = 0
  weights[is.nan(weights)] = 0
  weights[is.infinite(weights)] = 0
  weights[weights < 0] = 0
  
  return (weights)
}

GetPredictionErrors = function(df, Response, Predictor, Recursive = FALSE)
{
  require(lmtest)
  
  fit0 = lm(IncrementalPaid ~ NetEP:LagFactor + 0, weights = GetWeights(df, "NetEP", 0), data = df)
  fit1 = lm(IncrementalPaid ~ NetEP:LagFactor + 0, weights = GetWeights(df, "NetEP", 1), data = df)
  fit2 = lm(IncrementalPaid ~ NetEP:LagFactor + 0, weights = GetWeights(df, "NetEP", 2), data = df)
  
  df$predicted0 = predict(fit0)
  df$predicted1 = predict(fit1)
  df$predicted2 = predict(fit2)
  
  df$residuals0 = (df$IncrementalPaid - df$predicted0)^2
  
  TotalError0 = sum(df$IncrementalPaid) - sum(df$predicted0)
  TotalError1 = sum(df$IncrementalPaid) - sum(df$predicted1)
  TotalError2 = sum(df$IncrementalPaid) - sum(df$predicted2)
  
  MSE0 = mean(residuals(fit0)^2)
  MSE1 = mean(residuals(fit1)^2)
  MSE2 = mean(residuals(fit2)^2)
  
  fitBP = lm(residuals0 ~ NetEP:LagFactor + 0, data=df)
  summBP = summary(fitBP)
  AltBP = pf(summBP$fstatistic[1],summBP$fstatistic[2],summBP$fstatistic[3],lower.tail=FALSE)
  
  bp0 = bptest(fit0)
  bp1 = bptest(fit1)
  bp2 = bptest(fit2)
  
  dfUpper = GetUpperTriangle(df)
  fitUpper0 = lm(IncrementalPaid ~ NetEP:LagFactor + 0, weights = GetWeights(dfUpper, "NetEP", 0), data = dfUpper)
  fitUpper1 = lm(IncrementalPaid ~ NetEP:LagFactor + 0, weights = GetWeights(dfUpper, "NetEP", 1), data = dfUpper)
  fitUpper2 = lm(IncrementalPaid ~ NetEP:LagFactor + 0, weights = GetWeights(dfUpper, "NetEP", 2), data = dfUpper)
  
  df$PredictUpper0 = predict(fitUpper0, newdata = df[, c("NetEP", "LagFactor")])
  df$PredictUpper1 = predict(fitUpper1, newdata = df[, c("NetEP", "LagFactor")])
  df$PredictUpper2 = predict(fitUpper2, newdata = df[, c("NetEP", "LagFactor")])
  
  df$UpperError0 = df$IncrementalPaid - df$PredictUpper0
  df$UpperError1 = df$IncrementalPaid - df$PredictUpper1
  df$UpperError2 = df$IncrementalPaid - df$PredictUpper2
  
  UpperError0 = sum(df[df$DevelopmentYear>1997, "UpperError0"])
  UpperError1 = sum(df[df$DevelopmentYear>1997, "UpperError1"])
  UpperError2 = sum(df[df$DevelopmentYear>1997, "UpperError2"])
  
  UpperMSE0 = mean(df[df$DevelopmentYear>1997, "UpperError0"]^2)
  UpperMSE1 = mean(df[df$DevelopmentYear>1997, "UpperError1"]^2)
  UpperMSE2 = mean(df[df$DevelopmentYear>1997, "UpperError2"]^2)
  
  CompanyName = unique(df$GroupName)
  
  dfResults = data.frame(CompanyName = CompanyName
                         , TotalError0 = TotalError0
                         , TotalError1 = TotalError1
                         , TotalError2 = TotalError2
                         , MSE0 = MSE0
                         , MSE1 = MSE1
                         , MSE2 = MSE2
                         , bpP0 = bp0$p.value
                         , bpP1 = bp1$p.value
                         , bpP2 = bp2$p.value
                         , UpperError0 = UpperError0
                         , UpperError1 = UpperError1
                         , UpperError2 = UpperError2
                         , UpperMSE0 = UpperMSE0
                         , UpperMSE1 = UpperMSE1
                         , UpperMSE2 = UpperMSE2
                         , AltBP = AltBP)
  
  return (dfResults)
}

GetLineResults = function(df){
  lCompanyDFs = split(df, df$GroupCode)
  lTestResults = lapply(lCompanyDFs, GetPredictionErrors)
  dfTestResults = do.call("rbind", lTestResults)
  
  return(dfTestResults)
}

PlotResults = function(df, Title)
{
  par(mfrow = c(1,1))
  
  pointMag = 2
  
  plot(log(df$MSE2), pch = 19, col = "red", main=paste0("Log MSE - ", Title), xlab = "Company", ylab = "Ln(MSE)", cex=pointMag)
  axis(2, axTicks(2), format(axTicks(2), scientific = F))
  points(log(df$MSE1), pch=19, col="yellow", cex=pointMag)
  points(log(df$MSE0), pch=19, col="green", cex=pointMag)
  
  plot(log(df$UpperMSE2), pch = 19, col = "red", main=paste0("Log UpperMSE - ", Title), xlab = "Company", ylab = "Ln(UpperMSE)", cex=pointMag)
  axis(2, axTicks(2), format(axTicks(2), scientific = F))
  points(log(df$UpperMSE1), pch=19, col="yellow", cex=pointMag)
  points(log(df$UpperMSE0), pch=19, col="green", cex=pointMag)
  
  plot(df$UpperError2, pch = 19, col = "red", main=paste0("Upper Error - ", Title), xlab = "Company", ylab = "Upper Error", cex=pointMag)
  axis(2, axTicks(2), format(axTicks(2), scientific = F))
  points(df$UpperError1, pch=19, col="yellow", cex=pointMag)
  points(df$UpperError0, pch=19, col="green", cex=pointMag)
  abline(0,0)
  
  plot(df$AltBP, log(df$MSE0), pch=19, col="black", main = paste0("BP vs MSE - ", Title), xlab = "BP pVal", ylab = "Ln(MSE)", cex=pointMag)
  axis(2, axTicks(2), format(axTicks(2), scientific = F))
}

dfTestPPAuto = GetLineResults(dfPPAuto)
dfTestCAuto = GetLineResults(dfCAuto)
dfTestGL = GetLineResults(dfGL)
dfTestWC = GetLineResults(dfWC)

PlotResults(dfTestPPAuto, "PP Auto")
PlotResults(dfTestCAuto, "Commercial Auto")
PlotResults(dfTestGL, "GL")
PlotResults(dfTestWC, "WC")

plot(dfTestWC$AltBP, cex =2, pch=19)

plot(dfTestGL$AltBP, dfTestGL$MSE0, pch=19)


