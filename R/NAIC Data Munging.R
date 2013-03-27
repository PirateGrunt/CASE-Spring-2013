Cumul2Incr = function(OriginPeriodStart, DevelopmentLag, CumulIn)
{
  df = data.frame(OriginPeriodStart = OriginPeriodStart, DevelopmentLag = DevelopmentLag, Cumul = CumulIn)
  df = df[order(df$OriginPeriodStart, df$DevelopmentLag),]
  alist = split(df, as.factor(df$OriginPeriodStart))
  alist = lapply(alist, "[[", "Cumul")
  
  alist = sapply(alist, function(x){
    incr = c(x[1], diff(x))
    #incr = diff(x)
  }
  )
  NewCol = do.call("rbind", as.list(alist))
  return (NewCol)
}

AdjustNAICData = function(df){
  df$LagFactor = as.factor(df$DevelopmentLag)
  df$CumulativePaid = as.numeric(df$CumulativePaid)
  
  lCompanyDFs = split(df, df$GroupCode)
  
  lCompanyDFs = lapply(lCompanyDFs, function(x){
    x$IncrementalPaid = Cumul2Incr(x$OriginPeriodStart, x$DevelopmentLag, x$CumulativePaid)
    return (x)})
  df = do.call("rbind", lCompanyDFs)
  
  #================================================================
  # Remove extreme companies
  library(plyr)
  
  dfPaidSummary = ldply(lCompanyDFs, .fun=function(x){AvgPaid = mean(x$IncrementalPaid)
                                                 CompanyName = x[1,"GroupName"]
                                                 MinPaid = min(x$IncrementalPaid)
                                                 MaxPaid = max(x$IncrementalPaid)
                                                 return (data.frame(CompanyName = CompanyName, MinPaid = MinPaid, AvgPaid = AvgPaid, MaxPaid = MaxPaid))})
  
  blandCompanies = dfPaidSummary[(dfPaidSummary$AvgPaid >= 100 & dfPaidSummary$AvgPaid<=100000),"CompanyName"]
  
  df = df[df$GroupName %in% blandCompanies,]

  df$PriorCumulative = with(df, CumulativePaid - IncrementalPaid)
  df[df$DevelopmentLag == 1, "PriorCumulative"] = NA
  
  return(df)
}