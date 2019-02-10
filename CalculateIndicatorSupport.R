#' LIBRARY OF ROUTINES FOR CALCULATING SWEDISH WFD INDICATORS
#'
#' Function DF_Ncalculation calculates number of spatial and temporal components in covariance structure 
#' @param df A dataframe with monitoring data from the Swedish Monitoring program. 
#'   The dataframe should contain the following variables:
#'   \describe{ 
#'   \item{station}{An identifier for the monitoring station.} 
#'   \item{obspoint}{An identifier for the monitoring point.} 
#'   \item{year}{Year of the sampling.} 
#'   \item{month}{Month of the sampling.} 
#'   \item{date}{Date of the observation.}   
#'   \item{institution}{Provider of the observation.} }
#'
#' @param var A dataframe with monitoring data from the Swedish Monitoring program. 
#' 
#' Function returns in a list the following elements
#'   \describe{ 
#'   \item{n_obs}{Number of observations.} 
#'   \item{n_station}{Number of unique stations.} 
#'   \item{n_obspoint}{Number of unique observation points.} 
#'   \item{n_year}{Number of unique years.} 
#'   \item{n_month}{Number of unique months.} 
#'   \item{n_yearmonth}{Number of unique combinatinos of year and months.} 
#'   \item{n_stationyear}{Number of unique combinations of year and stations.} 
#'   \item{n_stationmonth}{Number of unique combinations of year and months.} 
#'   \item{n_stationdate}{Number of unique combinations of observations points and dates.} }
#'   \item{n_institution}{Number of unique combinations of data providers.} 
#' 
#' 
DF_Ncalculation <-
  function(df) {
           n_obs <- nrow(df)
           n_station <- length(unique(df$station))
           n_obspoint <- nrow(unique(data.frame(df$station,df$obspoint)))
           n_year <- length(unique(df$year))
           n_month <- length(unique(df$month))
           n_yearmonth <- nrow(unique(data.frame(df$year,df$month)))
           n_stationyear <- nrow(unique(data.frame(df$station,df$year)))
           n_stationmonth <- nrow(unique(data.frame(df$station,df$month)))
           n_stationdate <- nrow(unique(data.frame(df$station,df$date)))
           n_institution <- length(unique(df$institution))
           res <- list(n_obs=n_obs,n_station=n_station,n_obspoint=n_obspoint,n_year=n_year,n_month=n_month,n_yearmonth=n_yearmonth,n_stationyear=n_stationyear,n_stationmonth=n_stationmonth,n_stationdate=n_stationdate,n_institution=n_institution)
           return(res)
  }

#' Function SimVector generates a vector of n random normal variates 
#' @param n Number of elements in vector
#' @param var Variance of the random normal variates
SimVector <-
  function(n,var) {
    e_vector <- mat.or.vec(n, 1)
    for (i in 1:n) {e_vector[i] <- rnorm(1) * sqrt(var)}
    return(e_vector)
  }

#' Function SetVector_IndicatorSim simulates the error structure from the data frame 
#' @param mean The mean of the distribution
#' @param n_list List of dimensions for simulating the error structure
#' @param var_list List of variance components
#' @param df Dataframe with the error structure to be simulated
#' @param seasons Number of months in indicator - used for variance adjustment for finite populations
SetVector_IndicatorSim <-
  function(mean,n_list,var_list,df,seasons) {
    Random_station <- SimVector(n_list$n_station,var_list$V_station)
    Random_obspoint <- SimVector(n_list$n_obspoint,var_list$V_obspoint)
    Random_year <- SimVector(n_list$n_year,var_list$V_year*(1-n_list$n_year/6)) # Correction for discrete distribution of year
    Random_yearmonth <- SimVector(n_list$n_yearmonth,var_list$V_yearmonth*(1-n_list$n_yearmonth/6/seasons)) # Correction for discrete distribution of yearxmonth
    Random_stationyear <- SimVector(n_list$n_stationyear,var_list$V_stationyear)
    Random_stationmonth <- SimVector(n_list$n_stationmonth,var_list$V_stationmonth)
    Random_stationdate <- SimVector(n_list$n_stationdate,var_list$V_stationdate)
    Random_institution <- SimVector(n_list$n_institution,var_list$V_institution)
    Random_replication <- SimVector(n_list$n_obs,var_list$V_replication)
    
    simulvector_station <- Random_station[match(df$station,unique(df$station))]
    simulvector_obspoint <- Random_obspoint[row.match(data.frame(df$station,df$obspoint),unique(data.frame(df$station,df$obspoint)))]
    simulvector_year <- Random_year[match(df$year,unique(df$year))]
    simulvector_yearmonth <- Random_yearmonth[row.match(data.frame(df$year,df$month),unique(data.frame(df$year,df$month)))]
    simulvector_stationyear <- Random_stationyear[row.match(data.frame(df$station,df$year),unique(data.frame(df$station,df$year)))]
    simulvector_stationmonth <- Random_stationmonth[row.match(data.frame(df$station,df$month),unique(data.frame(df$station,df$month)))]
    simulvector_stationdate <- Random_stationdate[row.match(data.frame(df$station,df$date),unique(data.frame(df$station,df$date)))]
    simulvector_institution <- Random_institution[match(df$institution,unique(df$institution))]
    simulvector_obs <- mean[match(df$year,unique(df$year))]+simulvector_station+simulvector_obspoint+simulvector_year+simulvector_yearmonth+simulvector_stationyear+simulvector_stationmonth+simulvector_stationdate+simulvector_institution+Random_replication
    return(simulvector_obs)
  }


#' Function SetVector_IndicatorSimO2 simulates the error structure from the data frame for the oxygen indicator 
#' @param mean The mean of the distribution
#' @param var_list List of variance components
#' @param beta Variance from the lognormal distributions
#' @param df Dataframe with the error structure to be simulated
SetVector_IndicatorSimO2 <-
  function(mean,n_list,var_list,df,seasons) {
    Random_station <- SimVector(n_list$n_station,var_list$V_station)
    Random_obspoint <- SimVector(n_list$n_obspoint,var_list$V_obspoint)
    Random_year <- SimVector(n_list$n_year,var_list$V_year*(1-n_list$n_year/6)) # Correction for discrete distribution of year
    Random_yearmonth <- SimVector(n_list$n_yearmonth,var_list$V_yearmonth*(1-n_list$n_yearmonth/6/seasons)) # Correction for discrete distribution of yearxmonth
    Random_stationyear <- SimVector(n_list$n_stationyear,var_list$V_stationyear)
    Random_stationmonth <- SimVector(n_list$n_stationmonth,var_list$V_stationmonth)
    Random_stationdate <- SimVector(n_list$n_stationdate,var_list$V_stationdate)
    Random_institution <- SimVector(n_list$n_institution,var_list$V_institution)
    Random_replication <- SimVector(n_list$n_obs,var_list$V_replication)
    
    simulvector_station <- Random_station[match(df$station,unique(df$station))]
    simulvector_obspoint <- Random_obspoint[row.match(data.frame(df$station,df$obspoint),unique(data.frame(df$station,df$obspoint)))]
    simulvector_year <- Random_year[match(df$year,unique(df$year))]
    simulvector_yearmonth <- Random_yearmonth[row.match(data.frame(df$year,df$month),unique(data.frame(df$year,df$month)))]
    simulvector_stationyear <- Random_stationyear[row.match(data.frame(df$station,df$year),unique(data.frame(df$station,df$year)))]
    simulvector_stationmonth <- Random_stationmonth[row.match(data.frame(df$station,df$month),unique(data.frame(df$station,df$month)))]
    simulvector_stationdate <- Random_stationdate[row.match(data.frame(df$station,df$date),unique(data.frame(df$station,df$date)))]
    simulvector_institution <- Random_institution[match(df$institution,unique(df$institution))]
    simulvector_obs <- df$xvar+simulvector_station+simulvector_obspoint+simulvector_year+simulvector_yearmonth+simulvector_stationyear+simulvector_stationmonth+simulvector_stationdate+simulvector_institution+Random_replication
    return(simulvector_obs)
  }



#' Function Filter_df add year and month variables and removes missing observations 
#'
#' @param df Dataframe with the data to be analysed
#' @param MonthIncluce A list of numbered months to include in calculations
#' @var Name of the variable to be analysed
#'
Filter_df <-
  function(df,MonthInclude,startyear,endyear) {
    # Add month and year to df
    df <- mutate(df,month=month(date))
    df <- mutate(df,year=year(date))
    # Use only data within assessment period
    df <- filter(df,year>=startyear)
    df <- filter(df,year<=endyear)
    # Use only months specified in MonthInclude    
    df <- filter(df,month %in% MonthInclude)
    # Remove observations with missing values    
    df <- filter(df,!is.na(xvar))
    # Use the generic name yvar for the variable
#    df$yvar<-df[,var]
    return(df)
  }

#' Function Indicator_statistics calculates a list with statistics from the original data and the simulations 
#'
#' @param df Dataframe with the error structure to be simulated
#' @param dfsim Vector with the simulated indicator values for the assessment period
#' @param dfsimyear Matrix with simulated yearly indicator values
#' @param yearlist A list of the years with data, corresponding to those in dfsimyear
#' @param n_list A list with numbers of levels for different factors in error structure
#'
Indicator_statistics <-
  function(df,dfsim,dfsimyear,yearlist,n_list,flag) {
    # Estimate indicator median from original data
    median <- as.numeric(df %>%
                           group_by(year,station) %>%
                           summarise(yvar = mean(yvar)) %>%
                           group_by(year) %>%
                           summarise(yvar = mean(yvar)) %>%
                           summarise(yvar = mean(yvar)))
    # Estimate other indicator statistics from MC simulations
    mean <- mean(as.numeric(dfsim))
    stderr <- sd(as.numeric(dfsim))
    yearmean <- data.frame(year = yearlist,mean = apply(dfsimyear,2,mean),stderr = apply(dfsimyear,2,sd))
    flag <- ifelse(length(yearlist)<3,-1,flag)
    obs_sim <- as.numeric(dfsim)
    
    res <- list(median=median,mean=mean,stderr=stderr,yearmean=yearmean,indicator_sim=obs_sim,n_list=n_list,result_code=flag)
    return(res)
  }

#' Transformations modified from standard functios
#'
#' Function logit_w_replace is a logit transformation where 0 and 1 are replaced by a small value epsilon
#'
logit_w_replace <-
  function(x,epsilon=0.005) {
    x <- ifelse(x == 0,epsilon,x)
    x <- ifelse(x == 1,1-epsilon,x)
    res <- log(x/(1-x))
    return(res)
  }


#' Function EQR_trans translates into EQR scale using a RefCond 
#'
#' @param x Dataframe with the error structure to be simulated
#'
EQR_trans <-
  function(x,RefCond) {
    res <- ifelse(x<RefCond,1,RefCond/x)
    return()
  }

#' Function Varlist_lognormal backtransforms a list of variance components
#' @param mu Mean from the raw distribution
#' @param beta Variance from the lognormal distribution
Vartrans_lognormal <-
  function(mu,beta) {
    res <- mu^2*(exp(beta)-1)
    return(res)
  }

#' Function Varlist_lognormal backtransforms the list of variance components 
#' @param var_list List of variance components
#' @param beta Variance from the lognormal distributions
Varlist_lognormal <-
  function(var_list,mean) {
    var_list$V_station <- Vartrans_lognormal(mean,var_list$V_station)
    var_list$V_obspoint <- Vartrans_lognormal(mean,var_list$V_obspoint)
    var_list$V_year <- Vartrans_lognormal(mean,var_list$V_year)
    var_list$V_yearmonth <- Vartrans_lognormal(mean,var_list$V_yearmonth)
    var_list$V_tempres <- Vartrans_lognormal(mean,var_list$V_tempres)
    var_list$V_stationyear <- Vartrans_lognormal(mean,var_list$V_stationyear)
    var_list$V_stationmonth <- Vartrans_lognormal(mean,var_list$V_stationmonth)
    var_list$V_institution <- Vartrans_lognormal(mean,var_list$V_institution)
    return(var_list)
  }

#' Function stderr_aggr calculated the standard error the sum of a vector with standard errors  
#' @param x vector of stderr
stderr_aggr <-
  function(x) {
    sqrt(sum(x^2))/length(x)
  }

#' Function RefCond_LakeTPsummer calculates the reference condition for TP in µg/l as function of absorbance, altitude and lake depth
RefCond_LakeTPsummer <-
  function(AbsF,Altitude,LakeDepth=0) {
    if (LakeDepth == 0) logP=1.561+0.295*log10(AbsF)-0.146*log10(Altitude)
    else logP=1.627+0.246*log10(AbsF)-0.139*log10(Altitude)-0.197*log10(LakeDepth)
    return(10**logP)
  }

#' Function RefCond_RiverTP calculates the reference condition for TP in µg/l as function of absorbance, altitude and lake depth
RefCond_RiverTP <-
  function(AbsF,Altitude,CationConc=0) {
    if (CationConc == 0) logP=1.380+0.240*log10(AbsF)-0.0143*sqrt(Altitude)
    else logP=1.533+0.240*log10(CationConc)+0.301*log10(AbsF)-0.012*sqrt(Altitude)
    return(10**logP)
  }

#' Function RefCond_LakeSecchiDepth calculates the reference condition for Secchi depth in m as function of absorbance, altitude and lake depth
RefCond_LakeSecchiDepth <-
  function(AbsF,RefCondChla) {
    logSD=0.678-0.116*log10(AbsF)-0.471*log10(RefCondChla)
    return(10**logSD)
  }
