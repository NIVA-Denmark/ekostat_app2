# Include supporting routines for indicator calculations
source("CalculateIndicatorSupport.R")

# AGGREGATION ROUTINES BASED ON MEASUREMENTS
# Aggregation principle used for e.g. coastal chlorophyll
# Aggregate over stations to yearly means then over years
Aggregate_year_station <- function(df) {
  yearmeans <- df %>%    group_by(year,station) %>%
                         summarise(xvar = mean(xvar,na.rm = TRUE)) %>%
                         group_by(year) %>%
                         summarise(xvar = mean(xvar,na.rm = TRUE))
                         
  periodmean <- mean(yearmeans$xvar)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)
}

# Aggregation principle used for e.g. nutrients
# Aggregate over years and then period
Aggregate_year <- function(df) {
  yearmeans <- df %>% group_by(year) %>%
    summarise(xvar = mean(xvar,na.rm = TRUE))
  
  periodmean <- mean(yearmeans$xvar)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)
}

# Aggregation principle used for e.g. Secchi depth
# Aggregate over entire period
Aggregate_period <- function(df) {
  yearmeans <- df %>% group_by(year) %>%
    summarise(xvar = mean(xvar,na.rm = TRUE))
  
  periodmean <- mean(df$xvar)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)
}

# AGGREGATION PRINCIPLES BASED ON EQR VALUES
# Aggregation principle used for e.g. biovolume in lakes
# Aggregate over entire period and then calculate EQR value
Aggregate_period_P_EQR <- function(df) {
  yearmeans <- df %>% group_by(year) %>%
    summarise(xvar = mean(RefCond,na.rm = TRUE)/mean(xvar,na.rm = TRUE))
  
  periodmean <- mean(df$RefCond)/mean(df$xvar)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)
}

# Aggregation principle used for e.g. biovolume in lakes
# Aggregate over entire period and then calculate EQR value
Aggregate_period_N_EQR <- function(df) {
  yearmeans <- df %>% group_by(year) %>%
    summarise(xvar = mean(xvar,na.rm = TRUE)/mean(RefCond,na.rm = TRUE))
  
  periodmean <- mean(df$xvar)/mean(df$RefCond)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)
}

# Aggregation principle used for DJ-index in rivers
# Aggregate over entire period and then calculate EQR value after subtracting the value 5
Aggregate_period_N_EQR5 <- function(df) {
  yearmeans <- df %>% group_by(year) %>%
    summarise(xvar = (mean(xvar,na.rm = TRUE)-5)/(mean(RefCond,na.rm = TRUE)-5))
  
  periodmean <- mean(df$xvar)/mean(df$RefCond)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)
}

# Aggregation principle used for proportions with EQR calculation, e.g. cyanobacteria in lakes
# Aggregate over entire period and then calculate EQR value
Aggregate_period_Prop_EQR <- function(df) {
  yearmeans <- df %>% group_by(year) %>%
    summarise(xvar = (1-mean(xvar,na.rm = TRUE))/(1-mean(RefCond,na.rm = TRUE)))

  periodmean <- (1-mean(df$xvar))/(1-mean(df$RefCond))
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)
}

# Aggregation principle used for proportions with EQR calculation, e.g. cyanobacteria in lakes
# Aggregate over entire period and then calculate EQR value
Aggregate_period_TPI_EQR <- function(df) {
  yearmeans <- df %>% group_by(year) %>%
    summarise(x = mean(xvar,na.rm = TRUE),r50 = mean(RefCond),r75 = mean(HG_boundary),xvar = (r75-r50)/(x+r75-2*r50))
  yearmeans <- yearmeans %>% mutate(x = NULL,r50 = NULL, r75 = NULL)
  
  r50 = mean(df$RefCond)
  r75 = mean(df$HG_boundary)
  periodmean <- (r75-r50)/(mean(df$xvar)+r75-2*r50)
  
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)
}


# AGGREGATION PRINCIPLES BASED ON EQR OBSERVATIONS
# Aggregation principle used for e.g. coastal chlorophyll as EQR
# Compute EQR values and then aggregate over stations, years and period
AggregateEQR_year_station <- function(df) {
  
  df <- mutate(df,xvarEQR = ifelse(xvar<RefCond,1,RefCond/xvar))

  yearmeans <- df %>%    group_by(year,station) %>%
    summarise(xvarEQR = mean(xvarEQR,na.rm = TRUE)) %>%
    group_by(year) %>%
    summarise(xvar = mean(xvarEQR,na.rm = TRUE))   # should be returned in xvar
  
  periodmean <- mean(yearmeans$xvar)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)  
}

# Aggregation principle used for e.g. nutrients as EQR
# Aggregate over years and then period
AggregateEQR_year <- function(df) {
  
  df <- mutate(df,xvarEQR = RefCond/xvar)
  
  yearmeans <- df %>% group_by(year) %>%
    summarise(xvar = mean(xvarEQR,na.rm = TRUE))   # should be returned in xvar
  
  periodmean <- mean(yearmeans$xvar)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)  
}


# Aggregate over entire period
# Indicator response positive to degradation, i.e. chlorophyll
AggregateEQR_P_period <- function(df) {
  
  df <- mutate(df,xvarEQR = RefCond/xvar)
  
  yearmeans <- df %>% group_by(year) %>%
    summarise(xvar = mean(xvarEQR,na.rm = TRUE))   # should be returned in xvar
  
  periodmean <- mean(df$xvarEQR)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)  
}

# Aggregate over entire period
# Indicator response negative to degradation, i.e. Secchi depth
AggregateEQR_N_period <- function(df) {
  
  df <- mutate(df,xvarEQR = xvar/RefCond)
  
  yearmeans <- df %>% group_by(year) %>%
    summarise(xvar = mean(xvarEQR,na.rm = TRUE))   # should be returned in xvar
  
  periodmean <- mean(df$xvarEQR)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)  
}

# SPECIAL CASES FOR COASTAL INDICATORS
# Calculation of BQI indicator according to Handbook
BQIbootstrap <- function(df) {
  yearstatmeans <- df %>%    group_by(year,station) %>%
    summarise(xvar = mean(xvar,na.rm = TRUE))
  Nyearstat <- yearstatmeans %>% group_by(year) %>% summarise(n_station = length(xvar))
  BQIsimyear <- mat.or.vec(length(Nyearstat$n_station), 1)
  for(i in 1:length(Nyearstat$n_station)) {
     BQIsim <- trunc(runif(9999,1,Nyearstat$n_station[i]+1))
     BQIsim <- yearstatmeans$xvar[yearstatmeans$year == Nyearstat$year[i]][BQIsim]
     BQIsimyear[i] <- quantile(BQIsim,probs=0.2)
  }

  periodmean <- mean(BQIsimyear)
  yearmeans <- data.frame(year=Nyearstat$year,xvar = BQIsimyear)
  res <- list(periodmean=periodmean,yearmeans=yearmeans,error_code=0)
  return(res)  
}

# Calculation of Oxygen indicator according to Handbook
# First test calculates the average of O2 observations below the 25%-percentile - threshold is 3.5 ml/l
# Second test calculates the average of O2 observations (Jan-May) below the 25%-percentile 
OxygenTest1 <- function(df) {
  years <- df %>% group_by(year) %>% summarise()
  df1 <- filter(df,xvar<=quantile(xvar,na.rm=TRUE)[2])
  O2_test1 <- mean(df1$xvar)
  O2_test1_yearmeans <- df1 %>% group_by(year) %>% summarise(xvar = mean(xvar))
  O2_test1_yearmeans <- left_join(years,O2_test1_yearmeans,c("year"))
  df2 <- df1 %>% filter(month %in% c(1,2,3,4,5)) 
  O2_test2 <- mean(df2$xvar)
  O2_test2_yearmeans <- df2 %>% group_by(year) %>% summarise(xvar = mean(xvar))
  O2_test2_yearmeans <- left_join(years,O2_test2_yearmeans,c("year"))
  yearmeans <- data.frame(year=O2_test1_yearmeans$year,xvar = O2_test1_yearmeans$xvar)
  res <- list(periodmean=O2_test1,yearmeans=yearmeans,error_code=0)
  return(res)
}

# Calculation of Oxygen indicator according to Handbook
# First test calculates the average of O2 observations below the 25%-percentile - threshold is 3.5 ml/l
# Second test calculates the average of O2 observations (Jan-May) below the 25%-percentile 
# df contains oxygen profiles 
OxygenTest2 <- function(df) {
  df <- filter(df,!is.na(xvar))
  # create list of years for producing vectors of similar length in the returned list
  years <- df %>% group_by(year) %>% summarise()
  # Ensure that the profiles are sorted by depth and contain oxygen concentrations
  df <- df %>% group_by(station,date,time,station_depth,depth) %>% filter(is.na(xvar) == FALSE)
  # Use measured profile from surface until O2 concentrations has decreased >1 ml/l
  O2surface <- df %>% group_by(station,date,time,station_depth) %>% summarise(O2surface = O2[which.min(depth)])
  df <- full_join(df,O2surface,c("station","date","time","station_depth"))
  df$xvar <- ifelse(df$O2<df$O2surface-1,df$xvar,df$O2)
  # find depth for 3.5 ml/l by extrapolation, when this threshold is not in the profile
  O2bottom_ext <- df %>% group_by(station,date,time,station_depth) %>% summarise(n_obs =n(),
                                                                                 O2bottom1 = xvar[which.max(depth)],O2bottom2 = ifelse(n_obs>1,xvar[which.max(depth)-1],NA),
                                                                                 depth1 = depth[which.max(depth)],depth2 =ifelse(n_obs>1,depth[which.max(depth)-1],NA),
                                                                                 O2clinedepth_max=ifelse(n_obs>1,ifelse(O2bottom1>3.5 && O2bottom2-O2bottom1>0,depth1+(3.5-O2bottom1)/(O2bottom2-O2bottom1)*(depth2-depth1),1000),NA))  # find profile statistics for tests and indicator calculation
  O2bottom <- df %>% group_by(station,date,time,station_depth) %>% summarise(n_obs =n(), O2range = range(xvar)[2]-range(xvar)[1],
                                                                             max_depth=max(depth),O2bottom = xvar[which.max(depth)],O2clinedepth = ifelse(n_obs>1 && O2range>0,approx(xvar,depth,c(3.5))$y,NA))
  # If O2clinedepth is not in the profile then use the extrapolated value
  O2bottom <- full_join(O2bottom,O2bottom_ext,c("station","date","time","station_depth"))
  O2bottom <- O2bottom %>% mutate(O2clinedepth = ifelse(is.na(O2clinedepth),O2clinedepth_max,O2clinedepth),depth1 = NULL, depth2 = NULL, O2bottom1 = NULL, O2bottom2 = NULL, O2clinedepth_max = NULL)
  # Find the percent area affected by O2 concentrations <3.5 ml/l
  O2bottom <- O2bottom %>% mutate(area_hyp = 100-approx(WB_bathymetry$depth,WB_bathymetry$area_pct,O2clinedepth,yleft=0,yright=100)$y)
  # Calculate test1 as average of O2 observations at bottom (<1.5 m from bottom depth) below the 25-percentile for Jan-Dec
  lower_quantile <- quantile(O2bottom$O2bottom,na.rm=TRUE)[2]
  df1 <- O2bottom %>% filter(station_depth-max_depth<1.5) %>% filter(O2bottom<=lower_quantile) %>% mutate(year=lubridate::year(date))
  #  df1 <- df %>% filter(station_depth-depth<1.5) %>% filter(xvar<=quantile(xvar,na.rm=TRUE)[2])
  O2_test1 <- mean(df1$O2bottom)
  O2_test1_yearmeans <- df1 %>% group_by(year) %>% summarise(O2bottom = mean(O2bottom))
  # Complete with all years having O2 data
  O2_test1_yearmeans <- left_join(years,O2_test1_yearmeans,c("year"))
  # Calculate EQR from Table 7.1 in Handbook
  EQR_test1 <- approx(c(-5.0,0.0,1.0,2.1,3.5,7.0),c(0,0.2,0.4,0.6,0.8,1),O2_test1,yleft=0,yright=1)$y
  EQR_test1_yearmeans <- approx(c(-10.0,0.0,1.0,2.1,3.5,7.0),c(0,0.2,0.4,0.6,0.8,1),O2_test1_yearmeans$O2bottom,yleft=0,yright=1)$y
  # Calculate test2 as average of O2 concentrations below the 25-percentile for Jan-May
  df2 <- O2bottom %>% mutate(month = lubridate::month(date),year = lubridate::year(date)) %>% filter(month %in% c(1,2,3,4,5))
  # Return from function if no observations available (Jan-May) for calculation of O2_test2
  if (nrow(df2) == 0) {
    yearmeans <- df %>% group_by(year) %>% summarise(xvar = NA)  # Return NA values in res
    res <- list(periodmean=NA,yearmeans=yearmeans,error_code=-91)
    return(list(error_code=-91))
  }
  O2_test2 <- mean(df2$O2bottom)
  O2_test2_yearmeans <- df2 %>% group_by(year) %>% summarise(O2bottom = mean(O2bottom))
  # Complete with all years having O2 data
  O2_test2_yearmeans <- left_join(years,O2_test2_yearmeans,c("year"))
  # Calculate indicator for percent area affected by <3.5 ml/l
  df2 <- O2bottom %>% mutate(month = lubridate::month(date),year = lubridate::year(date)) %>% filter(month %in% c(6,7,8,9,10,11,12))
  # Return from function if no observations available (Jun-Dec) for calculation of hypoxic area
  if (nrow(df2) == 0) {
    yearmeans <- df %>% group_by(year) %>% summarise(xvar = NA)  # Return NA values in res
    res <- list(periodmean=NA,yearmeans=yearmeans,error_code=-92)
    return(list(error_code=-92))
  }
  hyparea <- mean(df2$area_hyp)
  hyparea_yearmeans <- df2 %>% group_by(year) %>% summarise(area_hyp = mean(area_hyp))
  # Complete with all years having O2 data
  hyparea_yearmeans <- left_join(years,hyparea_yearmeans,c("year"))
  # Calculate EQR from Table 7.1 in Handbook
  EQR_test2 <- approx(BoundariesHypoxicArea,c(0,0.2,0.4,0.6,0.8,1),hyparea,yleft=0,yright=1)$y
  EQR_test2_yearmeans <- approx(BoundariesHypoxicArea,c(0,0.2,0.4,0.6,0.8,1),hyparea_yearmeans$area_hyp,yleft=0,yright=1)$y
  O2_test1<-ifelse(is.nan(O2_test1),0,O2_test1) # Set O2_test1 to zero if no data to complete the if-clause below
  if (O2_test1>3.5 || O2_test2>3.5) {
    res <- list(periodmean=EQR_test1,yearmeans=data.frame(year=O2_test1_yearmeans$year,xvar = EQR_test1_yearmeans),error_code=0)
  } else {
    res <- list(periodmean=EQR_test2,yearmeans=data.frame(year=O2_test1_yearmeans$year,xvar = EQR_test2_yearmeans),error_code=0)
  }
  return(res)
}


#' Generic routine for calculating indicator statistics
#' 
#' @param Indicator The name identifier for the indicator to be calculated. 
#' @param df A dataframe with monitoring data from the Swedish Monitoring program. 
#'   The dataframe should contain the
#'   following variables:
#'   
#'   \describe{ 
#'   \item{station}{An identifier for the monitoring station.} 
#'   \item{date}{Date of the observation.} 
#'   \item{institution}{Provider of the observation.} 
#'   \item{chla}{Chlorophyll a concentration in sample.} 
#'   
#' @param MonthInclude A list of months to be included in the indicator
#' @param var_list List of variance components
#' @param MonthInclude A list of month numbers for filtering data
#' @param startyear The first year in the indicator calculation
#' @param endyear The last year in the indicator calculation
#' @param n_iter Number of iterations for Monte Carlo simulation
#'   
#' @return
#' @export
#' 
#' @examples 

CalculateIndicator <-
  function(Indicator,df,RefCond_sali,var_list,MonthInclude,startyear,endyear,n_iter=1000) {
# Set flag to zero and change it for error handling below
    flag <- 0
# Select the observation variable for the indicator
    xvar <- switch(Indicator,
                   CoastChla         = df$chla,
                   CoastChlaEQR      = df$chla,
                   CoastBiovol       = df$biovol,
                   CoastBiovolEQR    = df$biovol,
                   CoastSecchi       = df$secchi,
                   CoastSecchiEQR    = df$secchi,
                   CoastDINwinter    = df$DIN,
                   CoastDINwinterEQR = df$DIN,
                   CoastDIPwinter    = df$DIP,
                   CoastDIPwinterEQR = df$DIP,
                   CoastTNsummer     = df$TN,
                   CoastTNsummerEQR  = df$TN,
                   CoastTNwinter     = df$TN,
                   CoastTNwinterEQR  = df$TN,
                   CoastTPsummer     = df$TP,
                   CoastTPsummerEQR  = df$TP,
                   CoastTPwinter     = df$TP,
                   CoastTPwinterEQR  = df$TP,
                   CoastOxygen       = df$O2,
                   CoastBQI          = df$BQI,
                   CoastMSMDI        = df$MSMDI,
                   LakeBiovol        = df$biovol,
                   LakeBiovolEQR     = df$biovol,
                   LakePropCyano     = df$Proportion_cyanobacteria,
                   LakePropCyanoEQR  = df$Proportion_cyanobacteria,
                   LakeTPI           = df$TrophicPlanktonIndex,
                   LakeTPIEQR        = df$TrophicPlanktonIndex,
                   LakeNphytspec     = df$Nspecies_phytoplankton,
                   LakeNphytspecEQR  = df$Nspecies_phytoplankton,
                   LakeChla          = df$chla,
                   LakeChlaEQR       = df$chla,
                   LakeTMIEQR        = df$TrophicMacrophyteIndex,
                   LakeASPTEQR       = df$BenthicInvertebratesASPT,
                   LakeBQIEQR        = df$BenthicInvertebratesBQI,
                   LakeMILAEQR       = df$BenthicInvertebratesMILA,
                   LakeEQR8          = df$EQR8,
                   LakeAindexW5      = df$AindexW5,
                   LakeEindexW3      = df$EindexW3,
                   LakeTPsummerEQR   = df$TP,
                   LakeSecchiDepthEQR= df$SecchiDepth,
                   RiverIPS          = df$BenthicDiatomsIPS,
                   RiverIPSEQR       = df$BenthicDiatomsIPS,
                   RiverPctPT        = df$BenthicDiatomsPctPT,
                   RiverTDI          = df$BenthicDiatomsTDI,
                   RiverACID         = df$BenthicDiatomsACID,
                   RiverASPTEQR      = df$BenthicInvertebratesASPT,
                   RiverDJEQR        = df$BenthicInvertebratesDJ,
                   RiverMISAEQR      = df$BenthicInvertebratesMISA,
                   RiverVIX          = df$VIX,
                   RiverVIXh         = df$VIXh,
                   RiverVIXsm        = df$VIXsm,
                   RiverTPEQR        = df$TP
    )
    df <- mutate(df,xvar=xvar)
# Associating indicators with transformation from observations
    f_fun <- switch(Indicator,
                    CoastChla         = Aggregate_year_station,
                    CoastChlaEQR      = AggregateEQR_year_station,
                    CoastBiovol       = Aggregate_year_station,
                    CoastBiovolEQR    = AggregateEQR_year_station,
                    CoastSecchi       = Aggregate_period,
                    CoastSecchiEQR    = AggregateEQR_N_period,
                    CoastDINwinter    = Aggregate_year,
                    CoastDINwinterEQR = AggregateEQR_year,
                    CoastDIPwinter    = Aggregate_year,
                    CoastDIPwinterEQR = AggregateEQR_year,
                    CoastTNsummer     = Aggregate_year,
                    CoastTNsummerEQR  = AggregateEQR_year,
                    CoastTNwinter     = Aggregate_year,
                    CoastTNwinterEQR  = AggregateEQR_year,
                    CoastTPsummer     = Aggregate_year,
                    CoastTPsummerEQR  = AggregateEQR_year,
                    CoastTPwinter     = Aggregate_year,
                    CoastTPwinterEQR  = AggregateEQR_year,
                    CoastOxygen       = OxygenTest2,
                    CoastBQI          = BQIbootstrap,
                    CoastMSMDI        = Aggregate_period,
                    LakeBiovol        = Aggregate_period,
                    LakeBiovolEQR     = Aggregate_period_P_EQR,
                    LakePropCyano     = Aggregate_period,
                    LakePropCyanoEQR  = Aggregate_period_Prop_EQR,
                    LakeTPI           = Aggregate_period,
                    LakeTPIEQR        = Aggregate_period_TPI_EQR,
                    LakeNphytspec     = Aggregate_period,
                    LakeNphytspecEQR  = Aggregate_period_N_EQR,
                    LakeChla          = Aggregate_period,
                    LakeChlaEQR       = Aggregate_period_P_EQR,
                    LakeTMIEQR        = Aggregate_period_N_EQR,
                    LakeASPTEQR       = Aggregate_period_N_EQR,
                    LakeBQIEQR        = Aggregate_period_N_EQR,
                    LakeMILAEQR       = Aggregate_period_N_EQR,
                    LakeEQR8          = Aggregate_period,
                    LakeAindexW5      = Aggregate_period,
                    LakeEindexW3      = Aggregate_period,
                    LakeTPsummerEQR   = Aggregate_period_P_EQR,
                    LakeSecchiDepthEQR= Aggregate_period_N_EQR,
                    RiverIPS          = Aggregate_period,
                    RiverIPSEQR       = Aggregate_period_N_EQR,
                    RiverPctPT        = Aggregate_period,
                    RiverTDI          = Aggregate_period,
                    RiverACID         = Aggregate_period,
                    RiverASPTEQR      = Aggregate_period_N_EQR,
                    RiverDJEQR        = Aggregate_period_N_EQR5,
                    RiverMISAEQR      = Aggregate_period_N_EQR,
                    RiverVIX          = Aggregate_period,
                    RiverVIXh         = Aggregate_period,
                    RiverVIXsm        = Aggregate_period,
                    RiverTPEQR        = Aggregate_period_P_EQR
                    )
# Assigning transformations for measurements to obtain normal distributed variates
    g_fun <- switch(Indicator,
                    CoastChla         = log,
                    CoastChlaEQR      = log,
                    CoastBiovol       = log,
                    CoastBiovolEQR    = log,
                    CoastSecchi       = log,
                    CoastSecchiEQR    = log,
                    CoastDINwinter    = log,
                    CoastDINwinterEQR = log,
                    CoastDIPwinter    = log,
                    CoastDIPwinterEQR = log,
                    CoastTNsummer     = log,
                    CoastTNsummerEQR  = log,
                    CoastTNwinter     = log,
                    CoastTNwinterEQR  = log,
                    CoastTPsummer     = log,
                    CoastTPsummerEQR  = log,
                    CoastTPwinter     = log,
                    CoastTPwinterEQR  = log,
                    CoastOxygen       = identity,
                    CoastBQI          = identity,
                    CoastMSMDI        = logit_w_replace,
                    LakeBiovol        = log,
                    LakeBiovolEQR     = log,
                    LakePropCyano     = logit_w_replace,
                    LakePropCyanoEQR  = logit_w_replace,
                    LakeTPI           = identity,
                    LakeTPIEQR        = identity,
                    LakeNphytspec     = log,
                    LakeNphytspecEQR  = log,
                    LakeChla          = log,
                    LakeChlaEQR       = log,
                    LakeTMIEQR        = identity,
                    LakeASPTEQR       = identity,
                    LakeBQIEQR        = identity,
                    LakeMILAEQR       = identity,
                    LakeEQR8          = logit_w_replace,
                    LakeAindexW5      = logit_w_replace,
                    LakeEindexW3      = logit_w_replace,
                    LakeTPsummerEQR   = log,
                    LakeSecchiDepthEQR= identity,
                    RiverIPS          = identity,
                    RiverIPSEQR       = identity,
                    RiverPctPT        = logit_w_replace,
                    RiverTDI          = identity,
                    RiverACID         = identity,
                    RiverASPTEQR      = identity,
                    RiverDJEQR        = identity,
                    RiverMISAEQR      = identity,
                    RiverVIX          = logit_w_replace,
                    RiverVIXh         = logit_w_replace,
                    RiverVIXsm        = logit_w_replace,
                    RiverTPEQR        = log
    )    
# Assigning inverse transformations of g_fun
    g_fun_inv <- switch(Indicator,
                    CoastChla         = exp,
                    CoastChlaEQR      = exp,
                    CoastBiovol       = exp,
                    CoastBiovolEQR    = exp,
                    CoastSecchi       = exp,
                    CoastSecchiEQR    = exp,
                    CoastDINwinter    = exp,
                    CoastDINwinterEQR = exp,
                    CoastDIPwinter    = exp,
                    CoastDIPwinterEQR = exp,
                    CoastTNsummer     = exp,
                    CoastTNsummerEQR  = exp,
                    CoastTNwinter     = exp,
                    CoastTNwinterEQR  = exp,
                    CoastTPsummer     = exp,
                    CoastTPsummerEQR  = exp,
                    CoastTPwinter     = exp,
                    CoastTPwinterEQR  = exp,
                    CoastOxygen       = identity,
                    CoastBQI          = identity,
                    CoastMSMDI        = plogis,
                    LakeBiovol        = exp,
                    LakeBiovolEQR     = exp,
                    LakePropCyano     = plogis,
                    LakePropCyanoEQR  = plogis,
                    LakeTPI           = identity,
                    LakeTPIEQR        = identity,
                    LakeNphytspec     = exp,
                    LakeNphytspecEQR  = exp,
                    LakeChla          = exp,
                    LakeChlaEQR       = exp,
                    LakeTMIEQR        = identity,
                    LakeASPTEQR       = identity,
                    LakeBQIEQR        = identity,
                    LakeMILAEQR       = identity,
                    LakeEQR8          = plogis,
                    LakeAindexW5      = plogis,
                    LakeEindexW3      = plogis,
                    LakeTPsummerEQR   = exp,
                    LakeSecchiDepthEQR= identity,
                    RiverIPS          = identity,
                    RiverIPSEQR       = identity,
                    RiverPctPT        = plogis,
                    RiverTDI          = identity,
                    RiverACID         = identity,
                    RiverASPTEQR      = identity,
                    RiverDJEQR        = identity,
                    RiverMISAEQR      = identity,
                    RiverVIX          = plogis,
                    RiverVIXh         = plogis,
                    RiverVIXsm        = plogis,
                    RiverTPEQR        = exp
                    ) 
# Switch year for winter months (Nov+Dec) to include together with (Jan+Feb)
    if (Indicator %in% c("CoastDINwinterEQR","CoastDIPwinterEQR","CoastTNwinter","CoastTNwinterEQR","CoastTPwinter","CoastTPwinterEQR")) {
      df <- mutate(df,year=ifelse(month %in% c(11,12),year+1,year))
    }
# Filter dataframe to include observations used in indicator only
    df <- Filter_df(df,MonthInclude,startyear,endyear)    
# setting RefCond depending on salinity for indicators with salinity correction
    RefCond <- mat.or.vec(nrow(df), 1)
    if (Indicator %in% c("CoastChlaEQR","CoastBiovolEQR","CoastSecchiEQR","CoastDINwinterEQR","CoastDIPwinterEQR","CoastTNsummerEQR","CoastTPsummerEQR","CoastTNwinterEQR","CoastTPwinterEQR",
                         "LakeBiovolEQR","LakePropCyanoEQR","LakeTPIEQR","LakeNphytspecEQR","LakeChlaEQR","LakeTMIEQR","LakeASPTEQR","LakeBQIEQR","LakeMILAEQR","LakeTPsummerEQR","LakeSecchiDepthEQR",
                         "RiverIPSEQR","RiverASPTEQR","RiverDJEQR","RiverMISAEQR","RiverTPEQR")) {
       df <- filter(df,!is.na(sali))
       RefCond <- mat.or.vec(nrow(df), 1)
       sali_class <- findInterval(df$sali, c(seq(0, 35)))
       for (i in 1:nrow(df)) {RefCond[i] <- RefCond_sali[sali_class[i]]}
       }
    df <- mutate(df,RefCond = RefCond) 
# Adding H-G boundary to data for TPI calculations of EQR
    if (Indicator %in% c("LakeTPIEQR")) {
      HG_boundary <- mat.or.vec(nrow(df), 1)
      for (i in 1:nrow(df)) {HG_boundary[i] <- RefCond_sali[2]}
      df <- mutate(df,HG_boundary = HG_boundary) 
    }
# Calculate number of years, stations, months, institutions and combinations thereof in df 
    ndf <- DF_Ncalculation(df)
# Return from function if no observations for calculation
    if (ndf$n_obs == 0) return(list(result_code=-90))
# Estimate mean of the transformed observation for simulation
    alpha <- df %>% group_by(year) %>% summarise(mean = mean(g_fun(xvar),na.rm=TRUE))
# Calculate indicator
    mu_indicator <- f_fun(df)
    # Return from function if no observations in Jan-May (result_code=-91) or Jun-Dec (result_code=-92)
    if (mu_indicator$error_code != 0) return(list(result_code=mu_indicator$error_code))
# Simulate system with random variables for estimating the variance of the indicator
    simres <- vector("numeric",n_iter)
    simresyear <- matrix(nrow=ndf$n_year,ncol=n_iter)
    simrescode <- vector("numeric",n_iter)
# simulation loop - simres contains the residuals from n_iter simulations
    for (isim in 1:n_iter) {
      # simulate variations in the random factors using the data structure
      if (Indicator == "CoastOxygen") {
         simulobs <- SetVector_IndicatorSimO2(alpha$mean,ndf,var_list,df,length(MonthInclude))
      } else {
         simulobs <- SetVector_IndicatorSim(alpha$mean,ndf,var_list,df,length(MonthInclude))
      }
      # backtransform simulations from log domain to original domain
      simulobs <- g_fun_inv(simulobs)
      # add simulated observation to df
      simul_df <- df %>% mutate(xvar = NULL, xvar=simulobs)
      # Calculate indicator value for each year and period
      simul_indicator <- f_fun(simul_df)
      # Check for errors in simulations
      simrescode[isim] <- simul_indicator$error_code
      if (simul_indicator$error_code == 0) {
         simresyear[,isim]=simul_indicator$yearmeans$xvar
         simres[isim] <- simul_indicator$periodmean
      } else{
        simresyear[,isim] = NA
        simres[isim] = NA
      }
    } # end simulation loop
    
# Adjust simulations to have zero mean and then add indicator means - bias correction
    simres <- g_fun_inv(g_fun(simres)-g_fun(mean(simres))+g_fun(mu_indicator$periodmean))
    simresyear <- g_fun_inv(g_fun(simresyear)-g_fun(apply(simresyear,1,mean))+g_fun(mu_indicator$yearmeans$xvar))
    
# Calculate statistics
    period <- data.frame(mean=mean(simres),stderr=sd(simres),
                         lower_1  = quantile(simres,probs=0.005,na.rm=TRUE),
                         lower_5  = quantile(simres,probs=0.025,na.rm=TRUE),
                         lower_10 = quantile(simres,probs=0.05,na.rm=TRUE),
                         upper_10 = quantile(simres,probs=0.95,na.rm=TRUE),
                         upper_5  = quantile(simres,probs=0.975,na.rm=TRUE),
                         upper_1  = quantile(simres,probs=0.995,na.rm=TRUE),row.names = NULL)
    annual <- data.frame(year = mu_indicator$yearmeans$year,mean = apply(simresyear,1,mean),stderr = apply(simresyear,1,sd),
                         lower_1  = apply(simresyear,1,quantile,probs=0.005,na.rm=TRUE),
                         lower_5  = apply(simresyear,1,quantile,probs=0.025,na.rm=TRUE),
                         lower_10 = apply(simresyear,1,quantile,probs=0.05,na.rm=TRUE),
                         upper_10 = apply(simresyear,1,quantile,probs=0.95,na.rm=TRUE),
                         upper_5  = apply(simresyear,1,quantile,probs=0.975,na.rm=TRUE),
                         upper_1  = apply(simresyear,1,quantile,probs=0.995,na.rm=TRUE))
    # Check if there is at least 3 years of data for those indicators with such specification
    if(Indicator %in% c("CoastChla","CoastChlaEQR","CoastBiovol","CoastBiovolEQR","CoastDINwinter","CoastDINwinterEQR","CoastDIPwinter","CoastDIPwinterEQR","CoastTNsummer","CoastTNsummerEQR","CoastTPsummer","CoastTPsummerEQR","CoastTNwinter","CoastTNwinterEQR","CoastTPwinter","CoastTPwinterEQR","CoastOxygen",
                        "LakeBiovol","LakeBiovolEQR","LakePropCyano","LakePropCyanoEQR","LakeNphytspec","LakeChla","LakeChlaEQR","LakeTPsummerEQR","LakeSecchiDepthEQR","RiverTPEQR")) { 
       flag <- ifelse(length(annual$mean)<3,-1,flag)
    }

    res <- list(period=period,annual=annual,indicator_sim=simres,result_code=flag)
    return(res)
  }

#' Generic routine for calculating indicator statistics for non-monitored water body
#' 
#' @param unc_list A list of uncertainty objects from indicator calculations 
#'   All uncertainty objects contain the following variables:
#'   \describe{ 
#'   \item{period}{Mean for the entire assessment period} 
#'   \item{annual}{Annual means for the assessment period} 
#'   \item{indicator_sim}{The simulated distribution of the indicator} 
#'   \item{n_list}{List of dimensions for variance components}} 
#' @param var_list List of variance components containing
#'   \describe{ 
#'   \item{V_WBperiod}{Variance among water bodies for period indicator values} 
#'   \item{V_WBannual}{Variance among water bodies for annual indicator values}} 
#' @param var_WB The variance for the variation among water bodies within a type 
#' @param ntype_WB The total number of waterbodies within the type
#' @param startyear The first year in the indicator calculation
#' @param endyear The last year in the indicator calculation
#' @param n_iter Number of iterations for Monte Carlo simulation
#'   
#' @return An uncertainty object
#' @export
#' 
#' @examples 
CalculateIndicatorType <-
  function(Indicator,unc_list,var_list,ntype_WB,startyear,endyear,n_iter=1000) {
# Set flag to zero and change it for error handling below
    flag <- 0
# Calculations on list of uncertainty objects
    n_WB <- length(unc_list)
    n_yearlist <- vector("numeric",n_WB)
    for (i in 1:n_WB) {n_yearlist[i] <- nrow(unc_list[[i]]$annual)}
    n_year <- max(n_yearlist)
    n_WByear <- sum(n_yearlist)
    # Return from function if no information from other WBs is available for calculation
    if (n_WB == 0)  return(list(result_code=-80))
# Organise data from uncertainty objects into vectors and matrices
    periodWB_mean <- vector("numeric",n_WB)
    periodWB_stderr <- vector("numeric",n_WB)
    annualWB_year <- vector("numeric",n_WByear)
    annualWB_yearmean <- vector("numeric",n_WByear)
    annualWB_yearstderr <- vector("numeric",n_WByear)
#    annual_unc_list <- matrix(n_year,n_WB)
    icount <- 0
    for (i in 1:n_WB) {
      periodWB_mean[i] <- unc_list[[i]]$period$mean
      periodWB_stderr[i] <- unc_list[[i]]$period$stderr
      for (j in 1:n_yearlist[i]) {
        icount <- icount+1
        annualWB_year[icount] <- unc_list[[i]]$annual$year[j]
        annualWB_yearmean[icount] <- unc_list[[i]]$annual$mean[j]
        annualWB_yearstderr[icount] <- unc_list[[i]]$annual$stderr[j]
      }
    }
# Find distributions for period and annual means, add variance contribution from variation among WBs
    period_mean <- mean(periodWB_mean)
    period_stderr <- sqrt(stderr_aggr(periodWB_stderr)^2+var_list$V_WBperiod*(1-n_WB/ntype_WB))
    annual_WB <- data.frame(year=annualWB_year,mean=annualWB_yearmean,stderr=annualWB_yearstderr)
    annual <- annual_WB %>% group_by(year) %>% summarise(mean = mean(mean),stderr = sqrt(stderr_aggr(periodWB_stderr)^2+var_list$V_WBannual*(1-n_WB/ntype_WB)))
# Simulate distribution - the aggregate across WBs is assumed normal distributed
    simres <- rnorm(n_iter,mean=period_mean,sd=period_stderr)
# calculate information for uncertainty object
    period <- data.frame(mean=period_mean,stderr=period_stderr,
                         lower_1  = qnorm(0.005,period_mean,period_stderr),
                         lower_5  = qnorm(0.025,period_mean,period_stderr),
                         lower_10 = qnorm(0.05,period_mean,period_stderr),
                         upper_10 = qnorm(0.95,period_mean,period_stderr),
                         upper_5  = qnorm(0.975,period_mean,period_stderr),
                         upper_1  = qnorm(0.995,period_mean,period_stderr),row.names = NULL)
    annual <- data.frame(year = annual$year ,mean = annual$mean,stderr = annual$stderr,
                         lower_1  = qnorm(0.005,annual$mean,annual$stderr),
                         lower_5  = qnorm(0.025,annual$mean,annual$stderr),
                         lower_10 = qnorm(0.05,annual$mean,annual$stderr),
                         upper_10 = qnorm(0.95,annual$mean,annual$stderr),
                         upper_5  = qnorm(0.975,annual$mean,annual$stderr),
                         upper_1  = qnorm(0.995,annual$mean,annual$stderr))

    res <- list(period=period,annual=annual,indicator_sim=simres,result_code=flag)
    return(res)
  }    





