require(data.table)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# TODO unfulfilled workplace charging
# TODO build in the EV ready building code impact
# TODO build in variable charger utilization
# TODO might be in a different script, but med and heavy duty trucks
# TODO charger cost

# inputs ------------------------------------------------------------------

wash.LDV.growth <- 0.01
city.LDV.growth<- -0.01
wash.PHEV.decline <- 0.92
metro.city.PHEV.decline <- 0.8
logi.max <- 1.03
start.year <- 2019
electri.goal.year <- 2030
electri.goal.perc <- 0.3
workplace.access.now <- 0.1 # 10%
workplace.access.growth <- 0.01 # 1% CAGR
workplace.util.event.per.day <- 1.5

# energy per charging event for home, work, public L2, DCFC in kWh
PHEV.energy.per.charge <- c(home = 6.5, work = 7.5, pL2 = 3.25, DCFC = 0)
BEV.energy.per.charge <- c(home = 11.5, work = 15.5, pL2 = 5.5, DCFC = 16)

# charger utilization capacity
# hour in hour ;) and power in kW
pL2.hour <- c(min = 3, min.year = 2020, max = 8, max.year = 2025)
pL2.power <- c(min = 6.6, min.year =2020, max = 6.6, max.year = 2050)
DCFC.hour <- c(min = 2, min.year = 2020, max = 8, max.year = 2025)
DCFC.power <- c(min = 50, min.year = 2020, max = 150, max.year = 2035)

# commuters for the vehs in city
commuter.perc <- 0.8
# veh entering or leaving for WORK ONLY
# TODO update with SEA numbers
veh.entering.sea <- 10000 # numbers of vehs 
veh.leaving.sea.frac <- 0.0539 # fractions of vehs leaving

# sea metro EV buyer housing
# det, att, apt
# TODO this is currently the number from 2018 CA statewide
sea.metro.EV.housing <- c(det = 0.832, att = 0.081, apt = 0.087)

# Setups (functions and build tables) -------------------------------------

# # attempt at Bass
# T79 <- c(7,8,9,30)
# # Tdelt <- (1:100)/10
# Sales <- c(9000,10000,12065,wash[y == 2030,LDV]*0.3)
# Bass.nls <- nls(Sales ~ M * (((P + Q)^2/P) * exp(-(P + Q) * T79))/(1 + (Q/P) * exp(-(P + Q) * T79))^2, 
#                 start = list(M = wash[y == 2050,LDV], P = 0.01, Q = 0.20))
# summary(Bass.nls)

# set up 2019 sale data
EV <- as.data.table(
  rbind(
    data.frame(geo = "wash", y = 2019, BEV = 10152, PHEV = 1913, EV = 12065, LDV = 274264) ,
    data.frame(geo = "sea_metro" , y = 2019, BEV = 8148, PHEV = 1230, EV = 9414, LDV = 168178),
    # TODO this number is from SF
    data.frame(geo = "sea_city" , y = 2019, BEV = 1586, PHEV = 1121, EV = 2707, LDV = 31990)
  ))

# calcualte EV percentages
EV[,":="(BEV.p = BEV/LDV, PHEV.p = PHEV/LDV, EV.p = EV/LDV)]

# project LDV sale
for (i in c(2020:2050)){
  EV <- rbind(EV,
              data.frame(geo = "wash", y = i, LDV = EV[geo == "wash" & y == i - 1,LDV] * (1 + wash.LDV.growth)),
              data.frame(geo = "sea_metro", y = i, LDV = EV[geo == "sea_metro" & y == i - 1,LDV] * (1 + wash.LDV.growth)),
              data.frame(geo = "sea_city", y = i, LDV = EV[geo == "sea_city" & y == i - 1,LDV] * (1 + city.LDV.growth)),
              fill = T)
}

# funciton for finding 'a' value for logistic growth curve
find.a <- function(geo.i){
  return(logi.max/EV[geo == geo.i & y == start.year, EV.p] -1)
}

# function for finding slope needed to achieve x% by y year
find.slope <- function(percent, target.year, geo.i){
  slope <- -log((logi.max/percent -1)/find.a(geo.i))/ (target.year - start.year)
  return(slope)
}

# setup pL2 utilization table
pL2.util <- merge(
  merge(
    data.table(year = min(EV$y):max(EV$y)) ,
    data.table(year = pL2.hour['min.year']:pL2.hour['max.year'],
               hour = seq(pL2.hour['min'],pL2.hour['max'],length.out = pL2.hour['max.year'] - pL2.hour['min.year'] + 1)),
    all.x = T),
  data.table(year = pL2.power['min.year']:pL2.power['max.year'],
             power = seq(pL2.power['min'],pL2.power['max'],length.out = pL2.power['max.year'] - pL2.power['min.year'] + 1)), 
  all.x = T)

# fill in missing values
if(is.na(pL2.util$hour[1])){
  pos <- which(!is.na(pL2.util$hour))[1] 
  pL2.util$hour[1:pos] <- pL2.util$hour[pos]
} 

if(is.na(pL2.util$power[1])){
  pos <- which(!is.na(pL2.util$power))[1] 
  pL2.util$power[1:pos] <- pL2.util$power[pos]
} 

if(max(which(!is.na(pL2.util$hour))) <length(pL2.util$hour)){
  pos <- max(which(!is.na(pL2.util$hour)))
  pL2.util$hour[pos:length(pL2.util$hour)] <- pL2.util$hour[pos]
}

if(max(which(!is.na(pL2.util$power))) <length(pL2.util$power)){
  pos <- max(which(!is.na(pL2.util$power)))
  pL2.util$power[pos:length(pL2.util$power)] <- pL2.util$power[pos]
}

# setup DCFC utilization table
DC.util <- merge(
  merge(
    data.table(year = min(EV$y):max(EV$y)) ,
    data.table(year = DCFC.hour['min.year']:DCFC.hour['max.year'],
               hour = seq(DCFC.hour['min'],DCFC.hour['max'],length.out = DCFC.hour['max.year'] - DCFC.hour['min.year'] + 1)),
    all.x = T),
  data.table(year = DCFC.power['min.year']:DCFC.power['max.year'],
             power = seq(DCFC.power['min'],DCFC.power['max'],length.out = DCFC.power['max.year'] - DCFC.power['min.year'] + 1)), 
  all.x = T)

# fill in missing values
if(is.na(DC.util$hour[1])){
  pos <- which(!is.na(DC.util$hour))[1] 
  DC.util$hour[1:pos] <- DC.util$hour[pos]
} 

if(is.na(DC.util$power[1])){
  pos <- which(!is.na(DC.util$power))[1] 
  DC.util$power[1:pos] <- DC.util$power[pos]
} 

if(max(which(!is.na(DC.util$hour))) <length(DC.util$hour)){
  pos <- max(which(!is.na(DC.util$hour)))
  DC.util$hour[pos:length(DC.util$hour)] <- DC.util$hour[pos]
}

if(max(which(!is.na(DC.util$power))) <length(DC.util$power)){
  pos <- max(which(!is.na(DC.util$power)))
  DC.util$power[pos:length(DC.util$power)] <- DC.util$power[pos]
}

# Project sales -----------------------------------------------------------

# project EV % based on the input goal % and goal year
# WA, Seattle metro, and Seattle city all use Seattle city's b (slope)
EV[, EV.p := logi.max / (1 + sapply(geo,find.a) *exp(-mapply(find.slope,electri.goal.perc,electri.goal.year,"sea_city") * (y - start.year)))]
# can't have more than 100% EV sale, this happens because we set the max to 102%
EV[EV.p > 1, EV.p := 1]
EV[, EV := LDV * EV.p]

# estimate the ratio between PHEV and BEV sales based on the previous years ratio between PHEV and EV
for ( i in 2020:2050){
  for ( geo.i in c("wash","sea_metro","sea_city")){
    t <- EV[geo == geo.i & y == i -1,PHEV.p/EV.p] * ifelse(geo.i == "wash",wash.PHEV.decline,metro.city.PHEV.decline)
    EV[y == i & geo == geo.i, PHEV.p := EV.p * t]
    EV[y == i & geo == geo.i, BEV.p := (EV.p-PHEV.p)]
  }
}

# cumulative sales
EV[,':='(BEV = LDV * BEV.p, PHEV = LDV * PHEV.p)]
EV[, ':='(cumu.BEV = cumsum(BEV),
          cumu.PHEV = cumsum(PHEV),
          cumu.EV = cumsum(EV),
          cumu.LDV = cumsum(LDV)),geo]

# Stock turn over ---------------------------------------------------------

# read-in survival rate
survive.rate <- fread("inputs/survival-rate.csv")

# 
stock.turn.over <- as.data.table(expand.grid(EV[,unique(y)],EV[,unique(y)],c("wash","sea_metro","sea_city")))
setnames(stock.turn.over, c("cal.yr","mod.yr","geo"))
stock.turn.over[,age := cal.yr - mod.yr]
stock.turn.over <- merge(stock.turn.over,survive.rate[,.(age = Age,Survival.2005.plus)])

stock.turn.over <- merge(stock.turn.over,EV[,.(mod.yr = y, BEV, PHEV, LDV, geo)], by = c("mod.yr","geo"))
stock.turn.over[,':='(BEV = BEV * Survival.2005.plus,
                      PHEV = PHEV * Survival.2005.plus,
                      LDV = LDV * Survival.2005.plus)]
# deal with historical data later. If no historical sale then can just use retro projection like the execl sheet
# stock.turn.over[age>36,]

# View(stock.turn.over[geo == "wash" & mod.yr == 2020])

# fleet size
fleet.size <- stock.turn.over[,.(BEV = sum(BEV),
                                 PHEV = sum(PHEV),
                                 LDV = sum(LDV)),.(geo,cal.yr)]

# Split stock into zips ---------------------------------------------------

# TODO currently the LDV sale is dummy values
zip.LDV.sale <- fread("inputs/zip.LDV.sale.csv")
zip.LDV.sale[,LDV.per.pop := `2019.ldv.sale` / `2017.pop`]

# TODO pop projection dummy
# weight the population projection by 2019 veh per 2017 pop to project veh sales in zips
zip.pop <- fread("inputs/zip.pop.proj.csv")
zip.pop <- melt(zip.pop,id.vars = 'zip',variable.name = "year", value.name = "pop")
zip.pop <- merge(zip.pop, zip.LDV.sale[,.(zip,LDV.per.pop)])
zip.pop[,year := as.numeric(as.character(year))]
# remove the zips with 0 pop or 0 vehs
# zip.pop <- zip.pop[!zip %in% c(98154,98174,98195)]
zip.pop[,veh.frac.of.city := pop*LDV.per.pop/sum(pop*LDV.per.pop,na.rm = T), year]
zip.pop[,pop.frac := pop/sum(pop), year]

# merge up fleet and ZIP and split
# TODO note that 2019 fleet data is not available rn
zip <- merge(zip.pop,fleet.size[geo == "sea_city",.(year = cal.yr,BEV,PHEV,LDV)],by = "year",all.x = T)
zip[,':='(BEV = BEV * veh.frac.of.city,
          PHEV = PHEV * veh.frac.of.city,
          LDV = LDV * veh.frac.of.city)]

# Charger projection ------------------------------------------------------

# readin inputs
zip.res.unit.brkdwn <- fread("inputs/zip.res.unit.breakdown.csv")
zip.res.perc.of.city <- fread("inputs/zip.res.unit.perc.of.city.csv")
events.per.day <- fread("inputs/charge.events.per.day.csv")
# events.per.day <- melt.data.table(events.per.day,id.vars = 1:4,variable.name = "loc", value.name = "events.per.day") 
zip.job.proj <- fread("inputs/zip.job.proj.csv")
zip.job.proj <- melt.data.table(zip.job.proj, id.vars = "zip", variable.name = "year", value.name = "job.proj")
zip.job.proj[,':='(year = as.numeric(as.character(year)), zip = as.numeric(as.character(zip)))]
# calc the job perc of zip by year
zip.job.proj[,job.perc := job.proj / sum(job.proj), year]
# calc the job growth factor [for out-of-city commuter use]
zip.job.proj[,job.growth.factor := job.proj/.SD[year == 2020,job.proj],by = zip]
veh.per.veh.owning.hh <- fread("inputs/veh.per.veh.owning.hh.csv")
home.charger.access <- fread("inputs/home.charger.access.csv")

# estimate the EV buyer housing characteristics in each zip based on the SEA city or metro EV buyer housing breakdown
for (zip.i in zip[,unique(zip)]){
  sumprod <- as.numeric(sea.metro.EV.housing["det"] * zip.res.perc.of.city[zip == zip.i ,det] +
                          sea.metro.EV.housing["att"] * zip.res.perc.of.city[zip == zip.i ,att] +
                          sea.metro.EV.housing["apt"] * zip.res.perc.of.city[zip == zip.i ,apt]) 
  
  zip[year == 2020 & zip == zip.i,
      ':='(ev.in.det = sea.metro.EV.housing["det"] * zip.res.perc.of.city[zip == zip.i, det] / sumprod,
           ev.in.att = sea.metro.EV.housing["att"] * zip.res.perc.of.city[zip == zip.i, att] / sumprod,
           ev.in.apt = sea.metro.EV.housing["apt"] * zip.res.perc.of.city[zip == zip.i, apt] / sumprod)]
  
  # by 2040, the EV buyer's housing characteristics resemble the zip wide housing characteristics
  zip[year >= 2040 & zip == zip.i,
      ':='(ev.in.det = zip.res.unit.brkdwn[zip == zip.i, det],
           ev.in.att = zip.res.unit.brkdwn[zip == zip.i, att],
           ev.in.apt = zip.res.unit.brkdwn[zip == zip.i, apt])]
}

# interpolate EV owner housing characteristcis between 2020 and 2040
zip[year >= 2020, ev.in.det:= zoo::na.approx(ev.in.det, na.rm=TRUE), by = zip]
zip[year >= 2020, ev.in.att:= zoo::na.approx(ev.in.att, na.rm=TRUE), by = zip]
zip[year >= 2020, ev.in.apt:= zoo::na.approx(ev.in.apt, na.rm=TRUE), by = zip]

# calculate energy demand
zip[,':='(
  # pL2 pL2 pL2 pL2 pL2 pL2 pL2 pL2 pL2 pL2 pL2 pL2 pL2
  BEV.pL2.events =
    # BEV commuters with home L1 
    BEV * commuter.perc *events.per.day[EV == "BEV" & home.charger == "L1" & commuter == "T",pub.L2] *
    # in detached housing
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",home.L1.access]  +
       # in attached housing
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",home.L1.access] +
       # in apartments
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L1.access]) +
    
    # BEV commuters with home L2
    BEV * commuter.perc *events.per.day[EV == "BEV" & home.charger == "L2" & commuter == "T",pub.L2] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",home.L2.access]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",home.L2.access] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L2.access]) +
    
    # BEV commuters with no home charger
    BEV * commuter.perc *events.per.day[EV == "BEV" & home.charger == "no" & commuter == "T",pub.L2] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",no.home.charger]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",no.home.charger] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", no.home.charger]) +
    
    # BEV non-commuters with home L1 
    BEV * (1-commuter.perc) *events.per.day[EV == "BEV" & home.charger == "L1" & commuter == "F",pub.L2] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",home.L1.access]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",home.L1.access] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L1.access]) +
    
    # BEV non-commuters with home L2
    BEV * (1-commuter.perc) *events.per.day[EV == "BEV" & home.charger == "L2" & commuter == "F",pub.L2] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",home.L2.access]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",home.L2.access] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L2.access]) +
    
    # BEV non-commuters with no home charger
    BEV * (1-commuter.perc) *events.per.day[EV == "BEV" & home.charger == "no" & commuter == "F",pub.L2] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",no.home.charger]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",no.home.charger] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", no.home.charger]),
  
  PHEV.pL2.events =
    # PHEV commuters with home L1 
    PHEV * commuter.perc *events.per.day[EV == "PHEV" & home.charger == "L1" & commuter == "T",pub.L2] *
    (ev.in.det * home.charger.access[EV == "PHEV" & housing == "det",home.L1.access]  +
       ev.in.att * home.charger.access[EV == "PHEV" & housing == "att",home.L1.access] +
       ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L1.access]) +
    
    # PHEV commuters with home L2
    PHEV * commuter.perc *events.per.day[EV == "PHEV" & home.charger == "L2" & commuter == "T",pub.L2] *
    (ev.in.det * home.charger.access[EV == "PHEV" & housing == "det",home.L2.access]  +
       ev.in.att * home.charger.access[EV == "PHEV" & housing == "att",home.L2.access] +
       ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L2.access]) +
    
    # PHEV commuters with no home charger
    PHEV * commuter.perc *events.per.day[EV == "PHEV" & home.charger == "no" & commuter == "T",pub.L2] *
    (ev.in.det * home.charger.access[EV == "PHEV" & housing == "det",no.home.charger]  +
       ev.in.att * home.charger.access[EV == "PHEV" & housing == "att",no.home.charger] +
       ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", no.home.charger]) +
    
    # PHEV non-commuters with home L1 
    PHEV * (1-commuter.perc) *events.per.day[EV == "PHEV" & home.charger == "L1" & commuter == "T",pub.L2] *
    (ev.in.det * home.charger.access[EV == "PHEV" & housing == "det",home.L1.access]  +
       ev.in.att * home.charger.access[EV == "PHEV" & housing == "att",home.L1.access] +
       ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L1.access]) +
    
    # PHEV non-commuters with home L2
    PHEV * (1-commuter.perc) *events.per.day[EV == "PHEV" & home.charger == "L2" & commuter == "T",pub.L2] *
    (ev.in.det * home.charger.access[EV == "PHEV" & housing == "det",home.L2.access]  +
       ev.in.att * home.charger.access[EV == "PHEV" & housing == "att",home.L2.access] +
       ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L2.access]) +
    
    # PHEV non-commuters with no home charger
    PHEV * (1-commuter.perc) *events.per.day[EV == "PHEV" & home.charger == "no" & commuter == "T",pub.L2] *
    (ev.in.det * home.charger.access[EV == "PHEV" & housing == "det",no.home.charger]  +
       ev.in.att * home.charger.access[EV == "PHEV" & housing == "att",no.home.charger] +
       ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", no.home.charger]),
  
  # DCFC DCFC DCFC DCFC DCFC DCFC DCFC DCFC DCFC DCFC DCFC 
  BEV.DC.events =
    # BEV commuters with home L1 
    BEV * commuter.perc *events.per.day[EV == "BEV" & home.charger == "L1" & commuter == "T",DCFC] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",home.L1.access]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",home.L1.access] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L1.access]) +
    
    # BEV commuters with home L2
    BEV * commuter.perc *events.per.day[EV == "BEV" & home.charger == "L2" & commuter == "T",DCFC] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",home.L2.access]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",home.L2.access] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L2.access]) +
    
    # BEV commuters with no home charger
    BEV * commuter.perc *events.per.day[EV == "BEV" & home.charger == "no" & commuter == "T",DCFC] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",no.home.charger]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",no.home.charger] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", no.home.charger]) +
    
    # BEV non-commuters with home L1 
    BEV * (1-commuter.perc) *events.per.day[EV == "BEV" & home.charger == "L1" & commuter == "F",DCFC] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",home.L1.access]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",home.L1.access] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L1.access]) +
    
    # BEV non-commuters with home L2
    BEV * (1-commuter.perc) *events.per.day[EV == "BEV" & home.charger == "L2" & commuter == "F",DCFC] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",home.L2.access]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",home.L2.access] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L2.access]) +
    
    # BEV non-commuters with no home charger
    BEV * (1-commuter.perc) *events.per.day[EV == "BEV" & home.charger == "no" & commuter == "F",DCFC] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",no.home.charger]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",no.home.charger] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", no.home.charger]), 
  
  # home home home home home home home home home home home home home home home home home home home home
  BEV.home.events =
    # BEV commuters with home L1 
    BEV * commuter.perc *events.per.day[EV == "BEV" & home.charger == "L1" & commuter == "T", home] *
    # in detached housing
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",home.L1.access]  +
       # in attached housing
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",home.L1.access] +
       # in apartments
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L1.access]) +
    
    # BEV commuters with home L2
    BEV * commuter.perc *events.per.day[EV == "BEV" & home.charger == "L2" & commuter == "T",home] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",home.L2.access]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",home.L2.access] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L2.access]) +
    
    # BEV non-commuters with home L1 
    BEV * (1-commuter.perc) *events.per.day[EV == "BEV" & home.charger == "L1" & commuter == "F",home] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",home.L1.access]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",home.L1.access] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L1.access]) +
    
    # BEV non-commuters with home L2
    BEV * (1-commuter.perc) *events.per.day[EV == "BEV" & home.charger == "L2" & commuter == "F",home] *
    (ev.in.det * home.charger.access[EV == "BEV" & housing == "det",home.L2.access]  +
       ev.in.att * home.charger.access[EV == "BEV" & housing == "att",home.L2.access] +
       ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L2.access]),
  
  PHEV.home.events =
    # PHEV commuters with home L1 
    PHEV * commuter.perc *events.per.day[EV == "PHEV" & home.charger == "L1" & commuter == "T",home] *
    (ev.in.det * home.charger.access[EV == "PHEV" & housing == "det",home.L1.access]  +
       ev.in.att * home.charger.access[EV == "PHEV" & housing == "att",home.L1.access] +
       ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L1.access]) +
    
    # PHEV commuters with home L2
    PHEV * commuter.perc *events.per.day[EV == "PHEV" & home.charger == "L2" & commuter == "T",home] *
    (ev.in.det * home.charger.access[EV == "PHEV" & housing == "det",home.L2.access]  +
       ev.in.att * home.charger.access[EV == "PHEV" & housing == "att",home.L2.access] +
       ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L2.access]) +
    
    # PHEV non-commuters with home L1 
    PHEV * (1-commuter.perc) *events.per.day[EV == "PHEV" & home.charger == "L1" & commuter == "T",home] *
    (ev.in.det * home.charger.access[EV == "PHEV" & housing == "det",home.L1.access]  +
       ev.in.att * home.charger.access[EV == "PHEV" & housing == "att",home.L1.access] +
       ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L1.access]) +
    
    # PHEV non-commuters with home L2
    PHEV * (1-commuter.perc) *events.per.day[EV == "PHEV" & home.charger == "L2" & commuter == "T",home] *
    (ev.in.det * home.charger.access[EV == "PHEV" & housing == "det",home.L2.access]  +
       ev.in.att * home.charger.access[EV == "PHEV" & housing == "att",home.L2.access] +
       ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L2.access]) 
)]

# calculate workplace events and split into zips

# get job distribution
zip <- merge(zip,zip.job.proj[,.(zip,year,job.perc,job.growth.factor)], by = c('zip','year'))

n_year <- nrow(fleet.size[geo == "sea_city",])
city.wp.events <- 
  fleet.size[geo == "sea_city",
             .(year = cal.yr ,
               BEV.wp.events = 
                 # BEV commuters intra-city
                 BEV * commuter.perc * (1-veh.leaving.sea.frac) * 
                 # access to work place chargers
                 cumprod(c(workplace.access.now, rep(1+workplace.access.growth,n_year-1))) *
                 # events per day
                 (zip.res.unit.brkdwn[zip == "city", det] * home.charger.access[ EV == "BEV" & housing == "det", home.L2.access] *
                    events.per.day[commuter == "T" & EV == "BEV" & home.charger == "L2", work] +
                    zip.res.unit.brkdwn[zip == "city", att] * home.charger.access[ EV == "BEV" & housing == "att", home.L2.access] *
                    events.per.day[commuter == "T" &EV == "BEV" & home.charger == "L2", work] +
                    zip.res.unit.brkdwn[zip == "city", apt] * home.charger.access[ EV == "BEV" & housing == "apt", home.L2.access] *
                    events.per.day[commuter == "T" &EV == "BEV" & home.charger == "L2", work] +
                    zip.res.unit.brkdwn[zip == "city", det] * home.charger.access[ EV == "BEV" & housing == "det", home.L1.access] *
                    events.per.day[commuter == "T" &EV == "BEV" & home.charger == "L1", work] +
                    zip.res.unit.brkdwn[zip == "city", att] * home.charger.access[ EV == "BEV" & housing == "att", home.L1.access] *
                    events.per.day[commuter == "T" &EV == "BEV" & home.charger == "L1", work] +
                    zip.res.unit.brkdwn[zip == "city", apt] * home.charger.access[ EV == "BEV" & housing == "apt", home.L1.access] *
                    events.per.day[commuter == "T" &EV == "BEV" & home.charger == "L1", work] +
                    zip.res.unit.brkdwn[zip == "city", det] * home.charger.access[ EV == "BEV" & housing == "det", no.home.charger] *
                    events.per.day[commuter == "T" &EV == "BEV" & home.charger == "no", work] +
                    zip.res.unit.brkdwn[zip == "city", att] * home.charger.access[ EV == "BEV" & housing == "att", no.home.charger] *
                    events.per.day[commuter == "T" &EV == "BEV" & home.charger == "no", work] +
                    zip.res.unit.brkdwn[zip == "city", apt] * home.charger.access[ EV == "BEV" & housing == "apt", no.home.charger] *
                    events.per.day[commuter == "T" &EV == "BEV" & home.charger == "no", work]),
               
               PHEV.wp.events = 
                 # PHEV commuters intra-city
                 PHEV * commuter.perc * (1-veh.leaving.sea.frac) * 
                 # access to work place chargers
                 cumprod(c(workplace.access.now, rep(1+workplace.access.growth,n_year-1))) *
                 # events per day
                 (zip.res.unit.brkdwn[zip == "city", det] * home.charger.access[ EV == "PHEV" & housing == "det", home.L2.access] *
                    events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "L2", work] +
                    zip.res.unit.brkdwn[zip == "city", att] * home.charger.access[ EV == "PHEV" & housing == "att", home.L2.access] *
                    events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "L2", work] +
                    zip.res.unit.brkdwn[zip == "city", apt] * home.charger.access[ EV == "PHEV" & housing == "apt", home.L2.access] *
                    events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "L2", work] +
                    zip.res.unit.brkdwn[zip == "city", det] * home.charger.access[ EV == "PHEV" & housing == "det", home.L1.access] *
                    events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "L1", work] +
                    zip.res.unit.brkdwn[zip == "city", att] * home.charger.access[ EV == "PHEV" & housing == "att", home.L1.access] *
                    events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "L1", work] +
                    zip.res.unit.brkdwn[zip == "city", apt] * home.charger.access[ EV == "PHEV" & housing == "apt", home.L1.access] *
                    events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "L1", work] +
                    zip.res.unit.brkdwn[zip == "city", det] * home.charger.access[ EV == "PHEV" & housing == "det", no.home.charger] *
                    events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "no", work] +
                    zip.res.unit.brkdwn[zip == "city", att] * home.charger.access[ EV == "PHEV" & housing == "att", no.home.charger] *
                    events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "no", work] +
                    zip.res.unit.brkdwn[zip == "city", apt] * home.charger.access[ EV == "PHEV" & housing == "apt", no.home.charger] *
                    events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "no", work])
             )]


zip <- merge(zip, city.wp.events, "year")
zip[,BEV.wp.events := BEV.wp.events * job.perc]
zip[,PHEV.wp.events := PHEV.wp.events * job.perc]

# out-of-city vehs
out.of.city.vehs <- fleet.size[geo == 'sea_metro',.(year = cal.yr, BEV.into.sea = BEV/LDV* veh.entering.sea, PHEV.into.sea = PHEV/LDV*veh.entering.sea)] 
zip <- merge(zip, out.of.city.vehs, by = "year")
zip[,BEV.into.sea := BEV.into.sea * job.perc * job.growth.factor]
zip[,PHEV.into.sea := PHEV.into.sea * job.perc * job.growth.factor]

# out-of-city wp charging events
zip[,':='(
  BEV.out.of.city.wp.events = 
    # BEV commuters intra-city
    BEV.into.sea * commuter.perc * (1-veh.leaving.sea.frac) * 
    # access to work place chargers
    cumprod(c(workplace.access.now, rep(1+workplace.access.growth,n_year-1))) *
    # events per day
    (zip.res.unit.brkdwn[zip == "city", det] * home.charger.access[ EV == "BEV" & housing == "det", home.L2.access] *
       events.per.day[commuter == "T" & EV == "BEV" & home.charger == "L2", work] +
       zip.res.unit.brkdwn[zip == "city", att] * home.charger.access[ EV == "BEV" & housing == "att", home.L2.access] *
       events.per.day[commuter == "T" &EV == "BEV" & home.charger == "L2", work] +
       zip.res.unit.brkdwn[zip == "city", apt] * home.charger.access[ EV == "BEV" & housing == "apt", home.L2.access] *
       events.per.day[commuter == "T" &EV == "BEV" & home.charger == "L2", work] +
       zip.res.unit.brkdwn[zip == "city", det] * home.charger.access[ EV == "BEV" & housing == "det", home.L1.access] *
       events.per.day[commuter == "T" &EV == "BEV" & home.charger == "L1", work] +
       zip.res.unit.brkdwn[zip == "city", att] * home.charger.access[ EV == "BEV" & housing == "att", home.L1.access] *
       events.per.day[commuter == "T" &EV == "BEV" & home.charger == "L1", work] +
       zip.res.unit.brkdwn[zip == "city", apt] * home.charger.access[ EV == "BEV" & housing == "apt", home.L1.access] *
       events.per.day[commuter == "T" &EV == "BEV" & home.charger == "L1", work] +
       zip.res.unit.brkdwn[zip == "city", det] * home.charger.access[ EV == "BEV" & housing == "det", no.home.charger] *
       events.per.day[commuter == "T" &EV == "BEV" & home.charger == "no", work] +
       zip.res.unit.brkdwn[zip == "city", att] * home.charger.access[ EV == "BEV" & housing == "att", no.home.charger] *
       events.per.day[commuter == "T" &EV == "BEV" & home.charger == "no", work] +
       zip.res.unit.brkdwn[zip == "city", apt] * home.charger.access[ EV == "BEV" & housing == "apt", no.home.charger] *
       events.per.day[commuter == "T" &EV == "BEV" & home.charger == "no", work]),
  
  PHEV.out.of.city.wp.events = 
    # PHEV commuters intra-city
    PHEV.into.sea * commuter.perc * (1-veh.leaving.sea.frac) * 
    # access to work place chargers
    cumprod(c(workplace.access.now, rep(1+workplace.access.growth,n_year-1))) *
    # events per day
    (zip.res.unit.brkdwn[zip == "city", det] * home.charger.access[ EV == "PHEV" & housing == "det", home.L2.access] *
       events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "L2", work] +
       zip.res.unit.brkdwn[zip == "city", att] * home.charger.access[ EV == "PHEV" & housing == "att", home.L2.access] *
       events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "L2", work] +
       zip.res.unit.brkdwn[zip == "city", apt] * home.charger.access[ EV == "PHEV" & housing == "apt", home.L2.access] *
       events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "L2", work] +
       zip.res.unit.brkdwn[zip == "city", det] * home.charger.access[ EV == "PHEV" & housing == "det", home.L1.access] *
       events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "L1", work] +
       zip.res.unit.brkdwn[zip == "city", att] * home.charger.access[ EV == "PHEV" & housing == "att", home.L1.access] *
       events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "L1", work] +
       zip.res.unit.brkdwn[zip == "city", apt] * home.charger.access[ EV == "PHEV" & housing == "apt", home.L1.access] *
       events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "L1", work] +
       zip.res.unit.brkdwn[zip == "city", det] * home.charger.access[ EV == "PHEV" & housing == "det", no.home.charger] *
       events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "no", work] +
       zip.res.unit.brkdwn[zip == "city", att] * home.charger.access[ EV == "PHEV" & housing == "att", no.home.charger] *
       events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "no", work] +
       zip.res.unit.brkdwn[zip == "city", apt] * home.charger.access[ EV == "PHEV" & housing == "apt", no.home.charger] *
       events.per.day[commuter == "T" &EV == "PHEV" & home.charger == "no", work])
)]

# add out-of-city wp events to intracity events
zip[,BEV.wp.events := BEV.wp.events + BEV.out.of.city.wp.events]
zip[,PHEV.wp.events := PHEV.wp.events + PHEV.out.of.city.wp.events]

# calc energy demand
# TODO to vary the energy per session
# pL2
zip[,pL2.energy.demand := BEV.pL2.events * BEV.energy.per.charge['pL2'] + PHEV.pL2.events * PHEV.energy.per.charge['pL2']]
# DC
zip[,DC.energy.demand := BEV.DC.events * BEV.energy.per.charge['DCFC']]
# home
zip[,home.energy.demand := BEV.home.events * BEV.energy.per.charge['home'] + PHEV.home.events * PHEV.energy.per.charge['home']]
# work
zip[,wp.energy.demand := BEV.wp.events * BEV.energy.per.charge['work'] + PHEV.wp.events * PHEV.energy.per.charge['work']]


# calc chargers
# pL2 & DC
zip <- merge(zip,pL2.util[,.(year , pL2.util = hour * power)])
zip <- merge(zip,DC.util[,.(year , DC.util = hour * power)])
zip[,pL2.proj := pL2.energy.demand / pL2.util]
zip[,DCFC.proj := DC.energy.demand / DC.util]

# intracity work 
zip[,wp.proj := (BEV.wp.events + PHEV.wp.events)/ workplace.util.event.per.day]

# home
zip <- merge(zip, veh.per.veh.owning.hh[,.(zip = as.integer(zip),vpvoh = veh.per.veh.owning.hh)], by = "zip")
zip[,home.L1.proj := 
      BEV * ev.in.det * home.charger.access[EV == "BEV" & housing == "det", home.L1.access] + 
      BEV * ev.in.att * home.charger.access[EV == "BEV" & housing == "att", home.L1.access] +
      BEV * ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L1.access] +
      PHEV * ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L1.access] +
      PHEV * ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L1.access] +
      PHEV * ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L1.access]]

zip[,home.L2.proj := 
      BEV * ev.in.det * home.charger.access[EV == "BEV" & housing == "det", home.L2.access] + 
      BEV * ev.in.att * home.charger.access[EV == "BEV" & housing == "att", home.L2.access] +
      BEV * ev.in.apt * home.charger.access[EV == "BEV" & housing == "apt", home.L2.access] +
      PHEV * ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L2.access] +
      PHEV * ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L2.access] +
      PHEV * ev.in.apt * home.charger.access[EV == "PHEV" & housing == "apt", home.L2.access]]

zip[,home.L1.proj := home.L1.proj / vpvoh] # vpvoh vehicle per vehicle owning hhs
zip[,home.L2.proj := home.L2.proj / vpvoh]



# Quick and temp plotting -------------------------------------------------

ggplot(EV) +
  geom_line(aes(x = y, y = EV.p, color = geo))



