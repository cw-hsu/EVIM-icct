require(data.table)

# take in the output format from the R-version model
# can also mod the excel output to match. Will be time consuming tho


# readins -----------------------------------------------------------------

# assuming load profile can be 
load.profile <- fread("inputs/charger.load.profile.csv")
# zip <- read.csv("")

load.profile[,perc.energy := average.kW/sum(average.kW),charger.type]


# organize data and combine -----------------------------------------------

# can change later to hourly, 30 min, 15 min, or 5 min int
load.profile.1hr <- load.profile[time.int == 60]
# time int in minutes


energy <- zip[,.(zip,year,home = home.energy.demand, workplace = wp.energy.demand,pL2 = pL2.energy.demand, DC = DC.energy.demand)]
energy <- melt(energy,id.vars = c("zip","year"),variable.name = "charger.type",value.name = "energy")

power <- expand.grid(zip = unique(energy$zip),year = unique(energy$year),
                 charger.type = unique(energy$charger.type),time = unique(load.profile.1hr$time))

power <- merge(power,load.profile.1hr,by = c("time","charger.type"),all.x = T)
power <- merge(power,energy,by = c("zip","year","charger.type"))
setDT(power)

power[,energy := energy * perc.energy]
power[,power := energy/time.int*60]
