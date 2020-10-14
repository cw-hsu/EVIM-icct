require(data.table)
require(ggplot2)
require(magrittr)
require(scales)

# readin ------------------------------------------------------------------

# zip <- fread("")
energy <- zip[,.(zip,year,Home = home.energy.demand, Workplace = wp.energy.demand,'Public Level 2' = pL2.energy.demand, 'DC Fast' = DC.energy.demand)]
energy <- melt(energy,id.vars = c("zip","year"),variable.name = "charger.type",value.name = "energy")
energy[,zip := as.character(zip)]

theme_set(theme_bw(base_size = 16))
# cbp <-c( "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
icct <- c("#007D93","#D04F30","#A08341","#F3AC1F","#6B943D")
# Plot

# excluding these ZIPs, but they still have work place charging demand
energy[year %in%c(2025,2030,2035) & !zip %in% c(98195,98174,98154)] %>% 
  ggplot(aes(y=zip, x=energy)) + 
  geom_col(aes(fill = charger.type), position = "stack") +
  facet_wrap(~year) +
  scale_x_continuous(labels = comma) +
  scale_fill_manual(values = icct, guide = guide_legend(reverse = TRUE)) +
  ylab(NULL) +
  xlab("EV charging energy consumption (kWh)") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = NA),
    panel.grid.major.y = element_line(colour = "grey90",linetype = "dashed"),
    # panel.grid.major.x = element_line(linetype = "blank"),
    panel.grid.minor.x = element_line(linetype = "blank"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.key.size = unit(0.3, "cm")#,
    # plot.margin = unit(c(1,1,1,1),"cm")
  )
