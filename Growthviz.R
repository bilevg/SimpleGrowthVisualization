library(cshapes)
library(tidyr)
library(dplyr)
library(rworldmap)
library(spdep)
library(tripack)
library(maptools)
library(wbstats)
library(ggplot2)
library(gganimate)
library(gpclib)
gpclibPermit()
options(digits=8, max.print=1000, scipen=13)

## world bank indicator for gdp percent change annual

## ## get the GDPpc contant 2010$ and Population total indicators from Development Indicators
wb.df = wb(indicator=c('NY.GDP.MKTP.KD.ZG', 'SP.POP.TOTL'), country='countries_only', start=1990, end=2016)

wb.df = wb.df %>%
    select(country, date, iso3c, value, indicator) %>%
    spread(key = indicator, value=value) %>%
    rename(year = date, Population = 'Population, total', ISO1AL3 = iso3c,
           GDPgrowth = 'GDP growth (annual %)') %>%
    filter(Population >= 500000) %>%
    select(-Population)

themeops <- theme(axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.y=element_blank(), axis.ticks=element_blank(),
                  axis.title.y=element_blank(), legend.direction="vertical",
                  panel.grid=element_blank(),
                  legend.position="bottom",
                  legend.key = element_rect(colour = 'black',  size = .8, linetype='solid'),
                  text=element_text(size=10),
                  plot.title=element_text(hjust=.5, size=12))


##
for (year in 1990:2016){
    date = as.Date(paste(year, '6-30', sep='-'))
    map.year = cshp(date, useGW=TRUE)
    ## ## how many of the World Bank countries can we match? about 165 (pretty good)
    ## sum(unique(wb.df$iso3c) %in% map.all@data$ISO1AL3)
    ## drop unnecessary variables from cshape data
    map.year@data <- map.year@data %>% select(CNTRY_NAME, ISO1AL3)
    map.year@data$ISO1AL3 <- as.character(map.year@data$ISO1AL3)
    ## ## mask for countries not in World Bank data
    ## mask_missing <- map.year@data$ISO1AL3 %in% wb.df$ISO1AL3
    wb.year <- wb.df[wb.df$year == year, ]
    wb.year$GDPgrowth[wb.year$GDPgrowth > 5] <- 5
    wb.year$GDPgrowth[wb.year$GDPgrowth < -5] <- -5
    ## merge the two
    ## map.year@data <- data.frame(map.year@data, wb.year[match(map.year@data$ISO1AL3, wb.year$ISO1AL3), ])
    map.year@data <- join(map.year@data, wb.year, by='ISO1AL3')
    map.year@data$id <- map.year@data$ISO1AL3
    mapofyear <- fortify(model=map.year, region = 'ISO1AL3')
    map <- join(mapofyear, map.year@data, by='id')
#
    plot <- ggplot(map) + aes(long, lat, group=group, fill=GDPgrowth, frame=year) +
        geom_polygon() + geom_path(color='white', lwd=.2) + coord_fixed(1.2) +
        scale_fill_gradient2(midpoint=0,
                             breaks=c(-4,0,4), labels=c("-4%", "0%", "4%")) +
        guides(fill=guide_colorbar(title.position="top",
                             direction = "horizontal",
                             label.position = "bottom",
                             title="GDP % Change")) +
        themeops + ggtitle('Growth')
    ggsave(filename = paste('Growth_', year, '.png', sep=''), device = 'png', width = 4, height = 3, dpi = 300)
}
##

gganimate(plot)

gganimate(plot, "output.gif")

