library(cshapes)
library(tidyr)
library(dplyr)
library(rworldmap)
library(spdep)
library(tripack)
library(maptools)
library(wbstats)
library(ggplot2)
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

##
for (year in 1990:2016){
    date = as.Date(paste(year, '6-30', sep='-'))
    map.year = cshp(date, useGW=TRUE)
    ## ## how many of the World Bank countries can we match? about 165 (pretty good)
    ## sum(unique(wb.df$iso3c) %in% map.all@data$ISO1AL3)

    ## drop unnecessary variables from cshape data
    map.year@data <- map.year@data %>% select(CNTRY_NAME, ISO1AL3)
    map.year@data$ISO1AL3 <- as.character(map.year@data$ISO1AL3)

    ## eliminate countries not in World Bank data
    map.year@data <- map.year@data[map.year@data$ISO1AL3 %in% wb.df$ISO1AL3, ]

    wb.year <- wb.df[wb.df$year == year, ]
    ## merge the two
    ## map.year@data <- data.frame(map.year@data, wb.year[match(map.year@data$ISO1AL3, wb.year$ISO1AL3), ])
    map.year@data <- join(map.year@data, wb.year)
    ## map.year@data$id <- map.year$ISO1AL3
    mapofyear <- fortify(map.year)
    map <- join(mapofyear, map.year@data)

    plot <- ggplot(map) + aes(long, lat, group=group, fill=GDPgrowth) +
        geom_polygon() + geom_path(color='white')
}
##



