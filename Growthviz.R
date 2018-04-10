library(cshapes)
library(tidyr)
library(dplyr)
library(rworldmap)
library(spdep)
library(tripack)
library(maptools)
library(wbstats)
library(ggplot2)
## library(gganimate)
library(gpclib)
gpclibPermit()
options(digits=8, max.print=1000, scipen=13)

## world bank indicator for gdp percent change annual
start = 1961
end = 2016

## ## get the GDPpc contant 2010$ and Population total indicators from Development Indicators
wb.df = wb(indicator=c('NY.GDP.MKTP.KD.ZG', 'SP.POP.TOTL'), country='countries_only', start=start, end=end)


wb.df = wb.df %>%
    select(country, date, iso3c, value, indicator) %>%
    spread(key = indicator, value=value) %>%
    rename(year = date, Population = 'Population, total', ISO1AL3 = iso3c,
           GDPgrowth = 'GDP growth (annual %)') %>%
    filter(Population >= 500000) %>%
    select(-Population)


wb.world = wb(indicator=c('NY.GDP.MKTP.KD.ZG'), country='aggregates', start=start, end=end)
wb.world = wb.world %>%
    filter(country == 'World') %>%
    select(country, date, value, indicator) %>%
    spread(key = indicator, value=value) %>%
    rename(year = date, GDPgrowth = 'GDP growth (annual %)')

## get the colors right:
x <- seq(0,1, length.out=5)
colors <- c(seq_gradient_pal('red4', 'yellow')(x), seq_gradient_pal('yellow', 'green4')(x))
colors <- unique(colors)
colors[8] <- colors[9]; colors[2] <- colors[1]

ggplot(wb.world, aes(x=year, y=GDPgrowth, group=country)) +
    annotate('rect', ymin = c(-2:6), ymax = c(-1:7), xmin = -Inf, xmax = Inf,
             fill=colors, alpha=.9) +
    geom_line() + geom_point() +
    scale_x_discrete(breaks = seq(1960, 2016, 5)) +
    scale_y_continuous(breaks = seq(-2, 7)) +
    theme_minimal()



## options for map plot
themeops <- theme(plot.background = element_rect(fill = 'white'),
                  panel.background = element_rect(fill = 'white'),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.y=element_blank(), axis.ticks=element_blank(),
                  axis.title.y=element_blank(), legend.direction='vertical',
                  panel.grid=element_blank(),
                  legend.position='right',
                  ## legend.key = element_rect(colour = 'black',  size = .2, linetype='solid')
                  legend.key.height=unit(3,"line"),
                  plot.title=element_text(hjust=.5, size = 15))


library(parallel)

img <- image_graph(600, 340, res = 96)
## mclapply(start:end, mc.cores = detectCores(),
##          mc.preschedule = TRUE, function(year){
for (year in start:end){
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
    ## wb.year$GDPgrowth <- cut_interval(wb.year$GDPgrowth, 10)
    ## merge the two
    ## map.year@data <- data.frame(map.year@data, wb.year[match(map.year@data$ISO1AL3, wb.year$ISO1AL3), ])
    map.year@data <- join(map.year@data, wb.year, by='ISO1AL3', type='left')
    map.year@data$id <- map.year@data$CNTRY_NAME
    mapofyear <- fortify(model=map.year, region = 'CNTRY_NAME')
    map <- join(mapofyear, map.year@data, by='id')
    ##
    plot <- ggplot(map) + aes(long, lat, group=group, fill=GDPgrowth) +
        geom_polygon() + geom_path(color='black', lwd=.1) +
        coord_fixed(1.2) + xlim(-170, 185) + ylim(-55, 78) +
        scale_fill_gradient2(mid = 'yellow', low='red4', high = 'green4',
                             breaks=c(-5:5), limits=c(-5, 5),
                             labels=
                                 paste(c(paste('\u2264',-5), c(-4:4), paste('\u2265', 5)), '%', sep='')) +
        guides(fill=guide_colorbar(title.position="top",
                                   title = NULL,
                                   direction = "vertical")) +
        themeops + ggtitle('Annual Percent Change in GDP per capita')
    ## to get the correct dimensions, taking into account coord_fixed:
    ## (ylim[1] - ylim[2]) / (xlim[1] - xlim[2]) / coord_fixed = 2.224
    ## ggsave(filename = paste('Growth_', year, '.png', sep=''), device = 'png', width = 4 * 2.224,
    ##        height = 4,
    ##        dpi = 150)
    print(plot)
}
## )

## try out magick package
library(magick)

animation <- image_animate(img, fps=2)
image_write(animation, "growth.gif")
## gganimate(plot)

## gganimate(plot, "output.gif")




