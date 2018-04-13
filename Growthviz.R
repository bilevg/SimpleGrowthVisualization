library(cshapes)
library(tidyr)
library(dplyr)
library(rworldmap)
library(spdep)
library(tripack)
library(maptools)
library(wbstats)
library(ggplot2)
library(scales)
## library(gganimate)
library(gpclib)
library(grid)
library(gridExtra)
library(magick)
gpclibPermit()
options(digits=8, max.print=1000, scipen=13)

## world bank indicator for gdp percent change annual
start = 1961
end = 2016

## ## get the GDPpc contant 2010$ and Population total indicators from Development Indicators
wb.df = wb(indicator=c('NY.GDP.MKTP.KD.ZG', 'SP.POP.TOTL'), country='countries_only', start=start, end=end)

## data for map plot
wb.df = wb.df %>%
    select(country, date, iso3c, value, indicator) %>%
    spread(key = indicator, value=value) %>%
    mutate(date = as.numeric(date)) %>%
    rename(Population = 'Population, total', ISO1AL3 = iso3c,
           GDPgrowth = 'GDP growth (annual %)') %>%
    filter(Population >= 500000) %>%
    select(-Population)

## data for timeline plot
wb.world = wb(indicator=c('NY.GDP.MKTP.KD.ZG'), country='aggregates', start=start, end=end)
wb.world = wb.world %>%
    filter(country == 'World') %>%
    select(country, date, value, indicator) %>%
    spread(key = indicator, value=value) %>%
    mutate(date = as.numeric(date)) %>%
    rename(GDPgrowth = 'GDP growth (annual %)')

## color calculation for timeline plot with colored background
ncolors = 6
levels = 5
increment = (1/levels)
x <- seq(0,1, length.out=ncolors * levels)
colors <- c(seq_gradient_pal('red4', 'yellow')(x), seq_gradient_pal('yellow', 'green4')(x))
colors <- unique(colors)
## add one more on top for the >6 values
colors <- c(colors, rep(colors[length(colors)], 2))





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
                  legend.key.height=unit(2.2,"line"),
                  plot.title=element_text(hjust=.5, size = 15))


## library(parallel)

img <- image_graph(800, 600, res = 96)
## mclapply(start:end, mc.cores = detectCores(),
##          mc.preschedule = TRUE, function(year){
for (year in start:end){
    day = as.Date(paste(year, '6-30', sep='-'))
    map.year = cshp(day, useGW=TRUE)
    ## ## how many of the World Bank countries can we match? about 165 (pretty good)
    ## sum(unique(wb.df$iso3c) %in% map.all@data$ISO1AL3)
    ## drop unnecessary variables from cshape data
    map.year@data <- map.year@data %>% select(CNTRY_NAME, ISO1AL3)
    map.year@data$ISO1AL3 <- as.character(map.year@data$ISO1AL3)
    ## ## mask for countries not in World Bank data
    ## mask_missing <- map.year@data$ISO1AL3 %in% wb.df$ISO1AL3
    wb.year <- wb.df[wb.df$date == year, ]
    wb.year$GDPgrowth[wb.year$GDPgrowth > 6] <- 6
    wb.year$GDPgrowth[wb.year$GDPgrowth < -6] <- -6
    ## wb.year$GDPgrowth <- cut_interval(wb.year$GDPgrowth, 10)
    ## merge the two
    ## map.year@data <- data.frame(map.year@data, wb.year[match(map.year@data$ISO1AL3, wb.year$ISO1AL3), ])
    map.year@data <- join(map.year@data, wb.year, by='ISO1AL3', type='left')
    map.year@data$id <- map.year@data$CNTRY_NAME
    mapofyear <- fortify(model=map.year, region = 'CNTRY_NAME')
    map.df <- join(mapofyear, map.year@data, by='id')
    ##
    map <- ggplot(map.df) + aes(long, lat, group=group, fill=GDPgrowth) +
        geom_polygon() + geom_path(color='black', lwd=.1) +
        coord_fixed(1.2) + xlim(-170, 185) + ylim(-55, 78) +
        scale_fill_gradient2(mid = 'yellow', low='red4', high = 'green4',
                             breaks=seq(-6,6,2), limits=c(-6, 6),
                             labels=
                                 paste(c(paste('\u2264',-6), seq(-4,4,2), paste('\u2265', 6)), '%', sep='')) +
        guides(fill=guide_colorbar(title.position="top",
                                   title = NULL,
                                   direction = "vertical")) +
        themeops + ggtitle('Annual Percent Change in GDP per capita')
    ## to get the correct dimensions, taking into account coord_fixed:
    ## (ylim[1] - ylim[2]) / (xlim[1] - xlim[2]) / coord_fixed = 2.224
    ## ggsave(filename = paste('Growth_', year, '.png', sep=''), device = 'png', width = 4 * 2.224,
    ##        height = 4,
    ##        dpi = 150)
    ## print(plot)
    ## timeline plot
    timeline <- ggplot(wb.world[1:match(year, wb.world$date), ],
                       aes(x=date, y=GDPgrowth, group=country)) +
        annotate('rect', ymin = seq(-ncolors + increment/2, ncolors + increment/2, increment),
                 ymax = seq(-ncolors + 1, ncolors + 1, increment),
                 xmin = -Inf, xmax = Inf,
                 fill=colors) +
        geom_line() + geom_point() +
        scale_x_continuous(breaks = seq(1960, 2016, 5)) +
        scale_y_continuous(limits=c(-2, 8), breaks=seq(-2, 6, 2),
                           expand=c(0,0)) +
        expand_limits(x=2016) +
        theme_minimal() + theme(panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                axis.title.x=element_blank(),
                                axis.title.y=element_blank())
    ## combine the two plots into one and print
    ## grid.newpage()
    ## png(paste('Growth_', year, '.png', sep=''), width = 6, height = 4, units='in', res = 96)
    grid.arrange(map, timeline, ncol=1, nrow=2, heights=c(.8,.2), padding=0)
    ## map.v <- viewport(width = 1, height = .8, x = 0.5, y = 0.5)
    ## timeline.v <- viewport(width = 1, height = .2, x = 0.5, y = 0.15)
    ## save the file
    ## png(paste('Growth_', year, '.png', sep=''), width = 4 * 2.224, height = 4)
    ## print(map, vp =  viewport(layout.pos.row = 1, layout.pos.col = 1))
    ## print(timeline, vp =  viewport(layout.pos.row = 2, layout.pos.col = 1))
    ## dev.off()
}
## )

## try out magick package


animation <- image_animate(image_morph(img,  frames=56*3), loop=1, fps=10)
## animation <- image_animate(img, loop=1, fps = 1)
## image_write(animation, "growth.gif")
## gganimate(plot)

## gganimate(plot, "output.gif")



