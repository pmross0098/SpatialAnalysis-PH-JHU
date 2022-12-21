
library(raster)
library(sp)
library(rgdal)
library(gridExtra)
library(cowplot)

cols <- c('#8DD3C7','#FFFFB3','#BEBADA','#FB8072','#80B1D3')
fls <- c('../MaS_IntActivity/RDS-2020-0016_Washington/WA/WHP_WA.tif',
         '../MaS_IntActivity/RDS-2020-0016_Oregon/OR/WHP_OR.tif',
         '../MaS_IntActivity/RDS-2020-0016_Idaho/ID/WHP_ID.tif',
         '../MaS_IntActivity/RDS-2020-0016_Montana/MT/WHP_MT.tif',
         '../MaS_IntActivity/BC_FireThreat_Raster/bc_finerast8.tif')
states <- c('Washington','Oregon','Idaho','Montana','British Columbia')
ab_states <- c('WA','OR','ID','MT','BC')

# rast_lst <- list()
# par(mfrow = c(5, 1))
for (i in seq(1:5)) {
  rasty <- raster(fls[i])
  par(mar = c(2,2,2,2))
  raster::hist(x = rasty, main = paste0(states[i], ' Wildfire Risk'),
               col = cols[i], maxpixels = 3000000, xlab = NULL)
  dev.off()
}
# cowplot::plot_grid(plotlist = hist_lst)
# grid.arrange(hist_lst, nrow = 5)

## Create a tiled output of histograms of raster values
rast_lst <- list()
for (i in seq(1:5)) {
  rast_lst[i] <- raster(fls[i])
}
png(paste0('Raster_hist.png'), width = 5, height = 8, units = 'in', res = 100)
par(mfrow = c(5, 1), mar = c(2,2,2,2))
for (i in seq(1:5)) {
  raster::hist(x = rast_lst[[i]], main = paste0(states[i], ' Wildfire Risk'),
               col = cols[i], maxpixels = 400000, xlab = NULL)
}
dev.off()

