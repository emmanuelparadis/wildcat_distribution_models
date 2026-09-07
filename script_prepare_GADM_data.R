library(sf)
library(terra)
library(tigers)

ex <- c(xmin = -4.8194444444305, xmax = 8.25000000001498,
        ymin = 42.3194444444406,  ymax = 51.111111111108)

## mask with France's borders
od <- setwd("..................") # gie the path to the directory where the file below is stored
st_layers("gadm_410-levels.gpkg")
GADM <- read_sf("gadm_410-levels.gpkg", "ADM_0", as_tibble = FALSE)
setwd(od)

FRA <- GADM[GADM$COUNTRY == "France", "geom"]
## rasterize only the main polygon:
XY <- FRA$geom[[1]][[3]][[1]]
msk.fr <- polygon2mask(XY, ex)

writeRaster(rast(t(msk.fr), ext = ex, "rmsk_fr.tif") # 1.5 Mo

## make another mask at 30'' resolution matching the BioClim data:
msk.fr.3 <- polygon2mask(XY, ext(elev)[], 120, backgrd = NA_integer_)
rmsk3 <- rast(t(msk.fr.3), ext = ext(elev), crs = crs(elev))
writeRaster(rmsk3, "rmsk3.tif") # 350 ko
