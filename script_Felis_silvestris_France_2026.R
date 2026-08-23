### Files needed
## getNei.R
## buildHabitatIndex.R
## Export_Chat_forestier_FF20260429.xlsx
## (Export_Chat_forestier_FF20260429.tab.gz is the same file but saved as text and compressed with GZIP)
## gadm_410-levels.gpkg

### R packages (in addition to recommended R packages):
## maps [CRAN]
## mapdata [CRAN]
## terra [CRAN]
## sf [CRAN]
## tigers [CRAN]
## xtable [CRAN]
## maxentcpp [CRAN]

############################################################

##library(readxl)
##DF <- read_xlsx("Export_Chat_forestier_FF20260429.xlsx")

## The file Export_Chat_forestier_FF20260429.xlsx was provided by
## Ligue pour la Protection des Oiseaux (LPO); it was exported as
## tab-delimited file for the present analyses, and then compressed
## with GZIP (3 x smaller file size).

## read the whole dataset:
DF <- read.delim("Export_Chat_forestier_FF20260429.tab.gz", dec = ",")
## check the overall structure:
str(DF)
summary(DF)

## make a copy of the original dataset since it'll be modified below
DFb <- DF

ill <- 19:18 # column index of longitude and latitude

### Simple map
library(maps)
map("france", col = "lightgrey", lwd = 0.5)
points(DF$Lon..WGS84., DF$Lat..WGS84., pch = 3, col = "#0000FF33")

### Number of observations/records per year
barplot(table(DF$Année), las = 2)
Date <- as.Date(DF$Date, "%d/%m/%Y")

## no NA except in an "empty" column:
which(sapply(DF, function(x) sum(is.na(x))) > 0)
all(is.na(DF$Vérification))

### Map observations over decadal periods:
decade <- substr(as.character(DF$Année[DF$Année > 1960]), 1, 3)

## "poor-man's animation" adding the observations by decades:
map("france", col = "grey")
at <- -8
for (d in as.character(196:202)) {
    i <- which(decade == d)
    Sys.sleep(1) # 1 sec pause
    points(DF[i, ill], pch = 3, col = "#0000FF88")
    mtext(paste0(d, "0's"), at = at, font = 2)
    at <- at + 2
}

## do the same but on separate panels (saved as PDF for the article)
pdf("map_decades.pdf", 15, 8)
layout(matrix(1:6, 2, 3, TRUE))
par(mar = rep(0, 4))
for (d in c(7:9, 0:2)) {
    map("france", col = "grey", lwd = 0.25)
    points(DF[decade == d, ill], pch = 3, cex = 0.5, col = "#0000FF88")
    from <- d*10 + 1900
    if (d < 7) from <- from + 100
    to <- from + if (from == 2020) 5 else 9
    text(-2, 50.5, paste(from, to, sep = "-"), font = 2, cex = 1.5)
}
dev.off()

### Map observations over annual periods (new map for each year):

year <- DF$Année

for (d in 1960:2026) {
    map("france", col = "grey")
    points(DF[year == d, ill], pch = 3, col = "#0000FF88")
    text(-2, 50.5, d, font = 2, cex = 1.5)
    Sys.sleep(.5)
}

### First- vs. Second-hand observations

TAB <- table(DF$Année, DF$Donnée.de.seconde.main)
## drop 2026:
Year <- as.numeric(rownames(TAB))[-nrow(TAB)]
N1st <- TAB[-nrow(TAB), 1]
N2nd <- TAB[-nrow(TAB), 2]

at <- c(1:10, seq(20, 100, 10), seq(200, 1000, 100), 2000)
lab <- as.character(at)
lab[!log10(at) %in% 0:3] <- NA_character_
lab[length(lab)] <- "2000" # keep the last label

co <- c("blue2", "green")
lgd_txt <- paste0(c("First", "Second"), "-hand observations")

pdf("N_1st_vs_2nd_hand.pdf", 8.5, 6)
par(las = 1, mar = c(4.1, 5, 1, 1))
plot(Year, N1st+1, type = "l", ylab = "Number of observations (+ 1)", col = co[1], log = "y", yaxt = "n")
points(Year, N2nd+1, type = "l", col = co[2])
axis(2, at = at, labels = lab)
legend("top", , lgd_txt, bty = "n", lty = 1, col = co)
dev.off()

### Observers and their number of observations

ID <- DFb$ID.universel.observateur
table(cut(table(ID), c(0, 1, 5, 50, 100, Inf)))
## 1322 & 1\\
## 939 & 2--5\\
## 563 & 6--50\\
## 45 & 51--100\\
## 31 & $>100$\\

## One observer made 2163 observations between 1965 and 2026 (478
## before 1980):
table(DFb[ID == 26563, ]$Année < 1980)
table(DFb[ID == 26563, ]$Année)
map("france")
points(DFb[ID == 26563, ill]) # or...:
TMP <- DFb[ID == 26563, ]
for (i in 1:2163) {
    if (i > 1 && TMP$Année[i] != TMP$Année[i - 1]) {
        rect(-3, 50, -1, 51, border = NA, col = "white")
        text(-2, 50.5, TMP$Année[i], col = "red", cex = 2, font = 2)
    }
    points(TMP[i, ill], col = "#0000FF", pch = 3)
}

lui <- table(DFb[ID == 26563, ]$Année)
lui <- c(lui[1], "1966" = 0, "1967" = 0, lui[-1])
tous <- table(DFb$Année)[names(lui)]
co <- c("#5555FF", "green")

fac <- 0.8
pdf("Observer_X.pdf", 16*fac, 9*fac)
barplot(rbind(lui, tous - lui), las = 3, col = co, ylab = "Number of observations", yaxt = "n")
axis(2, at = seq(0, 2000, 500), xpd = TRUE)
legend("top", , c('Observer "X"', "Other observers"), pch = 22, pt.bg = co, bty = "n", pt.cex = 2.5, ncol = 2)
dev.off()

### Number of cats seen on each observation

## using the original (full) dataset (DFb):
toto <- data.frame(table(DFb$Nombre))
row.names(toto) <- toto$Var1
toto$Var1 <- NULL

## using the "stripped" dataset (DFb):
titi <- data.frame(table(DF$Nombre))
row.names(titi) <- titi$Var1
titi$Var1 <- NULL
names(titi) <- "Freq_final"

## merge the two tables:
toto$Freq_final <- 0
toto[row.names(titi), 2] <- titi$Freq_final

t(toto)
round(100 * t(toto) / colSums(toto), 1) # %ages

colSums(toto) == c(nrow(DFb), nrow(DF)) # check


### Détails

## number of unique "details" as reported by the observers:
length(unique(DFb$Détails)) # 185

## how many were captured and handled:
sum(t1 <- grepl("capturé", DFb$Détails)) # 999
sum(t2 <- grepl("en main", DFb$Détails)) # 999
sum(t1 & !t2) # 0
100 * sum(t1 & t2) / nrow(DFb) # 4.249256 %

## how many with no detail:
sum(DFb$Détails == "") # 16961
100 * sum(DFb$Détails == "") / nrow(DFb) # 72.14377 %

## How many were "seen":
## (calculate the %ages below excluding the 72% "empty details")
Nd <- sum(DFb$Détails != "") # 6549
sum(vu <- grepl("vu", DFb$Détails)) # 3303
100 * sum(vu) / Nd # 50.43518 %

## How many were "heard":
sum(entendu <- grepl("entendu", DFb$Détails)) # 29
100 * sum(entendu) / Nd # 0.4428157 # 0.4428157 %

sum(entendu & vu) # 0 # no seen and heard

## how many with a status (sex and/or age) but neither seen nor heard:
pat <- "adulte|immature|jeune|mâle|femelle"
ipat <- grepl(pat, DFb$Détails)
sum(ipat & !vu & !entendu) # 2436
100 * sum(ipat & !vu & !entendu) / Nd # 37.19652 %

## how many "signs of presence":
sum(presence <- grepl("indice de présence", DFb$Détails)) # 174
100 * sum(presence) / Nd # 2.656894 %

table(vu[DFb$Détails != ""], (DFb$Année >= 1980)[DFb$Détails != ""])

a <- 1950L
o <- c()
repeat {
    b <- a + 9L
    if (a == 2020L) b <- 2026L
    s_a <- DFb$Année >= a & DFb$Année <= b
    s_d <- DFb$Détails != ""
    o <- c(o, sum((vu | entendu | ipat) & s_a), sum(s_a), sum(s_a & s_d))
    a <- b + 1L
    if (a > 2026L) break
}

o <- matrix(o, ncol = 3L, byrow = TRUE)
rownames(o) <- paste(seq(1950, 2020, 10), c(seq(1959, 2019, 10), 2026), sep = "--")
for (j in 2:3) o[, j] <- round(100* o[, 1]/o[, j], 2)

## for nice printing:
o <- as.data.frame(o)
storage.mode(o$V1) <- "integer"
print(xtable(o), booktabs = TRUE)


### Comportement (behaviour)

length(unique(DFb$Comportement)) # 24

cbind(sort(table(DFb$Comportement)),
      sort(table(DF$Comportement)))
                                                          [,1]  [,2]
Accouplement,En chasse/Se nourrit,Se déplace                 1     1
Accouplement,Se déplace,En chasse/Se nourrit,Rut, parade     1     1
En chasse/Se nourrit,Rut, parade                             1     1
En chasse/Se nourrit,Se déplace,Autre                        1     1
En chasse/Se nourrit,Se déplace,Marquage de territoire       1     1
Se déplace,En chasse/Se nourrit,Rut, parade                  1     1
Sous une plaque                                              1     1
Se déplace,Rut, parade                                       2     2
Accouplement                                                 3     3
Inconnu                                                      3     3
Se déplace,Marquage de territoire,Rut, parade                3     3
Se déplace,En chasse/Se nourrit,Marquage de territoire       4     4
Marquage de territoire,Rut, parade                           5     5
Autre                                                        6     6
En chasse/Se nourrit,Marquage de territoire                  6     6
Prédaté                                                      9     9
Rut, parade                                                 23    23
Se déplace,Marquage de territoire                           70    70
En chasse/Se nourrit,Se déplace                             83    83
Se déplace,En chasse/Se nourrit                            198   194
En chasse/Se nourrit                                       630   627
Marquage de territoire                                     875   875
Se déplace                                                3117  3103


## concordance between no reported behaviour and no reported details:
table(DFb$Comportement == "", DFb$Détails == "")
table(DFb$Comportement == "", DFb$Détails == "") / nrow(DFb)

############################################################
### Finalize data preparation

## 1) drop localities in Corsica (F. lybica)
dr <- grep("Corse", DF$Département)
length(dr) # 3
DF <- DF[-dr, ]

## 2) drop observations older than a certain date:
dw <- which(DF$Année < 1980)
length(dw) # 640
DF <- DF[-dw, ]
dim(DF) # 22867    41

tabyr <- table(DF$Année)
Year <- 1980:2025
y <- as.vector(tabyr)
log <- c("", "y")
ylab <- "Number of records"
lby <- as.character(Year)
lby[as.logical(Year %% 5)] <- ""

pdf("N_records.pdf", 10, 9)
layout(matrix(1:2, 2))
par(las = 1, mar = c(4, 4, 0.5, 0.5))
for (i in 1:2) {
    plot(Year, y, "o", xlab = "", ylab = ylab, log = log[i], xaxt = "n")
    text(1980, 2000, LETTERS[i], cex = 1.5, font = 2, adj = c(0, 1))
    ## axis(1, at = Year, labels = lby, las = 2)
    axis(1, at = seq(1980, 2025, 5))
    mtext("Year", 1, 2.5)
}
dev.off()


ch <- chull(DF[, ill])
chxy <- DF[ch, ill]
map("france", col = "grey")
points(DF[, ill], pch = 3, col = "#0000FF66")
polygon(chxy)

## create two clusters with the parallel N 43.9
map("france", col = "grey")
north <- DF$Lat..WGS84. > 43.9
i <- which(north)
j <- chull(DF[i, ill])
poly.north <- as.matrix(DF[i[j], ill])
i <- which(!north)
j <- chull(DF[i, ill])
poly.south <- as.matrix(DF[i[j], ill])

library(tigers)
## cat(deparse(ext(r1)[])) # from script_CCI.R
ex <- c(xmin = -4.8194444444305, xmax = 8.25000000001498,
        ymin = 42.3194444444406,  ymax = 51.111111111108)

## alternative to have the grid fit with the BioClim's one:
## ex <- c(xmin = -5.5, xmax = 10, ymin = 41, ymax = 51.5)

msk.north <- polygon2mask(poly.north, ex)
msk.south <- polygon2mask(poly.south, ex)
msk <- msk.north + msk.south
dim(msk) # 4705 3165
sum(msk) # 4873713
length(msk) # 14891325

library(terra)
rmsk <- rast(t(msk), extent = ex)

## mask with France's borders
od <- setwd("./GADM")
library(sf)
st_layers("gadm_410-levels.gpkg")
GADM <- read_sf("gadm_410-levels.gpkg", "ADM_0", as_tibble = FALSE)
setwd(od)

FRA <- GADM[GADM$COUNTRY == "France", "geom"]
## rasterize only the main polygon:
XY <- FRA$geom[[1]][[3]][[1]]
msk.fr <- polygon2mask(XY, ex)
mskb <- msk + msk.fr
mskb[which(mskb == 1L)] <- 0L
mskb[which(mskb == 2L)] <- 1L

rmskb <- rast(t(mskb), extent = ex)
plot(FRA, border = "grey", reset = FALSE)
plot(rmskb, add = TRUE, col = c("#00330033", "#0000FF88"))
points(DF[, ill], pch = 3, col = "#33CC00")

## writeRaster(rmskb, "rmskb.tif") # 1.3 Mo
## writeRaster(rast(t(msk.fr), ext = ex, crs = crs(rmskb)), "rmsk_fr.tif") # 1.5 Mo

rmskb <- rast("rmskb.tif")
elev <- rast("FRA_wc2.1_30s_elev.tif")
bioc <- rast("FRA_wc2.1_30s_bio.tif")

## make a new mask at 30'' resolution matching the BioClim data:
msk.fr.3 <- polygon2mask(XY, ext(elev)[], 120, backgrd = NA_integer_)
rmsk3 <- rast(t(msk.fr.3), ext = ext(elev), crs = crs(elev))
## writeRaster(rmsk3, "rmsk3.tif") # 350 ko

plot(rmskb, col = c("#00000000", "#AAAAFF"), legend = FALSE, reset = FALSE, axes = FALSE)
plot(FRA, add = TRUE)
points(DF[, ill])#, pch = 19, col = "#00000055")

## change 0->NA for the map
v <- values(rmskb)
v[v == 0] <- NA_integer_
values(rmskb) <- v

lat <- 44; l <- 200; x0 <- -4.5
f <- function(x) abs(geod(c(x0, x), rep(lat, 2))[2] - l)
x1 <- nlm(f, 2)$estimate
x1 <- x1 - x0
y <- 46.2
h <- 0.05

xli <- c(-5, 10)
yli <- c(41.4, 51)

library(mapdata)

pdf("map.pdf", 9, 8) # "_v2" & 'width = 8' if plot(FRA....
par(mar = c(3.5, 3.5, 0.5, 0.5))
#plot(FRA, reset = FALSE, lwd = 0.25)
map("worldHires", xlim = xli, ylim = yli, lwd = 0.25, border = "white", col = "grey92", fill = TRUE)
plot(rmskb, col = "#AAAAFF", legend = FALSE, axes = FALSE, add = TRUE)
##points(DFb[, ill], cex = 0.7, pch = 19, col = "#00000033")
points(DFb[, ill], cex = 0.35, pch = 3)
for (i in 0:1) axisMap(i)
rose(-3.5, 44.7, .35)#, labels = c("N", "", "", ""))
rect(x0, y, x1, y + h, col = "slategrey", border = "slategrey")
text((x0 + x1)/2, y, paste(l, "km"), adj = c(0.5, 1.5))
dev.off()

############################################################
### Data selection for CV

## number of observations per observater:
tabID <- table(DF$ID.universel.observateur)
## find those who mode a single observation (see above):
sel <- match(names(tabID == 1), DF$ID.universel.observateur)
## drop them and proceed:
DF <- DF[-sel, ]

############################################################
### Combine "Lieu.dit", "Commune" & "Département" to find unique
### localities
DUP <- duplicated(DF[, 25:27])
LOC <- DF[!DUP, ill]

cells <- cellFromXY(rmskb, LOC)
n <- length(cells) # 13332
length(unique(cells)) # 13156

## finally rarefy:
LOC <- LOC[!duplicated(cells), ]
n <- length(cells <- unique(cells))

############################################################
### Matrice des LCC sur toute la France de 1992 à 2022 (extraite des rasters)
lcc <- readRDS("../../lcc.rds")
## the matrix of LCCs is on the same grod than rmskb:
prod(dim(rmskb)) == nrow(lcc)

## areas of pixels for each latitude
library(geosphere)
AREA <- numeric(nrow(rmskb))
## north->south so the index is the row of the raster
k <- 360
res <- 1/k

l1 <- ex[4]
for (i in 1:nrow(rmskb)) {
    l0 <- l1 - res
    P <- rbind(c(0, l0), c(0, l1), c(res, l1), c(res, l0), c(0, l0))
    AREA[i] <- areaPolygon(P)
    l1 <- l0
}

## find the pixels within the borders of FR
ii <- which(values(rast(t(msk.fr), ext = ex)) == 1)
areas <- AREA[rowFromCell(rmskb, ii)]
lcc_fr_2022 <- lcc[ii, "2022"]
areas_lcc_FR2022 <- aggregate(areas, by = list(lcc_fr_2022), FUN = sum)
Npx <- table(lcc_fr_2022) # number of pixels in each LCC
areas_lcc_FR2022$x <- areas_lcc_FR2022$x / 1e6 # m^2 -> km^2
areas_lcc_FR2022$y <- 100*areas_lcc_FR2022$x / sum(areas_lcc_FR2022$x)
areas_lcc_FR2022$z <- Npx
areas_lcc_FR2022$aa <- 100*Npx/sum(Npx)

LEGEND <- scan("~/data/GIS/ESACCI/LEGEND.txt", what = "", sep = "\n")
dim(LEGEND) <- c(length(LEGEND)/3, 3)

areas_lcc_FR2022$Group.1 <- LEGEND[match(areas_lcc_FR2022$Group.1, LEGEND[, 1]), 2]
names(areas_lcc_FR2022) <- c("Land cover class", "area_km2", "area_pct", "Npx", "pct")

## add totals:
tmp <- data.frame("Total", t(colSums(areas_lcc_FR2022[, -1])))
names(tmp) <- names(areas_lcc_FR2022)
storage.mode(tmp$Npx) <- "integer"
areas_lcc_FR2022 <- rbind(areas_lcc_FR2022, tmp)

## round:
for (i in c(2, 3, 5))
    areas_lcc_FR2022[[i]] <- round(areas_lcc_FR2022[[i]], 2)

print(xtable(areas_lcc_FR2022), booktabs = TRUE, include.rownames = FALSE)


### How LCC have changed in each pixel where at least one wild cat was observed?

LCCLOC <- lcc[cells, ]
uLCCLOC <- unique(LCCLOC)
dim(uLCCLOC) # 378  31

uu <- sort(unique(as.vector(uLCCLOC)))
m <- match(uu, LEGEND[, 1])
cols <- LEGEND[m, 3]
bks <- c(0, (uu[-1] + uu[-length(uu)])/2, 1e3)
counts <- table(match(apply(LCCLOC, 1, paste, collapse = "#"),
                      apply(unique(LCCLOC), 1, paste, collapse = "#")))

pdf("histories.pdf", 10, 50)
nr <- nrow(uLCCLOC)
image(1992:2022, 1:nr, t(uLCCLOC[nr:1, ]),
      las = 1, col = cols, breaks = bks)
mtext(counts, 4, 0.1, adj = 0, at = rev(seq_along(counts)), las = 1)
dev.off()

lu <- apply(uLCCLOC, 1, function(x) length(unique(x)))
a <- aggregate(as.numeric(counts), by = list(lu), FUN = sum)
data.frame(a, "Pct" = round(100 * a$x/sum(counts), 2))
##   Group.1     x   Pct
## 1       1 12192 91.45
## 2       2  1104  8.28
## 3       3    34  0.26
## 4       4     2  0.02

ii <- which(lu == 1)
leg <- LEGEND[match(uLCCLOC[ii, 1], LEGEND[, 1]), 2]
cc <- counts[ii]
attributes(cc) <- NULL
o <- order(cc, decreasing = TRUE)

df <- data.frame(leg, cc)[o, ]
names(df) <- c("Land cover classs", "Number of localities")

library(xtable)
print(xtable(df), booktabs = TRUE, file = "tab_lcc.tex")

############################################################
### MaxEnt modelling

library(maxentcpp)

rmsk3 <- rast("rmsk3.tif") # the mask at 30'' reso.

## crop and mask the env. data to the FR borders:
elev <- crop(elev, rmsk3, mask = TRUE)
bioc <- crop(bioc, rmsk3, mask = TRUE)
NR3 <- nrow(elev)
NC3 <- ncol(elev)

## Build a raster with the LCC of the 10''-pixel at the the center of
## the 30''-pixel

## get the coordinates (lonlat) of the center of each pixel of these
## env. data rasters:

## k <- 1/120
## xx <- ext(elev)[1] + k/2 + 0:(ncol(elev) - 1) * k
## yy <- ext(elev)[3] + k/2 + 0:(nrow(elev) - 1) * k
## xy <- expand.grid(xx, rev(yy)) #!the order of arguments is important!

## ... much simpler (terra::cells() excludes cells with NA):
cells_elev <- terra::cells(elev)
xy <- xyFromCell(elev, cells_elev)
## find the positions of these in the 10''-mask:
ic <- cellFromXY(rmskb, xy)
length(ic) # 913313
length(ic) == sum(!is.na(values(elev))) # check

source("getNei.R")
source("buildHabitatIndex.R")

X <- getNei2(ic, 5) # ~9 sec, much faster is order is < 5
## X <- sapply(ic, order = 0:5, getNei) # ~1 min
if (is.matrix(X)) X <- t(X)

## the TOP-10 LCC in FR:
sel_lcc <- c(11L, 130L, 60L, 30L, 10L, 70L, 100L, 190L, 90L, 12L)

for (i in sel_lcc)
    assign(paste0("mosaic", i), buildHabitatIndex2(X, i))

## special function to have the LCC rasters fit with the WorldClim ones
## 'NR3', 'NC3', 'cells_elev' must exist in the workspace
buildLCCraster <- function(x, scale = 100) {
    v <- integer(NR3 * NC3)
    v[] <- NA_integer_
    ## if (scale) x <- scale(x)
    v[cells_elev] <- scale * x # reuse masking
    v <- matrix(v, NR3, NC3, TRUE)
    rast(v, ext = ext(elev), crs = crs(elev))
}

for (i in sel_lcc)
    assign(paste0("r_mosaic", i),
           eval(parse(text = paste0("buildLCCraster(mosaic", i, ")"))))

for (i in sel_lcc) {
    cmd <- paste0("buildLCCraster(lcc[ic, 31] == ", i,", 1)")
    assign(paste0("r_lcc", i), eval(parse(text = cmd)))
}

## workflow and comments from https://alrobles.github.io/maxentcpp/

## 1. Load environmental rasters
r_elev <- elev
r_bio1 <- bioc[[1]]
r_bio4 <- bioc[[4]]
r_bio12 <- bioc[[12]]
r_bio15 <- bioc[[15]]

FROM <- ls(pattern = "^r_")
TO <- sub("r_", "g_", FROM)
CMD <- paste(TO, "<-", "maxent_grid_from_terra(", FROM, ")")
eval(parse(text = CMD))

## 2. Occurrence and background points
info <- maxent_grid_info(g_elev)
dim <- do.call(maxent_dimension, info[1:5])
df_occ <- as.data.frame(xyFromCell(rmskb, cells))
occ <- maxent_read_occurrences(df_occ, dim, lon_col="x", lat_col="y")
bg  <- maxent_background_indices(g_elev, n = n)

## 3. Extract values & generate features
all_rows <- c(bg$rows, occ$rows)
all_cols <- c(bg$cols, occ$cols)
n_total <- length(all_rows)
sample_indices <- seq(length(bg$rows), n_total - 1L)

foo <- function(x)
    sapply(seq_along(all_rows), function(i)
        grid_get_value(x, all_rows[i], all_cols[i]))

FROM <- ls(pattern = "^g_")
TO <- paste(sub("g_", "", FROM), "vals", sep = "_")
CMD <- paste(TO, "<-", "foo(", FROM, ")")
eval(parse(text = CMD))

obj_nms <- ls(pattern = "_vals$")
##obj_nms <- obj_nms[-c(1, 2, 9, 10, 11, 12, 7, 15)] # remove some vars here
list_vars <- lapply(obj_nms, get)
var_nms <- gsub("_vals$", "", obj_nms)
names(list_vars) <- var_nms

type <- c("linear", "quadratic", "hinge")# "threshold" not nice
features <- maxent_generate_features(list_vars, types = type)

## 3bis. colinearity
tmp <- unlist(list_vars)
tmp[tmp == -9999] <- NA_real_
m <- cor(matrix(tmp, ncol = length(list_vars)), use = "p")
dimnames(m) <- list(var_nms, var_nms)
round(m, 3)
## print(xtable(m), file = "colin.tex")
##plot(hclust(as.dist(1 - m)))

## 4. Train
fs <- maxent_featured_space(n_total, sample_indices, features)
result <- maxent_fit(fs, max_iter = 2000, convergence = 1e-3)
result[1:4]

## 5. Evaluate
list_grd <- lapply(paste0("g_", var_nms), get)

pres_preds <- maxent_extract_predictions_raw(
    fs, list_grd, var_nms, occ$rows, occ$cols)
bg_preds <- maxent_extract_predictions_raw(
    fs, list_grd, var_nms, bg$rows, bg$cols)

if (anyNA(pres_preds)) pres_preds[is.na(pres_preds)] <- 0

maxent_evaluate(pres_preds, bg_preds)

pred <- maxent_project_cloglog(fs, list_grd, var_nms)
r_maxent_pred <- maxent_grid_to_terra(pred)

## 7. Variable Importance
vars_contrib <- maxent_percent_contribution(fs, var_nms)
XLAB <- var_nms
XLAB[1:5] <- c("Temperature (°C)", "Temperature seasonality",
               "Rainfall (mm)", "Rainfall seasonality",
               "Altitude (m)")
XLAB[16] <- "Cropland (%)"
XLAB[18] <- "Herbaceous (%)"
XLAB[19] <- "Tree or shrub (%)"
XLAB[21] <- "Urban areas (%)"
XLAB[23] <- "Deciduous forest (%)"
XLAB[24] <- "Needleaved forest (%)"

##system("rm -Rf plots/")
##maxent_plot_response_curves(fs, list_grd, XLAB, "./", "Felis silvestris", thumbnail = FALSE)

myplot <- function(x, xlab = "...", ylim = c(0, 1), ...) {
    plot(x, type = "l", ylim = ylim, xlab = xlab,# yaxt = "n",
         ylab = "Cloglog prediction", ...)
    ##axis(2, seq(0, 1, 0.2), c("0", "", "", "", "", "1"))
}

pdf("maxent_response_curve.pdf", 9, 8)
layout(matrix(c(1:7, 0, 8:10, 0), 4, 3))
par(mar = c(4, 4, 2, 1), las = 1)
for (i in 1:25) {
    if (!vars_contrib$contribution[i]) next
    tmp <- maxent_response_curve(fs, list_grd, XLAB, i - 1L)
    myplot(tmp, xlab = XLAB[i])#, ylim = NULL)
}
dev.off()

## 8. MESS

mess <- maxent_mess(list_grd, list_vars, var_nms)
plot(maxent_grid_to_terra(mess$mess_grid))
plot(maxent_grid_to_terra(mess$mod_grid))

############################################################
### MaxLike modelling

## the half-deviance (= minus log-likelihood) function
f <- function(par) {
    A <- drop(x %*% par) # where the cat was observed
    B <- sum(plogis(drop(z %*% par))) # for all pixels
    -sum(plogis(A, log.p = TRUE)) + n * log(B)
}

MASK <- rast("rmsk_fr.tif") # if all FR
MASK <- rast("rmskb.tif") # if restrict with chulls

## build the background pixels
iMask <- which(values(MASK) == 1) # easier to replicate
nz <- 10 * n # size of the background
rs <- sample(iMask, nz)

X <- t(getNei2(cells, 5))
Z <- t(getNei2(rs, 5))
##sel_lcc <- c(10L, 11L, 60L, 70L, 190L)
sel_lcc <- c(11L, 130L, 60L, 30L, 10L, 70L, 100L, 190L, 90L, 12L)

for (i in sel_lcc) {
    #assign(paste0("mosaic", i, "_x"), buildHabitatIndex2(X, i))
    assign(paste0("mosaic", i, "_z"), buildHabitatIndex2(Z, i))
}

## indices of the localities in the 30''-raster
ix <- cellFromXY(elev, LOC)
iz <- cellFromXY(elev, xyFromCell(MASK, rs))

elev_x <- values(elev)[ix]; elev_x[is.na(elev_x)] <- 0
bio1_x <- values(bioc[[1]])[ix]
bio4_x <- values(bioc[[4]])[ix]
bio12_x <- values(bioc[[12]])[ix]
bio15_x <- values(bioc[[15]])[ix]

elev_z <- values(elev)[iz]; elev_z[is.na(elev_z)] <- 0
bio1_z <- values(bioc[[1]])[iz]
bio4_z <- values(bioc[[4]])[iz]
bio12_z <- values(bioc[[12]])[iz]
bio15_z <- values(bioc[[15]])[iz]

## restrict to variables with positive contributions in MaxEnt
##SELS <- vars_contrib$name[vars_contrib$contribution > 0]

VARS_x <- c("bio1_x", "bio4_x", "bio12_x", "bio15_x", "elev_x",
            paste0("mosaic", sel_lcc, "_x"))
VARS_z <- gsub("_x$", "_z", VARS_x)

## or drop the 'lcc*' variables
##SELS <- grep("elev|bio", var_nms, value = TRUE)
##SELS <- c(SELS, paste0("mosaic", c(10, 11, 60, 70, 190)))

## get other environmental variables (only for 'central' pixel)
library(splines)
bar <- function(SELS) {
    f <- get("f", envir = .GlobalEnv)
    environment(f) <- environment()

    VARS_x <- paste0(SELS, "_x")
    VARS_z <- paste0(SELS, "_z")

    form_x <- paste("~", paste(VARS_x, collapse = " + "))
    ## form_x <- gsub("elev_x", "poly(elev_x, 2, raw = TRUE)", form_x)
    form_x <- gsub("elev_x", "bs(elev_x, NULL, 1000, 1)", form_x)
    form_z <- gsub("_x", "_z", form_x)
    FORMS_x <- eval(parse(text = form_x), .GlobalEnv)
    FORMS_z <- eval(parse(text = form_z), .GlobalEnv)

    x <- model.matrix(FORMS_x)
    z <- model.matrix(FORMS_z)
    np <- ncol(x) # nb. of parameters
    ctr <- list(eval.max = 1e3, iter.max = 1e3, rel.tol = 1e-1)
    ## don't use 'lower' and 'upper' in nlminb()!!!
    o <- nlminb(rep(0, np), f, control = ctr)
    2 * (o$objective + np) # AIC
}

SELS <- gsub("_x$", "", VARS_x)

AIC <- bar(SELS)

## backward procedure
repeat {
    cat("Starting AIC:", AIC, "\n")
    np_start <- length(SELS)
    kept <- 0L
    candidate <- 1L
    repeat {
        SELS_save <- SELS
        cat("Removing", SELS[candidate], "... ")
        SELS <- SELS[-candidate]
        AIC_new <- bar(SELS)
        cat("AIC =", AIC_new)
        if (AIC_new > AIC) {
            cat(" putting back")
            SELS <- SELS_save
            kept <- kept + 1L
            candidate <- candidate + 1L
        } else {
            AIC <- AIC_new
        }
        cat("\n")
        if (candidate > length(SELS)) break
    }
    if (np_start == kept) break
}

## forward procedure (not used in the paper)
aic_0 <- numeric(np_max <- length(SELS))
for (i in 1:np_max) {
    cat("Adding", SELS[i], "... ")
    aic_0[i] <- bar(SELS[i])
    cat("AIC =", aic_0[i], "\n")
}
k <- which.min(aic_0)
AIC <- aic_0[k]
cat("keeping", SELS[k], "\tAIC =", AIC, "\n")
repeat {
    added <- 0L
    for (i in 1:np_max) {
        if (i %in% k) next
        cat("Adding", SELS[i], "... ")
        j <- sort(c(k, i))
        AIC_new <- bar(SELS[j])
        cat("AIC =", AIC_new)
        if (AIC_new < AIC) {
            cat("\tkept")
            AIC <- AIC_new
            added <- added + 1L
            k <- j
        }
        cat("\n")
    }
    if (!added) break
}
cat("Final:", SELS[k], "\n")

### First set of models to test effect of 'mosaic order'
FORMS_x <- list("~ idxHab60_x", "~ idxHab130_x", "~ idxHab60130_x")

### Second set of models with 'mosaic order = 5'
FORMS_x <- list("~ idxHab60_x",
                "~ idxHab60_x + alt_x",
                "~ idxHab60_x + log10(alt_x + 1)",
                "~ idxHab60_x + log10(alt_x + 1) + bio1_x",
                "~ idxHab60_x + log10(alt_x + 1) + bio12_x",
                "~ idxHab60_x + log10(alt_x + 1) + bio1_x + bio12_x",
                "~ idxHab60_x + log10(alt_x + 1) + bio1_x + bio4_x + bio12_x",
                "~ idxHab60_x + log10(alt_x + 1) + bio1_x + bio4_x + bio12_x + bio15_x",
                "~ idxHab60_x + log10(alt_x + 1) + bio1_x + bio4_x",
                "~ idxHab60_x + log10(alt_x + 1) + bio12_x + bio15_x",
                "~ idxHab60_x + bio1_x",
                "~ idxHab60_x + bio12_x",
                "~ idxHab60_x + bio1_x + bio12_x",
                "~ idxHab60_x + bio1_x + bio4_x + bio12_x",
                "~ idxHab60_x + bio1_x + bio4_x + bio12_x + bio15_x",
                "~ idxHab60_x + bio1_x + bio4_x",
                "~ idxHab60_x + bio12_x + bio15_x")

FORMS_z <- lapply(FORMS_x, gsub, pattern = "_x", replacement = "_z")

FUN <- function(x) eval(parse(text = x), .GlobalEnv)
FORMS_x <- lapply(FORMS_x, FUN)
FORMS_z <- lapply(FORMS_z, FUN)

AIC <- numeric()
#for (ORDERS in list(0:1, 0:2, 0:3, 0:4, 0:5)) { #for 1st set of models
for (ORDERS in list(0:5)) { # for 2nd set of models

    ## the pixels of the locations + the neighbourghs (wrt order)
    ## X <- t(sapply(cells, order = ORDERS, getNei))
    X <- t(getNei2(cells, order = ORDERS))
    ##str(X)
    stopifnot(nrow(X) == n)
    denom <- ncol(X) * ncol(lcc)

    idxHab60_x <- buildHabitatIndex2(X, 60L)
    ##idxHab130_x <- buildHabitatIndex(X, 130L, denom)
    ##idxHab60130_x <- buildHabitatIndex(X, c(60L, 130L), denom)

    ##Z <- t(sapply(rs, order = ORDERS, getNei))
    Z <- t(getNei2(rs, order = ORDERS))

    idxHab60_z <- buildHabitatIndex2(Z, 60L)
    ##idxHab130_z <- buildHabitatIndex(Z, 130L, denom)
    ##idxHab60130_z <- buildHabitatIndex(Z, c(60L, 130L), denom)

    for (k in seq_along(FORMS_x)) {
        ## build the model matrices:
        x <- model.matrix(FORMS_x[[k]])
        z <- model.matrix(FORMS_z[[k]])

        np <- ncol(x) # nb. of parameters
        o <- nlminb(rep(0, ncol(x)), f, lower = rep(-50, np))
        AIC <- c(AIC, 2 * (o$objective + np)) # AIC
    }
}

## Results from first set of models:
matrix(round(AIC), 3)
       [,1]   [,2]   [,3]   [,4]   [,5]
[1,] 312845 311976 311361 310926 310557
[2,] 314083 313734 313372 313098 312853
[3,] 312704 312229 311805 311455 311198
## Results from second set of models:
Model <- sapply(FORMS_x, function(x) gsub("_x", "", as.character(x)[2]))
Model <- gsub("idxHab60", "Forest", Model)
data.frame(Model = Model, round(AIC))
                                                   Model round.AIC.
1                                                 Forest     310557
2                                           Forest + alt     309465
3                                Forest + log10(alt + 1)     309062
4                         Forest + log10(alt + 1) + bio1     308002
5                        Forest + log10(alt + 1) + bio12     308906
6                 Forest + log10(alt + 1) + bio1 + bio12     309529
7          Forest + log10(alt + 1) + bio1 + bio4 + bio12     305862
8  Forest + log10(alt + 1) + bio1 + bio4 + bio12 + bio15     305082
9                  Forest + log10(alt + 1) + bio1 + bio4     306866
10               Forest + log10(alt + 1) + bio12 + bio15     312132
11                                         Forest + bio1     307593
12                                        Forest + bio12     309491
13                                 Forest + bio1 + bio12     306523
14                          Forest + bio1 + bio4 + bio12     305898
15                  Forest + bio1 + bio4 + bio12 + bio15     308519
16                                  Forest + bio1 + bio4     306600
17                                Forest + bio12 + bio15     312159

## parameter estimates:
## set k <- 8 and rerun nlminb() above
o

profileCI <- function(o) {
    np <- length(o$par)
    res <- matrix(NA_real_, np, 2L)
    v0 <- rep(0, np)
    for (i in 1:np) {
        for (j in 1:2) {
            v <- v0
            inc <- c(-.1, .1)[j]
            v[i] <- inc
            while (abs(inc) > 1e-6) {
                repeat {
                    if (f(o$par + v) - o$objective > 1.92) {
                        v[i] <- v[i] - inc
                        break
                    }
                    v[i] <- v[i] + inc
                }
                inc <- inc / 10
            }
            res[i, j] <- v[i]
        }
    }
    res
}

CIres <- profileCI(o)

M <- cbind(o$par, o$par + CIres)
rownames(M) <- gsub("_x", "", colnames(x))
round(M, 3)

## for the null model
x <- model.matrix(~ rep(1, n) - 1)
z <- model.matrix(~ rep(1, nz) - 1)

## auc
pres_preds <- 1/(1 + exp(-drop(x %*% o$par)))
bg_preds <- 1/(1 + exp(-drop(z %*% o$par)))
## then go to script.R -> .C("auc", ...) line 71

### Predictions

## make a mask for all continental France
r3 <- rast("rmsk3.tif")
newcells <- cellFromXY(rmsk.fr, xyFromCell(r3, terra::cells(r3)))
##rmsk.fr <- rast("rmsk_fr.tif")
##newcells <- which(values(rmsk.fr) == 1)
ncells <- length(newcells)
newX <- getNei2(newcells, order = 5)
newX <- t(newX)

i <- cellFromXY(elev, xyFromCell(rmskb, newcells))
v <- values(bioc)[i, ]

new_bio4 <- v[, 4] # seasonality (SD(temperature))
new_bio12 <- v[, 12] # rainfall
new_bio15 <- v[, 15] # seasonality (SD(rainfall))
new_elev <- values(elev)[i, ] # altitude

new_mosaic11 <- buildHabitatIndex2(newX, 11L)
new_mosaic130 <- buildHabitatIndex2(newX, 130L)
new_mosaic60 <- buildHabitatIndex2(newX, 60L)
new_mosaic10 <- buildHabitatIndex2(newX, 10L)
new_mosaic70 <- buildHabitatIndex2(newX, 70L)
new_mosaic100 <- buildHabitatIndex2(newX, 100L)
new_mosaic190 <- buildHabitatIndex2(newX, 190L)
new_mosaic90 <- buildHabitatIndex2(newX, 90L)


XX <- cbind(1, new_bio4, new_bio12, new_bio15,
            bs(new_elev, NULL, 1000, 1),
            new_mosaic11, new_mosaic130, new_mosaic60, new_mosaic10,
            new_mosaic70, new_mosaic100, new_mosaic190, new_mosaic90)

beta <- o$par

predlm <- drop(XX %*%  beta)
Probhat <- 1/(1 + exp(-predlm))

## reuse the masked raster of France:
v <- values(r3)
v[!is.na(v)] <- Probhat
values(r3) <- v

##v <- values(rmsk.fr)
##v[v == 0] <- NA
##v[newcells] <- Probhat
##values(rmsk.fr) <- v

lat <- 44; l <- 200; x0 <- -4.5
x1 <- nlm(function(x) abs(geod(c(x0, x), rep(lat, 2))[2] - l), 2)$estimate
x1 <- x1 - x0
y <- 46.2
h <- 0.05

## pdf("predictions_maxent.pdf", 11, 8)
pdf("predictions.pdf", 11, 8)
par(mar = c(4, 4, 0.1, 2))
## plot FRA with type = "n" to have the correct projection
map("france", type = "n", xlim = c(-4.8, 8.25), ylim = c(42.32, 51.1))
plot(r3, axes = FALSE, add = TRUE)
##plot(rmsk.fr, axes = FALSE, add = TRUE)
for (a in 0:1) axisMap(a)
par(xpd = TRUE)
rose(-3.5, 44.7, 0.35)#, labels = c("N", "", "", ""))
rect(x0, y, x1, y + h, col = "slategrey", border = "slategrey")
text((x0 + x1)/2, y, paste(l, "km"), adj = c(0.5, 1.5))
dev.off()


makeSeq <- function(x, n = 100)
    seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n)

myhist <- function(x, ...)
    hist(x, main = "", freq = FALSE, border = "white",
         xlab = "", ylab = "", col = "#5555FF", xaxt = "n", yaxt = "n", ...)

myplot <- function(x, y, xlab = "", ...)
    plot(x, y, "l", xlab = xlab, ylab = "Predicted partial effect")

XLAB <- c("Temperature seasonality",
          "Rainfall (mm)", "Rainfall seasonality", "Altitude (m)",
          "Herbaceous cover", "Grassland", "Deciduous forest",
          "Cropland", "Needleaved forest",
          "Mosaic tree, shrub, herbaceous", "Urban areas",
          "Mixed forest")
effects <- strsplit(as.character(FORMS_x)[2], " \\+ ")[[1]]

m <- matrix(1:24, 8, 3)
##m <- rbind(m, m + 9)

pdf("partial_effects.pdf", 9, 9.5)
layout(m, heights = rep(c(.7, 1), 2))
par(las = 1)
a <- 2L # skip intercept
e <- new.env()
for (i in seq_along(effects)) {
    the_var_name <- paste0(SELS[i], "_x")
    v <- get(the_var_name)
    xx <- makeSeq(v, if (grepl("elev", the_var_name)) 100 else 2)
    assign(the_var_name, xx, envir = e)
    the_X <- eval(parse(text = effects[i]), e)
    if (!is.matrix(the_X)) dim(the_X) <- c(length(the_X), 1)
    par(mar = c(0, 4, 0, 2))
    myhist(v)

    par(mar = c(4.75, 4, 0, 2))
    b <- a + ncol(the_X) - 1L
    beta <- o$par[a:b]

    lowCI <- drop(the_X %*% (beta + CIres[a:b, 1L]))
    upCI <- drop(the_X %*% (beta + CIres[a:b, 2]))
    xy <- cbind(c(xx, rev(xx)), c(lowCI, rev(upCI)))
    plot(xy, type = "n", xlab = XLAB[i], ylab = "Partial effect")
    polygon(xy, col = "beige", border = "beige")
    lines(xx, drop(the_X %*% beta), col = "red", lwd = 2)

    ##myplot(xx, drop(the_X %*% beta), xlab = SELS[i])
    ##for (j in 1:2)
    ##    lines(xx, drop(the_X %*% (beta + CIres[a:b, j])), lty = 2)
    a <- b + 1L
}
dev.off()
