library(terra)

ex <- c(xmin = -4.8194444444305, xmax = 8.25000000001498,
        ymin = 42.3194444444406,  ymax = 51.111111111108)

od <- setwd("............") # give the path to the directory where the ESA CCI data are store
fl <- "ESACCI-LC-L4-LCCS-Map-300m-P1Y-1992_2015-v2.0.7.tif"
r <- rast(fl)

r1 <- crop(r, ex)

## Matrix of LCC over France 1992-2022:
lcc <- values(r1) # 1992-2015
## add columns for the years 2016-2022:
lcc <- cbind(lcc, matrix(NA_integer_, nrow(lcc), 7L))

## append the LCC data 2016-2022
for (yr in 2017:2022) {
    fl <- paste0("C3S-LC-L4-LCCS-Map-300m-P1Y-", yr, "-v2.1.1.nc")
    v <- values(crop(rast(fl), ex))[, "lccs_class"]
    lcc[, yr - 1991] <- v
}

setwd(od)

colnames(lcc) <- 1992:2022
saveRDS(lcc, "lcc.rds")

## the matrix of LCCs is on the same grid than 'rmsk.fr':
rmsk.fr <- rast("rmsk_fr.tif")
prod(dim(rmsk.fr)) == nrow(lcc)

## areas of pixels for each latitude
library(geosphere)
AREA <- numeric(nrow(rmsk.fr))
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

LEGEND <- scan("................/LEGEND.txt", what = "", sep = "\n") # give the path the file
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
## sort by areas:
areas_lcc_FR2022 <- areas_lcc_FR2022[order(areas_lcc_FR2022$area_km2), ]

print(xtable(areas_lcc_FR2022), booktabs = TRUE, include.rownames = FALSE)

############################################################
### How LCC have changed in each pixel where at least one wild cat was
### observed?

## 'cells' is taken in the script script_Felis_silvestris_France_2026.R
LCCLOC <- lcc[cells, ]

## table of the LCCs where wildcats were observed between 1980 and 2025
df <- table(LCCLOC[, 31])
df <- as.data.frame(df)
df$Var1 <- LEGEND[match(df$Var1, LEGEND[, 1]), 2]
df <- df[order(df$Freq, decreasing = TRUE), ]
names(df) <- c("Land cover class (2022)", "Number of localities")
library(xtable)
print(xtable(df), booktabs = TRUE, include.rownames = FALSE, file = "tab_lcc.tex")

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


source("getNei.R")
i <- getNei2(cells, 5)
dim(i) <- NULL
i <- unique(i)
length(i) # 825162

f <- compiler::cmpfun(function(x) length(unique(x)))
res <- apply(lcc[i, ], 1, f)
table(res)
##      1      2      3      4
## 758987  64466   1687     22
round(100 * table(res) / length(res), 3)
##      1      2      3      4
## 91.980  7.813  0.204  0.003
