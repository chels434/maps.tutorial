# =================================================================
# Creating Maps Tutorial
# =================================================================

library(rgdal)
lnd_sport <- readOGR(dsn = "data", layer = "london_sport")
str(lnd_sport)

# Check out the attributes of ldn_sport
head(lnd_sport@data, n = 2)
mean(lnd_sport$Partic_Per) # sports participation per 100 people
nrow(lnd_sport)
ncol(lnd_sport)

# Check out the geometry pf ldn_sport
plot(lnd_sport) 

plot(lnd_sport@data) # same command gives you a completely diff plot. 

# select rows of lnd_sport@data where sports participation is less than 15
lnd_sport@data[lnd_sport$Partic_Per < 15, ]
lnd_sport@data[1:2, 1:3]

# Plot zones where sports participation is greater than 25 % 
plot(lnd_sport[lnd_sport$Partic_Per > 25, ]) 
# in context of whole map: 
plot(lnd_sport) # plot the london_sport object
sel <- lnd_sport$Partic_Per > 25 # select the zones with high sports participation 
plot(lnd_sport[ sel , ], col = "blue", add = TRUE) # add selected zones to existing map

# Save as RData
save(lnd_sport, file = "sport.RData") 
rm(lnd_sport) 
load("sport.RData") 


print(lnd_sport)
names(lnd_sport) # gives column heads of ldn_sport@data
summary(lnd_sport) # gives spatial info and data summary

# =================================================================
# Part III: Manipulating spatial data
# =================================================================

# if proj info is missing and you know the CRS: 
proj4string(lnd_sport) <- CRS("+init=epsg:27700")

# shows how to search the list of available EPSG codes and create a new
# version of lnd_sport in WGS84 (A COMMON CRS system)
EPSG <- make_EPSG() # create data frame of available EPSG codes
EPSG[grepl("WGS 84$", EPSG$note), ] # search for WGS 84 code == *4326*

# code     note                                prj4
# 249 4326 # WGS 84 +proj=longlat +datum=WGS84 +no_defs

# convert the lnd_sport object into a new form, with the 
# Coordinate Reference System (CRS) specified as WGS84
lnd_sport_wgs84 <- spTransform(lnd_sport, CRS("+init=epsg:4326")) 
plot(lnd_sport_wgs84)


## Attribute joins ##
library(rgdal) # ensure rgdal is loaded
# Create new object called "lnd" from "london_sport" shapefile 
lnd <- readOGR(dsn = "data", "london_sport")
plot(lnd) # plot the lnd object
nrow(lnd) # return the number of rows

# Create and look at new crime_data object
crime_data <- read.csv("data/mps-recordedcrime-borough.csv")
# display first 3 lines
head(crime_data, 3)
# summary of crime type
summary(crime_data$CrimeType)
# Extract "Theft & Handling" crimes and save
crime_theft <- crime_data[crime_data$CrimeType == "Theft & Handling", ] 
# take a look at the result (replace 2 with 10 to see more rows)
head(crime_theft, 10) 
# Calculate the sum of the crime count for each district and save 
# result as a new object 
# ** the ~ symbol means "by" borough **
crime_ag <- aggregate(CrimeCount ~ Borough, FUN = sum, data = crime_theft)
# Show the first two rows of the aggregated crime data
head(crime_ag, 2)

# want to join crime_ag to ldn
# Compare the name column in lnd to Borough column in 
# crime_ag to see which rows match.
lnd$name %in% crime_ag$Borough
# Return rows which do not match
lnd$name[!lnd$name %in% crime_ag$Borough]

levels(crime_ag$Borough)[1:25] # names of boroughs

# Select the problematic row - the one which *does not* 
# (via the ! symbol) match 
sel <- !lnd$name %in% crime_ag$Borough 
# create selection

# Rename row 25 in crime_ag to match row 25 in lnd
# Note that we cannot rename the variable directly, 
# as it is stored as a factor.
levels(crime_ag$Borough)[25] <- as.character(lnd$name[sel])
summary(lnd$name %in% crime_ag$Borough) # now all columns match

library(dplyr)
# dataset to add to 
head(lnd$name) 
# the variables to join
head(crime_ag$Borough) 
# rename the Borough heading to name like lnd
crime_ag <- rename(crime_ag, name = Borough) 

lnd@data <- left_join(lnd@data, crime_ag)
lnd@data

# =================================================================
# CHALLENGE
# =================================================================





# =================================================================

# CLIPPING AND SPATIAL JOINS 
library(rgdal)
# create new stations object using the "lnd-stns" shapefile. 
stations <- readOGR(dsn = "data", layer = "lnd-stns")

proj4string(stations) # this is the full geographical detail. == WGS84
proj4string(lnd) # what's the coordinate reference system (CRS) == airy
bbox(stations) # the extent, 'bounding box' of stations 
bbox(lnd) # return the bounding box of the lnd object

# OSGB 1936 (or EPSG 27700) is the official CRS for the UK
# Create reprojected stations object
stations27700 <- spTransform(stations, CRSobj = CRS(proj4string(lnd)))
stations <- stations27700 # overwrite the stations object 
rm(stations27700) # remove the stations27700 object to clear up 
plot(lnd) # plot London for context (see Figure 6)
points(stations) # overlay the station points

# want to clip the points that fall inside london
?sp::over
?rgeos::gIntersects

# “output all stations within the lnd object bounds”
stations <- stations[lnd, ]
plot(stations) # test the clip succeeded 

# OR use over like:
# over(target later to be altered, source layer to clip by)
sel <- over(stations, lnd)  
sel
stations <- stations[!is.na(sel[,1]),]
plot(stations)
summary(stations)
summary(sel)


# =================================================================
# for rgeos::gIntersects : 
# library(rgeos)
# load("data/lnd.RData")
# load("data/stations.RData")
# proj4string(stations) # this is the full geographical detail. == WGS84
# proj4string(lnd) 
# int <- gIntersects(stations, lnd, byid = TRUE)  # find which stations intersect 
# class(int)  # it's outputed a matrix
# dim(int)  # with 33 rows (one for each zone) and 2532 cols (the points)
# summary(int)
# summary(int[, c(200, 500)])  # not the output of this
# plot(lnd)
# points(stations[200, ], col = "red")  # note point id 200 is outside the zones
# points(stations[500, ], col = "green")  # note point 500 is inside
# which(int[, 500] == T)  # this tells us that point 500 intersects with zone 32
# points(coordinates(lnd[32, ]), col = "black")  # test the previous statement
# 
# clipped <- apply(int == F, MARGIN = 2, all)
# plot(stations[which(clipped), ])  # shows all stations we DO NOT want
# stations.cl <- stations[which(!clipped), ]  # use ! to select the invers
# points(stations.cl, col = "green")  # check that it's worked
# 
# stations <- stations.cl
# rm(stations.cl)  # tidy up: we're only interested in clipped ones

# The first line instructs R to look at each column
# (MARGIN = 2, we would use MARGIN = 1 for row-by-row analysis) 
# and report back whether all of the values are false. 
# This creates the inverse selection that we want, hence the use 
# of ! to invert it. We test that the function works on a new object 
# (often a good idea, to avoid overwriting useful data) with plots and, 
# once content that the clip has worked, save the sample of points to 
# our main stations object and remove the now duplicated stations.cl object.
# ===================================================================


# SPATIAL AGGREGATION
stations_agg <- aggregate(x = stations["CODE"], by = lnd, FUN = length)
summary(stations_agg) # spatial object
head(stations_agg@data) # number of stations by borough


# returns a list of numbers, instead of sp object
lnd$n_points <- stations_agg$CODE
lnd$n_points
summary(lnd)

lnd_n <- aggregate(stations["NUMBER"] , by = lnd, FUN = mean)
summary(lnd_n)

q <- cut(lnd_n$NUMBER, breaks= c(quantile(lnd_n$NUMBER)), include.lowest=T) 
summary(q) # check what we've created

# Splits # of stations per borough by quantiles
greys <- paste0("grey", seq(from = 20, to = 80, by = 20)) # create grey colours 
clr <- as.character(factor(q, labels = greys)) # convert output of q's to greys 
plot(lnd_n, col = clr) # plot 
legend(legend = paste0("q", 1:4), fill = greys, "bottomleft")
areas <- sapply(lnd_n@polygons, function(x) x@area)
plot(lnd_n$NUMBER, areas)

# Plot subways and train stations.  
plot(1:10, pch=1:10)
levels(stations$LEGEND) # we want A roads and rapid transit stations (RTS) 
sel <- grepl("A Road Sing|Rapid", stations$LEGEND) # selection for plotting 
sym <- as.integer(stations$LEGEND[sel]) # symbols
plot(lnd_n, col = clr) 
points(stations[sel,], pch = sym)
legend(legend = c("A Road", "RTS"), "bottomright", pch = unique(sym))
legend(legend = paste0("q", 1:4), fill = greys, "topright")

# =================================================================
# Part IV: Map making with ggplot2
# =================================================================
library(ggplot2)

# set up a ggplot object, 
p <- ggplot(lnd@data, aes(Partic_Per, Pop_2001))

# add "geoms"
p + geom_point()
p + geom_point(colour = "red", size=2)

# scale the points by size of borough population and 
# colour them by sports participation
p + geom_point(aes(colour=Partic_Per, size=Pop_2001)) # not shown

#add text to the plot
p + geom_point(aes(colour = Partic_Per, size = Pop_2001)) + 
  geom_text(size = 2, aes(label = name))

# The “polygons” slot contains the geometry of the polygons in
# the form of the XY coordinates used to draw the polygon outline.
# For ggplot, you need to extract polygons using fortify()
library(maptools)
lnd_f <- fortify(lnd) # lost the attribute information associated with the lnd object.
head(lnd_f, n = 2)

lnd$id <- row.names(lnd) # allocate an id variable to the sp data 
head(lnd@data, n = 10) # final check before join (requires shared variable name)

#join tables by id
lnd_f <- left_join(lnd_f, lnd@data)
head(lnd_f, n = 2)
lnd_f[1:2, 1:8]



# coord_equal() is the equivalent of asp=T in regular plots with R:
# group=group points ggplot to the group column added by fortify() and it 
# identifies the groups of coordinates that pertain to individual polygons
map <- ggplot(lnd_f, aes(long, lat, group = group, fill = Partic_Per)) + 
  geom_polygon() +
  coord_equal() +
  labs(x = "Easting (m)", y = "Northing (m)",
       fill = "% Sports\nParticipation") + ggtitle("London Sports Participation")
map # base color is blue --> to change:
map + scale_fill_gradient(low = "white", high = "black")
map + scale_fill_gradient(low = "white", high = "red")

# saving as a pdf, and a larger png
ggsave("my_map.pdf")
ggsave("large_plot.png", scale = 3, dpi = 400)


# ggpmap takes maps from map servers such as Google and OpenStreetMap:
library(ggmap)

# lnd object loaded previously is in British National Grid but the ggmap 
# image tiles are in WGS84. We therefore need to use the lnd_sport_wgs84 object 
b <- bbox(lnd_sport_wgs84)
b[1, ] <- (b[1, ] - mean(b[1, ])) * 1.05 + mean(b[1, ])
b[2, ] <- (b[2, ] - mean(b[2, ])) * 1.05 + mean(b[2, ])
# scale longitude and latitude (increase bb by 5% for plot)
# replace 1.05 with 1.xx for an xx% increase in the plot size

lnd_b1 <- ggmap(get_map(location = b)) # create basemap for london with internet
lnd_wgs84_f <- fortify(lnd_sport_wgs84, region = "ons_label") 
# re-join attributes
lnd_wgs84_f <- left_join(lnd_wgs84_f, lnd_sport_wgs84@data, 
                         by = c("id" = "ons_label"))

# overlay spatial polygons over base map 
lnd_b1 +
  geom_polygon(data = lnd_wgs84_f,
               aes(x = long, y = lat, group = group, fill = Partic_Per),
               alpha = 0.5)

# for black and white, use different base map
# download basemap 
lnd_b2 <- ggmap(get_map(location = b, source = "stamen", 
                     maptype = "toner", crop = TRUE))


library(mapproj)
lnd_b2 +
  geom_polygon(data = lnd_wgs84_f,
               aes(x = long, y = lat, group = group, fill = Partic_Per), 
               alpha = 0.5)

# download basemap
lnd_b3 <- ggmap(get_map(location = b, source = "stamen", 
                        maptype = "toner", crop = TRUE, zoom = 11))

lnd_b3 +
  geom_polygon(data = lnd_wgs84_f,
               aes(x = long, y = lat, group = group, fill = Partic_Per), 
               alpha = 0.5)




# Advanced Task: Faceting for Maps

# convert the data from ‘flat’ to ‘long’ format with tidyr,
# which is the form required by ggplot2 for faceting graphics:
# the date of the population survey becomes a variable in its own right, 
# rather than being strung-out over many columns.

london_data <- read.csv("data/census-historic-population-borough.csv") 
library(tidyr) # if not install it, or skip the next two steps
ltidy <- gather(london_data, date, pop, -Area.Code, -Area.Name) 
head(london_data)
head(ltidy, 2) # check the output (not shown)

# merge population data with ldn_f
head(lnd_f, 2) # identify shared variables with ltidy
ltidy <- rename(ltidy, ons_label = Area.Code) # rename Area.code variable 
lnd_f <- left_join(lnd_f, ltidy)
head(lnd_f, 2) 

# remove the text "pop_" in the date variable
lnd_f$date <- gsub(pattern = "Pop_", replacement = "", lnd_f$date)
head(lnd_f, 2) 


ggplot(data = lnd_f, # the input data
  aes(x = long, y = lat, fill = pop/1000, group = group)) + # define variables 
  geom_polygon() + # plot the boroughs
  geom_path(colour="black", lwd=0.05) + # borough borders
  coord_equal() + # fixed x and y scales
  facet_wrap(~ date) + # one plot per time slice
  scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", # colors
    midpoint = 150, name = "Population\n(thousands)") + # legend options 
  theme(axis.text = element_blank(), # change the theme options
  axis.title = element_blank(), # remove axis titles 
  axis.ticks = element_blank()) # remove axis ticks

ggsave("figure/facet_london.png", width = 9, height = 9) # save figure





