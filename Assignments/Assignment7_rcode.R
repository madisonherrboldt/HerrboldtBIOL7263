### GIS in R Assignment 7
### Madison Herrboldt
### November 15th 2022

#install packages
install.packages(c("sp","rgdal","raster","rgeos","geosphere","dismo"))

#load packages
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(geosphere)
library(dismo)

#create a stack of the climate variable rasters I want to use for my climate niche
my_clim_stack <- stack(raster("WORLDCLIM_rasters/wc2.1_10m_bio_1.tif"), raster("WORLDCLIM_rasters/wc2.1_10m_bio_7.tif"),raster("WORLDCLIM_rasters/wc2.1_10m_bio_12.tif"))

#add names to the climate variables in my climate stack
names(my_clim_stack) <- c("Annual_Mean_Temperature", "Temperature_Annual_Range", "Annual_Precipitation")

#open a new plot window
dev.new()

#plot one of my climate rasters
plot(my_clim_stack[[1]]) # plot mean annual temperature

#pick out 8 locations for my climate niche and assign to my_sites
my_sites <- as.data.frame(click(n=8))

#add the headers longitude and latitude to the points I picked out
names(my_sites) <- c('longitude', 'latitude')
my_sites

#extract the climate variables (annual mean temp., temp. annual range, and annual precip.) for my 8 chosen sites
#and assign to env
env <- as.data.frame(extract(my_clim_stack, my_sites))
env

# join environmental data and your site data
my_sites <- cbind(my_sites, env)
my_sites

#get projected coordinates
myCrs <- projection(my_clim_stack) # get projection info
myCrs

#make into a shapefile
my_sites_shape <- SpatialPointsDataFrame(coords = my_sites, data = my_sites, proj4string = CRS(myCrs))

#plot shapefile with other variables and points
plot(my_clim_stack[[1]])
points(my_sites_shape, pch = 16)

#generate set of random points for comparison to our selected locations
bg <- as.data.frame(randomPoints(my_clim_stack, n = 10000))
head(bg)

#add the header names longitude and latitude
names(bg) <- c("longitude", "latitude")
head(bg)

#extract the climate variables for the random points
bgenv <- as.data.frame(extract(my_clim_stack, bg))
head(bgenv)

#combine point data and environmental data
bg <- cbind(bg, bgenv)
head(bg)

#training the model
#create a variable (pres_bg) and combine my site data with the background points
pres_bg <- c(rep(1, nrow(my_sites)), rep(0, nrow(bg)))
pres_bg
train_data <- data.frame(pres_bg = pres_bg, rbind(my_sites, bg))
head(train_data)

#making the model using a generalized linear model with pres_bg as the dependent variable 
#and my climate variables as independent variables
my_model <- glm(pres_bg ~ Annual_Mean_Temperature*Temperature_Annual_Range*Annual_Precipitation + I(Annual_Mean_Temperature^2) + 
                  I(Temperature_Annual_Range^2) + I(Annual_Precipitation^2), data = train_data, family = "binomial", 
                weights = c(rep(1, nrow(my_sites)), rep(nrow(my_sites)/nrow(bg), nrow(bg))))
summary(my_model)

#use model to predict climate niche
my_world <- predict(my_clim_stack, my_model, type = "response")
my_world

#plot my world
plot(my_world)
points(my_sites_shape, pch = 16)

#save my world
writeRaster(my_world, "my_climate_space/my_world", format = "GTiff", overwrite = TRUE, progress = "text")

#threshold my perferred climate
my_world_threshold <- my_world >= quantile(my_world, 0.75)
plot(my_world_threshold)


my_world_threshold <- calc(my_world_threshold, fun = function(x) ifelse(x==0|is.na(x), NA, 1))

my_best_sites <- randomPoints(my_world_threshold, 10000)
my_best_env <- as.data.frame(extract(my_clim_stack, my_best_sites))

#generate my worlds climate niche
smoothScatter(x=bgenv$Annual_Mean_Temperature, y=bgenv$Annual_Precipitation, col = "lightblue")
points(my_best_env$Annual_Mean_Temperature, my_best_env$Annual_Precipitation, col = "red", pch =16, cex=0.2)
points(my_sites$Annual_Mean_Temperature, my_sites$Annual_Precipitation, pch=16)
legend("topright", inset = 0.01, legend = c("world","my niche", "my locations"), pch=16, col = c("lightblue","red","black"),
       pt.cex = c(1,0.4,1))
