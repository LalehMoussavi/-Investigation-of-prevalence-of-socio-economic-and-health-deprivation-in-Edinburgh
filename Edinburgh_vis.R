setwd("~/Desktop/Visual Analytics/Assignment 2/Edinburgh")

library(kohonen)
library(ggplot2)
library(rgdal)
library(gridExtra)
library(grid)

data <- read.table(file="Edited_Data.csv", sep=",", header=TRUE)
Edinburgh_map <- readOGR(dsn="SG_SIMD_2016_EDINBURGH", layer="SG_SIMD_2016_EDINBURGH")

class(data)
class(Edinburgh_map)

names(data)
names(Edinburgh_map)

nrow(data)
nrow(Edinburgh_map)

proj4string(Edinburgh_map)

plot(Edinburgh_map)

Edinburgh_map <- spTransform(Edinburgh_map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

Edinburgh_fort <- fortify(Edinburgh_map, region= "DataZone")

Edinburgh_fort <- merge(Edinburgh_fort, data, by.x="id", by.y="Data_Zone")

ggplot(data=Edinburgh_fort, aes(x=long, y=lat, fill=Total_population, group=group)) +
  scale_fill_gradient2(low="darkred", mid="darkblue", high="green", midpoint = 4)+
  geom_polygon(colour="black")+
  coord_equal()+
  theme()

p1 <- ggplot(data=Edinburgh_fort, aes(x=long, y=lat, fill=Working_age_population_revised, group=group)) +
  scale_fill_gradient2(low="red", mid="yellow", high="green", midpoint = 50)+
  geom_polygon(colour="transparent")+
  theme(legend.position="bottom")+
  coord_equal()+
  theme()

p2 <- ggplot(data=Edinburgh_fort, aes(x=long, y=lat, fill=Income_rate, group=group)) +
  scale_fill_gradientn(colours=c("darkred", "red", "yellow", "blue", "green"))+
  geom_polygon(colour="transparent")+
  theme(legend.position="bottom")+
  coord_equal()+
  theme()

grid.arrange(p1, p2, ncol=2, 
             top=textGrob("Data Comparison Plot", vjust=2, gp=gpar(fontsize=15,font=3)))

data_train <- data[, c(11,13,15,17)]

data_train_matrix <- as.matrix(scale(data_train))

names(data_train_matrix) <- names(data_train)

som_grid <- somgrid(xdim =8, ydim=7, topo="hexagonal")  

som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=500, 
                 alpha=c(0.1,0.01), 
                 keep.data = TRUE )

plot(som_model, type = "changes")

source('coolBlueHotRed.R')

plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed)

plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed)

plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)

plot(som_model, type = "codes")

var <- 1
plot(som_model, type = "property", property = getCodes(som_model)[,var], main=colnames(getCodes(som_model))[var], palette.name=coolBlueHotRed)

mydata <- getCodes(som_model)
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)


plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")


som_cluster <- cutree(hclust(dist(getCodes(som_model))), 6)


pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')


plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters")
add.cluster.boundaries(som_model, som_cluster)


geog_names <- Edinburgh_map@data$Intermedia

geog_names[duplicated(geog_names)] <- NA

naset <- which(!is.na(geog_names))

naset <- sample(naset, length(naset)-10)
geog_names[naset] <- NA


plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters", labels=geog_names)
add.cluster.boundaries(som_model, som_cluster)


cluster_details <- data.frame(id=data$Data_Zone, cluster=som_cluster[som_model$unit.classif])


mappoints <- merge(Edinburgh_fort, cluster_details, by="id")


ggplot(data=mappoints, aes(x=long, y=lat, group=group, fill=factor(cluster))) + 
  geom_polygon(colour="transparent")  + 
  coord_equal() + 
  scale_fill_manual(values = pretty_palette) 


Edinburgh_map <- merge(Edinburgh_map, data, by.x="DataZone", by.y="Data_Zone")

Edinburgh_map <- merge(Edinburgh_map, cluster_details, by.x="DataZone", by.y="Data_Zone")

