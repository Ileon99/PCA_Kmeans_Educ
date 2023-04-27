library(dplyr)
library(clusterSim)
library(factoextra)
library(fpc)
library(NbClust)
library(data.table)
library(tidyr)
library(magrittr)
library(tidyselect)
library(stargazer)
library(xtable)
library(feather)
library(gridExtra)
library("FactoMineR")
library(stargazer)

#load educ_indices

educ_indices2010 <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/PCA_Kmeans_Educ/Data/Cleaned Data/educ_indices2010.feather")
eikm_test2010 <-read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/PCA_Kmeans_Educ/Data/Cleaned Data/eikm_2010.feather")
treatment_group <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/PCA_Kmeans_Educ/Data/Cleaned Data/treatment_group.feather")

#To see how many clusters we should have in 2010 we use the elbows technique.

#First we will realize the clustering and than we will use panel data

##########################   Data Preparation  ##########################


#We keep only the variables that we will use 

eikm_2010 <- educ_indices2010[educ_indices2010$province_c != 90 & educ_indices2010$province_c != 20 ,
                              c("beer", "hser","hscr","beer_rur","beer_urb","hser_urb","hser_rur","educ_years","ar", "pop_per_IE","spt")]

eikm_2010 <- eikm_2010 %>%  na.omit()

#Now we standardize our data to have a mean equal to 0 and a standard deviation equal to one: 

eikm_2010 <- as.data.frame(scale(eikm_2010))

treat_check <- educ_indices2010[educ_indices2010$province_c != 90 & educ_indices2010$province_c != 20 ,
                                c("province","province_c")]

remove(educ_indices2010)

#################### K-means without PCA  ########################

#Elbows technique

#To see how many clusters we should have in 2010 we use the elbows technique.

elb_wss <- rep(0,times=10)
for (k in 1:10){
  clus <- kmeans(eikm_2010,centers=k)
  elb_wss[k] <- clus$tot.withinss
}

plot(1:10,elb_wss,type="b",xlab="Nb. of clusters",ylab="WSS")

remove(elb_wss)

#We cannot say much with the elbows technique because the WSS function seem to be quasi linear
#There is a quasi inflexion point with 4 clusters

#Therefore let's try visualizing the clusters

k2 <- kmeans(eikm_2010, centers = 2, nstart = 25)
k3 <- kmeans(eikm_2010, centers = 3, nstart = 25)
k4 <- kmeans(eikm_2010, centers = 4, nstart = 25)
k5 <- kmeans(eikm_2010, centers = 5, nstart = 25)

# plots to compare
p2 <- fviz_cluster(k2, geom = "point",  data = eikm_2010) + ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point",  data = eikm_2010) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = eikm_2010) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "point",  data = eikm_2010) + ggtitle("k = 4")


grid.arrange(p2, p3, p4, p4, nrow = 2)

#We can conclude that the clustering with k = 4 is the most coherent 
#Because we have one cluster at the center around 0 dimension 1 and quite high dimension 2
#Let's realize a PCA to understand our dimensions

#Cluster 3 would be the control group and the others the treatment group

k4$cluster

treat_check$kmeans <- k4$cluster
treat_check$kmeans <- ifelse(treat_check$kmeans== 1, 0,1)

remove(k2)
remove(k3)
remove(k4)
remove(k5)
remove(p2)
remove(p3)
remove(p4)
remove(p5)
remove(clus)
remove(k)

##################### K-means with PCA  ##########################

#K-means with high dimensions using a PCA
#First we realize a PCA with the 11 variables we have and we use the Kaiser method to choose how many dimensions we should use

out.pca<-PCA(eikm_2010,scale.unit=TRUE, ncp = 3,graph=FALSE)

print(out.pca$eig)

stargazer(out.pca$eig)

#With the Kaiser method we see that we should keep 3 dimensions

#Let's analyze the correlation cercle

jpeg("C:/Users/ignac/OneDrive/Documentos/PHD/PCA_Kmeans_Educ/Documentation/correlation_circle.jpg")

fviz_pca_var (out.pca, col.var = "red")

dev.off()


#We see which variables contribute to each dimension 

var <- get_pca_var(out.pca)

var$contrib

stargazer(var$contrib, type = "text")

#By seeing the contribution of each variable we conclude that we can keep only two dimensions to have :
#Dimension 1 : enrollment rates and student per teacher
#Dimension 2 : education years, assistance rate, population per IE

out.pca2<-PCA(eikm_2010,scale.unit=TRUE, ncp = 2,graph=FALSE)

var <- get_pca_var(out.pca2)

var$contrib

#Let's take the coordinates of the two dimensions for each province : 

ind <- get_pca_ind(out.pca2)

variables.pca <- ind$coord

#Let's realize with these variables the k-means

#Elbows technique 

elb_wss2 <- rep(0,times=10)
for (k in 1:10){
  clus <- kmeans(variables.pca,centers=k)
  elb_wss2[k] <- clus$tot.withinss
}

jpeg("C:/Users/ignac/OneDrive/Documentos/PHD/PCA_Kmeans_Educ/Documentation/elbows.jpg")

plot(1:10,elb_wss2,type="b",xlab="Nb. of clusters",ylab="WSS")

dev.off()

#Following this technique we should create 4 clusters

clustering2 <- kmeans(variables.pca, 4)


fviz_cluster(clustering2, geom="text", variables.pca)


treat_check$PCA_2dim <- clustering2$cluster
treat_check$PCA_2dim <- ifelse(treat_check$PCA_2dim== 1, 0,1)


# We take cluster 1 as the control group and groups 2, 3 and four as the treatment_group


remove(clus)
remove(clustering2)
remove(ind)
remove(ind4)
remove(out.pca)
remove(out.pca2)
remove(var)
remove(var4)
remove(variables.pca)
remove(variables.pca4)
remove(elb_wss2)
remove(elb_wss4)
remove(k)

################  K-means PCA avec trois dimensions  ###########################


out.pca3<-PCA(eikm_2010,scale.unit=TRUE, ncp = 3,graph=FALSE)

var3 <- get_pca_var(out.pca3)

var3$contrib

#Let's take the coordinates of the three dimensions for each province : 

ind3 <- get_pca_ind(out.pca3)

variables.pca3 <- ind3$coord

#Let's realize with these variables the k-means

#Elbows technique 

elb_wss3 <- rep(0,times=10)
for (k in 1:10){
  clus <- kmeans(variables.pca3,centers=k)
  elb_wss3[k] <- clus$tot.withinss
}

plot(1:10,elb_wss3,type="b",xlab="Nb. of clusters",ylab="WSS")


#Following this technique we should create 4 clusters same as with two dimensions

clustering3 <- kmeans(variables.pca3, 4)

clustering3$cluster

fviz_cluster(clustering3, geom="text", variables.pca3)

treat_check$PCA_3dim <- clustering3$cluster
treat_check$PCA_3dim <- ifelse(treat_check$PCA_3dim == 3, 0, 1)

remove(clus)
remove(clustering3)
remove(out.pca3)
remove(var3)
remove(variables.pca3)
remove(elb_wss3)
remove(k)
remove(ind3)


####################### K-means PCA avec quatre dimensions ##########################


out.pca4<-PCA(eikm_2010,scale.unit=TRUE, ncp = 4,graph=FALSE)

var4 <- get_pca_var(out.pca4)

var4$contrib

#Let's take the coordinates of the two dimensions for each province : 

ind4 <- get_pca_ind(out.pca4)

variables.pca4 <- ind4$coord

#Let's realize with these variables the k-means

#Elbows technique 

elb_wss4 <- rep(0,times=10)
for (k in 1:10){
  clus <- kmeans(variables.pca4,centers=k)
  elb_wss4[k] <- clus$tot.withinss
}

plot(1:10,elb_wss4,type="b",xlab="Nb. of clusters",ylab="WSS")


#Following this technique we should create 4 clusters same as with two dimensions

clustering4 <- kmeans(variables.pca4, 4)

clustering4$cluster

fviz_cluster(clustering4, geom="text", variables.pca4)


# With 4 dimensions is hard to understand which provinces have better education and Pichincha is isolated
# The provinces that were befor in the control group converge all to the cluster that is in the middle (0,0)

treat_check$PCA_4dim <- clustering4$cluster
treat_check$PCA_4dim <- ifelse(treat_check$PCA_4dim == 3, 0, 1)

remove(clus)
remove(clustering4)
remove(out.pca4)
remove(var4)
remove(variables.pca4)
remove(elb_wss4)
remove(k)
remove(ind4)

###################  Cluster from 2010 to 2018   ##########################

#load educ_indices 

educ_indices <- read_feather("C:/Users/ignac/OneDrive/Documentos/PHD/PCA_Kmeans_Educ/Data/Cleaned Data/educ_indices.feather")

treat_check$"2010" <- rep(0, length(treat_check$province))
treat_check$"2011" <- rep(0, length(treat_check$province))
treat_check$"2012" <- rep(0, length(treat_check$province))
treat_check$"2013" <- rep(0, length(treat_check$province))
treat_check$"2014" <- rep(0, length(treat_check$province))
treat_check$"2015" <- rep(0, length(treat_check$province))
treat_check$"2016" <- rep(0, length(treat_check$province))
treat_check$"2017" <- rep(0, length(treat_check$province))
treat_check$"2018" <- rep(0, length(treat_check$province))

i=7

for (year in 2010:2018) {
  
  #Data preparation
  
  #Taking only the data from each year for the provinces we study and the variables we study
  
  eikm_y <- educ_indices[educ_indices$province_c != 90 & educ_indices$province_c != 20 & educ_indices$year == year ,
                         c("beer", "hser","hscr","beer_rur","beer_urb","hser_urb","hser_rur","educ_years","ar", "pop_per_IE","spt")]
  
  #Avoiding NAs in our sample
  
  eikm_y <- eikm_y %>%  na.omit()
  
  #Now we standardize our data to have a mean equal to 0 and a standard deviation equal to one: 
  
  eikm_y <- as.data.frame(scale(eikm_y))
  
  #PCA
  
  out.pcay<-PCA(eikm_y,scale.unit=TRUE, ncp = 2,graph=FALSE)
  
  #Let's take the coordinates of the two dimensions for each province : 
  
  indy <- get_pca_ind(out.pcay)
  
  variables.pcay <- indy$coord
  
  #k means with k = 4
  
  clustering2y <- kmeans(variables.pcay, 4)
  
  # keeping the clustering in th dataset treat_check
  
  treat_check[,i] <- clustering2y$cluster
  
  i = i + 1
  
}


treat_check$"2010" <- ifelse(treat_check$"2010"== 4, 0, 1)
treat_check$"2011" <- ifelse(treat_check$"2011"== 2, 0, 1)
treat_check$"2012" <- ifelse(treat_check$"2012"== 2, 0, 1)
treat_check$"2013" <- ifelse(treat_check$"2013"== 4, 0, 1)
treat_check$"2014" <- ifelse(treat_check$"2014"== 3, 0, 1)
treat_check$"2015" <- ifelse(treat_check$"2015"== 4, 0, 1)
treat_check$"2016" <- ifelse(treat_check$"2016"== 2, 0, 1)
treat_check$"2017" <- ifelse(treat_check$"2017"== 4, 0, 1)
treat_check$"2018" <- ifelse(treat_check$"2018"== 3, 0, 1)


# As we find the same control group when we do not consider Chimborazo we have all the time the same clustering 
#Except for 2012-2013 when Pichincha is the only province that remains in the control group
# In the same years, Santa Elena is the only time that it is in the control group

remove(clustering2y,eikm_y)
remove(indy,out.pcay,treatment_group,variables.pcay,i,year)

####################### Robustness for 2012 and 2013 ###############

#We will do the same clustering for 2012 and 2013


treat_check$"2012bis" <- rep(0, length(treat_check$province)) 
treat_check$"2013bis" <- rep(0, length(treat_check$province))

i= 16

for (year in 2012:2013) {
  
  #Data preparation
  
  #Taking only the data from each year for the provinces we study and the variables we study
  
  eikm_y <- educ_indices[educ_indices$province_c != 90 & educ_indices$province_c != 20 & educ_indices$year == year,
                         c("beer", "hser","hscr","beer_rur","beer_urb","hser_urb","hser_rur","educ_years","ar", "pop_per_IE","spt")]
  
  #Avoiding NAs in our sample
  
  eikm_y <- eikm_y %>%  na.omit()
  
  #Now we standardize our data to have a mean equal to 0 and a standard deviation equal to one: 
  
  eikm_y <- as.data.frame(scale(eikm_y))
  
  #PCA
  
  out.pcay<-PCA(eikm_y,scale.unit=TRUE, ncp = 2,graph=FALSE)
  
  #Let's take the coordinates of the two dimensions for each province : 
  
  indy <- get_pca_ind(out.pcay)
  
  variables.pcay <- indy$coord
  
  #k means with k = 4
  
  clustering2y <- kmeans(variables.pcay, 4)
  
  # keeping the clustering in th dataset treat_check
  
  treat_check[,i] <- clustering2y$cluster
  
  i = i + 1
  
}

remove(clustering2y, eikm_y, indy, out.pcay,variables.pcay,i,year)

#When we analyze the temporal variance of our control group we get that : 

#There are some provinces that are in the control group in 2010 and are not in 2011

#Azuay : 2012-2013
#Chimborazo : is in 2010 but not in any other
#Loja : 2012-2013
#Tungurahua : 2012-2013

#Picincha is always in the control group and Santa elena is in the control group only on 2012-2013

#When we see what is happening in the clustering in 2012 and 2013, we see that 
#Pichincha and Santa elena are the only provinces in the control group, and there
#is another cluster where there are all the other control provinces, in 2012 with Guayas
#in 2013 with Napo

#There are three other provinces that appears in the control group in some years : 

#Carchi : 2011-2015

#Guayas: 2011 - 2015 - 2018 

#Imbabura : 2015 - 2016 - 2017

#Napo : 2011 - 2018

#Santa Elena: 2012-2013

# We should add one by one each province to the control group in order to check for robustness 

################################# Descriptive statistics ##############################################

# Load required packages

library(cartography)
library(sf)
library(tidyverse)
library(RColorBrewer)


setwd("C:/Users/ignac/OneDrive/Documentos/PHD/DB MINEDUC/Clean_MINEDUC/Shapefile ECU")
data <- st_read("nxprovincias.shp")

# Convert province ID to numeric and merge with treatment group data
data$province_c <- as.numeric(data$DPA_PROVIN)
data_map <- merge(treatment_group, data, by = "province_c")
data_map$treat_group[data_map$province == "GALAPAGOS"]=NA

# Convert treat_group to a factor
data_map$treat_group <- factor(data_map$treat_group)

# Convert data_map to sf object
data_map_sf <- st_as_sf(data_map)

# Define color scheme for the map
colors <- c("grey70", "grey20")
names(colors) <- levels(data_map$treat_group)

pdf("EcuadorMaps_Clusteering.pdf")

# Plot the map using ggplot2
ggplot(data_map_sf) +
  geom_sf(aes(fill = treat_group), color = "black", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "white") +
  theme_void() +
  labs(title = "Results of the Clustering",
       fill = "Treatment Group") +
  theme(plot.title = element_text(face = "bold", size = 18),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.2, color = "gray90"),
        panel.grid.minor = element_line(size = 0.2, color = "gray90"))

dev.off()

#################################################################################################

#Now we will create for each variable if it is higher than the mean equal to 1, 0 otherwise

treatment_group1 <- treatment_group

var2010 <- educ_indices2010[educ_indices2010$province_c != 90 & educ_indices2010$province_c != 20 ,
                              c("beer", "hser","hscr","beer_rur","beer_urb","hser_urb","hser_rur","educ_years","ar", "pop_per_IE","spt")]

# Iterate through each variable in the dataset
for (i in names(var2010)) {
  # Calculate the mean of the variable
  print(i)
  var_mean <- mean(var2010[[i]])
  print(var_mean)
  
  # Create a new variable corresponding to 1 if higher than the mean, 0 otherwise
  var2010[,i] <- ifelse(var2010[[i]] < var_mean, 1, 0)
  
}

var2010 <- as.data.frame(var2010)

var2010$province <- educ_indices2010$province[educ_indices2010$province_c != 90 & educ_indices2010$province_c != 20]



treatment_group1 <- merge(treatment_group, var2010, by = c("province"))



# Convert province ID to numeric and merge with treatment group data
data$province_c <- as.numeric(data$DPA_PROVIN)
data_map <- merge(treatment_group1, data, by = "province_c")
data_map$treat_group[data_map$province == "GALAPAGOS"]=NA



for (i in names(var2010)) {
  # Convert treat_group to a factor
  data_map[[i]] <- factor(data_map[[i]])
  
}



# Convert data_map to sf object
data_map_sf <- st_as_sf(data_map)

# Define color scheme for the map
colors <- c("grey70", "grey20")
names(colors) <- levels(data_map$treat_group)


# Plot the map using ggplot2 : Basic Education Enrollment Rate
p1 <- ggplot(data_map_sf) +
  geom_sf(aes(fill = beer), color = "black", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "white") +
  theme_void() +
  labs(title = "Basic Education Enrollment Rate",
       fill = "Lower to the mean") +
  theme(plot.title = element_text(face = "bold", size = 5),
        legend.title = element_text(face = "bold", size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.2, color = "gray90"),
        panel.grid.minor = element_line(size = 0.2, color = "gray90"))

# Plot the map using ggplot2 : High School Enrollment Rate
p2 <- ggplot(data_map_sf) +
  geom_sf(aes(fill = hser), color = "black", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "white") +
  theme_void() +
  labs(title = "High School Enrollment Rate",
       fill = "Lower to the mean") +
  theme(plot.title = element_text(face = "bold", size = 5),
        legend.title = element_text(face = "bold", size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.2, color = "gray90"),
        panel.grid.minor = element_line(size = 0.2, color = "gray90"))

# Plot the map using ggplot2 :Basic Education Enrollment Rate in the Urban Areas
p3 <- ggplot(data_map_sf) +
  geom_sf(aes(fill = beer_urb), color = "black", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "white") +
  theme_void() +
  labs(title = "Urban Basic Education Enrollment Rate",
       fill = "Lower to the mean") +
  theme(plot.title = element_text(face = "bold", size = 5),
        legend.title = element_text(face = "bold", size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.2, color = "gray90"),
        panel.grid.minor = element_line(size = 0.2, color = "gray90"))

# Plot the map using ggplot2 : Urban High School Enrollment Rate
p4 <- ggplot(data_map_sf) +
  geom_sf(aes(fill = hser_urb), color = "black", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "white") +
  theme_void() +
  labs(title = "Urban High School Enrollment Rate",
       fill = "Lower to the mean") +
  theme(plot.title = element_text(face = "bold", size = 5),
        legend.title = element_text(face = "bold", size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.2, color = "gray90"),
        panel.grid.minor = element_line(size = 0.2, color = "gray90"))

# Plot the map using ggplot2 :Basic Education Enrollment Rate in the Rural Areas
p5 <- ggplot(data_map_sf) +
  geom_sf(aes(fill = beer_rur), color = "black", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "white") +
  theme_void() +
  labs(title = "Rural Basic Education Enrollment Rate",
       fill = "Lower to the mean") +
  theme(plot.title = element_text(face = "bold", size = 5),
        legend.title = element_text(face = "bold", size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.2, color = "gray90"),
        panel.grid.minor = element_line(size = 0.2, color = "gray90"))


# Plot the map using ggplot2 : Rural High School Enrollment Rate
p6 <- ggplot(data_map_sf) +
  geom_sf(aes(fill = hser_rur), color = "black", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "white") +
  theme_void() +
  labs(title = "Rural High School Enrollment Rate",
       fill = "Lower to the mean") +
  theme(plot.title = element_text(face = "bold", size = 5),
        legend.title = element_text(face = "bold", size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.2, color = "gray90"),
        panel.grid.minor = element_line(size = 0.2, color = "gray90"))


# Plot the map using ggplot2 : High School Approval Rate
p7 <- ggplot(data_map_sf) +
  geom_sf(aes(fill = hscr), color = "black", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "white") +
  theme_void() +
  labs(title = "High School Approval Rate",
       fill = "Lower to the mean") +
  theme(plot.title = element_text(face = "bold", size = 5),
        legend.title = element_text(face = "bold", size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.2, color = "gray90"),
        panel.grid.minor = element_line(size = 0.2, color = "gray90"))

# Plot the map using ggplot2 : Education Years
p8<- ggplot(data_map_sf) +
  geom_sf(aes(fill = educ_years), color = "black", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "white") +
  theme_void() +
  labs(title = "Education Years",
       fill = "Lower to the mean") +
  theme(plot.title = element_text(face = "bold", size = 5),
        legend.title = element_text(face = "bold", size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.2, color = "gray90"),
        panel.grid.minor = element_line(size = 0.2, color = "gray90"))

# Plot the map using ggplot2 : Assistance Rate
p9 <- ggplot(data_map_sf) +
  geom_sf(aes(fill = ar), color = "black", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "white") +
  theme_void() +
  labs(title = "Assistance Rate",
       fill = "Lower to the mean") +
  theme(plot.title = element_text(face = "bold", size = 5),
        legend.title = element_text(face = "bold", size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.2, color = "gray90"),
        panel.grid.minor = element_line(size = 0.2, color = "gray90"))

# Plot the map using ggplot2 : Population per Institution of Education
p10 <- ggplot(data_map_sf) +
  geom_sf(aes(fill = pop_per_IE), color = "black", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "white") +
  theme_void() +
  labs(title = "Population per Institution of Education",
       fill = "Lower to the mean") +
  theme(plot.title = element_text(face = "bold", size = 5),
        legend.title = element_text(face = "bold", size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.2, color = "gray90"),
        panel.grid.minor = element_line(size = 0.2, color = "gray90"))

# Plot the map using ggplot2 : Student Per Teacher
p11 <- ggplot(data_map_sf) +
  geom_sf(aes(fill = spt), color = "black", size = 0.1) +
  scale_fill_manual(values = colors, na.value = "white") +
  theme_void() +
  labs(title = "Student Per Teacher",
       fill = "Lower to the mean") +
  theme(plot.title = element_text(face = "bold", size = 5),
        legend.title = element_text(face = "bold", size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(size = 0.2, color = "gray90"),
        panel.grid.minor = element_line(size = 0.2, color = "gray90"))

library(cowplot)

pdf("Ecuador_Maps_Vars.pdf")

plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11, nrow = 4 )

dev.off()

pdf("Ecuador_Maps_Vars1.pdf")

plot_grid(p1,p2,p3,p4, nrow = 2 )

dev.off()

pdf("Ecuador_Maps_Vars2.pdf")

plot_grid(p5,p6,p7,p8, nrow = 2 )

dev.off()

pdf("Ecuador_Maps_Vars3.pdf")

plot_grid(p9,p10,p11, nrow = 2 )

dev.off()
