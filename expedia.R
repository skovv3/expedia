expedia <- read.csv("C:\Users\skovv\Downloads\test.csv")
View(expedia)

str(expedia)
#table(expedia$orig_destination_distance, useNA = "ifany")
#summary(expedia$orig_destination_distance)

#data preparation : imputing missing values...
aggregate(data = expedia, orig_destination_distance ~ srch_destination_type_id, mean, na.rm = TRUE )

avg_orig_destination_distance <- ave(expedia$orig_destination_distance, expedia$srch_destination_type_id, FUN = function(x) mean(x,na.rm = TRUE))

expedia$orig_destination_distance <- ifelse(is.na(expedia$orig_destination_distance), avg_orig_destination_distance, expedia$orig_destination_distance )

summary(expedia$orig_destination_distance)

interests <- expedia[,c(3,4,5,6,7,8,9,10,11,12,15,16,17,18,19,20,21,22)]

expedia_scale <- as.data.frame(lapply(interests, scale)) #getting Error

expedia_cluster <- kmeans(expedia_scale,8)

expedia_cluster$size

library(cluster)
library(tools)
library(HSAUR)
library(fpc)

#dissExpedia <- daisy(interests)
#dE2 <- dissExpedia^2
#sk2 <- silhouette(expedia_cluster$c1, dE2)
#plot(sk2)

#plotcluster(interests, expedia_cluster$cluster)
clusplot(interests, expedia_cluster$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

