library(Hmisc)
library(RColorBrewer)
library(ggplot2)
library(sp)
library(rworldmap)
library(ggmap)
library(corrplot)

#set the working directory
setwd("C:/Users/evanf/Documents/Smurfit/Capstone/Final Submission")

# my data is saved as datax
datax <- read.csv("final_dataset.csv")
colnames(datax)[1] <- "link"
names(datax)

# check that it changed
names(datax)

# check for any missing values in the data
apply(is.na(datax), 2, sum)

# check for any duplicate values
sum (is.na(duplicated(datax)))

# Remove the non-useful text rows for now
datax <- subset(datax, select = - c(link, facilities, features, description))




####DATA EXPLORATION####
dim(datax)
names(datax)
str(datax)
head(datax)
summary(datax)
describe(datax)
tail(datax)
var(datax)
datax[1:5,]

attach(datax)

write.csv(table(datax$county), file = "county_count.csv")
table(datax$ber)
table(datax$dwelling_type)
table(datax$dwelling_type_name)
table(datax$town)
table(datax$bedrooms)
table(datax$bathrooms)
table(datax$price)
table(datax$north_south)
table(datax$newCounty)


TownNumber <- table(datax$newTown)
TownNumber
TownPrice <- aggregate(price ~ newTown, mean, data = datax)
TownPrice
TownPricePerMetre <- aggregate(price_per_m2 ~ newTown, mean, data = datax)
TownPricePerMetre


write.csv(TownNumber, file = "town_number.csv")
output_town <- (cbind(table(datax$newTown), "Town2"= TownPrice$newTown, "Price" = TownPrice$price, "Town3"= TownPricePerMetre$newTown, "PricePM" = TownPricePerMetre$price_per_m2))
write.csv(output_town, file = "town_prices.csv", row.names=FALSE)

data6 <- data.frame(table(datax$newCounty))
colnames(data6)[1] <- "Postcode"
colnames(data6)[2] <- "Count"

ggplot(data6, aes(x = Postcode, y = Count)) +
  geom_bar(stat = "identity")



ggplot(data6, aes(x = reorder(Postcode, -Count), y = Count)) +
  geom_bar(stat = "identity", width=0.7, fill="steelblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Number of Properties in Dataset from Each Postcode") +
  labs(x = "Postcode")



barplot(datax$newCounty)

# Change Price column to Numeric
datax$price = as.numeric(as.character(datax$price))
datax$price_per_m2 = as.numeric(as.character(datax$price_per_m2))
datax$longitude = as.numeric(as.character(datax$longitude))
datax$latitude = as.numeric(as.character(datax$latitude))
datax$log_price_m2 <- log(datax$price_per_m2)
datax$colour <- ifelse(datax$price_per_m2<2000, "black", ifelse(datax$price_per_m2<3000,"blue",ifelse(datax$price_per_m2<4000,"green",ifelse(datax$price_per_m2<5500,"yellow",ifelse(datax$price_per_m2<7000,"orange","red")))))

datax$colour
datax$log_price_m2

CountyPrice <- aggregate(price ~ newCounty, mean, data = datax)
CountyPrice
data1 <- CountyPrice[order(CountyPrice[,2],decreasing=TRUE),]
data1
barplot(data1[,2],names.arg=data1[,1],cex.names=0.8, cey.names = 0.8, las=2, col=brewer.pal(5,"Pastel1"), main="Average Property Price By Dublin Postcode", ylim = c(0, 1200000))


CountyPricePerMetre <- aggregate(price_per_m2 ~ newCounty, mean, data = datax)
CountyPricePerMetre
data2 <- CountyPricePerMetre[order(CountyPricePerMetre[,2],decreasing=TRUE),]
data2
barplot(data2[,2],names.arg=data2[,1],cex.names=0.8,las=2, col=brewer.pal(5,"Pastel1"), main="Average Property Price Per Metre Squared By Dublin Postcode", ylab = "Price")



DwellingPrice <- aggregate(price ~ dwelling_type, mean, data = datax)
DwellingPrice
data5 <- DwellingPrice[order(DwellingPrice[,2],decreasing=TRUE),]
data5
barplot(data5[,2],names.arg=data5[,1],cex.names=0.8, las=2, col=brewer.pal(5,"Pastel1"), main="Average Price By Dwelling Type", ylim = c(0, 1200000))


DwellingPrice <- aggregate(price ~ dwelling_type, mean, data = datax)
DwellingPrice
data5 <- DwellingPrice[order(DwellingPrice[,2],decreasing=TRUE),]
data5
barplot(data5[,2],names.arg=data5[,1],cex.names=0.8, las=2, col=brewer.pal(5,"Pastel1"), main="Average Price By Dwelling Type", ylim = c(0, 1200000))

DwellingPricePM <- aggregate(price_per_m2 ~ dwelling_type, mean, data = datax)
DwellingPricePM
data10 <- DwellingPricePM[order(DwellingPricePM[,2],decreasing=TRUE),]
data10
barplot(data10[,2],names.arg=data10[,1],cex.names=0.8, las=2, col=brewer.pal(5,"Pastel1"), main="Average Price By Dwelling Type", ylim = c(0, 8000))


datax$bedrooms <- ifelse(datax$bedrooms >= 8, "8+", datax$bedrooms)
table(datax$bedrooms)
BedPrice <- aggregate(price ~ bedrooms, mean, data = datax)
BedPrice
data3 <- BedPrice[order(BedPrice[,1],decreasing=FALSE),]
data3
barplot(data3[,2],names.arg=data3[,1],cex.names=0.8,las=0, col=brewer.pal(5,"Pastel1"), main="Average Property Price By Number of Bedrooms", ylab = "Price", xlab = "Bedrooms")

datax$bathrooms <- ifelse(datax$bathrooms >= 8, "8+", datax$bathrooms)
table(datax$bathrooms)
BathPrice <- aggregate(price ~ bathrooms, mean, data = datax)
BathPrice
data4 <- BathPrice[order(BathPrice[,1],decreasing=FALSE),]
data4
barplot(data4[,2],names.arg=data4[,1],cex.names=0.8,las=0, col=brewer.pal(5,"Pastel1"), main="Average Property Price By Number of Bathrooms", ylab = "Price", xlab = "Bathrooms")

BerPrice <- aggregate(price ~ ber_new, mean, data = datax)
BerPrice
data6 <- BerPrice[order(BerPrice[,1],decreasing=FALSE),]
data6
barplot(data6[,2],names.arg=data6[,1],cex.names=0.8,las=1, col=brewer.pal(5,"Pastel1"), main="Average Property Price By BER rating", xlab = "BER", ylim = c(0, 1200000))

DividePrice <- aggregate(price ~ north_south, mean, data = datax)
DividePrice
data7 <- DividePrice[order(DividePrice[,2],decreasing=TRUE),]
data7
barplot(data7[,2],names.arg=data7[,1],cex.names=0.8, cey.names = 0.8, las=2, col=brewer.pal(5,"Pastel1"), main="Average Property Price By Side of City", ylim = c(0, 700000))


DividePricePM <- aggregate(price_per_m2 ~ north_south, mean, data = datax)
DividePricePM
data9 <- DividePricePM[order(DividePricePM[,2],decreasing=TRUE),]
data9
barplot(data9[,2],names.arg=data9[,1],cex.names=0.8, cey.names = 0.8, las=2, col=brewer.pal(5,"Pastel1"), main="Average Property Price By Side of City")


ggplot(data9)


geom_bar(aes(x=data7[,2], y=data7[,1]), fill="tan1", colour="sienna3")+
geom_line(aes(x=data9[,2], y=data9[,1]))

  
#  geom_text(aes(label=Rate, x=Year, y=Rate*max(df$Response)), colour="black")+
#  geom_text(aes(label=Response, x=Year, y=0.95*Response), colour="black")+
#  scale_y_continuous(sec.axis = sec_axis(~./max(df$Response)))


newmap <- getMap(resolution = "low")
plot(newmap, xlim = c(-6.5, -6), ylim = c(53.2, 53.5), asp = 1)
points(datax$latitude,datax$longitude , col = datax$colour,cex=0.9)
datax$latitude

dublin <- get_map("Dublin Airport,Ireland",zoom=10,maptype = "toner")

p<-ggmap(dublin)
p
p+geom_point(data=datax, aes(x = latitude, y = longitude),col=datax$colour)

# + scale_color_manual(values=c(`1`='blue', `0`='red')) + theme(legend.position = "right")
  
# geom_text(stat = 'count', aes(label = ..count..), vjust = 0)

# p+geom_density2d(data=datax,aes(x=latitude,y=longitude, fill = datax$price_per_m2),size=0.3)+stat_density2d(data=datax,aes(x=latitude,y=longitude, fill = ..level.., alpha = ..level..),size=0.1, bins = 64, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)
ggplot_build(p)

dev.off()
# Remove the non-useful text rows for now
datay <- subset(datax, select = c(price, bedrooms, bathrooms, area, newCounty, newTown, dwelling_type, ber_new, longitude, latitude, price_per_m2, north_south))


#####Correlation Plot#####
# corrplot(datay, method = "circle", na.label = "NA")
corrplot(cor(sapply(datay,as.numeric), use = "pairwise.complete.obs"),method = "number", title = "Corrplot for Daft data", mar=c(0,0,1,0))

corrplot(cor(sapply(datay,as.numeric), use = "pairwise.complete.obs"),method = "circle", title = "Corrplot for Daft data", mar=c(0,0,1,0))
