require(ggplot2)
require(plyr)
require(reshape2)
require(gridExtra)
require(scales)
require(leaflet)


data = read.csv("/Users/franck/Downloads/AB_NYC_2019.csv/AB_NYC_2019.csv", header = TRUE)

class(data)
summary(data$neighbourhood)

########################################### Première étude ##########################
# Moyenne prix par nuit dans les districts de NY

df=as.data.frame(data)
df=ddply(data, .(neighbourhood_group), summarize, ord =mean(price))
head(df)


p <- ggplot(data = df, aes(x =neighbourhood_group, y = ord)) + geom_bar(stat="identity", fill="steelblue")
p + coord_cartesian( ylim = c(0, 250))+ xlab("District") +
  ylab("Moyenne Prix/Nuit") +
  ggtitle("Moyenne des prix par nuits dans les differents distrcits de New York")

### Prix par nuit dans le district de Brooklyn en fonction du nombre de nuit(s) minimum

df=as.data.frame(data)
dfb =df[df$neighbourhood_group=="Brooklyn",]
head(dfb)
dfb=ddply(dfb, .(minimum_nights), summarize, ord =mean(price))



p <- ggplot(data = dfb, aes(minimum_nights,y=ord)) +
  geom_point() +   geom_smooth(data=dfb, aes(x=minimum_nights, y = ord), color='red', level = 0)+
  xlab("minimum nights") +
  ylab("price/night") +
  ggtitle("Prix par nuit dans le district de Brooklyn en fonction du nombre de nuit(s) minimum")
p + coord_cartesian(xlim = c(0, 60), ylim = c(0, 250))

#########################################################################

### Prix par nuit dans le district de Manhattan en fonction du nombre de nuit(s) minimum

df=as.data.frame(data)
dfb =df[df$neighbourhood_group=="Manhattan",]
head(dfb)
dfb=ddply(dfb, .(minimum_nights), summarize, ord =mean(price))



p <- ggplot(data = dfb, aes(minimum_nights,y=ord)) +
  geom_point() +   geom_smooth(data=dfb, aes(x=minimum_nights, y = ord), color='green', level = 0)+
  xlab("minimum nights") +
  ylab("price/night") +
  ggtitle("Prix par nuit dans le district de Manhattan en fonction du nombre de nuit(s) minimum")
p + coord_cartesian(xlim = c(0, 60), ylim = c(0, 250))

###


df=as.data.frame(data)
dfb =df[df$room_type=="Private room",]
head(dfb)
dfb=ddply(dfb, .(minimum_nights), summarize, ord =mean(price))



p <- ggplot(data = dfb, aes(minimum_nights,y=ord)) +
  geom_point() +   geom_smooth(data=dfb, aes(x=minimum_nights, y = ord), color='red', level = 0)+
  xlab("minimum nights") +
  ylab("price/night") +
  ggtitle("Prix par nuit dans le district de Manhattan en fonction du nombre de nuit(s) minimum")
p + coord_cartesian(xlim = c(0, 60), ylim = c(0, 250))


#################################################################################################

###Prix par nuit dans le district de Brooklyn en fonction du nombre de nuit(s) minimum pour une chambre privée et logement entier
df=as.data.frame(data)
dfb =df[df$neighbourhood_group=="Brooklyn",]
dfb =dfb[dfb$room_type=="Private room",]
head(dfb)
dfb=ddply(dfb, .(minimum_nights), summarize, ord =mean(price))



p <- ggplot(data = dfb, aes(minimum_nights,y=ord)) +
  geom_point() +   geom_smooth(data=dfb, aes(x=minimum_nights, y = ord), color='red', level = 0)+
  xlab("Nombre de nuit minimum") +
  ylab("Prix/Nuit") +
  ggtitle("Prix par nuit dans le district de Brooklyn en fonction du nombre de nuit(s) minimum pour une chambre privée")
p + coord_cartesian(xlim = c(0, 60), ylim = c(0, 250))

##

df=as.data.frame(data)
dfb =df[df$neighbourhood_group=="Brooklyn",]
dfb =dfb[dfb$room_type=="Entire home/apt",]
head(dfb)
dfb=ddply(dfb, .(minimum_nights), summarize, ord =mean(price))



p <- ggplot(data = dfb, aes(minimum_nights,y=ord)) +
  geom_point() +   geom_smooth(data=dfb, aes(x=minimum_nights, y = ord), color='red', level = 0)+
  xlab("Nombre de nuit minimum") +
  ylab("Prix/Nuit") +
  ggtitle("Prix par nuit dans le district de Brooklyn en fonction du nombre de nuit minimum pour un appartement entier")
p + coord_cartesian(xlim = c(0, 60), ylim = c(0, 250))


#################################################################################################

###Prix par nuit dans le district de Manhattan en fonction du nombre de nuit(s) minimum pour une chambre privée et logement entier


df=as.data.frame(data)
dfb =df[df$neighbourhood_group=="Manhattan",]
dfb =dfb[dfb$room_type=="Private room",]
head(dfb)
dfb=ddply(dfb, .(minimum_nights), summarize, ord =mean(price))



p <- ggplot(data = dfb, aes(minimum_nights,y=ord)) +
  geom_point() +   geom_smooth(data=dfb, aes(x=minimum_nights, y = ord), color='green', level = 0)+
  xlab("Nombre de nuit minimum") +
  ylab("Prix/Nuit") +
  ggtitle("Prix par nuit dans le district de Manhattan en fonction du nombre de nuit minimum pour une chambre privée")
p + coord_cartesian(xlim = c(0, 60), ylim = c(0, 250))


#####
df=as.data.frame(data)
dfb =df[df$neighbourhood_group=="Manhattan",]
dfb =dfb[dfb$room_type=="Entire home/apt",]

dfb=ddply(dfb, .(minimum_nights), summarize, ord =mean(price))
head(dfb)



p <- ggplot(data = dfb, aes(minimum_nights,y=ord)) +
  geom_point() +   geom_smooth(data=dfb, aes(x=minimum_nights, y = ord), color='green', level = 0)+
  xlab("Nombre de nuit minimum") +
  ylab("Prix/Nuit") +
  ggtitle("Prix par nuit dans le district de Manhattan en fonction du nombre de nuit minimum pour un appartement entier")
p + coord_cartesian(xlim = c(0, 60), ylim = c(0, 300))


########################################      2éme étude    ########################################

#Nombre de logement entier disponible à la location pour un logement entier à Brooklyn et Manhattan en fonction du nombre de jour de réservation possible


############################  For Brooklyn  ################

df=as.data.frame(data)
dfb =df[df$neighbourhood_group=="Brooklyn",]
dfb =dfb[dfb$room_type=="Entire home/apt",]

# On enlève les logements disponible 0 jours car cela n'a pas de sens
dfb = dfb[dfb$availability_365 >0,]


# room_type)[1] : somme sur les logements entiers
dfb=ddply(dfb, .(availability_365,neighbourhood_group), summarize, ord = mean(summary(room_type)[1]))
head(dfb)


p <- ggplot(data = dfb, aes(availability_365,y=ord)) +
  geom_point() +   geom_smooth(data=dfb, aes(x=availability_365, y = ord), color='blue', level = 0)+
  xlab("Nombre de jour disponible par an") +
  ylab("Nombre de logement entier") +
  ggtitle("Nombre de logement entier disponible à la location à Brooklyn  en fonction du nombre de jour de réservation possible")

# Pas plus de 102 logements entier disponible pour cet étude --> on reduit l'échelle volontairement
p + coord_cartesian(xlim = c(1, 365), ylim = c(0, 110))


################################## For Manhattan ####################


df=as.data.frame(data)
dfb =df[df$neighbourhood_group=="Manhattan",]
dfb =dfb[dfb$room_type=="Entire home/apt",]

# On enlève les logements disponibles 0 jours car cela n'a pas de sens
dfb = dfb[dfb$availability_365 >0,]


# room_type)[1] : somme sur les logements entiers
dfb=ddply(dfb, .(availability_365,neighbourhood_group), summarize, ord = mean(summary(room_type)[1]))
head(dfb)

p <- ggplot(data = dfb, aes(availability_365,y=ord)) +
  geom_point() +   geom_smooth(data=dfb, aes(x=availability_365, y = ord), color='red', level = 0)+
  xlab("Nombre de jour disponible par an") +
  ylab("Nombre de logement entier") +
  ggtitle("Nombre de logement entier disponible à la location à Manhattan en fonction du nombre de jour de réservation possible")

# Pas plus de 128logements entier disponible pour cet étude --> on reduit l'échelle volontairement
p + coord_cartesian(xlim = c(1, 365), ylim = c(0, 128))
