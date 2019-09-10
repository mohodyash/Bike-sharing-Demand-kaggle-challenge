bike <- read.csv('bikeshare.csv')

head(bike)

#data analysis
library(ggplot2)
library(dplyr)

ggplot(bike, aes(temp, count)) + geom_point(alpha=0.4)

ggplot(bike, aes(temp, count)) + geom_point(alpha=0.4, aes(color=temp)) + theme_bw()


#convert to timestamp 

bike$datetime <- as.POSIXct(bike$datetime) 

pl <- ggplot(bike, aes(datetime, count))+ geom_point(aes(color=temp),alpha=0.5)

pl + scale_color_continuous(low = '#55D8CE', high='#FF6F2E') + theme_bw()

# correlation betbwwwt temp count
cor(bike[,c('temp','count')])


ggplot(bike, aes(factor(season),count)) + geom_boxplot(aes(color=factor(season)))+ theme_bw()


# feature engennering
bike$hour <- sapply(bike$datetime, function(x){format(x,'%H')})


# it is for working day

pl <- ggplot(filter(bike,workingday==1), aes(hour, count))

pl <- pl + geom_point(position = position_jitter(w=1, h=0), aes(color=temp), alpha=0.5)

pl <- pl + geom_point(aes(color=temp))
print(pl)

pl <- pl + scale_color_gradientn(colours = c('dark blue', 'blue','light blue', 'light green', 'yellow', 'orange', 'red' ))

print(pl+ theme_bw())


# it is for non working day


pl <- ggplot(filter(bike,workingday==0), aes(hour, count))

pl <- pl + geom_point(position = position_jitter(w=1, h=0), aes(color=temp), alpha=0.5)

pl <- pl + geom_point(aes(color=temp))
print(pl)

pl <- pl + scale_color_gradientn(colours = c('dark blue', 'blue','light blue', 'light green', 'yellow', 'orange', 'red' ))

print(pl+ theme_bw())














 









