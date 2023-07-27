
library(ggplot2)
library(plotly)
#library(plyr)
library(dplyr)
library(psych)

#file1 = "C:/Users/alelen/Documents/Export Data/All_points_7.txt"
#headers = read.csv(file1, skip = 5, header = F, nrows = 1, as.is = T)
# df = read.csv(file1, skip = 7, header = F)
#colnames(df) <- headers

file1 = "C:/Users/alelen/Documents/Export Data/All_points_cleaned8.txt"
headers = read.csv(file1, header = F, nrows = 1, as.is = T)
df = read.csv(file1, skip = 1, header = F)
colnames(df) <- headers

df2 <- df %>%
  select(Group, Point, X, Y, Z) %>%
  mutate(d= sqrt(X^2+Y^2+Z^2))

df2 %>%
  group_by(Point) %>%
  summarise(mean = mean(d), sd = sd(d))

df_point <- df %>%
  select(Point, X) %>%
  group_by(Point) %>%
  summarise(mean=mean(X), sd=sd(X),)

df3 <- df2 %>%
  select(Point, d) %>%
  group_by(Point) %>%
  summarise(mean=mean(d), sd=sd(d),n=n())




p<-ggplot(df3, aes(x=Point, y=mean)) + 
  geom_point()+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                position=position_dodge(0.05))

# p

fig <- plot_ly(data = df3, x = ~Point, y = 0, type='scatter', mode='markers',
               error_y=~list(array=sd, 
                             color = '#000000'))

fig

  

describe(df3)

