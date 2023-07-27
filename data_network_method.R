library(ggplot2)
library(plotly)
library(dplyr)
library(psych)
library(tidyverse)

file1 = "C:/Users/alelen/Documents/Export Data/All_points_cleaned8.txt"
headers = read.csv(file1, header = F, nrows = 1, as.is = T)
df_big = read.csv(file1, skip = 1, header = F)
colnames(df_big) <- headers

df_interesting <- df %>%
  select(Collection, Group, Point, X, Y, Z) %>%
  mutate(d= sqrt(X^2+Y^2+Z^2))

df_by_collection <- df_interesting %>%
  group_by(Collection, Point) %>%
  summarize(mean=mean(d), sd=sd(d), n=n())

df_split <- group_split(df_by_collection)


group_keys(df_by_collection)

df_by_collection %>%
  describe("1.5 GeV Ring OH")

df_interesting %>%
  group_by(Collection) %>%
  group_map(~ broom::tidy(lm(d, data=.x))) %>%
  bind_rows()

fig <- plot_ly(data = df_by_collection, x = ~Point, y = 0, type='scatter', mode='markers',
               error_y=~list(array=sd, 
                             color = '#000000'))

fig
