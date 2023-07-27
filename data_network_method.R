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

df_interesting$Group <- as.character(df_interesting$Group)

df_23 <- subset(df_interesting, df_interesting$Group %in% c("  ST002", "  ST003") )
# df_23_split <- split(df_interesting, df_interesting$Group %in% c("  ST002", "  ST003") )
# df_2 <- split(df_interesting, df_interesting$Group =="ST002")
# df_4 <- split(df_interesting, df_interesting$Group)

df_intersection <- df_23 %>%
  group_by(Point) %>%
  mutate(mean=mean(d), sd=sd(d), n=n(), "sd-x"=sd(X), "sd-y"=sd(Y), "sd-z"=sd(Z))


df_intersection <- df_intersection %>%
  mutate( X= case_when(Group=="  ST002" ~ X+50e-6),
          Y= case_when(Group=="  ST002" ~ Y+30e-6),
          Z= case_when(Group=="  ST002" ~ Z-5e-6) )

View(
  df_intersection %>% 
  group_by(Point) %>%
  summarize(X)
)
View(describe(df_intersection))

fig <- plot_ly(data = df_by_collection, x = ~Point, y = 0, type='scatter', mode='markers',
               error_y=~list(array=sd, 
                             color = '#000000'))

fig
