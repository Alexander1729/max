library(ggplot2)
library(plotly)
library(dplyr)
library(psych)
library(tidyverse)

library(readr)
# rank_d_r <- read_csv("Project Points/rank_d_r.csv")

file1 = "Project Points/rank_d_r.csv"
headers = read.csv(file1, header = F, nrows = 1, as.is = T)
df = read.csv(file1, skip = 1, header = F)
colnames(df) <- headers

df_bin <- df %>% 
  mutate(m_distance1000 = m_distance/1000,
         m_distance_group = cut(m_distance1000, breaks=c(0,1,2,3,4,5,6,7,8,9,10,
                                                         11,12,13,14,15,16,17,18,19,20,
                                                         21,22,23,24,25,26
         ), right = T, labels = F))

fig <- plot_ly(data = df, x=df_bin$m_distance_group, y=df_bin$rank, type='scatter')

fig <- fig %>% layout(yaxis=list(type='linear'))

fig


fig <- fig %>% layout(title = 'rank % and measured distance',
                      xaxis = list(title = 'Measured distance (m) ',zeroline = TRUE),
                      yaxis = list(title = 'rank (%)'))
df_bin <- df %>% 
  mutate(m_distance1000 = m_distance/1000,
         m_distance_group = cut(m_distance1000, breaks=c(0,1,2,3,4,5,6,7,8,9,10,
                                                         11,12,13,14,15,16,17,18,19,20,
                                                         21,22,23,24,25,26
         ), right = T, labels = F))

df_bin_group <- df_bin %>%
  group_by(m_distance_group) %>%
  summarise(rank = mean(rank))

df_bin_group2 <- df_bin %>%
  group_by(m_distance_group)

fig <- plot_ly(data = df_bin_group, 
               x=df_bin_group$m_distance_group, 
               y=df_bin_group$rank, 
               type='scatter', mode='markers')

#fig <- fig %>% layout(yaxis=list(type='linear'))
fig <- fig %>% layout(title = 'group(1m interval) -> avg(rang)',
                     xaxis = list(title = 'Measured distance (1m group) (m) ',zeroline = TRUE),
                     yaxis = list(title = 'avg(rang) %'))

fig
