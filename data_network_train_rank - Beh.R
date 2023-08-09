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

fig <- plot_ly(data = df, x = ~m_distance/1000, y = df$rank, fill='Temperature', type='scatter', mode='markers')


fig <- fig %>% layout(title = 'rank % and measured distance',
                      xaxis = list(title = 'Measured distance (m) ',zeroline = TRUE),
                      yaxis = list(title = 'rank (%)'))


fig

