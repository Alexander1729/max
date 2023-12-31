library(dplyr)
file2 = "C:/Users/alelen/Documents/Export Data/random_group.txt"
df_group <- read.table(file2, sep="", header=T, row.names = 1)

d1 <- subset(df_group, group %in% c("A", "B", "C"))
d2 <- subset(df_group, group == "D")

# Create DataFrame
df1 <- data.frame(
  id = c(10,11,12,13),
  name = c('sai','ram','deepika','sahithi'),
  gender = c('M','M','F','F'),
  dob = as.Date(c('1990-10-02','1981-3-24','1987-6-14','1985-8-16')),
  state = c('CA','NY','DE',NA),
  row.names=c('r1','r2','r3','r4')
)
df1

df1 %>% select(2,3)
df1 %>% select('name','gender')
df1 %>% select(-c('name','gender'))

df1$name
