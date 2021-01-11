# Data Science; Statistical analysis and data mining


# Name:    
# Chapter: 
# Course:  R: An Introduction (R01_Intro)
# Date: 

# INSTALL AND LOAD PACKAGES ################################

# install.packages("animation")
library(datasets)  # Load base packages manually

library(dplyr)
library(ggplot2)
library(pacman)

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")
# install.packages("tidyverse") #ggplot

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, psych, rio, tidyverse, plyr # summarise  nie dzia�a z plyr
               , readxl)

# dplr, ggplot2, data.table, shiny, plyr, reshape2, tidyr, stringr, lubridate, zoo, devtools, tidyverse, XML

# LOAD DATA ################################################


# WORK WITH DATA ###########################################

head(mtcars)
summary(iris)
describe(iris)
str(iris)
glimpse(iris)
typeof(mtcars)
class(mtcars)

dim(mtcars)
length(mtcars)
length(mtcars[,1])
nrow(mtcars)
ncol(mtcars)



plot(iris$Petal.Length, iris$Petal.Width,
     col = "#cc0000",  # Hex code for datalab.cc red
     pch = 19,         # Use solid circles for points
     main = "Iris: Petal Length vs. Petal Width",
     xlab = "Petal Length",
     ylab = "Petal Width")

plot(iris$Petal.Length, iris$Petal.Width)

barplot(mtcars$cyl)
barplot(table(mtcars$cyl),
        col = "#cc5395")


par(mfrow = c(1, 1))

hist(iris$Sepal.Length)
hist(mtcars$qsec [mtcars$gear==4],
     xlim = c(1,30))

hist(mtcars$wt [mtcars$gear==4],
     xlim = c(1,30),
     col="blue",
     axes = FALSE)

plot(mtcars$wt, mtcars$mpg)
plot(mtcars$wt [mtcars$gear==4], mtcars$mpg [mtcars$gear==4],
     col="red",
     pch=4)
rug(mtcars$wt, lwd=2)


hist(iris$Petal.Length[iris$Species == "versicolor"],
     main = "Petal Length: Versicolor",
     freq=F)
curve(dnorm(x, 
            mean = mean(iris$Petal.Length[iris$Species == "versicolor"]), 
            sd=sd(iris$Petal.Length[iris$Species == "versicolor"])),
      add=T)

lines(density(iris$Petal.Length[iris$Species == "versicolor"]), col="blue", lwd=2)
density(iris$Petal.Length[iris$Species == "versicolor"])
versi <- iris$Petal.Length[iris$Species == "versicolor"]
rug(versi, lwd=2)



(subset(mtcars, wt>2 & wt<4, select = c(wt, mpg) ))
transform(airquality, new = -Ozone, Temp = (Temp-32)/1.8)
mutate(airquality, new = -Ozone, Temp = (Temp-32)/1.8)
summarise(airquality, new = -Ozone, Temp = (Temp-32)/1.8)
arrange(airquality, new = -Ozone, Temp = (Temp-32)/1.8)
select(mtcars, mpg, disp, gear)
merge
join
testDF <- data.frame(v1 = c(1,3,5,7,8,3,5,NA,4,5,7,9),
                     v2 = c(11,33,55,77,88,33,55,NA,44,55,77,99) )
by1 <- c("red", "blue", 1, 2, NA, "big", 1, 2, "red", 1, NA, 12)
by2 <- c("wet", "dry", 99, 95, NA, "damp", 95, 99, "red", 99, NA, NA)
aggregate(x = testDF, by = list(by1, by2), FUN = "mean")
na.omit(data.frame(by1, by2, testDF))
aggregate(x = data.frame(by1, by2), by =c(list(testDF$v1), list(testDF$v2)), FUN = "mean")
arrange

head(iris)
i.setosa <- iris[iris$Species == "setosa", c(1:3)]
i.setosa

class(i.setosa)

vNumeric   <- c(1, 2, 3)
vCharacter <- c("a", "b", "c")
vLogical   <- c(T, F, T)
vNumeric[1:2]



10%%3
l_w=c(1:10)
l_w==5
l_w[l_w==5]
l_w[l_w>5 & l_w<9]
(rep1=rep(c(F, T),each=5))
l_w[rep1]



dfa <- cbind(vNumeric, vCharacter, vLogical)
dfa  # Matrix of one data type
is.matrix(dfa)
(matrix1 <- matrix(1:10, nrow=5, byrow = T))
dim(matrix1)[1]
m1 <- (cbind(matrix1, 1:5))
(rbind(matrix1, 14:15))
(cbind(matrix1, matrix1))
(c(matrix1, matrix1))
m1[4:5,2:3]
m1[c(rep(T, 11),F)]



df <- as.data.frame(cbind(vNumeric, vCharacter, vLogical))
df  # Makes a data frame with three different data types
df2 <- cbind.data.frame(vNumeric, vCharacter, vLogical)
df2


(typeof(as.integer(5)))
(coerce5 <- as.numeric(c("1", "2", "3")))
typeof(coerce5)

# DEFINE EXISTING VARIABLE AS FACTOR #######################

x3  <- c(1:3)
x3
(df3 <- cbind.data.frame(x3, y))
str(df3)
(df3$x3 <- factor(df3$x3,
                  labels = c("macOS", "Windows", "Linux")))
typeof(df3$x3)
df3
str(df3)

gv <- c("Male", "Female", "Female", "Male", "Male")
class(gv)
(fgv <- factor(gv))
class(fgv)
(as.data.frame(cbind(m1, gv)))

(dv <- c('evening', 'morning', 'afternoon', 'midday', 'midnight', 'evening'))
fdv <- (factor(dv, order = T))
summary(fdv)

class(mtcars[,1])
mtcars[,1]
mtcars$mpg


# Create a, b, c, d variables
a <- c(10,20,30,40)
b <- c('book', 'pen', 'textbook', 'pencil_case')
c <- c(TRUE,FALSE,TRUE,FALSE)
d <- c(2.5, 8, 10, 7)
# Join the variables to create a data frame
df <- data.frame(a,b,c,d)
df
names(df) <- c('ID', 'items', 'store', 'price')
str(df)
df[1,2]
df[,c('ID',"store")]
df$ID
quantity1 <- c(10, 35, 40, 5)
df$quantity <- quantity1
subset(df, price>5)
cbind(df, c(1:4))
df[, df$quantity>11]


df <- EuStockMarkets[1:10,]
df
PATH <-'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/prison.csv'
df <- read.csv(PATH)[1:5]
str(df)
head(import(PATH)[1:5])
head(df)




# ORDERED FACTORS AND LABELS ###############################
set.seed(1234)
df <- tibble(  
  c1 = rnorm(50, 5, 1.5),   
  c2 = rnorm(50, 5, 1.5),  
  c3 = rnorm(50, 5, 1.5),
  c4 = rnorm(50, 5, 1.5), 	
  c5 = rnorm(50, 5, 1.5)
)
# Sort by c1
df <-df[order(df$c1),]
head(df)

tcars <- as_tibble(mtcars)
head(tcars)
(tcars[order(tcars$cyl, -tcars$mpg),])
mtcars[order(mtcars$cyl, -mtcars$mpg), ]


x5  <- c(1:3)
df5 <- cbind.data.frame(x5, y)
(df5$x5 <- ordered(df5$x5,
                   levels = c(3, 1, 2),
                   labels = c("No", "Maybe", "Yes")))
df5
typeof(df5$x5)
str(df5)

(rep(1:4, each = 2, len = 10))


import1 <- import("C:/Users/Ukasz/Desktop/qwer1.csv")
head(import1)


cars <- mtcars[, c(1:4, 6, 7, 9:11)]
head(cars)

hc <- cars   %>%  # Get cars data
  dist   %>%  # Compute distance/dissimilarity matrix
  hclust      # Computer hierarchical clusters

plot(dist(cars))

hc1 <- cars %>% dist

plot(hc)          # Plot dendrogram
rect.hclust(hc, k = 2, border = "blue")


# For entire data frame ####################################
pc <- prcomp(cars,
             center = T,  # Centers means to 0 (optional)
             scale = T)   # Sets unit variance (helpful)

plot(pc)
biplot(pc)


count(mtcars, c("cyl", "gear"))

# Data analysis, data manipulation library(dplyr)

df1 <- data_frame(
  cc1 = c('A','B','C','D','F'),
  cc2 = c(5, 5, 8, 0, 9)
)
df1
typeof(df1)

df1$cc3 <- c(1:5)
as_tibble(cc3)


df_p <- tribble(~ID, ~y, "A", 5, "B", 5, "C", 8, "D", 0, "F", 9)
df_s <- tribble(
  ~ID, ~z,
  "A", 30,
  "B", 21,
  "C", 22,
  "D", 25,
  "E", 29)

df_p

inner_join(df_p, df_s, by="ID")
full_join(df_p, df_s, by="ID")

df_p <- tribble(
  ~ID, ~year, ~items,
  "A", 2015,3,
  "A", 2016,7,
  "A", 2017,6,
  "B", 2015,4,
  "B", 2016,8,
  "B", 2017,7,
  "C", 2015,4,
  "C", 2016,6,
  "C", 2017,6)
df_s <- tribble(
  ~ID, ~year, ~prices,
  "A", 2015,9,
  "A", 2016,8,
  "A", 2017,12,
  "B", 2015,13,
  "B", 2016,14,
  "B", 2017,6,
  "C", 2015,15,
  "C", 2016,15,
  "C", 2017,13)
full_join(df_p, df_s, by= c("ID", "year"))


# Data Cleaning

# Create a messy dataset
messy <- data.frame(
  country = c("A", "B", "C"),
  q1_2017 = c(0.03, 0.05, 0.01),
  q2_2017 = c(0.05, 0.07, 0.02),
  q3_22017 = c(0.04, 0.05, 0.01),
  q4_2018 = c(0.03, 0.02, 0.04))
messy

(as_tibble(messy))

# Reshape the data
tidier <- messy %>% gather(quarter, growth, q1_2017:q4_2018)
tidier
tidier2 <- pivot_longer(messy, !country, names_to="quarter", values_to="growth")
tidier2

messy_1 <- tidier %>% spread(quarter, growth)
messy_1

messy_2 <- tidier2 %>% pivot_wider(names_from = quarter, values_from = growth)
messy_2

(sep1 <- tidier %>% separate(quarter, c("Qrt","year"), sep='_'))
separate(tidier2, quarter, c("Qrt","year"), sep="_")

(unite(sep1, quarter, Qrt, year, sep="&"))



# Create origin dataframe

producers <- data.frame(   
  surname =  c("Spielberg","Scorsese","Hitchcock","Tarantino","Polanski"),    
  nationality = c("US","US","UK","US","Poland"),    
  stringsAsFactors=F)
producers

# Create destination dataframe
movies <- data.frame(    
  surname = c("Spielberg",
              "Scorsese",
              "Hitchcock",
              "Hitchcock",
              "Spielberg",
              "Tarantino",
              "Polanski"),    
  title = c("Super 8",
            "Taxi Driver",
            "Psycho",
            "North by Northwest",
            "Catch Me If You Can",
            "Reservoir Dogs",
            "Chinatown"),                
  stringsAsFactors=FALSE)

# Merge two datasets
m1 <- merge(producers, movies, by.x = "surname")
m1
dim(m1)

# Change name of ` movies ` dataframe
colnames(movies)[colnames(movies)=="surname"] <- "name"
colnames(movies)
m2 <- merge(producers, movies, by.x="surname", by.y="name")
m2
identical(m1, m2)

producers <- rbind(producers, c("Lucas", "US"))
m3 <- merge(producers, movies, by.x = "surname", by.y = "name", all.x = T)
m3


set.seed(123)
## Create the data
x = rnorm(1000)
ts <- cumsum(x)
## Stationary the serie
diff_ts <- diff(ts)
par(mfrow=c(1,2))
## Plot the series
plot(ts, type='l')
plot(diff(ts), type='l')
plot(x, type='l')

identical(x[-1], diff_ts)
round(x[-1] - diff_ts)
length(diff_ts)
dim(diff_ts)


log(exp(1)**2, base = exp(1))
log(seq(45,55, 1))


# standaryzacja
speed <- cars$speed
mean(speed)
median(speed)
sd(speed)
head(scale(speed))
(cars$speed[1]-mean(cars$speed))/sd(cars$speed)
scale(cars$speed)[1]
quantile(cars$speed)
summary(cars$speed)



x <- 10
doda� <- function(y)
{x+y}
doda�(3)
ls(environment())

# normalizacja
# Create a data frame
data_frame <- tibble(  
  c1 = rnorm(50, 5, 1.5), 
  c2 = rnorm(50, 5, 1.5),    
  c3 = rnorm(50, 5, 1.5),    
)
data_frame$c1_norm <- (data_frame$c1 - min(data_frame$c1))/(max(data_frame$c1) - min(data_frame$c1))
head(data_frame)
normalize <- function(x)
{
  return((x - min(x))/(max(x) - min(x)))
}
norm1 <- normalize(data_frame$c1)
head(norm1)

split_data <- function(df, train=T)
{
  split <- (1: round(nrow(airquality)*.8))
  if(train == T)
  {return(airquality[split, ])} 
  else 
  {return(airquality[-split, ])}
}
split_data(airquality, train=F)



# Create fruit vector
fruit <- c('Apple', 'Orange', 'Passion fruit', 'Banana')
# Create the for statement
for (i in fruit){
  print(i)
}

list <- c()
for (i in c(1:4)){
  list[i] <-  (i^2)
}
print(list)


# Create a list with three vectors
fruit <- list(
  Basket = c('Apple', 'Orange', 'Passion fruit', 'Banana'), 
  Money = c(10, 12, 15), 
  purchase = FALSE)
for (r in fruit){
  print(r)
}

mat <- matrix(data=seq(1, 12), nrow=6, ncol = 2)
for (r in 1:nrow(mat)) {
  for (c in 1:ncol(mat)) {
    print(paste("row", r, "col", c, "val =", mat[r,c]))
  }
}

i <- 1
while (i<=10){
  i <- i+1
  print(i)
}
begin <- 1
while (begin <= 10){
  cat('This is loop number', begin)
  begin <- begin+1
  print(begin)
}


# apply
m1 <- matrix(c(1:10), nrow=5, ncol=6)
am1 <- apply(m1, c(1,2), sum)

movies <- c("SPYDERMAN","BATMAN","VERTIGO","CHINATOWN")
movies_lower <- lapply(movies, tolower)
str(movies_lower)
movies_lower <- unlist(movies_lower)
movies_lower <- sapply(movies, tolower)


dt <- cars
lmn_cars <- lapply(dt, min)
smn_cars <- sapply(dt, min)

describe(dt)
dt[1]
avg <- function(x) {  
  ( min(x) + max(x) ) / 2}
fcars <- sapply(dt, avg)

below_avg <- function(x){
  avg <- mean(x)
  return(x[x<avg])
}
dt_s <- sapply(dt, below_avg)
dt_l <- lapply(dt, below_avg)
identical(dt_s,dt_l)
tapply(iris$Sepal.Width, iris$Species, median)
tapply(mtcars$mpg, mtcars$wt, mean)


#Import Data into R
PATH <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/mtcars.csv'
df <- read.csv(PATH, header = T, sep = ",")
nrow(df)
class(df$X)
df2 <- import(PATH)

readxl_example()
readxl_example("geometry.xls")
read_xlsx("C:/Users/Ukasz/Documents/R/win-library/4.0/readxl/extdata/deaths.xlsx")

example <- readxl_example("datasets.xlsx")
excel_sheets(example)
quake <- read_excel(example, sheet = "quakes")
quake2 <- as_tibble(import(example, sheet = "quakes"))
iris <- read_excel(example, n_max = 5, col_names = T)
example2 <- read_excel(example, range = "A1:B5", col_names = T)
example3 <- read_excel(example, range = cell_rows(1:5), col_names = T)
iris_row_with_header <-read_excel(example, range =cell_rows(2:3), col_names=TRUE)
iris_row_no_header <-read_excel(example, range =cell_rows(2:3),col_names =FALSE)
iris_na <- read_excel(example, na = "sethosa")
sum(is.na(iris_na))

library(haven)
PATH_sas <- 'https://github.com/guru99-edu/R-Programming/blob/master/binary.sas7bdat?raw=true'
df <- read_sas(PATH_sas)
head(df)
PATH_stata <- 'https://github.com/guru99-edu/R-Programming/blob/master/binary.dta?raw=true'
df <- read_dta(PATH_stata)
head(df)
PATH_spss <- 'https://github.com/guru99-edu/R-Programming/blob/master/binary.sav?raw=true'
df <- read_sav(PATH_spss)
head(df)

# Cleaning Data
# Replace Missing Values(NA)
mutate(airquality, Ozone=-Ozone, new= (Temp-32)/1.8) 

PATH <- "https://raw.githubusercontent.com/guru99-edu/R-Programming/master/test.csv"
df_titanic <- read.csv(PATH, sep = ",")
apply(df_titanic, 2, anyNA)
list_na <- colnames(df_titanic)[apply(df_titanic, 2, anyNA)]
df_titanic_drop <- na.omit(df_titanic)
nrow(df_titanic_drop)
# Create mean
colnames(df_titanic) %in% list_na
list_na %in% colnames(df_titanic)
avrage_missing <- apply(
  df_titanic[, colnames(df_titanic) %in% colnames(df_titanic)[apply(df_titanic,2,anyNA)]],
  2,
  mean,
  na.rm = T
  )

df_titanic_replace <- mutate(
  df_titanic,
  replace_age = ifelse(is.na(Age), avrage_missing[1], Age),
  replace_fare = ifelse(is.na(Fare), avrage_missing[2], Fare)
)
df_titanic_replace2 <- df_titanic %>%
  mutate(replace_mean_age  = ifelse(is.na(Age), avrage_missing[1], Age),
         replace_mean_fare = ifelse(is.na(Fare), avrage_missing[2], Fare))
sum(is.na(df_titanic_replace2$Age))
sum(is.na(df_titanic_replace2$replace_mean_age))
df_titanic_short <- 
  tibble(
    sapply(df_titanic,
            function(x) ifelse(is.na(x), 
                               mean(x, na.rm = T), 
                               x)
           )
  )


# Export Data
setwd("C:/Users/Ukasz/Desktop")
directory <- getwd()

df <- mtcars %>%
  select(mpg, disp, gear) %>%
  group_by(gear) %>%
  summarize(mean_mpg = mean(mpg), mean_disp = mean(disp))
write.csv(df, "df.csv")

# Run this code to create the function
open_folder <-function(dir){
    shell.exec(dir)  
}
# Call the function to open the folder
open_folder(directory)
shell.exec(directory)
write.xlsx(df, "df.xlsx")
write_sav(df, "df.sav")
write_sas(df, "df.sas7bdat")
write_dta(df, "df.dta")
save(df, file="df.RData")

install.packages("googledrive")
library(googledrive)
drive_upload("df.csv", "Nauka/Programowanie/R/")
drive_upload("df.csv", "Nauka/Programowanie/R/df.csv")
drive_upload("df.csv", "Nauka/Programowanie/R/", "df.csv")
drive_browse("Nauka/Programowanie/R/df.csv")
drive_download("Nauka/Programowanie/R/df.csv")
drive_get("df.csv")
drive_download(as_id(drive_get("df.csv")))
drive_rm("Nauka/Programowanie/R/df.csv")

# Correlation 
PATH <-"https://raw.githubusercontent.com/guru99-edu/R-Programming/master/british_household.csv"
data <- read.csv(PATH)
nrow(data)
nrow(filter(data, income<500))
data <- mutate(data, 
       log_income = log(income), 
       log_totexp = log(totexp),
       children_fac = factor(children, order = T, labels=c("No","Yes"))
       )
glimpse(data)
data1 <- mutate(mtcars, cyl_fac=factor(cyl, order=T, labels = c("No", "Yes", "Meaby")))
data <- select(as_tibble(data), -c(X, children, totexp, income))
is.data.frame(data)
is_tibble(as_tibble(data))
glimpse(data)
cor(data$log_income, data$wfood, method = "pearson")
cor(data$log_income, data$wfood, method = "spearman")
data <- data[, 1:9]
mat_1 <- as.dist(round(cor(data),2))
library("Hmisc")
data_rcorr <- as.matrix(data)
mat_2 <- rcorr(data_rcorr)
p_value <- round(mat_2[["P"]],3)
library("GGally")
ggcorr(data)
ggcorr(data,
       nbreaks = 6,
       low = "steelblue",
       mid = "white",
       high = "darkred",
       geom = "circle")  
ggcorr(data,
       nbreaks = 6,
       label = TRUE,
       label_size = 3,
       color = "grey50")
library(ggplot2)
ggpairs(data, 
        columns = c("log_totexp", "log_income", "age", "wtrans"),
        title = "Bivariate analysis of revenue expenditure by the British household",
        upper = list(continuous = wrap("cor", size = 3), mapping = aes(color = children_fac)),
        lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.1))
        )


# Aggregate
data <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/lahman-batting.csv") %>%
  select(c(playerID, yearID, AB, teamID, lgID, G, R, HR, SH)) %>%
  arrange(playerID, teamID, yearID)
glimpse(data)
summarise(data, mean_run = mean(R))
summarise(data, mean_games = mean(G), mean_SH = mean(SH, na.rm = T))
summarise(group_by(data, lgID), mean_run = mean(R)) # nie dzia�a
data %>% group_by(lgID) %>% summarise(mean_run = mean(R))
ddply(
  data,
  "lgID",
  summarise,
  mean_run = mean(R)
)

by_cyl <- group_by(mtcars, cyl)
summarise(by_cyl, disp=mean(disp), hp=mean(hp))
ddply(mtcars, "cyl" , summarise, disp=mean(disp), hp=mean(hp))
filter(by_cyl, disp==min(disp))

## Creating identification number to represent 50 individual people
ID <- c(1:50)
## Creating sex variable (25 males/25 females)
Sex <- rep(c("male", "female"), 25) # rep stands for replicate
## Creating age variable (20-39 year olds)
Age <- c(26, 25, 39, 37, 31, 34, 34, 30, 26, 33, 
         39, 28, 26, 29, 33, 22, 35, 23, 26, 36, 
         21, 20, 31, 21, 35, 39, 36, 22, 22, 25, 
         27, 30, 26, 34, 38, 39, 30, 29, 26, 25, 
         26, 36, 23, 21, 21, 39, 26, 26, 27, 21) 
## Creating a dependent variable called Score
Score <- c(0.010, 0.418, 0.014, 0.090, 0.061, 0.328, 0.656, 0.002, 0.639, 0.173, 
           0.076, 0.152, 0.467, 0.186, 0.520, 0.493, 0.388, 0.501, 0.800, 0.482, 
           0.384, 0.046, 0.920, 0.865, 0.625, 0.035, 0.501, 0.851, 0.285, 0.752, 
           0.686, 0.339, 0.710, 0.665, 0.214, 0.560, 0.287, 0.665, 0.630, 0.567, 
           0.812, 0.637, 0.772, 0.905, 0.405, 0.363, 0.773, 0.410, 0.535, 0.449)
## Creating a unified dataset that puts together all variables
data_2 <- tibble(ID, Sex, Age, Score)
ddply(
  data_2, 
  "Sex",
  summarise,
  m = mean(Score), # calculates the mean
  s = sd(Score),   # calculates the standard deviation
  n = length(Score))     # calculates the total number of observations

library(ggplot2)
ggplot(
  ddply(data, "lgID", summarise, mean_games=length(HR)),
  aes(x = lgID, y = mean_games, fill = lgID)
) +
geom_bar(stat = "identity")+
theme_classic() +
labs(
  x = "baseball league",
  y = "Average home run",
  title = paste(
    "Example group_by() with summarise()"
  )
)

ex1 <- as.tibble(
ddply(
  data, "yearID", summarise, mean_game_year = mean(G)
))
head(ex1)
ggplot(
  ex1,
  aes(x = yearID, y = mean_game_year)
  )+
  geom_line()+
  theme_classic()+
  labs(
    x = "Year",
    y = "Average games played",
    title = paste(
      "Average games played from 1871 to 2016"
    )
  )

ddply(
  data,
  "lgID",
  summarise,
  median_at_bat_league = median(AB),
  median_at_bat_league_no_zero = median(AB[AB>0])
)
data$AB==0
ddply(
  data,
  "lgID",
  summarise,
  sum_homerun_league = sum(HR)
)
ddply(
  data,
  "teamID",
  summarise,
  sd_at_bat_league = sd(HR)
)
ddply(
  data,
  "playerID",
  summarise,
  min_G = min(G),
  max_G = max(G)
)
ddply(
  data,
  "playerID",
  summarise,
  number_year = n_distinct(yearID)
) %>% arrange(desc(number_year))
desc(as.tibble(c(5, 1, 3, 5, 76)))
ddply(
  data,
  "playerID",
  summarise,
  first_appearance = min(yearID),
  last_appearancef = max(yearID)
)
max(order(data$yearID[data$playerID == "aardsda01"]))
max(data$yearID[data$playerID == "aardsda01"])
ddply(
  data,
  "teamID",
  summarise,
  second_game = nth(yearID, 2)
)%>%
  arrange(desc(second_game))
ddply(
  data,
  "teamID",
  summarise,
  number_player = n_distinct(playerID)
) %>% arrange(desc(number_player))
ddply(
  data,
  c("yearID", "teamID"),
  summarise,
  mean_games = mean(G)
)%>%
  arrange(desc(mean_games))
filter(data, yearID > 1980 & yearID < 2000) %>%
 filter(R > 10) %>%
  ddply(
    "yearID",
    summarise,
    mean_game_year = mean(G)
  )
filter(data, HR>0) %>%
  ddply(
    "playerID",
    summarise,
    average_HR_game = sum(HR)/sum(G)
  )%>%
  summarise(total_average_homerun = mean(average_HR_game))


# select, filter, arrange
PATH <- "https://raw.githubusercontent.com/guru99-edu/R-Programming/master/travel_times.csv"
df <- read.csv(PATH)
glimpse(df)
sum(df$Comments=="")
step_1_df <- select(df, -Comments)
glimpse(step_1_df)
n_distinct(step_1_df$GoingTo)
table(step_1_df$GoingTo)
select_home <- filter(df, GoingTo == "Home")
glimpse(select_home)
dim(select_home)
select_work <- filter(df, GoingTo == "GSK")
select_home_wed <- filter(df, GoingTo == "Home", DayOfWeek == "Wednesday")
dim(select_home_wed)
glimpse(select_home_wed)

step_2_df <- arrange(step_1_df, GoingTo, Distance)
glimpse(step_2_df)


# Data Analysis, Scatter Plot in R using ggplot2
library(ggplot2)
ggplot(mtcars, aes(x = mpg, y = drat)) +
  geom_point(aes(color = factor(gear)), pch=19)
my_graph <- ggplot(mtcars, aes(x = log(mpg), y = log(drat))) +
  geom_point(aes(color = factor(gear)), pch=18, size=3) +
  stat_smooth(method = "lm",
              col = "#C42126",
              se=F,
              size=1)
my_graph +
  scale_x_continuous(breaks = seq(1, 3.6, by=0.2)) +
  scale_y_continuous(breaks = seq(1, 1.6, by=0.1)) +
  theme_bw() +
labs(
  x = "Drat definition",
  y = "Mile per hours",
  title = paste("Plot Mile per hours and drat, in log. Average mpg is", mean_mpg),
  subtitle = "Relationship break down by gear class",
  caption = "Authors own computation (from guru99.com)"
)
mean_mpg <- mean(mtcars$mpg)
ggsave("my_plot.jpg")


# boxplot()
air_data <- as.tibble(airquality) %>%
  select(-c(Solar.R, Temp)) %>%
  mutate(Month = factor(Month, order=T, labels=c("May", "June", "July", "August", "September")))
air_data <- mutate(air_data, day_cat = factor(ifelse(Day<10, "Begin", ifelse(Day<20, "Middle", "End"))))
air_data_nona <- na.omit(air_data)
glimpse(air_data_nona)
box_plot <- ggplot(air_data_nona, aes(x=Month, y=Ozone))
box_plot+
  geom_boxplot()+
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3,
    color = "steelblue"
  )+
  theme_classic()
#coord_flip()
box_plot+
  geom_boxplot()+
  geom_dotplot(
   binaxis="y",
   binwidth=3,
   dotsize = 1,
   stackdir="center"
  )+
  theme_classic()
ggplot(air_data_nona, aes(Month, Ozone, color=Month))+
  geom_boxplot()+
  theme_bw()
ggplot(air_data_nona, aes(Month, Ozone))+
  geom_boxplot(aes(fill=day_cat))+
  theme_bw()
ggplot(air_data_nona, aes(day_cat, Ozone))+
  geom_boxplot(aes(fill=Month))+
  theme_bw()
box_plot+
  geom_boxplot()+
  geom_jitter(
    shape=15,
    color = "steelblue",
    position = position_jitter(width=.21)
  )
box_plot +
  geom_boxplot(notch = T) +
  theme_classic()


# Bar Chart & Histogram

ggplot(mtcars, aes(factor(cyl), fill = factor(cyl))) +
  geom_bar(alpha=.5) +
  theme_classic()
data_hist2 <- mtcars %>%
  mutate(am = factor(am, labels = c("Automatic", "Manual")),
         cyl = factor(cyl)) %>%
  ddply("cyl",
        summarise,
        sum_cyl=length(cyl)
)
ggplot(data_hist2, aes(cyl, sum_cyl)) +
  geom_bar(stat = "identity")

length(grDevices::colors())
data <- mtcars %>%
  mutate(am = factor(am, labels = c("Automatic", "Manual")),
         cyl = factor(cyl))
ggplot(data, aes(cyl, fill=am)) +
  geom_bar(position = position_dodge()) +
  theme_bw()
data_hist <- mutate(mtcars, cyl=factor(cyl)) %>%
  ddply(
    "cyl",
    summarise,
    mean_mpg=round(mean(mpg),2)
  )
ggplot(data_hist, aes(cyl, mean_mpg, fill=cyl)) +
  geom_bar(stat = "identity", width=.5) +
  geom_text(aes(label=mean_mpg), hjust = 1, color = "white", size = 3) +
  coord_flip() +
  theme_classic()

# student's test

set.seed(123)
sugar_cookie <- rnorm(30, 9.99, .04)
head(sugar_cookie)
# H0 : mu = 10
t.test(sugar_cookie, mu = 10)	

set.seed(123)
# sales before the program
sales_before <- rnorm(7, mean = 50000, sd = 50)
# sales after the program.This has higher mean
sales_after <- rnorm(7, mean = 50075, sd = 50)
# draw the distribution
t.test(sales_before, sales_after,var.equal = TRUE)


# ANOVA
PATH <- "https://raw.githubusercontent.com/guru99-edu/R-Programming/master/poisons.csv"
df <- as.tibble(read.csv(PATH)) %>%
  mutate(X=factor(X)) %>%
  select(-X) %>%
  mutate(poison = factor(poison, ordered = T))
  
df %>%
  group_by(poison) %>%
  summarise(
    count_poison = n(),
    mean_time = mean(time, na.rm = TRUE),
    sd_time = sd(time, na.rm = TRUE)
  )
df2 <- ddply(
  df,
  "poison",
  summarise,
  count_poison = length(poison),
  mean_time = mean(time, na.rm = TRUE),
  sd_time = sd(time, na.rm = TRUE)
) %>% as.tibble()

ggplot(df, aes(poison, time, fill=poison))+
  geom_boxplot()+
  geom_jitter(
    shape=18,
    color=rep(c("steelblue", "green", "black"), 16)
  )
  theme_bw()
anova_one_way <- aov(time~poison, df)
summary(anova_one_way)
TukeyHSD(anova_one_way)
summary(aov(time~poison + treat, data = df))


# Regresja
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/women.csv'
df <-read.csv(path)
ggplot(df,aes(x=height, y =  weight))+
  geom_point()
beta <- cov(df$height,df$weight)/var(df$height)
alpha <- mean(df$weight) - beta * mean(df$height)

df <- mtcars %>%
  select(-c(am, vs, cyl, gear, carb))
glimpse(df)
model <- mpg ~ disp + hp + drat + wt + qsec
fit <- lm(model, df)
fit
summary(fit)
anova(fit)
par(mfrow=c(3,2))
plot(fit$fitted.values)
df <- mtcars %>%
  mutate(
    cyl = factor(cyl),
    vs = factor(vs),
    am = factor(am),
    gear = factor(gear),
    carb = factor(carb)
  )
df <- mutate(
  mtcars,
  cyl = factor(cyl),
  vs = factor(vs),
  am = factor(am),
  gear = factor(gear),
  carb = factor(carb)
)
summary(lm(model, df))

library(GGally)
ggscatmat(df, columns = 1:ncol(df), corMethod = "pearson")
df <- mtcars % > %
  select(-c(am, vs, cyl, gear, carb))
ggscatmat(df, columns = 1: ncol(df))

install.packages("olsrr")
library(olsrr)
model <- mpg~.
fit <- lm(model, df)
test <- ols_step_all_possible(fit)
plot(test)

ols_step_both_p(fit, pent=.1, prem=.3, details=T)

# Decision Tree
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read.csv(path)
head(titanic)
glimpse(titanic)
tail(titanic)
set.seed(678)
shuffle_index <- sample(1:nrow(titanic))
head(shuffle_index)
titanic <- titanic[shuffle_index, ]
head(titanic)

clean_titanic <- titanic %>%
  select(-c(home.dest, cabin, name, x, ticket)) %>%
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower'))) %>%
  mutate(survived = factor(survived, labels = c("No", "Yes"))) %>%
  na.omit()
glimpse(clean_titanic)

create_train_test <- function(data, size = .8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1 : total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

create_train_test <- function(data, size = .8, train = T) {
  if (train ==T) {
    return(data[1:(size * nrow(data)), ])
  } else {
    return(data[-(1:(size * nrow(data))), ])
  }
}

data_train <- create_train_test(clean_titanic, .8, T)
data_test <- create_train_test(clean_titanic, .8, F)
dim(data_train)
dim(data_test)

prop.table(table(data_train$survived))
prop.table(table(data_test$survived))

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
fit <- rpart(survived~., data = data_train, method = 'class')
rpart.plot(fit, type = 1 , extra = 106)

predict_unseen <-predict(fit, data_test, interval="prediction", vcov.=vcov)
table_mat <- table(data_test$survived, predict_unseen)

x <- rnorm(15)
y <- x + rnorm(15)
fit2 <- lm(y ~ x)
predict(fit2)
new <- data.frame(x = seq(-3, 3, 0.5))
predict(fit2, new, se.fit = TRUE)
table_mat_2 <- table(fit2, new)

rpart.control(minsplit = 20, minbucket = round(minsplit/3), maxdepth = 30)
accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$survived, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}
control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(survived~., data = data_train, method = 'class', control = control)
accuracy_tune(tune_fit)


#Random Forest
library(randomForest)
library(caret)
library(e1071)

data_train <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/train.csv")
data_train <- na.omit(data_train)
data_train <- mutate(data_train, Survived = factor(Survived, labels = c("No", "Yes")))
data_train <- data_train[(1:(nrow(data_train)*.1)), ]
glimpse(data_train)
data_test <- read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/test.csv") 
data_test <- na.omit(data_test)
data_test <- data_test[(1:(nrow(data_test)*.1)), ]
glimpse(data_test)

trControl <- trainControl(
  method = "cv",
  number=10,
  search="grid"
)

set.seed(1234)
rf_default <- train(Survived~.,
                    data = data_train,
                    trControl = trControl
                    )
print(rf_default)
best_mtry <- rf_default$bestTune$mtry
max(rf_default$results$Accuracy)

## Classification Example
data(iris)
TrainData <- iris[,1:4]
TrainClasses <- iris[,5]
knnFit1 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))
print(knnFit1)
varImpPlot(knnFit1)

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 15)) {
  set.seed(1234)
  rf_maxnode <- train(Survived~.,
                      data = data_train,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

fit_rf <- train(Survived~.,
                data_train,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 800,
                maxnodes = 24)
varImpPlot(fit_rf)
prediction <-predict(fit_rf, data_test)
confusionMatrix(prediction, data_test$survived)

# Generalized Linear Model
curve(1/(1 + exp(-x)), -10, 10,
      main = "sigmoid")
data_adult <-read.csv("https://raw.githubusercontent.com/guru99-edu/R-Programming/master/adult.csv")
glimpse(data_adult)
continuous <- select_if(data_adult, is.numeric)
summary(continuous)
ggplot(data_adult, aes(x=hours.per.week)) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  geom_density(alpha = .2, fill="red",
               )
top_one_precent <- quantile(data_adult$hours.per.week, .99)
data_adult_drop <- data_adult %>% filter(hours.per.week < top_one_precent)
data_adult_drop %>%
  select_if(is.numeric) %>%
  summary()
ggplot(data_adult_drop, aes(x=hours.per.week)) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  geom_density(alpha = .2, fill="red",
  )
data_adult_rescale <- data_adult_drop %>%
  mutate_if(is.numeric, funs(as.numeric(scale(.))))
head(data_adult_rescale)
data_adult_rescale <- data_adult_rescale %>% 
  mutate(
    workclass = factor(workclass),
    education = factor(education),
    marital.status = factor(marital.status),
    race = factor(race),
    gender = factor(gender),
    income = factor(income)
    )

factor <- tibble(select_if(data_adult_rescale, is.factor))
glimpse(factor)
ncol(factor)

graph <- lapply(names(factor),
                function(x) 
                  ggplot(factor, aes(get(x))) +
                  geom_bar() +
                  theme(axis.text.x = element_text(angle = 90)))
graph
ggplot(factor, aes(education))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90))

glimpse(data_adult_rescale)
recast_data <- data_adult_rescale %>%
  select(-x) %>%
  mutate(education = factor(
    ifelse(education == "Preschool" | education == "10th" | education == "11th" | education == "12th" | education == "1st-4th" | education == "5th-6th" | education == "7th-8th" | education == "9th", "dropout", 
           ifelse(education == "HS-grad", "HighGrad", 
                  ifelse(education == "Some-college" | education == "Assoc-acdm" | education == "Assoc-voc", "Community",
                         ifelse(education == "Bachelors", "Bachelors",
                                ifelse(education == "Masters" | education == "Prof-school", "Master", "PhD")))))))
glimpse(recast_data)

recast_data %>%
  group_by(education) %>%
  summarise(average_educ_year = mean(educational.num), count = n())
recast_data <- recast_data %>%
  mutate(marital.status = factor(
    ifelse(marital.status == "Never-married" | marital.status == "Married-spouse-absent", "Not_married", 
           ifelse(marital.status == "Married-AF-spouse" | marital.status == "Married-civ-spouse", "Married", 
                  ifelse(marital.status == "Separated" | marital.status == "Divorced", "Separated", "Widow")))))
table(recast_data$marital.status)
ggplot(recast_data, aes(race, fill=income))+
  geom_bar(position = "fill")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90))
ggplot(recast_data, aes(gender, hours.per.week))+
  geom_boxplot()+
  stat_summary(fun = mean,
               pch = 18,
               size = 1,
               color="steelblue")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90)) 
ggplot(recast_data, aes(hours.per.week, color=education, alpha=.5))+
  geom_density()+
  theme_classic()
ggplot(recast_data, aes(hours.per.week))+
  geom_density(aes(color=education), alpha=.5)+
  theme_classic()

anova <- aov(hours.per.week~education, recast_data)
summary(anova)

ggplot(recast_data, aes(x=age, y=hours.per.week))+
  geom_point(aes(color=income), size = 1)+
  stat_smooth(
    method="lm",
    formula=y ~ poly(x, 5),
    aes(color = income),
    se=T
  )+
  theme_dark()
  
library(GGally)
corr <- data.frame(lapply(recast_data, as.integer))
glimpse(corr)
# Plot the graph
ggcorr(corr,
       method = c("pairwise", "spearman"),
       nbreaks = 6,
       hjust = 0.8,
       label = TRUE,
       label_size = 3,
       color = "grey50")

data_train <- create_train_test(recast_data, .8, T)
data_test <- create_train_test(recast_data, .8, F)
dim(data_train)
dim(data_test)

formula <- income~.
logit <- glm(formula, data = data_train, family = 'binomial')
summary(logit)

predict <- predict(logit, data_test, type = 'response')
# confusion matrix
table_mat <- table(data_test$income, predict > 0.5)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test

precision <- function(matrix) {
  # True positive
  tp <- matrix[2, 2]
  # false positive
  fp <- matrix[1, 2]
  return (tp / (tp + fp))
}
recall <- function(matrix) {
  # true positive
  tp <- matrix[2, 2]
  # false positive
  fn <- matrix[2, 1]
  return (tp / (tp + fn))
}
prec <- precision(table_mat)
prec
rec <- recall(table_mat)
rec
f1 <- 2 * ((prec * rec) / (prec + rec))
f1

library(ROCR)
ROCRpred <- prediction(predict, data_test$income)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.7))

# K-means Clustering
df <- data.frame(age = c(18, 21, 22, 24, 26, 26, 27, 30, 31, 35, 39, 40, 41, 42, 44, 46, 47, 48, 49, 54),
                 spend = c(10, 11, 22, 15, 12, 13, 14, 33, 39, 37, 44, 27, 29, 20, 28, 21, 30, 31, 23, 24)
)
ggplot(df, aes(x = age, y = spend)) +
  geom_point()
scale(df$age)

PATH <-"https://raw.githubusercontent.com/guru99-edu/R-Programming/master/computers.csv"
df <- read.csv(PATH) %>%
  select(-c(X, cd, multi, premium))
glimpse(df)
summary(df)

rescale_df <- df %>%
  mutate(price_scal = scale(price),
         hd_scal = scale(hd),
         ram_scal = scale(ram),
         screen_scal = scale(screen),
         ads_scal = scale(ads),
         trend_scal = scale(trend)) %>%
  select(-c(price, speed, hd, ram, screen, ads, trend))

glimpse(rescale_df)
rescale_df$price_scal
set.seed(2345)
library(animation)
kmeans.ani(rescale_df[2:3], 3)

pc_cluster <- kmeans(rescale_df, 5)
glimpse(pc_cluster)
pc_cluster$size
kmean_withinss <- function(k) {
  cluster <- kmeans(rescale_df, k)
  return (cluster$tot.withinss)
}
kmean_withinss(2)
max_k <- 20
wss <- sapply(2:max_k, kmean_withinss)
# Create a data frame to plot the graph
elbow <-data.frame(2:max_k, wss)
# Plot the graph with gglop
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))+
  theme_bw()
pc_cluster_2 <-kmeans(rescale_df, 7)
pc_cluster_2$cluster
pc_cluster_2$size
center <-pc_cluster_2$centers
center
cluster <- c(1: 7)
center_df <- data.frame(cluster, center)
library(tidyr)
center_reshape <- gather(center_df, features, values, price_scal: trend_scal)
center_reshape <- pivot_longer(center_df, !cluster, names_to="features", values_to="values")
head(center_reshape)
library(RColorBrewer)
# Create the palette
hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')
# Plot the heat map
ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, 7, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90))+
  theme_classic()

#




# CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)
