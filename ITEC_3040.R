library(readxl)

#Importing the data set
DATA_3040 <- read_excel("C:/Users/tjogu/Documents/Uni_Courses/ITEC_3040/DATA_3040.xlsx")



#Seperating the columns
RentalRate<- print(DATA_3040$RentalRate)
Age<- print(DATA_3040$Age)
Expense<- print(DATA_3040$Expense)
Vaccancy<- print(DATA_3040$Vacancy)
Footage<- print(DATA_3040$Footage)




# Question 1.B): Calculating the mean, median and standard deviation of renatal rates
print('Mean: ')
mean(RentalRate)
print('Median: ')
median(RentalRate)
print('Standard Deviation: ')
sd(RentalRate)


# Question 1.C): 
#Findings: 

ba<- boxplot(Age)
be<- boxplot(Expense)
bv<- boxplot(Vaccancy)
bf<- boxplot(Footage)

# Question 1.D) Drawing a Histogram for [Age of properties, operating expense, vaccancy rates, and total sqft]
#Findings 
hist(Age)
hist(Expense)
hist(Vaccancy)
hist(Footage)


#Question 1.E) Finding the distribution of the age of the properties
plot(Age)

#Question 1.G) Z-score
# In the formula to calculate the Z-score we need to have a variable called X which is the value given.Since there is
# Value given i calculated the z-score with x as being every age in the data set. I used a loop to iterate through 
# the 65 ages and added that to my formula. 

print('Z-score: ')
i <- 1
while (i < 65){
  
mu<-mean(Age)
stdev<-sd(Age)
x<-Age[i]

Z_Score<-(x-mu)/stdev

 print(Z_Score)
i <- i + 1
}

#Question 1.H) Normalization on the age of properties based on the abs sd.
print('Z-score with abs sd: ')
n<-65
mu<-mean(Age)

sd1<-0.00

i <- 1
while (i < 65){
  
  mu<-mean(Age)
  stdev<-sd(Age)
  x<-Age[i]
  
  Z_Score<-(x-mu)/stdev
  
  sd1=(Z_Score-mu)
  i <- i + 1
  Sa<-sum(sd1)
  sas<-1/n
  sasd<-sas*Sa
  asd<-(x-mu)/sasd
  print(asd)
}

