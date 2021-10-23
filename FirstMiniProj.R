#Calculating the pollutant mean
pollutantmean<- function(directory, pollutant, id=1:332){
  my_list <- list.files(path = directory, full.names = T)#Creating a list of files and assigned it to a variable my_list
  alldata<- data.frame() # Creating an empty data frame to accommodate all files
  for(i in id){
    alldata<-rbind(alldata, read.csv(my_list[i])) #Reading the list of csv files within my_list and assigned it to a variable alldata
  } #Using for loops that runs through all the files and binding them row by row into the data frame
  mean(alldata[,pollutant], na.rm=T)# Returning the mean of the pollutant and ignoring missing values NA by using na.rm=T. 
}
  

#Eample output
#pollutantmean("C:/Users/USER/Desktop/specdata/", "sulfate", 1:10)
#[1] 4.064128
#pollutantmean("C:/Users/USER/Desktop/specdata/", "sulfate", 23)
#[1] 0.8310505
#pollutantmean("C:/Users/USER/Desktop/specdata/", "nitrate", 70:72)
#[1] 1.706047
#pollutantmean("C:/Users/USER/Desktop/specdata/", "nitrate", 23)
#[1] 1.280833


#Modifying the pollutant mean and named it the complete function
complete<- function(directory,id=1:332 ){
  my_list <- list.files(path = directory, full.names = T) #Creating a list of files and assigned it to a variable my_list
  alldata<- data.frame() # Creating an empty data frame to accommodate all files
  for(i in id){ 
    myfile_i<- read.csv(my_list[i]) #Reading the list of csv files within my_list[i] 
    nobs<- sum(complete.cases(myfile_i)) #Computing the sum of the completely observe cases of myfile_i
    tmp<- data.frame(i, nobs)  #Creating new variable tmp
    alldata<-rbind (alldata, tmp) #Using for loops that runs through all the files and binding them row wise
  }
  colnames(alldata)<- c("ID", "nobs") #Setting the column names, ID and nobs
  alldata #Returning the data frame 
  
}


#Example output
#complete("C:/Users/USER/Desktop/specdata/", 1)
#ID nobs
#1  1  117

#complete("C:/Users/USER/Desktop/specdata/", 3)
#ID nobs
#1  3  243

#complete("C:/Users/USER/Desktop/specdata/", 20)
#ID nobs
#1 20  124

#complete("C:/Users/USER/Desktop/specdata/", c(2,4,8,10,12)) 
#ID nobs
#1  2 1041
#2  4  474
#3  8  192
#4 10  148
#5 12   96


#Modifying code 1 and 2 and named it corr
corr<- function(directory, threshold = 0) {
  my_list <- list.files(path= directory, pattern=".csv")  #Creating a list of files and assigned it to a variable my_list
  alldata <- vector(mode = "numeric", length = 0) #Creating a numeric vector of length 0 
  
  for (i in 1:length(my_list)) {  #Looping through files
    myfile_i <- read.csv(my_list[i]) #Reading the list of csv files within my_list[i]
    cmpltcases <- sum((!is.na(myfile_i$sulfate)) & (!is.na(myfile_i$nitrate)))# Computing the sum of the sulfate and  nitrate  #Note that !is.na contains all of the non-NA values
    if (cmpltcases > threshold) {  # Using If function 
      fil_sulfate <- myfile_i[which(!is.na(myfile_i$sulfate)), ]  #With no missing values of sulfate
      fil_all <- fil_sulfate[which(!is.na(fil_sulfate$nitrate)), ] #With no missing values of sulfate and nitrate 
      alldata <- c(alldata, cor(fil_all$sulfate, fil_all$nitrate))  #Calculating the correlation between sulfate and nitrate with no missing 
      #values. 
      
    } 
  } 
  alldata #Returning a numeric vector of length 0
}


#Example output 
#cr<- corr("C:/Users/USER/Desktop/specdata/", 150)
# head(cr)
#[1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814
#summary(cr)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 


#cr<- corr("C:/Users/USER/Desktop/specdata/")
#head(cr)
#[1] -0.22255256 -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667
#summary(cr)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000 
#length(cr)
#[1] 323


#Plotting the 30-day mortality rates for heart attack given the data set outcome-of-care-measures.csv
outcome<- read.csv("C:/Users/USER/Desktop/specdata/outcome-of-care-measures.csv") #Importing the file outcome  of care measures and assigned it to outcome 
outcome[, 11]=as.numeric (outcome[, 11]) #Coercing the csv file and returns a numeric value
hist((outcome[, 11]), xlab= "Deaths", main= "Hospital 30-Day Death (Mortality) Rates from Heart Attack", col= "lightblue")  #Generating histogram, Deaths as x axis, making title using main, and assigning  color using col 
