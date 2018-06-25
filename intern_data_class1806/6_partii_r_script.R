##Installing packages (only has to be done once)
install.packages("tidyverse")
install.packages("placement")
install.packages("readxl")

##Loading packages (done every session)
library(tidyverse)
library(placement)
library(readxl)

##Setting working directory: "these are where the files I'm talking about are"
##You can do this view the "files" tab on the bottom right hand side
##Navigate to your folder of choice, then hit the settings icon, hit "set as working directory"
setwd("~/Documents/intern_data_class1806")

##Read in our data. We are creatively calling our data "data".
## the <- means we are defining the left as the right
data2 <- read_excel("0_original_data/abs_sa3_income_6524055002do0006_201115.xls",
                      sheet = 4,
                      range = "A7:AA365")

##Examining our data
View(data)  #Look at it
names(data) #Display the variable names

#So, we're part-way there. As with the Excel task, we only want to look at 


mean(data$mean)   #Get the mean of the values in DATASET$COLUMN
median(data$mean)   #Get the median of the values in DATASET$COLUMN


##  All looks good!
##  Now, 






##We are now going to attempt to get distances from 
##Testing the "placement" package to access Google Maps API
drive_time_test <- drive_time("Liverpool" ,
                                    "Sydney" ,
                                    travel_mode = "driving" ,
                                    privkey = "" ,
                                    units = "metric" ,
                                    verbose = "true"
                                    )

print(drive_time_test)


##Hmm. What if we did:
drive_time_test <- drive_time("Liverpool, NSW, Australia" ,
                                    "Sydney, NSW, Australia" ,
                                    travel_mode = "driving" ,
                                    privkey = "" ,
                                    units = "metric" ,
                                    verbose = "true"
)


print(drive_time_test)


##Works pretty well! Let's create a new dataset with that information added. We can do this using the "paste" and "data.frame" functions. 
new_sa3 <- paste0(data$sa3_name,", ",data$state, ", Australia")
new_capital <- paste0(data$capital,", ",data$state, ", Australia")
  
new_data <- data.frame(new_sa3, new_capital)



##Now we can run all 337 through. NOTE: this will take 5 or so minutes, depending on Google's mood.
drive_time <- drive_time(new_data$new_sa3 ,
                              new_data$new_capital ,
                              travel_mode = "driving" ,
                              privkey = "" ,
                              units = "metric" ,
                              verbose = "true"
)


View(drive_time)



##Done, great. Now we can export to csv and create a Grattan style graph with the data
#Create a nice dataset with the variables we want
final_data <- data.frame(data$sa3_name,drive_time$origin,data$state,drive_time$time_mins,data$mean)
write.csv(final_data,file = "final_data.csv")

##And lets write a CSV with all our transit data in it:
write.csv(drive_time,file = "drive_time_final.csv")



#OLS regression
fit <- lm(final_data$data.mean ~ final_data$drive_time.time_mins)
print(fit)
summary(fit)



############ EXTRA FUN ACTIVITIES

# Create a scatter plot with an OLS line of best fit:
ggplot(final_data, aes(x=data.mean, y=drive_time.time_mins)) +
  geom_point() +
  geom_smooth(method = lm)

# What if we used a different approach to identify a trend?:
ggplot(final_data, aes(x=data.mean, y=drive_time.time_mins)) +
  geom_point() +
  geom_smooth(method = loess)


# Create a scatter plot that distinuishes between states
ggplot(final_data, aes(x=data.mean, y=drive_time.time_mins, color=data.state)) +
  geom_point()


# Let's log drive time to see if we get a better picture of what's going on
ln_time <- log(final_data$drive_time.time_mins)
final_data <- data.frame(final_data, ln_time)

ggplot(final_data, aes(x=data.mean, y=ln_time, color=data.state)) +
  geom_point() +
geom_smooth(method=lm, se=FALSE, fullrange=FALSE)

##Looks like we see consistent negative relationships between time and income, except for Darwin. Why is this? And why does ACT have such a low absolute slope coefficient?
## Let's look at it with confidence intervals and marginal rugs:

ggplot(final_data, aes(x=data.mean, y=ln_time, color=data.state)) +
  geom_point() +
geom_smooth(method=lm, aes(fill=data.state))


##Well the NT confidence intervals are crazy. We should take this into consideration when we make any statements about driving time and income!
##ALso looks like QLD has its maximum interval close to zero. Would be interesting to look at this with a fixed effects regression or something of the like.
