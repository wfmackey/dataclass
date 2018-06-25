#### SET UP ####

    ##Installing packages (only has to be done once)
    install.packages("tidyverse")
    install.packages("placement")
    install.packages("readxl")
    install.packages("maps")
    
    ##Loading packages (done every session)
    library(tidyverse)
    library(placement)
    library(readxl)
    library(maps)
    
    # This tells R to prefer non-scientific notation (dw about this for now)
    options(scipen=10000)
    
    # Here we're defining some Grattan-y colors for future use
    gorange <- "#F68B33"
    gdark <- "#D4582A"
    
    # And here we're defining the temporary Google Maps API key that we'll use later
    internkey <- "AIzaSyCd9NCcspiyVioOWDi6MWBAMZPNj9zKsyA"
    
    ##Setting working directory: "these are where the files I'm talking about are"
    ##You can do this view the "files" tab on the bottom right hand side
    ##Navigate to your folder of choice, then hit the settings icon, hit "set as working directory"
              
              setwd("~/Documents/GitHub/dataclass/intern_data_class1806")
    
              #setwd("~/Documents/intern_data_class1806")
    
              
#### RECREATING EXCEL ANALYSIS IN R #####
  # 1: Step-by-step        
              ##Read in our data. We are creatively calling our data "data".
              ## the <- means we are defining the left as the right
              data1 <- read_excel("0_original_data/abs_sa3_income_6524055002do0006_201115.xls",
                                 sheet = 4,
                                 range = "A7:AA365",
                                 na = "na")  ## See what we're doing with our "na" values there?
                
                # Select variables (columns)
                # function: select(dataset, variable to keep, variable to keep, ...)
              data2 <- select(data1, 
                              "SA3", 
                              "SA3 NAME",
                              starts_with("2010")
                        )
                
                # Rename variables (columns)
                # function: rename(dataset, newname = oldname, newname = oldname, ...)
              data3 <- rename(data2, 
                               sa3 = "SA3",
                               name = "SA3 NAME",
                               earners = "2010-11",
                               age = "2010-11__1",
                               tot.income = "2010-11__2",
                               med.income = "2010-11__3",
                               av.income = "2010-11__4"
                        )
                
                # (Negated) Select variables
                # function: select(dataset, -variable to drop, -variable to drop, ...)
              data4 <-  select(data3, 
                               -av.income
                        )
                
                # Drop observations (rows) that have missing values
                # function: drop_na(data)
              data5 <- drop_na(data4)
                
                # Create a new variable
                # function: mutate(data, newvar = rule)
              data6 <- mutate(data5,
                              av.income = tot.income/earners,
                              sa3_1 = floor(sa3/10000)
                       )
    
              
    ## Sidenote:           
    # Great! Now we're going to do exactly the same thing, but using the 'piping' functions %>%
    # that are enabled by the tidyverse. Here's how they work:
              sum(1, 2)  # this equals three: sum of 1 and 2
              one <- 1   # defining one as 1
              two <- 2   # and two as 2
              sum(one, two) # now this also equals two
              sum(sum(one, two), two) # and this equals five: sum(sum(one, two), two) = sum(3, two) = sum(3, 2) = 5
              
             # With piping, we can take the results of one function and 'pipe' it into another
             # so: 
             sum(one, two) %>% sum(., two) # this means: sum 1 and 2 and take the result (.=3) and sum it with two = 5
             # We've just replaced the first argument of the piped function with the result from the first function
             # And we can ignore the ".", as it's there by default
              
    ##So we can do the whole thing again in a single "line" of code (notice the pipes):
    data <- read_excel("0_original_data/abs_sa3_income_6524055002do0006_201115.xls",
                          sheet = 4,
                          range = "A7:AA365",
                          na = "na") %>%
            select("SA3", 
                   "SA3 NAME",
                   starts_with("2010")) %>% 
            rename(sa3 = "SA3",
                   name = "SA3 NAME",
                   earners = "2010-11",
                   age = "2010-11__1",
                   tot.income = "2010-11__2",
                   med.income = "2010-11__3",
                   av.income = "2010-11__4"
                   ) %>% 
            select(-av.income) %>% 
            drop_na() %>% 
            mutate(av.income = tot.income/earners,
                   sa3_1 = floor(sa3/10000))
    
    # Done! We've imported the right part of the Excel sheet, kept and renamed the 
    # variables we want (and dropped the ones we didn't), and made a new variable
    # for average income (av.income). Let's see if it worked:
    
    
    ##Examining our data
        View(data)  #Look at it
        names(data) #Display the variable names
        
        mean(data$av.income)   #Get the mean of the values in DATASET$COLUMN
        median(data$av.income)   #Get the median of the values in DATASET$COLUMN
        
        # And looking at an ugly (but still informative) histogram
        hist(data$av.income)
        
        # Let's look at a ~better~ histogram using ggplot
        ggplot(data) +
            geom_histogram(aes(av.income), fill=gorange, color="white") +
            theme_minimal()
    
        
        # Let's look at a ~better~ histogram
        ggplot(data) +
          geom_density(aes(med.income), fill=gorange, color="black", alpha = 0.7) +
          geom_density(aes(av.income), fill=gdark, color="black", alpha = 0.7) +
          theme_minimal()
    ##  All looks okay. Cool. Cool cool cool. Now we'll do the correspondence portion of the Excel task
    #   Read in the correspondence table
        
        states <- read_excel("3_sa3_state_corresp.xlsx") %>% 
                  rename(sa3_1 = "SA3")
    
    
      # Now we apply the states and capitals to each observations in our dataset
      # function: inner_join(dataset1, dataset2, match 2 to 1 by rule)
            # note that inner_join will return all rows from 1 where there 
            # are matching values in 2, and all columns from 1 and 2.
            # There's a good resource on joins here: 
            # http://stat545.com/bit001_dplyr-cheatsheet.html#why-the-cheatsheet
      
      data <- inner_join(data, states, by = "sa3_1")
      
      # Let's look to see if it worked:
      View(data)
      
      # Good - but the "na" states at the bottom might cause issue. We can remove them using the drop_na function 
      # we used above
      data <- drop_na(data)
      
    # Wonderful. We have -- in a few detailed and reproducable steps -- recreated the work we
    # did in Excel. Now we can move to the (more) fun part.   
  

#####  WHAT IS THE RELATIONSHIP BETWEEN INCOME AND DISTANCE TO THE CAPITAL PER SA3 #####
    
      
    ##We are now going to attempt to get distances from 
    ##Testing the "placement" package to access Google Maps API
      # function: drive_time. We can look at the syntax of the function by
      ?drive_time
      
      # drive_time(address, dest, auth = "standard_api", privkey = NULL,
      #            clientid = NULL, clean = "TRUE", travel_mode = "driving",
      #            units = "metric", verbose = FALSE, add_date = "none",
      #            language = "en-EN", messages = FALSE, small = FALSE)
      
      #Okay:  let's test it out. We'll write our address as "Liverpool" (an SA3)
      #       and our dest as "Sydney"
      
      drive_time_test <- drive_time("Liverpool" ,
                                    "Sydney" ,
                                    travel_mode = "driving" ,
                                    privkey = internkey
                                    )
    
      drive_time_test
      
      # Ah that didn't work. What if we did: (remember, we're searching Google: so 
      # we should try to make as much like we would Google something; well, kinda)
      
      drive_time_test_better <- drive_time("Liverpool, NSW, Australia" ,
                                        "Sydney, NSW, Australia" ,
                                        travel_mode = "driving" ,
                                        privkey = internkey ,
                                        units = "metric"
    )
    
    
      drive_time_test_better
    
      # Yay! That worked. So, what do we need to do this for our whole dataset?
      # We can use the "paste0" function, which works like this:
      paste0("this ", "is ", "the HEX colour code for 'Grattan's orange' ", gorange)
      
      # It takes a bunch of arguments separated by commas and stiches them
      # together. So, if we wrote:
      paste0(data$name,", ",data$State,", Australia")
      
      # We would get the "name of the sa3, state, Australia" all in a single line.
      # And we can use this to feed the drive_time function (using Google) more 
      # information about where we actually want to search. 
      
      # Let's try the first 5 observations:
      drive_time <- drive_time(paste0(data$name[1:5], ", " , data$State[1:5], ", Australia") ,
                               paste0(data$Capital[1:5], ", " , data$State[1:5], ", Australia"),
                               travel_mode = "driving" ,
                               privkey = internkey
      )
      drive_time[,1:6]
  
    ## Cool, that worked! (That's presumptuous; I hope it has worked for everyone)  
      
    ##Now we can run all 337 through. NOTE: this will take 5 or so minutes, depending on Google's mood.
    drive_time <- drive_time(paste0(data$name, ", " , data$State, ", Australia") ,
                             paste0(data$Capital, ", " , data$State, ", Australia"),
                             travel_mode = "driving" ,
                             privkey = internkey
    )
    
    # Let's see if it worked:
    drive_time[,1:6]
    
    # Rather than manually looking through each observations (and we should spotcheck some observations),
    # can quickly look at a histogram to see if there are any funny numbers:
    ggplot(drive_time) + geom_histogram(aes(dist_num), fill = gorange, color="white") + theme_minimal()
    
    
    ##Done, great. Now we can export to csv and create a 'Grattan style' graph with the data
    #Combine our variables (columns) of our income data with the travel-time data:
    final_data <- bind_cols(data,
                            drive_time) %>% 
                  select(-input_url) ##this variable is long and annoying and we'll drop it right here
    
    
    # Now we can conduct some more serious analysis. First, let's look at distributions by state to
    # make sure everything makes sense
    ggplot(final_data) + 
      geom_histogram(aes(dist_num), fill = gorange) + 
      theme_minimal() +
      facet_wrap(~State, scales = "free")
    
    
# Now "write" a .csv file with our findings, so we can use it in a Grattan chart:
    write.csv(final_data,file = "final_data.csv")
    

    ############ EXTRA FUN ACTIVITIES

#simple OLS regression
    regression <- lm(av.income ~ time_mins, final_data)
    summary(regression)
    
# Let's create a base object for our graphics:    
base <- ggplot(final_data, aes(x=av.income, y=time_mins))

# Create a scatter plot with an OLS line of best fit:
base +
  geom_point() +
  geom_smooth(method = lm)

# What if we used a different approach to identify a trend?:
base +
  geom_point() +
  geom_smooth(method = loess)


# Create a scatter plot that distinuishes between states
base +
  geom_point(aes(color=State))


# Let's look at this on a log-scale
base +
  geom_point(aes(color=State)) +
  geom_smooth(method=lm, se=FALSE, color=gorange) +
  scale_y_log10(limits = c(1,5000))

##Looks like we see consistent negative relationships between time and income, except for Darwin. Why is this? And why does ACT have such a low absolute slope coefficient?
## Let's look at it with confidence intervals and marginal rugs:

base +
  geom_point(aes(color=State)) +
  geom_smooth(method=lm, se=FALSE, aes(fill=State, color=State)) +
  scale_y_log10(limits = c(1,5000))

# Woah, NT has a positive relationship. That's ood. Let's look at the standard errors there:
base +
  geom_point(aes(color=State)) +
  geom_smooth(method=lm, se=TRUE, aes(fill=State, color=State)) +
  scale_y_log10(limits = c(1,5000))


##Well the NT confidence intervals are crazy. We should take this into consideration when we make any statements about driving time and income!
##ALso looks like QLD has its maximum interval close to zero. Would be interesting to look at this with a fixed effects regression or something of the like.

# geocode(location, output = c("latlon", "latlona", "more", "all"),
#         source = c("google", "dsk"), messaging = FALSE, force = ifelse(source ==
#                                                                          "dsk", FALSE, TRUE), sensor = FALSE, override_limit = FALSE,
#         client = "", signature = "", nameType = c("long", "short"), data)


latlons <-  geocode_url(paste0(data$name, ", " , data$State, ", Australia"),
                        privkey = internkey)
                    
final_data <- bind_cols(final_data, latlons)

aus.map <- map_data("world") %>% filter(region=="Australia")

# Let's make Australia
australia <- 
  ggplot() +
  geom_polygon(data = aus.map, aes(x=long, y = lat, group = group), fill="grey", color="white", linetype="solid", alpha=0.6) +
  theme_void() +
  ylim(-44,-10) + xlim(113,154)

  australia

# Now we overlay our income and geographical data onto our little Australia
  incomemap <-  australia + 
                geom_point(data=final_data, 
                           aes(x=lng, y=lat, size=av.income, color=State), 
                           show.legend = T, alpha=.7) +
                scale_size_continuous(range = c(0.01, 10))
  
  
  incomemap  
  

  
  
  
  
  