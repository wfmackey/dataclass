#### SET UP ####
# !diagnostics off
## note that the hash symbol starts a comment -- anything after a # is ignored by the program. Very useful for notes!s

## To run a single line of code (the one your cursor is on), hit CTRL+ENTER on PC, or CMD+ENTER on Mac. 

    ##Installing packages (only has to be done _once_)
    install.packages("tidyverse")
    install.packages("readxl")
    install.packages("placement")
    install.packages("maps")
    
    ##Loading packages (done every session)
    library(tidyverse)
    library(readxl)
    library(placement)
    library(maps)
    
    ## This tells R to prefer non-scientific notation
    options(scipen=10000)
    
    ## Here we're defining some Grattan-y colors for future use
        # the "<-" symbol means [this] [is defined as] [that]
    gorange <- "#F68B33"    # Now -- this will appear in our "Global Environment" (top-right of the screen). Everything we define will appear there.
    gdark <- "#D4582A"
    
    ## And here we're defining the temporary Google Maps API key that we'll use later
    internkey <- "AIzaSyCd9NCcspiyVioOWDi6MWBAMZPNj9zKsyA"
    # ....please don't use this outside of this course ¯\_(ツ)_/¯
    
#--------------------------------------------------------------------------------------#
                  #### RECREATING EXCEL ANALYSIS IN R #####
#--------------------------------------------------------------------------------------#      

  # 1: Step-by-step        
              ##Read in our data. We are creatively calling our data "data".
              ## the "<-" means we are defining the left as the right
              ## read_excel() is a function that asks for: "what_excel_file_should_i_read", 
               # plus (optionally) "which sheet"; "what range in that sheet"; and "what 
               # should I do with empty values?"
              data1 <- read_excel("1_original_data/6524055002do0007_201115.xls",
                                 sheet = 4,           # Q1: what is this argument doing?
                                 range = "A7:G365",   
                                 na = "na")           # Q2: what is this argument doing?
                
              View(data1)   # hey, what does our dataset look like so far?
                            # not great! the variable names over two lines has confused it
              
              head(data1)   # this provides the first few rows in the console (down below)
        
              
              # Rename variables (columns)
              ## First: what are our variable (column) names?
              names(data1)
              ## Cool, some are okay, but we want to change them to be informative and without spaces
              ## function: rename(dataset, newname = oldname, newname = oldname, ...)
              data2 <- rename(data1, 
                               sa3 = "SA3",                 # Rename "SA3" to sa3
                               name = "SA3 NAME",           # Rename "SA3 NAME" to name (always keep your variable names concise and without.a.space)
                               earners = "persons",
                               age = "years",
                               tot_income = "$..5",
                               med_income = "$..6",
                               av_income = "$..7")
                
              # How'd we do:
              head(data2)
              # Nice one!
              
              # Drop variable using (negated) select variables
              # function: select(dataset, -variable to drop, -variable to drop, ...)
              data3 <-  select(data2, 
                               -av_income)
                
              # Drop observations (rows) that have _any_ missing values in _any_ variable (column)
              # function: drop_na(data)
              data4 <- drop_na(data3)
              
              # How many observations did we drop?
              nrow(data3) - nrow(data4)

              # Create a new variable (the one we just dropped) and one for the first digit of the SA3
              # function: mutate(data, newvar = rule)
              data5 <- mutate(data4,
                              av_income = tot_income/earners,
                              sa3 = sa3,
                              sa3_1 = floor(sa3/10000))
    
              
    ## A sidenote on PIPING:           
    # Great! Now we're going to do exactly the same thing, but using the 'piping' functions %>%
    # that are enabled by the tidyverse. Here's how they work:
              sum(1, 2)  # this equals three: sum of 1 and 2
              one <- 1   # defining one as 1
              two <- 2   # and two as 2
              sum(one, two) # now this also equals two
              sum(sum(one, two), two) # and this equals five: sum(sum(one, two), two) = sum(3, two) = sum(3, 2) = 5
              #    ^ (1 + 2 = 3) + 2 = 5
              
             # With piping, we can take the results of one function and 'pipe' it into another
             # so: 
             sum(one, two) %>% sum(., two) # this means: [sum 1 and 2] and take the result (.=3) and [sum it with two] = 5
             # We've just replaced the first argument of the piped function with the result from the first function with ., 
             # which means "whatever you were talking about before".
             # And, usually, we can ignore the "." in the first argument, as it's there by default:
             sum(one, two) %>% sum(two)
              
             
   ## So we can do the whole thing again in a single "line" of code (notice the pipes):
   data <- read_excel("1_original_data/6524055002do0007_201115.xls",
                sheet = 4,               # what is this doing?
                range = "A7:G365",       # what is this doing? 
                na = "na") %>%           # "pipe all the things I just did into the next thing"
           rename(sa3 = "SA3",                
                  name = "SA3 NAME",          
                  earners = "persons",
                  age = "years",
                  tot_income = "$..5",
                  med_income = "$..6",
                  av_income = "$..7") %>% 
          select(-av_income) %>% 
          drop_na(.) %>% 
          mutate(av_income = tot_income/earners,
                 sa3 = sa3,
                 sa3_1 = floor(sa3/10000))
    
    
    # Done! We've imported the right part of the Excel sheet, kept and renamed the 
    # variables we want (and dropped the ones we didn't), and made a new variable
    # for average income (av_income). Let's see if it worked:
    
    
    ##Examining our data
        View(data)  #Look at it
        names(data) #Display the variable names
        
        #Get the mean of the values in DATASET$COLUMN
        mean(data$av_income)     
          # Get the mean using dplyr's summarise function
          data %>% summarise(mean_income = mean(av_income)) # Q: how do we find out what the summarise function does?
          
          # Using this syntax we can get summaries by groups
          data %>% 
            group_by(sa3_1) %>% 
            summarise(mean_income = mean(av_income),
                      med_income  = median(av_income),
                      count_sa3s  = n())
          
        
        # And looking at an ugly (but still informative!) histogram
        hist(data$av_income)
        
        # Let's look at a ~better~ histogram using ggplot
        data %>% 
        ggplot(aes(av_income)) +
            geom_histogram(fill=gorange, color="white") +
            theme_minimal()
    
        
        # Let's look at a density plot; one for MED.INCOME, and one for av_income
        ggplot(data) +
          geom_density(aes(med.income), 
                       fill = gorange, color="black", alpha = 0.7) +
          geom_density(aes(av_income), 
                       fill = gdark, color="black", alpha = 0.7) +
          theme_minimal()
        
        
    ##  All looks okay. Cool. Cool cool cool. Now we'll do the correspondence portion of the Excel task
    #   Read in the correspondence table
        
        states <- read_excel("3_sa3_state_corresp.xlsx") %>% 
                  rename(sa3_1 = "SA3")
        
        head(states)  # look at it
    
      # Now we apply the states and capitals to each observations in our dataset
      # function: inner_join(dataset1, dataset2, match 2 to 1 by rule)
            # note that inner_join will return all rows from 1 where there 
            # are matching values in 2, and all columns from 1 and 2.
            # There's a good resource on joins here: 
            # http://stat545.com/bit001_dplyr-cheatsheet.html#why-the-cheatsheet
      
      data <- inner_join(data, states, by = "sa3_1")
      
      # Let's look to see if it worked:
      head(data)
      
      # Good - but the "na" states at the bottom might cause issue. We can remove them using the drop_na function 
      # we used above. We also NOTE why we are removing them
      data <- drop_na(data) # eg: dropping na states because they don't...
      
    # Wonderful. We have -- in a few detailed and reproducable steps -- recreated the work we
    # did in Excel. Now we can move to the (more) fun part.   
  
      
      
      
      
#--------------------------------------------------------------------------------------#
#####  WHAT IS THE RELATIONSHIP BETWEEN INCOME AND DISTANCE TO THE CAPITAL PER SA3 #####
#--------------------------------------------------------------------------------------#    
      
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
                                    privkey = internkey,
                                    units = "metric")
    
      drive_time_test
      
      # Ah that didn't work. What if we did: (remember, we're searching Google: so 
      # we should try to make as much like we would Google something; well, kinda)
      
      drive_time_test_better <- drive_time("Liverpool, NSW, Australia" ,
                                        "Sydney, NSW, Australia" ,
                                        travel_mode = "driving" ,
                                        privkey = internkey ,
                                        units = "metric")
    
    
      drive_time_test_better
    
      # Yay! That worked. So, what do we need to do this for our whole dataset?
      # First, lets look at the paste0 function:
      # We can use the "paste0" function, which works like this:
      paste0("this ", "is ", "the HEX colour code for 'Grattan's orange' ", gorange)
      
      # It takes a bunch of arguments separated by commas and stiches them
      # together. So, if we wrote:
      paste0(data$name,", ",data$State,", Australia")
      
      # We would get the "name of the sa3, state, Australia" all in a single line.
      # And we can use this to feed the drive_time function (using Google) more 
      # information about where we actually want to search. 
      
      # Let's try the first 5 observations:
        ## Note that data$name[1:5] means: "take the dataset 'data', only look at the column 'name', and then only look at the rows 1 to 5"
      drive_time <- drive_time(paste0(data$name[1:5], ", " , data$State[1:5], ", Australia") ,
                               paste0(data$Capital[1:5], ", " , data$State[1:5], ", Australia"),
                               travel_mode = "driving" ,
                               privkey = internkey)
      
      drive_time[,1:6]
  
    ## Cool, that worked! (That's presumptuous; I hope it has worked for everyone)  
      
    ##Now we can run all 337 through. NOTE: this will take 5 or so minutes, depending on Google's mood.
    drive_time <- drive_time(paste0(data$name,    ", " , data$State, ", Australia") ,
                             paste0(data$Capital, ", " , data$State, ", Australia"),
                             travel_mode = "driving" ,
                             privkey = internkey   
    )
    
    # Let's see if it worked:
    head(drive_time) # (scroll up a little in your console)
    
    # Rather than manually looking through each observations (although we should spotcheck some observations),
    # we can quickly look at a histogram to see if there are any ~funny~ numbers:
    ggplot(drive_time) + 
      geom_histogram(aes(dist_num), fill = gorange, color="white") + 
      theme_minimal()
    
    
    ##Done, great. Now we can export to csv and create a 'Grattan style' graph with the data
    #Combine our variables (columns) of our income data with the travel-time data:
    final_data <- bind_cols(data,
                            drive_time) %>% 
                  select(-input_url) ##this variable is long and annoying and we'll drop it right here
    
    
    # Now we can conduct some more serious analysis. First, let's look at distributions by state to
    # make sure everything makes sense
    final_data %>%        # here we are piping final_data into the ggplot function. This will be handy
                          # if we want to alter the data going into the plot
    ggplot() + 
      geom_histogram(aes(dist_num), fill = gorange) + 
      theme_minimal() +
      labs(x = "Distance from captial, km",
           y = "Number of SA3 areas") +
      facet_wrap(. ~ State) # this says: create an individual chart for each State
    
    
    # Let's free our scales from uniformity (there is only one thing changed from above in this bit)
    final_data %>%
    ggplot() + 
      geom_histogram(aes(dist_num), fill = gorange) + 
      theme_minimal() +
      labs(x = "Distance from captial, km",
           y = "Number of SA3 areas") +
      facet_wrap(~State, scales = "free") # <- hey that's the thing that changed!
    

# Now "write" a .csv file with our findings, so we can use it in a Grattan chart:
    write_csv(final_data, file = "final_data.csv")
    

    
    #--------------------------------------------------------------------------------------#
    #####  Additional activities #####
    
    # We'll try to get to this if we can. If we run out of time, you can play around in your
    # own time with the functions below.
    
    #--------------------------------------------------------------------------------------#    

  #simple OLS regression
      regression <- lm(av_income ~ time_mins, 
                         data = final_data)
      
      summary(regression)  # this function produces a summary of the regression defined above
    
  # Let's create a base object for our graphics:    
  base <- ggplot(final_data, aes(x=av_income, y=time_mins))

  # Create a scatter plot with an OLS line of best fit:
  base +
    geom_point() +
    geom_smooth(method = lm)
  
  # What if we used a different approach to identify a trend?:
  base +
    geom_point() +
    geom_smooth(method = loess) + # woah a wiggly line!
    theme_minimal() # changes the default look of the chart (it looks nicer, imho)
  
  
  # Create a scatter plot that distinuishes between states
  base +
    geom_point(aes(color = State)) + # woah colour!
    theme_minimal()
  
  
  # Let's look at this on a log-scale
  base +
    geom_point(aes(color = State)) +
    geom_smooth(method = lm, se = FALSE, color = gorange) +
    scale_y_log10(limits = c(1,5000)) +
    theme_minimal()
  # (note how scale_y_log10 sneakily, and helpfully, includes original 0 estimates at the bottom!)

  ##Looks like we see consistent negative relationships between time and income, except for Darwin. Why is this? And why does ACT have such a low absolute slope coefficient?
  ## Let's look at it with confidence intervals and marginal rugs:
  
  base +
    geom_point(aes(color = State)) +
    geom_smooth(aes(fill = State, color = State), method = lm, se = FALSE) +
    scale_y_log10(limits = c(1, 5000)) +
    theme_minimal()
  
  # Hmmm, NT has a positive relationship. That's ood. Let's look at the standard errors there:
  base +
    geom_point(aes(color = State)) +
    geom_smooth(aes(fill = State, color = State), method = lm, se = TRUE) +
    scale_y_log10(limits = c(1, 5000)) +
    theme_minimal()

  # lol there is no relationship between time-from-the-capital and income in NT
  # Let's JUST look at NT:
  final_data %>% 
    filter(State == "NT") %>%   # from the filtering we learnt up there somehwere!
    ggplot(aes(x = av_income, y = time_mins)) +
    geom_point(aes(color = State)) +
    geom_smooth(aes(fill = State, color = State), method = lm, se = TRUE) +
    scale_y_log10(limits = c(1, 5000)) +
    theme_minimal()
  
  
##Well the NT confidence intervals are crazy. We should take this into consideration when we make any statements about driving time and income!
##ALso looks like QLD has its maximum interval close to zero. Would be interesting to look at this with a fixed effects regression or something of the like.

  
  
  
  #--------------------------------------------------------------------------------------#
  #####  SIMPLE MAPPING OF DATA  #####
  #--------------------------------------------------------------------------------------#      
  
# Plotting a map is ~just like~ plotting a scatter-plot. 
  # We use x-coordinates as 
# geocode(location, output = c("latlon", "latlona", "more", "all"),
#         source = c("google", "dsk"), messaging = FALSE, force = ifelse(source ==
#                                                                          "dsk", FALSE, TRUE), sensor = FALSE, override_limit = FALSE,
#         client = "", signature = "", nameType = c("long", "short"), data)


latlons <-  geocode_url(paste0(data$name, ", " , data$State, ", Australia"),
                        privkey = internkey)
                    
final_data <- bind_cols(final_data, latlons)  ## this is combining the latlons data to our final_data

aus.map <- map_data("world") %>% 
           filter(region == "Australia")

# Let's make Australia
australia <- 
  ggplot() +
  geom_polygon(data = aus.map, aes(x=long, y = lat, group = group), fill="grey", color="white", linetype="solid", alpha=0.6) +
  theme_void() +
  ylim(-44,-10) + xlim(113,154)

# And view austrlia:
  print(australia)

# Now we overlay our income and geographical data onto our little Australia
  incomemap <-  australia + 
                geom_point(data = final_data, 
                           aes(x = lng, y = lat, size = av_income, color = State), 
                           show.legend = T, alpha=.4) +
                scale_size_continuous(range = c(0.00001, 10))
  
  
  incomemap  


  # Cool! There looks to be a problem with a QLD SA3 being plonked in Sydney. Can you tell me what happened there?
  

  # There are plenty of other resources about mapping (and about everything we've gone through),
  # but we need to end the class now. 
  
  
  
  