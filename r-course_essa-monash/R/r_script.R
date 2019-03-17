# Introduction to R:
# ESSA Monash
# by Will Mackey



# Part 1: Basics ------------------------------------------------------
  
  # Writing a script --------------------------------------------------

    # Hello I am starting my R script
    
    goodnumber <- 119
    
    2 * goodnumber

  # Functions ---------------------------------------------------------
    
    # Combine
    c(3, 4, 5)
    
    # Mean
    mean(c(3, 4, 5))
    
    # In steps
    goodnumbers <- c(3, 4, 5)
    mean(goodnumbers)
    
    # Looking at some data
    print(mtcars)
    
    # Find out more about functions or data
    ?mtcars
    ?mean
    
    # Look at a single column with $
    print(mtcars$mpg)
    
    # Get the mean of a single column
    mean(mtcars$mpg)
    
    # Exercises
    # (try to work them out yourself)
    
    
  # Packages ---------------------------------------------------------
    
    # Install the tidyverse package
    #   (you will need internet for this)
    #   Remove the ## and run the line to install the package
    
    ## install.packages("tidyverse")

      
    # Load the package
    library(tidyverse)

    
    
# Part 2: Read ------------------------------------------------------    
    
    # Reading data into R -------------------------------------------
    # Tip: when you open quotation marks "", hit TAB to navigate to a
    #      file
    gapminder <- read_csv("data/gapminder.csv")
    
    # Looking at our data
    gapminder
    head(gapminder)

    
# Part 3: Visualise -------------------------------------------------

    # Simple plot
    ggplot(data = gapminder) + 
      aes(x = lifeExp, 
          y = gdpPercap) +
      geom_point()
    
    # Save your plot
    ggsave("atlas/simple_plot.pdf")
    
    
    # Adding more and assigning to p
    p <- ggplot(data = gapminder) +
      aes(x = lifeExp,
          y = gdpPercap,
          colour = continent,
          size = pop) +
      geom_point(alpha = 0.3) +
      geom_smooth() +
      scale_y_log10() +
      facet_wrap(~continent)
    
    # Plotting p
    p
    
    # Adding facets
    p_facet <- p +
         facet_wrap(~continent)
    
  
    
    # Exercise
    ggplot(data = gapminder) +
      aes(x = gdpPercap,
          y = pop,
          colour = lifeExp,
          group = country) +
      geom_point(alpha = 0.8, 
                 size = 2) +
      scale_y_log10() +
      scale_x_log10() +
      theme_dark()

    
    # Interactive charts with plotly
    
      # Install required package:
      # (you will need to remove the ##):
      
      ##  install.packages("plotly")
      library(plotly)
      
      # Take our p plot and put it inside ggplotly:
      ggplotly(p_facet)
      
    
    
    
    # Animation
      # Install required packages:
      # (you will need to remove the ##):
    
      ##  install.packages("gifski")
      ##  install.packages("png")
      ##  install.packages("gganimate")
    
      # Load gganimate package
      library(gganimate)
    
      # Create a normal ggplot:
      ggplot(data = gapminder) +
        aes(x = lifeExp,
            y = gdpPercap,
            colour = continent) +
        geom_point(size = 3) +
        scale_y_log10() +
        facet_wrap(~continent) +
        labs(title = "Year {round(frame_time, 0)}") +
        # This is the animation part:
        transition_time(year)
    

# Part 4: Transform -------------------------------------------------
    
  # Adding a column
    gapminder07 <- gapminder %>% 
      mutate(gdp = gdpPercap * pop)
    
  # Filter to keep only 2007
    gapminder07 <- gapminder07 %>% 
      filter(year == 2007)
    
  # Drop the variable gdpPercap
    gapminder07 <- gapminder07 %>% 
      select(-gdpPercap)
    
    
  # Doing this all together
    gapminder07 <- gapminder %>% 
      mutate(gdp = gdpPercap * pop) %>% 
      filter(year == 2007) %>% 
      select(-gdpPercap)

    
  # Exercise
    # Create dataset
    aus <- gapminder %>% 
      filter(country == "Australia") %>% 
      mutate(gdpPClife = gdpPercap / lifeExp)
    
    # Plot
    aus %>% 
      ggplot() +
      aes(gdpPClife,
          pop,
          colour = year) +
      geom_point() +
      geom_line()
  
    
    
# Part 5: Maps -----------------------------------------------------
    
    # Install the sf package
    #   Remove the ## and run the line to install the package
    ##  install.packages("sf")
    
    # Load sf package
    library(sf)
    
    # Read gapminder + map data
    gapmap <- read_rds("data/gapmapdata.Rds")
    
    # Look at the names
    names(gapmap)
    
    # Plot an empty map using geom_sf
    ggplot(gapmap) +
      aes(geometry = geometry) +
      geom_sf()
    
    # Save empty map
    ggsave("atlas/empty_world.pdf")
    
    # Add some data and remove lines between countries
    ggplot(gapmap) +
      aes(geometry = geometry,
          fill = gdpPercap) +
      geom_sf(lwd = 0)
    
    # Save map
    ggsave("atlas/world_gdp_map.pdf")
    
    
    # We can use pipes %>% and filter to look at specific areas
    # (or we can remove areas)
    # Remove Antarctica
    gapmap %>% 
      filter(country != "Antarctica") %>% 
      ggplot() +
      aes(geometry = geometry,
          fill = gdpPercap) +
      geom_sf(lwd = 0)
    
    # Only include the continent 'Americas'
    gapmap %>% 
      filter(continent == "Americas") %>% 
      ggplot() +
      aes(geometry = geometry,
          fill = gdpPercap) +
      geom_sf(lwd = 0)

    ggsave("atlas/americas_gdp.pdf")
    
    # We can generate multiple plots and present them together using
    # the gridExtra package
    
    #   Remove the ## and run the line to install the package:
    ##  install.packages("gridExtra")
    
    library(gridExtra)
    
    # Plot 1: americas by gdpPercap
    plot1 <- gapmap %>% 
      filter(continent == "Americas") %>% 
      ggplot() +
      aes(geometry = geometry,
          fill = gdpPercap/1000) +  # GDP per capita in thousands
      geom_sf(lwd = 0) +
      coord_sf(datum = NA) +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(subtitle = "GDP per capita (thousands)",
           fill = "")
    
    # Plot 2: americas by gdpPercap
    plot2 <- gapmap %>% 
      filter(continent == "Americas") %>% 
      ggplot() +
      aes(geometry = geometry,
          fill = lifeExp) +
      geom_sf(lwd = 0) +
      coord_sf(datum = NA) +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(subtitle = "Life expectancy",
           fill = "")
    
    # Arrange the two plots side-by-side: 
    americas_map  <- grid.arrange(plot1, plot2, ncol = 2,
                                  top = "GDP and life expectancy in the Americas, 2007")
    
    # View the plot
    americas_map
    
    # Save your plot
    ggsave("atlas/americas_map.pdf", plot = americas_map)
    
        
    
