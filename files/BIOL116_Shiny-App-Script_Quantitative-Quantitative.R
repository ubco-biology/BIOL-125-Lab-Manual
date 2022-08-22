# Introduction & Instructions

  # This script is intended as an alternative to using the BIOL 116 Shiny App. 
  # You can use it to produce figures, calculate descriptive statistics, and 
  # perform statistical analyses on your data. 

  # This specific script is intended to be used if your response (dependent) and 
  # explanatory (independent) variables are **both quantitative.** 

  # Alongside each line of code are comments describing what to do or what the 
  # code is doing. 
  # Be sure to consult the rubric to ensure you have done everything that was 
  # asked for your assignment!
                                                                                             
# Step 1: Set Your Working Directory
                                                                                             
  # Follow the instructions at 
  # https://ubco-biology.github.io/Procedures-and-Guidelines/set-a-working-directory-in-rstudio.html 
  # in the Procedures and Guidelines Document to set your working directory. 
                                                                                             
# Step 2: Installing & Loading Required Packages
                                                                                             
  # Remove the # from the following lines of code, 
  # and copy the code into the RStudio Console (lower left pane of RStudio) 
  # to install the necessary packages. If you're on a lab computer, you may need
  # need to do this at each log in.
                                                                                               
    # install.packages("ggplot2")
    # install.packages("dplyr")
    # install.packages("broom")

  # Load required packages
                                                                                      
        library("ggplot2")  
        library("dplyr")  
        library("broom")
                                                                                             
# Step 3: Uploading Your Data
        
    # Make sure your data is saved as a UTF-8 CSV file and 
    # is located within your working directory.
        
    # Replace `insert-data-file-name-here.csv` with the file name that your data 
    # is saved as to your computer
        
    my_file <- "insert-data-file-name-here.csv" # assign file name to a variable
    my_data <- read.csv(file = my_file, header = TRUE) 
    # load csv into R as a dataframe assuming there's a header
                                                                                             
# Step 4: Visualizing Your Data
    
    x_var <- "X variable name" # Replace with the name of your x variable
    x_label <- "X label name" # Replace with your desired x axis label
    y_var <- "Y variable name" # Replace with the name of your x variable
    y_label <- "Y label name" # Replace with your desired y axis label
    caption <- "My caption" # Replace with your desired caption
                                                                                            
    # Scatterplot with Line of Best Fit
    
    ggplot(my_data, 
        aes(
          x = my_data[,colnames(my_data) == y_var], 
          y = my_data[,colnames(my_data) == y_var])) + # the variables to plot
    geom_point(color = 'black', alpha = 0.5, position = 'jitter') + 
      # creates the points, adjusts their transparency, and 
      # slightly moves each point so there is less overlap
    geom_smooth(method='lm') + # adds the line of best fit
    labs(x = x_label, y = y_label) + # axes labels
    theme(
        axis.line = element_line(colour = 'gray', size = .75), 
        # colour and size of axes
        panel.background = element_blank(), 
        # no colour for plot area
        plot.background = element_blank()) 
        # no colour for whole chart incl. behind axes
                                                                                             
# Step 5: Calculating Descriptive Statistics
    
    var1 <- my_data %>%
      summarise(
        n = n(), # calculates sample size
        Mean = round(mean(my_data[,colnames(my_data) == x_var], 
                          na.rm = T), 1), # calculates the mean
        SD = round(sd(my_data[,colnames(my_data) == x_var], 
                      na.rm = T), 2), # calculates the standard deviation
        Median = round(median(my_data[,colnames(my_data) == x_var], 
                              na.rm = T), 1), # calculates the median
        iqr = round(IQR(my_data[,colnames(my_data) == x_var], 
                        na.rm = T), 2)) %>% # calculates interquartile range
      as.data.frame() # creates a data frame of the descriptive stats  
    
    var2 <- my_data %>%
      summarise(
        n = n(), # calculates sample size
        Mean = round(mean(my_data[,colnames(my_data) == y_var], 
                          na.rm = T), 1), # calculates the mean
        SD = round(sd(my_data[,colnames(my_data) == y_var], 
                      na.rm = T), 2), # calculates the standard deviation
        Median = round(median(my_data[,colnames(my_data) == y_var], 
                              na.rm = T), 1), # calculates the median
        iqr = round(IQR(my_data[,colnames(my_data) == y_var], 
                        na.rm = T), 2)) %>% # calculates interquartile range
      as.data.frame() # creates a data frame of the descriptive stats  
                                                                                             
      stats <- full_join(var1, var2) #joins the two data frames
      
      # Renaming the Rows to Match Original Variable Names
      
      rownames(stats) <- c(x_var, y_var)
      
      #Displays the Descriptive Statistics
      
      stats 
                                                                                         
# Step 6: Performing Statistical Analyses
                                                                        
    cor <- cor.test(
      x = my_data[,colnames(my_data) == x_var], 
      y = my_data[,colnames(my_data) == y_var], # chooses the variables
            method = "pearson", # chooses correlation method
            conf.level = 0.95, # chooses confidence interval
            alternative = "two.sided") # chooses type of alternative hypothesis
                                                                                             
    cor.tidy <- tidy(cor) # makes the output easier to read
    cor.tidy # displays the output
                                                                                    
