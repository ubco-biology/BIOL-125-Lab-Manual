# Introduction & Instructions

# This script is intended as an alternative to using the BIOL 116 Shiny App. 
# As such, you can use it to produce figures, calculate descriptive statistics, and perform statistical analyses on your data. 
# This specific script is intended to be used if both your response (dependent) and explanatory (independent) variables are **categorical** 
# Alongside each line of code are comments describing what to do or what the code is doing. 
# **Things you are required to do are indicated with asterisks (*).**
# Use the 'Run' button located on the top right of this panel in RStudio to run each line of code
# Be sure to consult the rubric to ensure you have done everything that was asked for your assignment!
      
# Step 1: Set Your Working Directory
      
    # Follow the instructions [here](https://ubco-biology.github.io/Procedures-and-Guidelines/set-a-working-directory-in-rstudio.html) in the Procedures and Guidelines Document to set your working directory. 
    
# Step 1: Installing & Loading Required Packages
    
    # * Remove the # from the following lines of code, and copy the code into the RStudio Console (lower left pane of RStudio) to install the necessary packages:
      
        # install.packages("ggplot2")
        # install.packages("ggmosaic")
        # install.packages("dplyr")
        # install.packages("stats")
    
    # Once the required packages are installed, you need to load these packages within your R script. The code below will load these packages
  
        library("ggplot2")  
        library("ggmosaic")
        library("dplyr") 
        library("stats")
    
# Step 3: Uploading Your Data
    
    # Make sure your data is saved as a UTF-8 CSV file and is located within your working directory. In other words, your data should be saved in the same folder as the R Project you created in Step 1. 
    
    # * Replace `insert-data-file-name-here.csv` with the file name that your data is saved as to your computer
    
    my_data <- read.csv(file = "data.csv", header = TRUE) 
    
    # This code reads your CSV file into R
    # header = TRUE if the first row of values in your data has header information (aka column names)
    # header = FALSE if there is no header information
    
# Step 4: Producing Figures
    
    # * Replace **TWO** `insert-x-variable-name-here` with your explanatory (independent) variable's name
    # * Replace **TWO** `insert-y-variable-name-here` with your response (dependent) variable's name
    
    my_data$insert-x-variable-name-here <- as.factor(my_data$insert-x-variable-name-here) # factoring the explanatory variable
    my_data$insert-y-variable-name-here <- as.factor(my_data$insert-y-variable-name-here) # factoring the response variable
    
    # **Code for Creating a Mosaic Plot**
      
        # * Replace **TWO** `insert-x-variable-name-here` with your explanatory (independent) variable's name
        # * Replace **TWO** `insert-y-variable-name-here` with your response (dependent) variable's name
          
            my_data %>%
              ggplot() +
              geom_mosaic(aes(x = product(insert-x-variable-name-here), fill = insert-y-variable-name-here)) + # defines the variables 
              scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + # makes adjustments to the y axis 
              labs(x = "insert-x-variable-name-here", y = 'Relative frequency', fill = "insert-y-variable-name-here") + # axes labels
              theme(
                panel.background = element_blank(), # no colour for plot area
                plot.background = element_blank()) # no colour for whole chart incl. behind axes
    
        # Your plot will be located in the lower right pane of RStudio. To save your plot, click the 'Export' button.
        # If you are feeling ambitious, you are welcome to utilize [this resource](https://haleyjeppson.github.io/ggmosaic/) to alter the script to further customize your figures. 
    
# Step 5: Calculating Descriptive Statistics
    
    # * Replace **ONE** `insert-x-variable-name-here` with your explanatory (independent) variable's name
    # * Replace **ONE** `insert-y-variable-name-here` with your response (dependent) variable's name
    
    stats <- my_data %>%
      select(insert-x-variable-name-here, insert-y-variable-name-here) # defines which variables to display
    
    table(stats) # displays the descriptive statistics in a table in the lower left pane (Console)

    
# Step 6: Performing Statistical Analyses
    
    # The appropriate statistical test to assess for a relationship between two categorical variables depends on how many groups each categorical variable has. If BOTH categorical variables have exactly two groups then Fisher's Exact Test is best. If either of the categorical variables has more than two groups then a Chi-Square Contingency Analysis should be performed. 

    # * Once you have decided which test is best for you data, **DELETE** the R chunk for the opposite statistical test so it is not included in your final PDF output. 
    # * For example, if both categorical variables have two groups and you want to perform a Fisher's Exact Test: **KEEP** the R chunk that performs the Fisher's Exact Test but **DELETE** the R chunk that performs the Chi-Square Contingency Analysis

        # **Fisher's Exact Test**
      
              # * Replace **ONE** `insert-x-variable-name-here` with your explanatory (independent) variable's name
              # * Replace **ONE** `insert-y-variable-name-here` with your response (dependent) variable's name
    
              fisher.table <- xtabs(~ insert-y-variable-name-here + insert-x-variable-name-here, data = my_data) # Defines the variables and data to use for the test
              fisher.results <- fisher.test(fisher.table) # Stores the results in a table
              fisher.results # Displays the results of the Fisher's Test in the lower left pane (Console)

    
        # **Code for Chi-Square Contingency Analysis**
      
              # * Replace **ONE** `insert-x-variable-name-here` with your explanatory (independent) variable's name
              # * Replace **ONE** `insert-y-variable-name-here` with your response (dependent) variable's name

              chi.table <- xtabs(~ insert-y-variable-name-here + insert-x-variable-name-here, data = my_data) # Defines the variables and data to use for the test
              chisq.results <- chisq.test(chi.table) # Stores the results in a table
              chisq.results # Displays the results of the Fisher's Test in the lower left pane (Console)
    
    