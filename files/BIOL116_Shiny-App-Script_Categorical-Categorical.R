# Introduction & Instructions

  # This script is intended as an alternative to using the BIOL 116 Shiny App. 
  # You can use it to produce figures, calculate descriptive statistics, and 
  # perform statistical analyses on your data. 
  
  # This specific script is intended to be used if both your response (dependent) 
  # and explanatory (independent) variables are **categorical** 
  
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
  # install.packages("ggmosaic")
  # install.packages("dplyr")
  # install.packages("stats")

  # Load required packages

  library("ggplot2")  
  library("ggmosaic")
  library("dplyr") 
  library("stats")
    
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
    caption <- "My Caption" # Replace with your desired caption
    
    # Factoring the Categorical Variables
 
    my_data$x_var <- factor(my_data[,colnames(my_data) == x_var])
    my_data$y_var <- factor(my_data[,colnames(my_data) == y_var])
    
    #Removing Original Unfactored Variable to Avoid Duplicates
    
    drop <- c(x_var, y_var)
    my_data = my_data[,!(names(my_data) %in% drop)]
    
    # Mosaic Plot
          
    my_data %>%
      ggplot() +
      geom_mosaic(aes(x = product(x_var), fill = y_var)) + # defines the variables 
      scale_y_continuous(breaks = seq(0, 1, by = 0.2)) + # makes adjustments to the y axis 
      labs(x = x_label, 
           y = 'Relative frequency', 
           fill = y_label,
           caption = caption) + # defines axes labels and figure caption
      theme(
        panel.background = element_blank(), # no colour for plot area
        plot.background = element_blank()) # no colour for whole chart incl. behind axes
    
# Step 5: Calculating Descriptive Statistics
    
    # Creating a Frequency Table for Specified Variables 
    
    stats <- my_data %>% 
      select(x_var, y_var) 
    
    # Renaming the Factored Variables to the Original Variable Name
    
    colnames(stats)[which(names(stats) == "x_var")] <- x_var
    colnames(stats)[which(names(stats) == "y_var")] <- y_var
    
    # Displaying the Final Frequency Table
    
    table(stats) 

# Step 6: Performing Statistical Analyses

    # Fisher's Exact Test 
    # (if you have two categorical variables each with 2 groups)
    
    fisher.table <- xtabs(~ y_var + x_var, data = my_data) 
    # Defines the variables and data to use for the test
    
    fisher.results <- fisher.test(fisher.table) 
    # Stores the results in a table
    
    fisher.results 
    # Displays the results of the Fisher's Test in the lower left pane (Console)

    # Code for Chi-Square Contingency Analysis 
    # (if you have two categorical variables and at least one of them has >2 groups)

    chi.table <- xtabs(~ y_var + x_var, data = my_data) 
    # Defines the variables and data to use for the test
    
    chisq.results <- chisq.test(chi.table) 
    # Stores the results in a table
    
    chisq.results 
    # Displays the results of the Fisher's Test in the lower left pane (Console)
    
    