# Introduction & Instructions

  # This script is intended as an alternative to using the BIOL 116 Shiny App. 
  # You can use it to produce figures, calculate descriptive statistics, and 
  # perform statistical analyses on your data. 

  # This specific script is intended to be used if your response (dependent) 
  # variable is **quantitative** and your explanatory (independent) variable is 
  # **categorical** 

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
    # install.packages("car")
    
    # Load required packages
    
    library("ggplot2")  
    library("dplyr") 
    library("car")
    
# Step 3: Uploading Your Data
    
    # Make sure your data is saved as a UTF-8 CSV file and 
    # is located within your working directory.
    
    # Replace `insert-data-file-name-here.csv` with the file name that your 
    # data is saved as to your computer

    my_file <- "insert-data-file-name-here.csv" # assign file name to a variable
    my_data <- read.csv(file = my_file, header = TRUE) 
    # load csv into R as a dataframe assuming there's a header
    
# Step 4: Visualizing Your Data
    
    x_var <- "X variable name" # Replace with the name of your x variable
    x_label <- "X label name" # Replace with your desired x axis label
    y_var <- "Y variable name" # Replace with the name of your x variable
    y_label <- "Y label name" # Replace with your desired y axis label
    caption <- "My caption" # Replace with your desired caption
    
    # Defining a Function to Calculate the Confidence Interval 
    
    confint.fun.ttest <- function(x, conf = 0.95){
      return(data.frame(Mean = mean(x, na.rm = T),
                        ymin  = t.test(x, conf.level = conf)$conf.int[1],
                        ymax = t.test(x, conf.level = conf)$conf.int[2]))}

    # Boxplot

    ggplot(my_data, 
      aes(
        x = factor(my_data[,colnames(my_data) == x_var]), 
        y = my_data[,colnames(my_data) == y_var])) + # the variables to plot
      geom_boxplot(width= 0.5, fill = 'lightgrey') + 
        # creates the boxplot, defines its size and colour
        stat_summary(fun.data = confint.fun.ttest, geom = 'errorbar', 
        # creates the error bars for the confidence intervals, defines their  
        # color, size, and position
                      colour = 'black', width = 0.07, 
                      position = position_nudge(x = 0.4)) +
        stat_summary(fun = mean, 
        # adds the mean to the graph as a point, defines colour, size, 
        # and position of the point
                      geom = 'point', 
                      colour = 'firebrick', 
                      size = 2, 
                      position = position_nudge(x = 0.4)) +
        labs(x = x_label, y = y_label) + # axes labels
        theme(
          axis.line = element_line(colour = 'gray', size = .75), 
          # colour and size of axes
          panel.background = element_blank(), # no colour for plot area
          plot.background = element_blank()) 
          # no colour for whole chart incl. behind axes

    # Stripchart
                    
    ggplot(my_data,
      aes(
        x = factor(my_data[,colnames(my_data) == x_var]), 
        y = my_data[,colnames(my_data) == y_var])) + # the variables to plot 
      geom_jitter(colour = 'black', size = 3, shape = 1, width = 0.1) + 
      # creating data points, slightly shifting them to reduce overlap, 
      # defines colour and size of points
        stat_summary(fun.data = confint.fun.ttest, geom = 'errorbar', 
      # creates the error bars for the confidence intervals, defines their color, 
      # size, and position
                      colour = 'black', width = 0.07, 
                      position = position_nudge(x = 0.15)) +
        stat_summary(fun = mean, 
      # adds the mean to the graph as a point, defines colour, size, 
      # and position of the point
                      geom = 'point', 
                      colour = 'firebrick', 
                      size = 2, 
                      position = position_nudge(x = 0.15)) +
        labs(x = x_label, y = y_label) + # axes labels
        theme(
          axis.line = element_line(colour = 'gray', size = .75),
          panel.background = element_blank(),
          plot.background = element_blank())

# Step 5: Calculating Descriptive Statistics

    stats <- my_data %>%
              group_by(my_data[,colnames(my_data) == x_var]) %>% 
              # groups data by your categorical variable
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
    
    # Renaming the X Variable Shown in The Output
    
    colnames(stats)[which(names(stats) == "my_data[, colnames(my_data) == x_var]")] <- x_var

    # Displaying the Descriptive Stats
    
    stats 

# Step 6: Performing Statistical Analyses
    
    # t-Test (if your categorical variable has exactly 2 groups)

        t.test(
          my_data[,colnames(my_data) == y_var] ~ my_data[,colnames(my_data) == x_var], 
          data = my_data, # chooses the variables and data set
               var.equal = TRUE, # assumption of equal variance is met
               conf.level = 0.95) # defines the confidence interval 

    # ANOVA (if your categorical variable has > 2 groups)

        lm <- lm(
          my_data[,colnames(my_data) == y_var] ~ my_data[,colnames(my_data) == x_var], 
          data = my_data) # creates a linear model 
        lm.anova <- anova(lm) # performs the ANOVA
        lm.anova # displays the results 
