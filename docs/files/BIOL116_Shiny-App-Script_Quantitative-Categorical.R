# Introduction & Instructions

# This script is intended as an alternative to using the BIOL 116 Shiny App. 
# As such, you can use it to produce figures, calculate descriptive statistics, and perform statistical analyses on your data. 
# This specific script is intended to be used if your response (dependent) variable is **quantitative** and your explanatory (independent) variable is **categorical** 
# Alongside each line of code are comments describing what to do or what the code is doing. 
# **Things you are required to do are indicated with asterisks (*).**
# Use the 'Run' button located on the top right of this panel in RStudio to run each line of code
# Be sure to consult the rubric to ensure you have done everything that was asked for your assignment!

      
# Step 1: Set Your Working Directory
      
    # Follow the instructions [here](https://ubco-biology.github.io/Procedures-and-Guidelines/set-a-working-directory-in-rstudio.html) in the Procedures and Guidelines Document to set your working directory. 
    
# Step 2: Installing & Loading Required Packages
    
    # * Remove the # from the following lines of code, and copy the code into the RStudio Console (lower left pane of RStudio) to install the necessary packages:
      
    # install.packages("ggplot2")
    # install.packages("dplyr")
    # install.packages("car")
    
    # Once the required packages are installed, you need to load these packages. To load the packages, run the code below.  
    
    library("ggplot2")  
    library("dplyr") 
    library("car")
    
# Step 3: Uploading Your Data
    
    # Make sure your data is saved as a UTF-8 CSV file and is located within your working directory. In other words, your data should be saved in the same folder as the R Project you created in Step 1. 
    
    # * Replace `insert-data-file-name-here.csv` with the file name that your data is saved as to your computer

    my_data <- read.csv(file = "insert-data-file-name-here.csv", header = TRUE) 
    
    # This code reads your CSV file into R
    # header = TRUE if the first row of values in your data has header information (aka column names)
    # header = FALSE if there is no header information

# Step 4: Producing Figures
    
    # The two best options for displaying a categorical explanatory variable with a quantitative response variable are a boxplot and stripchart. Typically a stripchart is used when there are < 20 data points within each group, and a boxplot is used if there is > 20 data points in each group. 
    
    # * Replace **TWO** `insert-x-variable-name-here` with your explanatory (independent) variable's name

    my_data$insert-x-variable-name-here <- as.factor(my_data$insert-x-variable-name-here) # factoring the categorical variable
    
    confint.fun.ttest <- function(x, conf = 0.95){ # defining a function to calculate the confidence interval
      return(data.frame(Mean = mean(x, na.rm = T),
                        ymin  = t.test(x, conf.level = conf)$conf.int[1],
                        ymax = t.test(x, conf.level = conf)$conf.int[2]))}

    # **Code for Creating a Boxplot**

        # * Replace **TWO** `insert-x-variable-name-here` with your explanatory (independent) variable's name
        # * Replace **TWO** `insert-y-variable-name-here` with your response (dependent) variable's name

        ggplot(my_data, 
            aes(x = insert-x-variable-name-here, y = insert-y-variable-name-here)) + # the variables to plot
            geom_boxplot(width= 0.5, fill = 'lightgrey') + # creates the boxplot, defines its size and colour
                stat_summary(fun.data = confint.fun.ttest, geom = 'errorbar', # creates the error bars for the confidence intervals, defines their color, size, and position
                             colour = 'black', width = 0.07, 
                             position = position_nudge(x = 0.4)) +
                stat_summary(fun = mean, # adds the mean to the graph as a point, defines colour, size, and position of the point
                             geom = 'point', 
                             colour = 'firebrick', 
                             size = 2, 
                             position = position_nudge(x = 0.4)) +
            labs(x = "insert-x-variable-name-here", y = "insert-y-variable-name-here") + # axes labels
            theme(
              axis.line = element_line(colour = 'gray', size = .75), # colour and size of axes
              panel.background = element_blank(), # no colour for plot area
              plot.background = element_blank()) # no colour for whole chart incl. behind axes

    # **Code for Creating a Stripchart**

        # * Replace **TWO** `insert-x-variable-name-here` with your explanatory (independent) variable's name
        # * Replace **TWO** `insert-y-variable-name-here` with your response (dependent) variable's name
                    
        ggplot(my_data,
               aes(x = insert-x-variable-name-here, y = insert-y-variable-name-here)) + # the variables to plot 
                geom_jitter(colour = 'black', size = 3, shape = 1, width = 0.1) + # creating data points, slightly shifting them to reduce overlap, defines colour and size of points
                stat_summary(fun.data = confint.fun.ttest, geom = 'errorbar', # creates the error bars for the confidence intervals, defines their color, size, and position
                             colour = 'black', width = 0.07, 
                             position = position_nudge(x = 0.15)) +
                stat_summary(fun = mean, # adds the mean to the graph as a point, defines colour, size, and position of the point
                             geom = 'point', 
                             colour = 'firebrick', 
                             size = 2, 
                             position = position_nudge(x = 0.15)) +
            labs(x = "insert-x-variable-name-here", y = "insert-y-variable-name-here") + # axes labels
            theme(
              axis.line = element_line(colour = 'gray', size = .75),
              panel.background = element_blank(),
              plot.background = element_blank())

        # Your plot will be located in the lower right pane of RStudio. To save your plot, click the 'Export' button.
        # If you are feeling ambitious, you are welcome to utilize [this resource](https://appsilon.com/ggplot2-boxplots/) for boxplots or this [one](http://www.sthda.com/english/wiki/ggplot2-scatterplot-easy-scatter-plot-using-ggplot2-and-r-statistical-software) for stripcharts to alter the script to further customize your figures. 

# Step 5: Calculating Descriptive Statistics

    # * Replace **ONE** `insert-x-variable-name-here` with your explanatory (independent) variable's name
    # * Replace **FOUR** `insert-y-variable-name-here` with your response (dependent) variable's name

    stats <- my_data %>%
              group_by(insert-x-variable-name-here) %>% # groups data by your categorical variable
            summarise(
              n = n(), # calculates sample size
              Mean = round(mean(insert-y-variable-name-here, na.rm = T), 1), # calculates the mean
              SD = round(sd(insert-y-variable-name-here, na.rm = T), 2), # calculates the standard deviation
              Median = round(median(insert-y-variable-name-here, na.rm = T), 1), # calculates the median
              iqr = round(IQR(insert-y-variable-name-here, na.rm = T), 2)) %>% # calculates the interquartile range
            as.data.frame() # creates a data frame of the descriptive stats    
    
    stats # displays the data frame


# Step 6: Performing Statistical Analyses

# The appropriate statistical test to assess for a relationship between a categorical and quantitative variable depends on how many groups the categorical variable has. If the categorical variable has exactly two groups the t-test is best. If the categorical variable has more than two groups then an ANOVA should be performed. 

    # * Once you have decided which test is best for you data, **DELETE** the R chunk for the opposite statistical test so it is not included in your final PDF output. 
    # * For example, if your categorical variable has two groups and you want to perform a t-test: **KEEP** the R chunk that performs the t-test but **DELETE** the R chunk that performs the ANOVA 

    # **Code for t-Test**

        # * Replace **ONE** `insert-x-variable-name-here` with your explanatory (independent) variable's name
        # * Replace **ONE** `insert-y-variable-name-here` with your response (dependent) variable's name

        t.test(insert-y-variable-name-here ~ insert-x-variable-name-here, data = my_data, var.equal = TRUE, conf.level = 0.95)

        # chooses the variables and data set, var.equal = TRUE indicates that the assumption of equal variance is met, defines the confidence interval used


    # **Code for ANOVA**

        # * Replace **ONE** `insert-x-variable-name-here` with your explanatory (independent) variable's name
        # * Replace **ONE** `insert-y-variable-name-here` with your response (dependent) variable's name

        lm <- lm(insert-y-variable-name-here ~ insert-x-variable-name-here, data = my_data) # creates a linear model using the variables from your data set
        lm.anova <- anova(lm) # performs the ANOVA
        lm.anova # displays the results 
