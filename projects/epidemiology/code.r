setwd("D:/University of Edinburgh Masters/Sem 1/statistical-programming/projects/project2")

## Benjamin Capon - github.com/bcapon
## Tom Davies - github.com/tomdvies
## Cyrus Seyrafi - github.com/hanman453

## The code was completed collaboratively mainly during workshops with all 3 team
## members consistently present and contributing ideas, pushing to Github on Tom's
## laptop (to avoid git conflicts). All team members pulled their weight by making
## further contributions to the code in their own time at home when necessary. Coding,
## commenting/editing, and breaking down the problems were done in different ratios
## within the group but the time committed to the project and overall proportion of
## contributions are seen as equal amongst group members.

## This script uses NHS data on death counts during the 2020 COVID-19 pandemic
## to predict when individuals in England first contracted the disease. We consider
## only the first 150 rows provided in our data, base our results on the
## infection-to-death durations provided by the ISARIC study, and assume that all
## infections and deaths may take place in the first 310 days of 2020. To do this,
## we will use a simulation based method and iterate it until our fatal incident 
## rates match the death time distribution very well. The data will be plotted and
## our uncertainty will be  quantified by using a bootstrapping method only once 
## our results have converged.

## Preliminaries ##

## We lay the groundwork for our simulation by saving NHS data from engcov.txt
## as julian_dates and deaths_col, and initialising vectors t and deaths_vec to
## match dates with death counts.

# Read in the first 150 rows of data using 'header = TRUE' to specify that the
# first row are our column names.
data <- read.table("engcov.txt", header = TRUE)[1:150, ]

# Save Julian dates of the year (i.e. # days since 1 January 2020) as julian_dates.
julian_dates <- data[, "julian"]

# Save death counts per day as deaths_col, represented by the nhs column in data.
deaths_col <- data[, "nhs"]

# t will be a vector of the Julian dates of all n = 29442 deaths in the data. The
# rep function can do this in a one-liner.
t <- rep(julian_dates, deaths_col)

# Initialise deaths_vec, for which each element represents death count per Julian
# date up to day 310. We will assume infections and deaths can occur anywhere
# between days 1 and 310 on the Julian calender throughout this script.
deaths_vec <- numeric(310)
deaths_vec[julian_dates] <- deaths_col

# length(t) # For Debugging, should be equal to n (29442).

## Goodness-of-Fit ##

## We initialise a function, Pearson, to serve as our goodness-of-fit test to
## measure the discrepancy between simulated and actual death counts through a
## modified Pearson Statistic. The function takes predictions and actual dates of
## death in tabulated forms as inputs and returns the modified Pearson Statistic.
## We will use vectors, rather than for loops for example,to calculate this as it
## is the most efficient way of doing calculations like these in R.

pearson <- function(tabulated_predictions, tabulated_actual) {
  # N.B. pmax returns a vector of maximums of tabulated_predictions entries and 1.
  return(sum(((tabulated_actual - tabulated_predictions)**2) / pmax(1, tabulated_predictions)))
}

## Prediction ##

## We define the function, switch_distribution, to simulate individual death
## dates from individual infection dates when sign = 1, or to simulate
## individual infection dates from individual death dates when sign = -1,
## using the given ISARIC probability distribution of infection-to-death durations. 
## The function takes inputs of old_days (t0 or t), normed_duration_probs, the
## aforementioned sign, and returns a new_days vector. we will do this by adding 
## or subtracting a duration_sample, making sure the days remain between 1 and 310.

switch_distribution <- function(old_days, normed_duration_probs, sign = 1) {
  # Take an initial sample of durations from infection date to death or vice versa.
  duration_sample <- sample(1:80, length(old_days), replace = TRUE, prob = normed_duration_probs)

  # Update new_days - our estimates of the infection dates or death dates depending
  # on the sign.
  new_days <- old_days + sign * duration_sample

  # Slight chance infections to deaths dates go over 310 OR deaths to infections
  # date might also go under 1 , find these indices.
  ii <- which(new_days > 310 | new_days < 1)

  # We aim to make ii an empty vector and keep iterating until it is.
  while (length(ii) > 0) {
    # Undo above addition on ii indices to get the original new_days of interest.
    new_days[ii] <- new_days[ii] - sign * duration_sample[ii]

    # Re-sample all at once with a sample of the same length as ii.
    duration_sample[ii] <- sample(1:80, length(ii), replace = TRUE, prob = normed_duration_probs)

    # Add the new samples to give a new_days[ii] which is hopefully all in [1,130].
    new_days[ii] <- new_days[ii] + sign * duration_sample[ii]

    # Check if there are still days outside of [1,310] and carry out another
    # iteration if there is.
    ii <- ii[which(new_days[ii] > 310 | new_days[ii] < 1)]
  }

  # Return the new_days vector which contains only days between 1 and 310.
  return(new_days)
}

## Simulation ##

## We write a function, deconv, which performs an algorithm to return inferred fatal
## incidence rates from Covid deaths in English NHS hospitals. The function takes
## t, deaths, the number of repetitions we want to carry out n.rep (with default
## 100), a Boolean for if we want to conduct bootstrapping bs, and our simulated
## death days t0 (NULL until bootstrapping) as inputs. We output a history vector of
## modified Pearson statistics (P) calculated at the end of iteration, a matrix
## inft with columns representing t0 after every iteration, and t0 representing
## the final vector t0. We will do this through a simulation based method, starting
## off by assigning each victim a guessed time of infection and randomly drawing
## from the infection-to-death distribution until we match the death data closely.

deconv <- function(t, deaths, n.rep = 100, bs = FALSE, t0 = NULL) {
  # Based on the ISARIC study, the distribution of infection-to-death durations
  # follow a log normal distribution with a meanlog of 3.152 and a sdlog of 0.451.
  meanlog <- 3.152
  sdlog <- 0.451
  n <- length(t)

  # We calculate the probabilities of dying on a specific day by taking the dlnorm
  # in integer steps from 1 to 80. We will assume the duration between infection
  # and dying falls in this range and approximate the probabilities as rectangles
  # with a width of one and heights corresponding to dlnorm between 1 and 80.
  duration_probs <- dlnorm(1:80, mean = meanlog, sd = sdlog)

  # Normalise the duration_probs by dividing by the sum of the probabilities.
  normed_duration_probs <- duration_probs / sum(duration_probs)

  # Default case to cover is when t0 is NULL
  if (is.null(t0)) {
    # Carry out our first simulation converting death days to days of infection
    # using the duration probabilities we just calculated.
    t0 <- switch_distribution(t, normed_duration_probs, sign = -1)
  }

  # Copy deaths into original_deaths if bootstrapping, we will explain why down below.
  if (bs) {
    original_deaths <- deaths
  }

  # Initialise P_vec and inft. P_vec should have length n.rep representing P after
  # every rep and inft should have n.rep columns of t0 after each repetition where
  # t0 has 310 rows.
  P_vec <- rep(NA, n.rep)
  inft <- matrix(NA, 310, n.rep)
  
  # For convergence purposes, we will do this step n.rep times.
  for (rep in 1:n.rep) {
    # If bootstrapping, we treat the original_deaths as estimates of the expected
    # values of Poisson random variables and simulate Poisson data to use in
    # place of original_deaths.
    if (bs) {
      deaths <- rpois(length(original_deaths), original_deaths)
    }

    # Add the a new durations sample to our initial guesses for the days of
    # infection to get a vector of predicted death days.
    t_predicted <- switch_distribution(t0, normed_duration_probs)

    # Tabulate the vector of predicted death days to get the number of deaths per
    # day, assuming deaths occur from days 1 to 310 as mentioned previously.
    t_predicted_tabulated <- tabulate(t_predicted, 310)

    # Calculate a baseline modified Pearson statistic to see the goodness of
    # fit of our deaths per day compared to the actual figures.
    P <- pearson(t_predicted_tabulated, deaths)
    # Print the value of P before any fitting (but not when bootstrapping).
    if (rep == 1 && !bs) {
      cat("P at initialisation is", P) # For Debugging.
      cat("\n")
    }

    # Here we assign steps which will get smaller as iterations go on to improve
    # our convergence rate. N.B. if bootstrapping, t0 has converged and so we will
    # use the smallest steps.
    if (bs) {
      moves <- c(-2, -1, 1, 2)
    } else if (rep <= 50) {
      moves <- c(-8, -4, -2, -1, 1, 2, 4, 8)
    } else if (rep <= 75) {
      moves <- c(-4, -2, -1, 1, 2, 4)
    } else {
      moves <- c(-2, -1, 1, 2)
    }

    # We will sample n random moves all at once to maximise efficiency. These
    # random moves will help us try to match the real death distribution more
    # accurately.
    random_moves <- sample(moves, n, replace = TRUE)

    # Some may push us out from [1,310] we are confined to, find their indices.
    ii <- which(t_predicted + random_moves > 310 | t0 + random_moves < 1)

    # Here we aim to make ii an empty vector and keep iterating until it is.
    while (length(ii) > 0) {
      # Resample any random_moves pushing us out from [1,310].
      random_moves[ii] <- sample(moves, length(ii), replace = TRUE)
      # See if any random_moves[ii] will still push us out of [1,310].
      ii <- ii[which(t_predicted[ii] + random_moves[ii] > 310 | t0[ii] + random_moves[ii] < 1)]
    }

    # Loop over every single element of t0 in random order and we will propose
    # random moves and accept or decline them based on whether the modified Pearson
    # statistic has decreased or increased. 
    for (i in sample(1:n, n)) {
      # Remove the death from the old day before we make a random move.
      t_predicted_tabulated[t_predicted[i]] <- t_predicted_tabulated[t_predicted[i]] - 1
      # Update the death to the new day using the random move.
      t_predicted[i] <- t_predicted[i] + random_moves[i]
      # Add the death to the new day.
      t_predicted_tabulated[t_predicted[i]] <- t_predicted_tabulated[t_predicted[i]] + 1

      # Calculate a new modified Pearson statistic for comparison.
      P_updated <- pearson(t_predicted_tabulated, deaths)

      # If the fit is better, perfect! Accept P and update infection day at this index.
      if (P_updated < P) {
        P <- P_updated
        t0[i] <- t0[i] + random_moves[i]
      } else {
        # If not, revert to all previous versions so our our P does not increase!
        t_predicted_tabulated[t_predicted[i]] <- t_predicted_tabulated[t_predicted[i]] - 1
        t_predicted[i] <- t_predicted[i] - random_moves[i]
        t_predicted_tabulated[t_predicted[i]] <- t_predicted_tabulated[t_predicted[i]] + 1
      }
    }

    # If we are not bootstrapping, log the updated P value every 10 iterations.
    if (rep %% 10 == 0 && !bs) {
      cat("Updated P on iteration", rep, "is", P) # For Debugging.
      cat("\n")
    }

    # After each repetition, we assign our final P value to a vector of P's and assign
    # the final t0 to the rep'th column in inft.
    P_vec[rep] <- P
    inft[, rep] <- tabulate(t0, 310)

    ## We initiate the COVID-19 Incidents and Deaths plot to show our
    ## simulation of the predicted days of incidents, real or sampled deaths,
    ## and simulated deaths through each of the 100 iterations of our model.
    
    # Save an image if not bootstrapping.
    if(!bs){
      png(filename = paste0("frames/frame_", rep, ".png"), 
          width = 1920, height = 1080, res = 300)
    }
    
    # Initiate the iterative plot, t0 tabulated for days from 1:310.
    plot(1:310, inft[, rep],

      # Plots dates of COVID contraction as Incidents in black.
      type = "l", col = "black", xlab = "Day", ylab = "Frequency",

      # Titles plot 'Iteration #' or 'Bootstrap #' depending on if bootstrapping.
      main = paste(if (bs) "Bootstrap" else "Iteration", rep, "- COVID-19 Incidents and Deaths"),
      
      # Set a ylim to improve plot visualisation.
      ylim = c(0,1500)
    )

    # Plots real dates of death (or sampled dates of death when bootstrapping) in red.
    lines(1:310, deaths, col = "red", lwd = 1.5)

    # Plots simulated dates of death in slightly lighter blue over the top for comparison.
    lines(1:310, t_predicted_tabulated, col = rgb(0, 0, 1, alpha = 0.7), lwd = 1.5)
    
    # Initialise vertical line to represent day 84, the first day of lockdown in the UK.
    abline(v = 84, col = "black", lty = 2)
    
    # Initiates a legend, explaining the three lines and accounts for if bs = TRUE.
    legend("topright",
      legend = c("Incidents", if (bs) "Sampled Deaths" else "Real Deaths", "Simulated Deaths", "UK Lockdown"),
      col = c("black", "red", rgb(0, 0, 1, alpha = 0.7), "black"), # Line colours for the lines.
      lty = c(1, 1, 1, 2) # Line types: solid, solid, solid, dashed.
    )
    
    if(!bs){
      dev.off()
    }
  }

  return(list(P = P_vec, inft = inft, t0 = t0))
}

## Execution ##

## We run the simulation twice: first without bootstrapping, then with bootstrapping 
## to quantify the uncertainty of out estimate for t0.

# Set our number of repetitions to 100, as in the default, to prove convergence has 
# definitely occurred and so we get an excellent quantification of our uncertainty. 
n.rep <- 100

# Run the simulation without bootstrapping first to get a converged t0.
predictions <- deconv(t, deaths_vec, n.rep, bs = FALSE)
cat("Beginning Bootstrapping", "\n")

# Run the simulation with bootstrapping, supplying our converged t0 this time.
predictions_bs <- deconv(t, deaths_vec, n.rep, bs = TRUE, t0 = predictions$t0)

## Results ##

## We present our results through two final plots: (1) COVID-19 Incidents and
## Deaths, which presents our final predictions for COVID-19 incidents and deaths
## per day across 2020, and (2) Modified Pearson Statistic by Iteration, which
## illustrates the fit of the simulation model to the real death data.
## N.B. our plots are best viewed when they are in full screen.

# We will choose to set our confidence interval to 95%.
confidence_interval <- 0.95

# Find upper and lower quantiles for initial infections across bootstrap samples.
lower_quantile <- apply(predictions_bs$inft, 1, quantile, 1 - confidence_interval)
upper_quantile <- apply(predictions_bs$inft, 1, quantile, confidence_interval)

# Tabulate final vector of incident dates to find infection counts per day.
t0_vec <- tabulate(predictions$t0, 310)

# Define range of the x-axis as the days from 1:310.
x <- 1:310

# Initialise COVID-19 Incidents and Deaths plot.
plot(x, y = t0_vec, type = "l", xlab = "Day", ylab = "Frequency", 
  main = "COVID-19 Incidents and Deaths")

# Initialise grey polygon to represent range of 95% confidence interval per day.
polygon(c(x, rev(x)), c(upper_quantile, rev(lower_quantile)), 
  col = rgb(0, 0, 0, alpha = 0.3), border = NA)

# Initialise black line to represent predicted incidents.
lines(x, y = t0_vec, type = "l", col = "black")

# Initialise red line to represent real deaths.
lines(x, y = deaths_vec, type = "l", col = "red")

# Initialise vertical line to represent day 84, the first day of lockdown in the UK.
abline(v = 84, col = "black", lty = 2)

# Initialise a legend to explain the lines and confidence interval on the plot.
legend("topright",
  legend = c("Incidents", "Real Deaths", "Confidence Interval", "UK Lockdown"),
  col = c("black", "red", NA, "black"), # Line colours for the lines and NA for the polygon.
  lty = c(1, 1, NA, 2), # Line types: solid, solid, none (polygon), dashed.
  pch = c(NA, NA, 22, NA), # 22 (filled square) for the polygon, NA for others.
  pt.bg = c(NA, NA, rgb(0.5, 0.5, 0.5, alpha = 0.3), NA), # Fill colour for the polygon.
  pt.cex = c(1, 1, 2, 1), # Adjust size for the polygon box.
  border = c(NA, NA, NA, NA) # No border for the filled box.
)

# Create a new window if running from the CLI (can freely comment out if in RStudio).
dev.new()

# Initialise Modified Pearson Statistic by Iteration plot. The plot shows how the 
# Modified Pearson Statistic, which represents our data's goodness-of-fit, 
# decreases through the simulation's iterations to convergence.
plot(
  x = 1:n.rep, y = predictions$P, type = "l", xlab = "Iteration",
  ylab = "Modified Pearson Statistic",
  main = "Modified Pearson Statistic by Iteration", col = "blue"
)
