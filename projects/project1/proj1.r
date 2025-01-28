# Benjamin Capon - S2746256
# Tom Davies - S2210543
# Cyrus Seyrafi - S2766504

## The code was completed collaboratively mainly during workshops with all 3 team members consistently
## present and contributing ideas, pushing to Github on Tom's laptop (to avoid git conflicts). 
## All team members pulled  their weight by making further contributions to the code in their 
## own time at home when necessary. Coding, commenting/editing, and breaking down the problems 
## were done in different ratios within the group but the time committed to the project 
## and overall proportion of contributions are seen as equal amongst group members.

## This script uses a p-th order Markov model to generate random sequences of text based on word patterns
## in James Joyce's Ulysses. This will be done by using a 'small language model’ in which we will write
## a far simpler and much smaller version of the typical and revolutionary large language models. A high 
## level outline of what the code is doing will be written in parts below each question, with line-by-line 
## comments and function descriptions to show how the task is carried out.

## Q1, Q2, Q3 ##

## We begin by scanning the text from our repository, skipping the introductory information in the header
## We then save this text as a vector a

# Import text, skipping chapter index/footnote
a <- scan("4300-0.txt",
  what = "character", skip = 73, nlines = 32858 - 73,
  fileEncoding = "UTF-8"
)

# Replace "_(" with ""
a <- gsub("_(", "", a, fixed = TRUE)
#a

## Q4 ##

## We define the function split_punct to take a word vector (e.g. a) and a punctuation vector 
## (e.g. c(",", ".", ";", "!", ":", "?")) and split the punctuation marks from their respective 
## entries in the word vector to return a new vector
## To do this, we will find the indices of words which contain punctuation and use these to help
## split the word vector into a new separated one

split_punct <- function(word_vec, punctuation) {
  # Copy the old word_vec to a new_word_vec which we will modify (we don't want to modify word_vec
  # as we would like to keep this for operations)
  new_word_vec <- word_vec
  
  # Loop over punctuation to extract from word_vec
  for (punct in punctuation) {
    # Find the indices of words with punctuation
    changed_indices <- grep(punct, new_word_vec, fixed = TRUE)
    # When adding the nth punctuation back in, we need send it to index + n
    changed_indices <- changed_indices + 1:length(changed_indices)
    # Remove punctuation, and replace with ""
    new_word_vec <- gsub(punct, "", new_word_vec, fixed = TRUE)
    # New vector to store the punctuation + words
    out_vec <- rep("", length(changed_indices) + length(new_word_vec))
    # Set the punctuation elements
    out_vec[changed_indices] <- punct
    # Set the words back in
    out_vec[-changed_indices] <- new_word_vec
    # Update our stored copy
    new_word_vec <- out_vec
  }
  
  # Finally, return our new_word_vec with the punctuation separated
  return(new_word_vec)
}

## Q5 ##

## We apply our split_punct function to a for various punctuation marks and replace a with it

punctuation <- c(",", ".", ";", "!", ":", "?")
a <- split_punct(a, punctuation)
#a # For Debugging

## Q6 ##

## We create vector b, composed of the m = 1000 most common words in lower_a (i.e. the text)
## We find the frequencies of all the unique words in the text, and sort them to find top m = 1000 for b


# Start off by creating a word vector lower_a in which every word in a is lowercase using the built
# in tolower() function. This will be used later to lets keep this as a global vector.
lower_a <- tolower(a)

# We create a most_common function which returns a vector of about m = 1000 most common words in 
# the input word_vec
# m is an optional argument which can be set to any number
# We will do this by matching the word_vec to the unique words in it, counting them and finding the 
# top ~ 1000 words by sorting
most_common <- function(word_vec, m = 1000) {
  # unique_words is the vector of unique words in word_vec
  unique_words <- unique(word_vec)
  # unique_words # For Debugging
  
  # matches is the vector mapping each word in word_vec to its position in unique_words
  matches <- match(word_vec, unique_words)
  # matches # For Debugging
  
  # tabulated is the table containing the frequency of each word in unique_words in word_vec
  lower_a
  tabulated <- tabulate(matches)
  # tabulated # For Debugging
  
  # Use index.return to keep indices in matches, need to unlist tabulated to sort
  # sorted_tabulated_indices is the decreasing order frequency vector of each word in word_vec
  sorted_tabulated_indices <- sort(unlist(tabulated), index.return = TRUE, decreasing = TRUE)
  # sorted_tabulated_indices # For Debugging
  
  # In case there are many words with same count extending after m, we extend m to new_m
  # with new_m being equal to m + number of words past m with same word count as m
  final_words <- which(sorted_tabulated_indices[[1]] == sorted_tabulated_indices[[1]][m])
  # final_words # For Debugging
  
  # Our new value for m will be based on the final word with the same word count as
  # the old m-th most common words
  new_m <- max(final_words)
  # new_m # For Debugging
  
  # options(max.print = new_m) # For Debugging
  # Return a vector of the new_m most common words in the text
  return(unique_words[sorted_tabulated_indices[[2]][1:new_m]])
}

# b is the vector of approximately m = 1000 most common words in lower_a
b <- most_common(lower_a)

## Q7 ##

## We create matrix M, which stores sequences of mlag words from lower_a to represent the Markov model
## mlag represents the model's order (i.e. the number of words back we are using to predict the next word)

# Match words in lower_a to b
matched_a <- match(lower_a, b)

# Define order of Markov model
mlag <- 4

# Create empty matrix M with mlag + 1 columns and total word count - mlag rows
M <- matrix(nrow = length(matched_a) - mlag, ncol = mlag + 1)

# Fill M with mlag-length sequences of words from matched_a
for (i in 1:(mlag + 1)) {
  M[, i] <- matched_a[i:(length(matched_a) - mlag + i - 1)]
}

# Remove sequences beginning with 'NA' (i.e. beginning with words not in 'b')
M <- M[-which(is.na(M[, 1])), ]

# M # For Debugging
# M <- matrix(matched_a[which(matched_a != "NA")], ncol = 1) # For Debugging

## Q8, Q9, Q10 ##
## We use the Markov model to generate nw-length sequences of common words in Ulysses (words in b)
## We then simulate nw = 50 word sections of text, modify our code to maintain capitalisation for
## majority-capital words in the text, and ensure there are no spaces before punctuation marks

## Note that the following code will be functions used to implement Q8, Q9, and Q10 and the simulations
## will be executed at the end

# The first function, sample_next_word, takes prev_tokens and token_matrix (the matrix M from Q7) as inputs
# and outputs a sample of the following words
# There are many edge cases which are covered by the if statements but the goal here is to try and find
# all possible next terms given the prev_tokens
sample_next_word <- function(prev_tokens, token_matrix) {
  # Setup the number of words we want in a sequence, p
  p <- length(prev_tokens)

  # 0th order model => select a random entry from the first column of the token matrix
  if (p == 0) {
    return(sample(token_matrix[, 1], 1))
  }

  # Find all the possible next terms. We will use an apply function which will return these for us
  # and use the which operator to find the indices to index on in a one-liner
  next_words <- token_matrix[which(apply(as.matrix(token_matrix[, 1:p]), 1, function(row) all(row == prev_tokens))), ]
  
  # If there is only one word after, need to convert to a matrix instead of a vector otherwise we get an error
  if (!is.matrix(next_words)) {
    next_words <- t(as.matrix(next_words)) # Transpose to turn from row to column vector
  }
  
  # Pick out the next word
  future_chars <- next_words[, p + 1]

  # Here's an edge case, if there is only one and it is not NA, return it
  if (length(future_chars) == 1 && !is.na(future_chars)) {
    return(future_chars[1])
  } else if (length(future_chars) == 1) {
    return(NA)
  }

  # Remove the NA entries - this doesn't work if future_chars is a scalar so will do an if statement here
  # Checking which indices are NOT NA
  future_chars <- future_chars[-which(is.na(future_chars))]
  
  # If we only have one, return it
  if (length(future_chars) == 1) {
    return(future_chars[1])
  }
  
  # If we have no non NA entries, return NA
  if (length(future_chars) == 0) {
    return(NA)
  }
  
  # Else, sample from the non NA entries
  return(sample(future_chars, 1))
}

# This function takes tokens, b, and punctuation vectors as inputs and returns a sentence formatted as
# required in Q10
# To do this, we will use the tokens to get the word vector and separate the words correctly by removing
# any whitespace before the punctuation
print_token <- function(tokens, b, punctuation) {
  # With the new 'b', convert tokens -> strings
  word_vec <- b[tokens]
  word_vec
        
  # Paste the word vector word_vec together with whitespace between each word
  # We want to replace punctuation strings like " ," with "," to remove the unnecessary white space before it
  sampled_sentence <- paste(word_vec, collapse = " ")

  # Iterating through the punctuation provided in Q5 fixing the spacing:
  for (punct in punctuation) {
    sampled_sentence <- gsub(paste(" ", punct, sep = ""), punct, sampled_sentence, fixed = TRUE)
  }

  return(sampled_sentence)
}

# The function sample_string takes nw, the token_matrix from Q7, and a first_token and returns out_text
# The function generates a sequence of nw words using the Markov model, if no word is found at the mlag
# given, then try again with the mlag - 1 Markov model, down to 0
sample_string <- function(nw, token_matrix, first_token = NA) {
  # If a first word is given, start from that, else randomly sample one
  if (is.na(first_token)) {
    out_text <- c(sample_next_word(c(), M))
  } else {
    out_text <- c(first_token)
  }
  
  # Loop through every word
  for (i in 2:nw) {
    # Look through each model; from mlag:0 so we include the 0th order model
    for (j in mlag:0) {
      # If on the base model, sample randomly
      if (j == 0) {
        out_text[i] <- sample_next_word(c(), M)
        
        # cat("zero") # For Debugging
        # cat("\n") # For Debugging
      } else if (i > j) { # skip lags too long for current i
        # If on the jth model, pass in the j last tokens and sample the next word
        out_text[i] <- sample_next_word(out_text[(i - j):(i - 1)], M)
        
        # If we sampled a word, break out otherwise move to lower model
        if (!is.na(out_text[i])) {
          break
        }
      }
    }
  }

  return(out_text)
}

# The function sample_string_baseline returns a sequence of nw words using a 0-th order Markov model
# and takes inputs of nw and a token_matrix
# We will loop through every word and use the sample_next_word function to draw words independently
sample_string_baseline <- function(nw, token_matrix){
  out_text <- c(sample_next_word(c(), token_matrix))
  
  # Loop through every word
  for (i in 2:nw) {
    out_text[i] <- sample_next_word(c(), token_matrix)
  }
  
  return(out_text)
}

# Function for Q10, taking inputs of a, lower_a (since we have it stored), and b 
# so that words which are uppercase more than lowercase in b will appear so in a modified b vector
# The function does this by comparing the number of times an upper/lowercase word shows up and how
# often lowercase words show up. This allows us to form an inequality to find which are more likely
# than not to be uppercase in b
modify_b <- function(a, lower_a, b){
  # Match the words in 'a' to their index in b and the words in lower_a to their index in b
  match_a <- match(a, b)
  match_lower_a <- match(lower_a, b)
  # match_a # For Debugging
  # match_lower_a # For Debugging
  
  # Number of times a lowercase word shows up
  tabulated_match_a <- tabulate(match_a) 
  # Number of times an upper/lowercase words shows up
  tabulated_match_lower_a <- tabulate(match_lower_a) 
  # Number of uppercase words will be the difference of these two tables
  tabulated_match_upper_a <- tabulated_match_lower_a - tabulated_match_a
  # Find the indices where a word is more likely to be uppercase than not using the which operator
  upper_indices <- which(tabulated_match_upper_a > tabulated_match_a) 
  # print(b[upper_indices])
  
  # Go through these indices in b and capitalise them so we have a modified b vector
  for (i in upper_indices) {
    substr(b[i], 1, 1) <- toupper(substr(b[i], 1, 1))
  }
  
  # Finally, return the modified b
  return(b)
}

# Let's use the modify_b function to include the commonly uppercase words
modified_b <- modify_b(a, lower_a, b)
#print(modified_b) # For Debugging

# With the preceding functions and a placeholder out_text, let's put Q8 into action with nw = 50!
nw = 50
out_text <- sample_string(nw, M)
sampled_sentence <- print_token(out_text, modified_b, punctuation)
print(sampled_sentence)

# And with the preceding functions and a placeholder out_baseline, let's put Q9 into action!
out_baseline <- sample_string_baseline(nw, M)
sampled_baseline <- print_token(out_baseline, modified_b, punctuation)
print(sampled_baseline)
