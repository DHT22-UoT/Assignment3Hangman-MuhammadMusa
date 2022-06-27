#' Muhammad Musa
#' MSC2011 Assignment 3 - Hangman

# Create a dictionary of names.
names <- c("toronto", "raptors", "watermelon", "biotechnology", "lebron")

# COMMENT: I think we were supposed to "read"/import an external txt file with names listed in one coloumn.



# Sample one word at random and store it as namedrawn.
namedrawn <- sample(names, 1)

# Changing the display of unknown words to '_'.
ndash <- replicate(nchar(namedrawn), "_")

# Informing the user the length of the word.
namedrawnlength <- nchar(namedrawn)
print(paste("The length of the unknown word is: ", namedrawnlength))

# Informing user of the rules and number of incorrect tries allowed before game ends.
attempts <- 6
print(paste("In order to win, you must guess the unknown word within", attempts, "incorrect tries!"))

# Splitting the random unknown words into individual letters.
unknownletters <- strsplit(as.character(namedrawn), split = "")[[1]]

# while loop that would make the game going until the set number of incorrect tries have been reached.
# COMMENT: I think its a smart  rule, to have a set of incorrect answers allowed (I think this is more elegant)
# I had the default tries as "1 + the number of letters of the word" 
while (attempts > 0) {
  
  # Asking user to guess a letter.
  #COMMENT: tolower is a very good way to make sure the input is not subject to case-sensitivity!
  userinput <- tolower(readline(prompt = "Please Input a single letter: "))
  
  # Checking to ensure user enters one letter only. If the entry is incorrect, displaying the appropriate message.
  while (nchar(userinput) != 1 | !is.na(as.numeric(userinput))) {
    print("INVALID ENTRY, PLEASE TRY AGAIN")
    userinput <- tolower(readline(prompt = "Please Input a single letter: "))
  }
  
  print("Entry is Valid!")
  
  # After valid entry, checking to see if user's input matches a letter in the unknown word, and printing the appropriate message.
  if (grepl(userinput, namedrawn, ignore.case = T)) {
    print(paste("Good job, the letter", userinput, "is in the unknown word"))
    
    # If there is a match, replacing the '_' with user's input at the appropriate position.
    pos <- which(unknownletters == userinput)
    ndash <- replace(ndash, pos, userinput) 
    print(ndash)
    
    # When all the positions have been filled, inform user they won and end the game. 
    if (paste(ndash, collapse = '') == namedrawn) {
      print(paste("YOU WIN, The unknown word was:", namedrawn))
      break
    }
    
    # if the users guess was incorrect, decreasing number of tries and reminding user of the number of tries remaining.
  } else {
    attempts <- attempts-1
    print(paste("Sorry, the letter", userinput, "is not present. Number of tries left:", attempts))
    print(ndash)
  }
  
}


# When all incorrect tries have been reached, asking the user to input there final guess.
# COMMENT:: For the user to really understand you are asking for the full word guess, you can word this 
# more explicitly.
userguess <- readline(prompt = "You are out of tries. Please enter your guess: ")

# Informing the user if their guess was correct or incorrect.

if (userguess == namedrawn) {
  print(paste("YOU WIN, the unknown word was:", namedrawn))
} else {
  print(paste("YOU LOSE. The known word was:", namedrawn))
}

#COMMENT:
# I tried to end my HANGMAN the same way as yours, but I didnt do an else statement.
# I think thats why mine didnt work - Your code runs very well! - thanks for giving me an 
# idea of why my code didnt end properly!