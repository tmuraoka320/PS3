###
### Problem Set 03
### Taishi Muraoka
### February 25
###



##
## 1. Let's Make a Deal with S3
##

# a. define a new class, door.

# This function asks you to select Door from 1, 2, or 3.
# This function does not take any argument, 
# and whatever you enter will be ignored.
selecting_door <- function(...){
  right <- FALSE
  while(right==FALSE){ # loop until a player select the right number
    select_number <- readline(prompt="Please pick one number from 1, 2, or 3:")
    if(!(select_number %in% c(1, 2, 3))){
      print("Invalid number! Select again.")
    }
    else{
      right <- TRUE
    }
  }
  select_number <- as.numeric(select_number) # change to numeric
  class(select_number) <- "door" # change class to "door"
  return(select_number)
}

selected_door <- selecting_door() # here, you are asked to enter 1, 2, or 3

selected_door

# b. create a method for door objects called PlayGame
PlayGame <- function(x){UseMethod("PlayGame")}

# This function takes "door" object and tells you whether you win or not.
PlayGame.door <- function(x){ # x should be "door" object
  if(!(x %in% c(1, 2, 3))){ # chech if x is 1, 2, or 3
    print("You have to pick number from 1, 2, or 3!") # if not, print this and end
  }
  else{
    car <- sample(1:3, 1) # pick which door has a car
    print(sprintf("The car is behind Door %s", as.character(car))) # give the answer
    if(x==car){ # if the numbers are same
      print("Congraturations! You got the car!")
    }
    else{ # if the numbers are not same
      print("Unfortunatelly, You got the goat. Cheer up!")
    }
  }
}

PlayGame(selected_door) # this works

PlayGame(structure(list(5), class="door")) # this does not work since you select 5



##
## 2. Let's Make a Deal with S4
##
