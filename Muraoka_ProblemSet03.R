###
### Problem Set 03
### Taishi Muraoka
### February 25
###



##
## 1. Let's Make a Deal with S3
##

##
## a. define a new class, door
##

# This function asks you to select Door from 1, 2, or 3
# and stores the number you select as a "door" object.
# This function does not take any argument, 
# and whatever you enter will be ignored.
selecting_door <- function(...){
  right <- FALSE
  while(right==FALSE){ # loop until a player select the right number
    select_number <- readline(prompt="Please pick one number from 1, 2, or 3:")
    if(!(select_number %in% c(1, 2, 3))){
      print("Invalid number! Select again.") # if a player does not select 1, 2, or 3
                                             # he is forced to select again
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



##
## b. create a method for door objects called PlayGame
##
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

##
## a. define a new class, door
##

# This function asks you to select Door from 1, 2, or 3
# and stores the number you select as a "door" object.
# This function does not take any argument, 
# and whatever you enter will be ignored.
selecting_door2 <- function(...){
  # set s4 class with "door"
  # x should be numeric
  setClass(Class="door", 
           slots=list(x="numeric"))
  
  # check validity of the argument, whether x is 1, 2, 3 or not
  # but this is actually never used since a player are forced to select 1, 2, or 3
  # when running the function
  setValidity("door", function(object){
    door_number <- object@x
    if(!(door_number %in% c(1, 2, 3))){ # if x is not 1, 2, or 3, print this
      print("Invalid! You have to pick number from 1, 2, or 3!")
    }
  })
  
  right <- FALSE
  while(right==FALSE){ # loop until a player select the right number
    select_number <- readline(prompt="Please pick one number from 1, 2, or 3:")
    if(!(select_number %in% c(1, 2, 3))){
      print("Invalid number! Select again.") # if a player does not select 1, 2, or 3
                                             # he is forced to select again
    }
    else{
      right <- TRUE
    }
  }
  object <- new("door", x=as.numeric(select_number)) # create a "door" object
  return(object)
}

selected_door2 <- selecting_door2() # here, you are asked to enter 1, 2, or 3

selected_door2

# non-function version
#setClass(Class="door", 
#         slots=list(x="numeric"))
#setValidity("door", function(object){
#  door_number <- object@x
#  if(!(door_number %in% c(1, 2, 3))){
#    print("Invalid number! You have to pick number from 1, 2, or 3!")
#  }
#})
#selected_door2 <- new("door", x=1)



# b. create a method for door objects called PlayGame
setGeneric("PlayGame",
           function(object="door"){
             standardGeneric("PlayGame")
           })

# This function takes "door" object and tells you whether you win or not.
setMethod("PlayGame", "door",
          function(object){ # object should be "door"
            if(!(object@x %in% c(1, 2, 3))){ # chech if x is 1, 2, or 3
              print("You have to pick number from 1, 2, or 3!")
                    # if not, print this and end
            }
            else{
              car <- sample(1:3, 1) # pick which door has a car
              print(sprintf("The car is behind Door %s", as.character(car)))
                    # give the answer
              if(object@x==car){ # if the numbers are same
                print("Congraturations! You got the car!")
              }
              else{ # if the numbers are not same
                print("Unfortunatelly, You got the goat. Cheer up!")
              }
            }
          })

PlayGame(selected_door2)
