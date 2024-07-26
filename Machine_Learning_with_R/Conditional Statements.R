###Conditional Statements Training Exercise###
### Exercise 1:
#Write a script that prints "Hello" if the variable x is equal to 1:


x = 1
if (x == 1) {
  print("Hello")
}

##Exercises
### Exercise 2:
#Write a script that prints "Hi there" if the variable x is greater than 5, otherwise print "Hello".
#Input: x <- 7

x = 7
if (x > 5) {
  print("Hi there")
} else {
  print("Hello")
}


### Exercise 3:
#Write a script that determines whether a given number `x` is divisible by 3. 
#If it is, print "Divisible by 3"; otherwise, print "Not divisible by 3".

#Input: `x <- 13`


x = 13
if (x %% 3 == 0) {
  print("Divisible by 3")
} else {
  print("Not divisible by 3")
}


### Exercise 4:
#Create a script that checks if a given variable `x` is a data frame. 
#If `x` is a data frame, print "Is a Data Frame"; otherwise, print "Not a Data Frame". 
#Hint: You may want to explore the function `is.data.frame()`.

#Input: `x <- data.frame()`


x = data.frame()
if (is.data.frame(x)) {
  print("Is a Data Frame")
} else {
  print("Not a Data Frame")
}



### Exercise 5:
#Create a script that given a numeric vector x with a length 3, 
#will print out the elements in order from high to low. 
#You must use if,else if, and else statements for your logic. (This code will be relatively long)
#Input: x <- c(3,7,1)


x = c(3, 7, 1)
if (x[1] >= x[2] & x[2] >= x[3]) {
  high_val = x[1]
  mid_val  = x[2]
  low_val  = x[3]
} else if (x[1] >= x[3] & x[3] >= x[2]) {
  high_val = x[1]
  mid_val  = x[3]
  low_val  = x[2]
} else if (x[2] >= x[1] & x[1] >= x[3]) {
  high_val = x[2]
  mid_val  = x[1]
  low_val  = x[3]
} else if (x[2] >= x[3] & x[3] >= x[1]) {
  high_val = x[2]
  mid_val  = x[3]
  low_val  = x[1]
} else if (x[3] >= x[1] & x[1] >= x[2]) {
  high_val = x[3]
  mid_val  = x[1]
  low_val  = x[2]
} else {
  high_val = x[3]
  mid_val  = x[2]
  low_val  = x[1]
}
print(paste("High:", high_val, "Mid:", mid_val, "Low:", low_val))




### Exercise 5:
#Write a script that uses if,else if, 
#and else statements to print the max element in a numeric vector with 3 elements.
#Input: x <- c(20, 10, 1)

x = c(20, 10, 1)
if (x[1] >= x[2] & x[1] >= x[3]) {
  max_element = x[1]
} else if (x[2] >= x[1] & x[2] >= x[3]) {
  max_element = x[2]
} else {
  max_element = x[3]
}
print(paste("The maximum element is:", max_element))

