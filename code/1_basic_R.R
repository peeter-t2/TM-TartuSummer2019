# Originally compiled by David Lorenz 2017, see R_Glossary v1-4.pdf
# Adapted for Text mining in R and reproducible research in Digital Methods in Humanities and Social Sciences Summer School, Tartu 2019 
# by Peeter Tinits

# Run* these commands one by one (line by line). Mostly, you will be able to figure out what's going on, even without fully understanding the "language"...

## *You can either copy-paste a line into the R console and hit Enter. Or run it from this script by CTRL+R (Windows) or CMD+Enter (Mac). This executes the line that the cursor is in. (Or highlight several lines, then these are executed at once.)

#Recommended to turn on soft-wrap:
#Tools -> Global options -> Code -> Editing -> Soft-wrap R source files (turn it on)


#Simple operations
1+1
4-2
4*2
27*17
459/17
sqrt(25)

#Making variables/containers
x <- 4
y <- 2
x_2 = 4
y_2 = 2
x
y

#Operations with variables
x+y
x*y

#Check if matches
1==1
1==2
x==y
x==y*2

#Make complex variables
z <- x+y*2
z <- (x+y)*2
z

#Make and read vectors
1:10
myFirstVector <- c(1:10)
myFirstVector
mySecondVector <- c("a", "b", "c", "d","e","f","g","h","i","j")
mySecondVector
class(myFirstVector)
class(mySecondVector)
myThirdVector <- c(a,b,c,d)  # does not work - why?
myThirdVector <- c("a","b","c","d")


myFirstVector[6]
mySecondVector[c(2,3,5)]  # what does c() inside the square brackets do?
myFirstVector[6,8,9] # does not work, needs c()


myFirstVector %in%  mySecondVector #no matches
mySecondVector %in%  myThirdVector #first four match
