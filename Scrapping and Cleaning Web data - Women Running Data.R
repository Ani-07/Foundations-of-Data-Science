# Scrapping and Cleaning Web data - Women Running Data

# Use the Race Results 2001-2012 link (http://cherryblossom.org/aboutus/results_list.php ) 
# to extract the Race Results for Order of Finish - Women for a couple of years.

# We shall first load the XMl library and also extract the url of the respective years
# for which we shall be analyzing the data. I have selected years - 2004, 2005, 2006, 2007
# and 2012 to provide a good mix of data.

library(XML)
ubase = "http://www.cherryblossom.org/"
years = c(2004:2007, 2012)
womenURLs = 
  c("results/2004/women.htm", "results/2005/CB05-F.htm", 
    "results/2006/women.htm", "results/2007/women.htm", 
    "results/2012/2012cucb10m-f.htm")
urls = paste(ubase, womenURLs, sep = "")

# We shall now define a function to retrieve the data from the website in a 

extractResTable =
  
  # Retrieve data from web site, 
  # find the preformatted text,
  # and write lines or return as a character vector.
  #
  function(url = "http://www.cherryblossom.org/results/2009/09cucb-F.htm",
           year = 1999, sex = "male", file = NULL){
    doc = htmlParse(url)
    if (year == 2000) {
      # Get preformatted text from 4th font element
      # The top file is ill formed so the <pre> search doesn't work.
      ff = getNodeSet(doc, "//font")
      txt = xmlValue(ff[[4]])
      els = strsplit(txt, "\r\n")[[1]]
    }else if (year == 2009 & sex == "male") {
      # Get preformatted text from <div class="Section1"> element
      # Each line of results is in a <pre> element
      div1 = getNodeSet(doc, "//div[@class='Section1']")
      pres = getNodeSet(div1[[1]], "//pre")
      els = sapply(pres, xmlValue)
    }else {
      # Get preformatted text from <pre> elements
      pres = getNodeSet(doc, "//pre")
      txt = xmlValue(pres[[1]])
      els = strsplit(txt, "\r\n")[[1]]   
    }
    if (is.null(file)) {
      return(els)
    # Write the lines as a text file.
    writeLines(els, con = file)
  }
  }

# We shall now apply the above function to all the urls we saved above.
# The information extracted from the website shall be saved in womentables.
# We shall also add the name of the respecitve year to the data extracted.

womenTables = mapply(extractResTable, url = urls, year = years)
names(womenTables) = years
sapply(womenTables, length)

save(womenTables, file = "CBWomenTextTables.rda")

# Now let us explore the extracted information to understand the type of
# formatting

womenTables[[1]][1:10]
womenTables[[2]][1:10]

# We can see that there are lot of additional information on top of the tables and
# there are certain spacer rows as well. We shall now write functions to extract the
# required information alone into separate lists with custom headings.

findColLocs = function(spacerRow) {
  
  spaceLocs = gregexpr(" ", spacerRow)[[1]]
  rowLength = nchar(spacerRow)
  
  if (substring(spacerRow, rowLength, rowLength) != " ")
    return( c(0, spaceLocs, rowLength + 1))
  else return(c(0, spaceLocs))
}


selectCols = 
  function(colNames, headerRow, searchLocs) {
    sapply(colNames, 
           function(name, headerRow, searchLocs){
             startPos = regexpr(name, headerRow)[[1]]
             if (startPos == -1) 
               return( c(NA, NA) )
             
             index = sum(startPos >= searchLocs)
             c(searchLocs[index] + 1, searchLocs[index + 1] - 1)
           },
           headerRow = headerRow, searchLocs = searchLocs )
  }

extractVariables = 
  function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time")){
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    
    # Obtain the starting and ending positions of variables
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    invisible(Values)
  }

# We shall now apply these functions to the womentables and save the 
# output as womenResMat

womenResMat = lapply(womenTables, extractVariables)

length(womenResMat)
sapply(womenResMat, nrow)

age = sapply(womenResMat,function(x) as.numeric(x[ , 'ag']))

boxplot(age, ylab = "Age", xlab = "Year")

# We can observe from the boxplot that for the year 2006 there seems to be some 
# error in the data.

#for 2006 selestion
womenTables[[3]][2200:2205]

# We can observe that there is an error with the location of the age coloumn and that 
# is the reason for the error.

# We would need to modify selectCols() by changing the index for end of each variable 
# when we perform the extraction.

selectCols = function(shortColNames, headerRow, searchLocs) {
  sapply(shortColNames, function(shortName, headerRow, searchLocs){
    startPos = regexpr(shortName, headerRow)[[1]]
    if (startPos == -1) return( c(NA, NA) )
    index = sum(startPos >= searchLocs)
    c(searchLocs[index] + 1, searchLocs[index + 1])
  }, headerRow = headerRow, searchLocs = searchLocs )
}

womenResMat = lapply(womenTables, extractVariables)

age = sapply(womenResMat, function(x) as.numeric(x[ , 'ag']))

boxplot(age, ylab = "Age", xlab = "Year")

# The boxplot based on data extracted using the new function has adjusted for
# the error and the boxplot for 2006 also shows normal data.

# Now let us check for NA values

sapply(age,  function(x) sum(is.na(x)))

# There seem to be a few NA values for age spread across the years.

age2005 = age[["2005"]]
badAgeIndex = which(is.na(age2005)) + 5
womenTables[['2005']][ badAgeIndex ]

# There seem to be Blank lines are scattered throughout the file. We can modify the extraction by checking for 
# blank rows and removing them

extractVariables = 
  function(file, varNames =c("name", "home", "ag", "gun",
                             "net", "time")){
    # Find the index of the row with =s
    eqIndex = grep("^===", file)
    # Extract the two key rows and the data 
    spacerRow = file[eqIndex] 
    headerRow = tolower(file[ eqIndex - 1 ])
    body = file[ -(1 : eqIndex) ]
    # Remove footnotes and blank rows
    footnotes = grep("^[[:blank:]]*(\\*|\\#)", body)
    if ( length(footnotes) > 0 ) body = body[ -footnotes ]
    blanks = grep("^[[:blank:]]*$", body)
    if (length(blanks) > 0 ) body = body[ -blanks ]
    
    
    # Obtain the starting and ending positions of variables   
    searchLocs = findColLocs(spacerRow)
    locCols = selectCols(varNames, headerRow, searchLocs)
    
    Values = mapply(substr, list(body), start = locCols[1, ], 
                    stop = locCols[2, ])
    colnames(Values) = varNames
    
    return(Values)
  }

womenResMat = lapply(womenTables, extractVariables)

# Now let us go into our analysis of the data available. First let us format
# the runtime data. In order to perform our analysis we would need to first
# convert the data into minutes and also ensure they are are under numeric
# format.

# We shall use the below function to convert the time to the required format.

convertTime = function(time) {
  timePieces = strsplit(time, ":")
  timePieces = sapply(timePieces, as.numeric)
  sapply(timePieces, function(x) {
    if (length(x) == 2) x[1] + x[2]/60
    else 60*x[1] + x[2] + x[3]/60
  })
}

# Let us now use the above function to create a new df with the details of the
# runners of all the different years.

createDF = 
  function(Res, year, sex) {
    # Determine which time to use
    useTime = if( !is.na(Res[1, 'net']) )  
      Res[ , 'net']
    else if( !is.na(Res[1, 'gun']) ) 
      Res[ , 'gun']
    else 
      Res[ , 'time']
    
    runTime = convertTime(useTime)
    
    Results = data.frame(year = rep(year, nrow(Res)),
                         sex = rep(sex, nrow(Res)),
                         name = Res[ , 'name'],
                         home = Res[ , 'home'],
                         age = as.numeric(Res[, 'ag']), 
                         runTime = runTime,
                         stringsAsFactors = FALSE)
    invisible(Results)
  }

womenDF = mapply(createDF, womenResMat, year = c(2004:2007, 2012), sex = rep("F", 5), SIMPLIFY = FALSE)

# We may observe that there are certain warnings popping up. We can use the
# below command to see the warnings.

warnings()[ c(1:2, 49:50) ]

sapply(womenDF, function(x) sum(is.na(x$runTime)))

# The warnings seem to arise from missing values which have been coerced as 
# "NA" in the dataframe created.

# Therfore, we would need to ensure that the function also removes these erroneous values
# while creating the dataframe.

createDF = function(Res, year, sex) {
  # Determine which time to use
  if ( !is.na(Res[1, 'net']) ) useTime = Res[ , 'net']
  else if ( !is.na(Res[1, 'gun']) ) useTime = Res[ , 'gun']
  else useTime = Res[ , 'time']
  
  # Remove # and * and blanks from time
  useTime = gsub("[#\\*[:blank:]]", "", useTime)
  runTime = convertTime(useTime[ useTime != "" ])
  
  # Drop rows with no time
  Res = Res[ useTime != "", ]
  
  Results = data.frame(year = rep(year, nrow(Res)),
                       sex = rep(sex, nrow(Res)),
                       name = Res[ , 'name'], home = Res[ , 'home'],
                       age = as.numeric(Res[, 'ag']), 
                       runTime = runTime,
                       stringsAsFactors = FALSE)
  invisible(Results)
}

womenDF = mapply(createDF, womenResMat, year = c(2004:2007, 2012),
               sex = rep("M", 5), SIMPLIFY = FALSE)

sapply(womenDF, function(x) sum(is.na(x$runTime)))

# As we can see from above that a few of the runtimes with respect to 2006 are
# still showing as NA. Let us now try to rectifiy the same.

separatorIdx = grep("^===", womenTables[["2006"]])
separatorRow = womenTables[['2006']][separatorIdx]
separatorRowX = paste(substring(separatorRow, 1, 63), " ", 
                      substring(separatorRow, 65, nchar(separatorRow)), 
                      sep = "")
womenTables[['2006']][separatorIdx] = separatorRowX

womenResMat = sapply(womenTables, extractVariables)
womenDF = mapply(createDF, womenResMat, year = c(2004:2007, 2012),
               sex = rep("M", 5), SIMPLIFY = FALSE)

sapply(womenDF, function(x) sum(is.na(x$runTime)))

# We can observe that the NA values have been reduced to 0 for 2006 as well.

boxplot(sapply(womenDF, function(x) x$runTime), 
        xlab = "Year", ylab = "Run Time (min)")

# Now let save the data for all the years into one dataframe.

cbWomen = do.call(rbind, womenDF)

# Let us now create a scatterplot between runtime and age to observe the relationship
# between the data

plot(runTime ~ age, data = cbWomen, ylim = c(40, 180), xlab = "Age (years)", ylab = "Run Time (minutes)")

# The above scatterplot shows how that the lower runtime seem to be slightly
# concentrated towards the younger runners with slightly higher runtime for
# a the older runner.

# We can try to create a better scatterplot using the RcolorBrewer library

library(RColorBrewer)
ls("package:RColorBrewer")

display.brewer.all()

Purples8 = brewer.pal(9, "Purples")[8]
Purples8

Purples8A = paste(Purples8, "14", sep = "")

plot(runTime ~ jitter(age, amount = 0.5), 
     data = cbWomen, 
     pch = 19,cex = 0.2, col = Purples8A,
     ylim = c(45, 165), xlim = c(15, 85),
     xlab = "Age (years)", ylab = "Run Time (minutes)")

# The above scatterplot is slightly better than the earlier scatterplot with the
# smaller dots showing a better distribution.

# We can try to obtain more information about the density of the data by using the smooth
# scatter function

smoothScatter(y = cbWomen$runTime, x = cbWomen$age,
              ylim = c(40, 165), xlim = c(15, 85),
              xlab = "Age (years)", ylab = "Run Time (minutes)")

# The smoothscatter plot provides a better picture of the flow of runtime
# with the age. We can see how the colour is dark between 20-30 (age) and
# 80-100 (runtime) showing high concentration of data there.

# Now let us create a subset of the data to undertake deeper analysis

cbWomenSub = cbWomen[cbWomen$runTime > 30 &
                   !is.na(cbWomen$age) & cbWomen$age > 15, ]

# We shall also categorize the data based on range of ages. Which we can
# use to observe difference in runtime between age categories.

ageCat = cut(cbWomenSub$age, breaks = c(seq(15, 75, 10), 90))
table(ageCat)

plot(cbWomenSub$runTime ~ ageCat, 
     xlab = "Age (years)", ylab = "Run Time (minutes)")

# The box plot clearly indicates that with increase in age there is an increase in
# run time with also an increase in the variance of run time.

# Now, let us try to run a linear regression model between runtime and age.

lmAge = lm(runTime ~ age, data = cbWomenSub)

lmAge$coefficients

summary(lmAge)

# The above shows the intercept value and the coefficient value for the
# relationship between runtime and age. Also, provides the p value for
# testing whether the coefficients are different from zero.

# We can also plot the residuals from the linear model with the age data

smoothScatter(x = cbWomenSub$age, y = lmAge$residuals,
              xlab = "Age (years)", ylab = "Residuals")
abline(h = 0, col = "purple", lwd = 3)

# Now let us try to fit the data with a loess model (ie. a polynomial 
# regression fitting model).

resid.lo = loess(resids ~ age, data = data.frame(resids = residuals(lmAge),age = cbWomenSub$age))

age20to80 = 20:80
age20to80

# Now we can use the results of the model to predict for different age categories.

resid.lo.pr = 
  predict(resid.lo, newdata = data.frame(age = age20to80))

lines(x = age20to80, y = resid.lo.pr, col = "green", lwd = 2)

womenRes.lo = loess(runTime ~ age, cbWomenSub)

womenRes.lo.pr = predict(womenRes.lo, data.frame(age = age20to80))

# Ideally, the mean of the residuals should be zero for a properly contructed model. 
# This indicates that our linear model may not a very good model

# We can also try to use a linear piecewise model by segregating the age categories.

over50 = pmax(0, cbWomenSub$age - 50)

lmOver50 = lm(runTime ~ age + over50, data = cbWomenSub)

summary(lmOver50)

decades = seq(30, 60, by = 10)
overAge = lapply(decades, 
                 function(x) pmax(0, (cbWomenSub$age - x)))
names(overAge) = paste("over", decades, sep = "")
overAge = as.data.frame(overAge)

lmPiecewise = lm(runTime ~ . , 
                 data = cbind(cbWomenSub[, c("runTime", "age")], 
                              overAge))

summary(lmPiecewise)

# We now have the summary of the linear piecewise model. We basically
# have similar information to the linear regression model with details
# for specific age categories.

overAge20 = lapply(decades, function(x) pmax(0, (age20to80 - x)))
names(overAge20) = paste("over", decades, sep = "")
overAgeDF = cbind(age = data.frame(age = age20to80), overAge20)

# We can now use the above linear piecewise model to predict the 
# performance for different age catogories.

predPiecewise = predict(lmPiecewise, overAgeDF)

plot(predPiecewise ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Run Time Prediction")

lines(x = age20to80, y = womenRes.lo.pr, 
      col = "green", lty = 2, lwd = 3)
legend("topleft", col = c("purple", "green"),
       lty = c(1, 2), lwd= 3,
       legend = c("Piecewise Linear", "Loess Curve"), bty = "n")

# We can see from the above graph that there is a difference between the
# prediction between the loess curve model and the lm piecewise model

# Let us now plot the number of runners over the years.

numRunners = with(cbWomen, tapply(runTime, year, length))
plot(numRunners ~ names(numRunners), type="l", lwd = 2,
     xlab = "Years", ylab = "Number of Runners")

# We can see from the above graph that there has been an increase in number of 
# runners over the years.

# Let us now isolate the results for years 2004 and 2012 to observe
# the density of the age of runners. 

summary(cbWomenSub$runTime[cbWomenSub$year == 2004])

summary(cbWomenSub$runTime[cbWomenSub$year == 2012])

age2004 = cbWomenSub[ cbWomenSub$year == 2004, "age" ]
age2012 = cbWomenSub[ cbWomenSub$year == 2012, "age" ]

plot(density(age2004, na.rm = TRUE), 
     ylim = c(0, 0.06), col = "purple",
     lwd = 3,  xlab = "Age (years)",  main = "")
lines(density(age2012, na.rm = TRUE), 
      lwd = 3, lty = 2, col="green")
legend("topleft", col = c("purple", "green"), lty= 1:2, lwd = 3,
       legend = c("2004", "2012"), bty = "n")

# We can observe there has been a slight change in the density of runners
# from 2004 to 2012. The concentration of age of runners seems to have 
# increased slightly from 30s to 35s


# Now let us fit loess curves for 2004 and 2012 performance

mR.lo04 = loess(runTime ~ age, cbWomenSub[ cbWomenSub$year == 2004,])
mR.lo.pr04 = predict(mR.lo04, data.frame(age = age20to80))

mR.lo12 = loess(runTime ~ age, cbWomenSub[ cbWomenSub$year == 2012,])
mR.lo.pr12 = predict(mR.lo12, data.frame(age = age20to80))

# Now let us plot the above predictions for 2004 and 2012.

plot(mR.lo.pr04 ~ age20to80,
     type = "l", col = "purple", lwd = 3,
     xlab = "Age (years)", ylab = "Fitted Run Time (minutes)")

lines(x = age20to80, y = mR.lo.pr12,
      col = "green", lty = 2, lwd = 3)

legend("topleft", col = c("purple", "green"), lty = 1:2, lwd = 3,
       legend = c("2004", "2012"), bty = "n")

# We can observe that from 2004 to 2012 there has been a decrease in performance 
# of runners between ages 20 to 50 while the performance has improved very slightly
# for runners between 50 -70.

# Let us now plot the difference in performance between 2004 to 2012 with each age
# group.

gap12 = mR.lo.pr12 - mR.lo.pr04

plot(gap12 ~ age20to80, type = "l" , xlab = "Age (years)", 
     ylab = "Difference in Fitted Curves (minutes)", lwd = 2)

# We can observe from the above that there is a great difference in performance
# for runners between 30 to 50 while, there difference in performance is very
# small for runner near the age of sixty.


# Overall, the observations seem similar to the analysis of the performance of men that 
# was analyzed in class.