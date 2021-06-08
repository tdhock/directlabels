## Lattice allows easy visualization of many variables
options(width=55)
library(lattice)
dotplot(variety ~ yield | site, data = barley, groups = year,auto.key=list(space="right"),layout=c(1,6),xlab = "Barley Yield (bushels/acre)")

## Aspect ratio in scatterplots is important
xyplot(sunspot.year~1700:1988,xlab="Year",type="l",scales=list(x=list(alternating=2)),main = "Yearly Sunspots")

## Lattice also automatically calculates aspect ratio for optimal decoding
xyplot(sunspot.year~1700:1988,xlab="Year",type="l",scales=list(x=list(alternating=2)),main = "Yearly Sunspots",aspect="xy")





## Load a data set
data(Chem97,package="mlmRev")
head(Chem97)

## Simple histogram
histogram(~gcsescore,Chem97)

## Histograms conditional on a categorical variable
histogram(~gcsescore|factor(score),Chem97)

## Box and whisker plots
bwplot(gcsescore~gender|factor(score),Chem97,layout=c(6,1))

## Conditioned plots of kernel density estimates
densityplot(~gcsescore|factor(score),Chem97)

## Hide the actual points with the plot.points argument
densityplot(~gcsescore|factor(score),Chem97,plot.points=FALSE)

## Conditioned and grouped density plots
densityplot(~gcsescore|factor(score),Chem97,plot.points=FALSE,groups=gender)

## Add a legend with the auto.key argument
densityplot(~gcsescore|factor(score),Chem97,plot.points=FALSE,groups=gender,auto.key=list())

## Legend layout with the columns argument
densityplot(~gcsescore|factor(score),Chem97,plot.points=FALSE,groups=gender,auto.key=list(columns=2))

## Legend positioning with the space argument
densityplot(~gcsescore|factor(score),Chem97,plot.points=FALSE,groups=gender,auto.key=list(columns=2,space="bottom"))

## Show all default settings
show.settings()

## Show settings good for printout
show.settings(standard.theme(color=FALSE))

## Change the settings
br <- simpleTheme(col=c("black","red"))
show.settings(br)

## Change group colors with par.settings
densityplot(~gcsescore|factor(score),Chem97,plot.points=FALSE,groups=gender,auto.key=list(columns=2,space="bottom"),par.settings=br)





## Load a tabular data set
print(VADeaths)

## Convert to data frame to work with lattice
vad <- as.data.frame.table(VADeaths)
names(vad) <- c("age","demographic","deaths")
head(vad)

## Grouped dotplots work well for these data
dotplot(age~deaths,vad,groups=demographic,type="o")

## Plots can be saved as R objects
dots <- dotplot(age~deaths,vad,groups=demographic,type="o")
dots

## Saved plots can be updated later
dots2 <- update(dots,type="l",xlim=c(5,80))
dots2

## Add a confusing legend ... how can we label more intuitively?
update(dots2,auto.key=list(points=FALSE,lines=TRUE))




## Load some earthquake measurements
data(Earthquake,package="nlme")
head(Earthquake)

## Scatterplot with xyplot
xyplot(accel~distance,Earthquake)

## Log scales with scales argument
xyplot(accel~distance,Earthquake,scales=list(log=TRUE))

## Type "p" is the default
xyplot(accel~distance,Earthquake,scales=list(log=TRUE),type=c("p"))

## Type "g" adds a grid
xyplot(accel~distance,Earthquake,scales=list(log=TRUE),type=c("p","g"))

## Type "smooth" adds a smooth line
xyplot(accel~distance,Earthquake,scales=list(log=TRUE),type=c("p","g","smooth"))

## Add some labels
xyplot(accel~distance,Earthquake,scales=list(log=TRUE),type=c("p","g","smooth"),sub="(log scale)",xlab="Distance from epicenter (km)",ylab="Maximum horizontal acceleration (g)",main="Larger quakes are felt closer to the epicenter")




## Volcano elevation data in matrix form
dim(volcano)
print(volcano[1:5,1:5])

## Plot volcano elevations in a matrix using color
levelplot(volcano)

## Use a different color scale
my.colors <- sapply(0:100,function(l)hcl(l=l))
levelplot(volcano,col.regions=my.colors)

## Use 3d wireframe plots
wireframe(volcano,drape=TRUE,col.regions=my.colors)

## Combine plots using latticeExtra
library(latticeExtra)
both <- c(wireframe(volcano,drape=TRUE),levelplot(volcano))
both

## Globally change the plot parameters
trellis.par.set(regions=list(col=my.colors))
both




## Longitudinal data
data(BodyWeight,package="nlme")
head(BodyWeight)

## Conditional scatterplots reveal difference between treatments
xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1))

## Legends with more than a few items are very confusing
xyplot(weight~Time|Diet,BodyWeight,groups=Rat,type='l',layout=c(3,1),auto.key=list(space="right",points=FALSE,lines=TRUE))

