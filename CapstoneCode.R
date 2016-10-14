# Foundations of Data Science
# Capstone Project Code
# Project - World Population Projections

### PART 1 - INSTALLING PACKAGES AND DATASETS
## Installing required packages
install.packages("bayesPop", dependencies=TRUE)
# Dependencies=TRUE implies bayesLife and bayesTFR are automatically installed as well
library(bayesPop)
library(bayesLife)
library(bayesTFR)

## Installing required dataset
install.packages("wpp2012")
library(wpp2012)

## Example if you want to look at certain data set
data(tfr) # total fertility rates
str(tfr)
View(tfr)
head(tfr)

## Extra notes on data and equation used
# Demographic balancing equation
# Pc,t = Pc,t−1 + Bc,t − Dc,t + Mc,t
# Uses following inputs of each country
# 1.Sex- and age-specific population estimates at the initial time t = 0
# 2.Projections of total fertility rate (TFR)
# 3.Projections of fertility distribution over ages
# 4.Projections of sex ratio at birth
# 5.Projections of male and female life expectancy at birth (e0)
# 6.Historical data on sex- and age-specific death rates (for t ≤ 0)
# 7.Projections of sex- and age-specific net migration

### PART TWO - SIMULATING TOTAL FERTILITY RATE FUTURE TRAJECTORIES
## Note that these simulations take several hours

# Set working directory
tfr.dir <- "/Users/rabeylaslam/Desktop/CapstoneDataTFR"

# Step 1 Estimate parameters of the Phase II model using Markov Chain Monte Carlo (MCMC)
mc1 <- run.tfr.mcmc(output.dir=tfr.dir, iter='10000', wpp.year=2012, start.year=1950, present.year=2010, parellal = TRUE)

# Step 2 Estimate the parameters of the Phase III model by MCMC
mc2 <- run.tfr3.mcmc(sim.dir=tfr.dir, iter='10000')

# Step 3 Using the estimated parameters, generate future TFR trajectories
tfr.pred <- tfr.predict(sim.dir=tfr.dir, end.year=2100, nr.traj=1000, burnin=4000, burnin3=4000)

# Step 4 Retrieving and plotting information by country (if needed)
tfr.pred <- get.tfr.prediction(tfr.dir)
tfr.trajectories.plot(tfr.pred, "Sudan", nr.traj=50)
tfr.trajectories.table(tfr.pred, "Sudan")

### PART THREE - SIMULATING FUTURE TRAJECTORIES OF LIFE EXPECTANCY
## Note that these simulations take several hours

data(e0F, package="wpp2012")
head(e0F)
View(e0F)

# Set working directory
e0.dir <- "/Users/rabeylaslam/Desktop/CapstoneDataLifeExpectancy"

# Step 1 Estimate parameters for female e0 via MCMC
mc <- run.e0.mcmc(output.dir=e0.dir, sex="Female", iter='20000', wpp.year=2012, parellal = TRUE)

# Step 2 Using estimated parameters, generate future female and male e0 trajectories
e0.pred <- e0.predict(sim.dir=e0.dir, end.year=2100, nr.traj=1000, burnin=8000)

# Retrieving and plotting information (if needed)
e0.pred <- get.e0.prediction(e0.dir)
e0.trajectories.plot(e0.pred, "Brazil", nr.traj=10, both.sexes=TRUE)
e0.joint.plot(e0.pred, "Brazil", nr.points=100, pi=80, years=c(2010, 2050, 2100))

### PART FOUR - PRODUCING FUTURE POPULATION TRAJECTORIES

# Set working directory
pop.dir <- "/Users/rabeylaslam/Desktop/CapstoneDataPopProj"

# Predicting and storing population projections
pop.pred <- pop.predict(end.year=2100, start.year=1950, present.year=2010,
wpp.year=2012, output.dir=pop.dir, nr.traj=1000,
inputs=list(tfr.sim.dir=tfr.dir,
e0F.sim.dir=e0.dir, e0M.sim.dir="joint_"))

### PART FIVE - RESULTS VISUALIZATION
## IMPORTANT - to access population prediction objects in a later session use
pop.pred <- get.pop.prediction(pop.dir)

## Country specific analysis (examples include China, India and Italy)
## Can be used for other countries as well
# A simple summary function gives a quick look at various quantiles of the country’s projections, e.g. Italy
summary(pop.pred, country="Italy")

# Country-specific (China) projections
country <- "China"
pop.trajectories.plot(pop.pred, country=country, sum.over.ages=TRUE, nr.traj=50)
pop.byage.plot(pop.pred, country=country, year=2100, nr.traj=50, pi=80, ylim=c(0,130000))
pop.byage.plot(pop.pred, country=country, year=2010, add=TRUE, show.legend=FALSE, col="blue")
legend("topright", legend=2010, col="blue", lty=1, bty="n")

# Country specific (China) - generating population projection by age and sex
country <- "China"
pop.pyramid(pop.pred, country, year=c(2100, 2010))
pop.trajectories.pyramid(pop.pred, country, year=c(2100, 2015, 1950), nr.traj=0, proportion=TRUE, age=1:23, pi=80)
 
# Country-specific (India) projections
country <- "India"
pop.trajectories.plot(pop.pred, country=country, sum.over.ages=TRUE, nr.traj=50)
pop.byage.plot(pop.pred, country=country, year=2100, nr.traj=50, pi=80, ylim=c(0,130000))
pop.byage.plot(pop.pred, country=country, year=2010, add=TRUE, show.legend=FALSE, col="blue")
legend("topright", legend=2010, col="blue", lty=1, bty="n")

# Country specific (India) - generating population projection by age and sex
country <- "India"
pop.pyramid(pop.pred, country, year=c(2100, 2010))
pop.trajectories.pyramid(pop.pred, country, year=c(2100, 2015, 1950), nr.traj=0, proportion=TRUE, age=1:23, pi=80)

## Region-specific analysis
# For aggregate analysis
pop.aggr <- pop.aggregate(pop.pred,
                          # World, Africa, Europe, Northern America, Asia, Latin Am.
                          regions=c(900, 903, 908, 905, 935, 904), verbose=TRUE)
get.pop.aggregation(pop.dir)

# Population projections by region, years
par(mfrow=c(1,1))
for (region in c(900, 903, 908, 905, 935, 904))
  pop.trajectories.plot(pop.aggr, region, sum.over.ages=TRUE, nr.traj=50)

# Population projection by region, age / sex
pop.pyramid(pop.aggr, 900, year=c(2100, 2010), proportion=TRUE)
pop.pyramid(pop.aggr, 903, year=c(2100, 2010), proportion=TRUE)
pop.pyramid(pop.aggr, 908, year=c(2100, 2010), proportion=TRUE)
pop.pyramid(pop.aggr, 905, year=c(2100, 2010), proportion=TRUE)
pop.pyramid(pop.aggr, 935, year=c(2100, 2010), proportion=TRUE)
pop.pyramid(pop.aggr, 904, year=c(2100, 2010), proportion=TRUE)