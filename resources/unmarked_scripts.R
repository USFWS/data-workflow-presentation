library(unmarked)
data(crossbill)
umf <- unmarkedFrameOccu(
  y=as.matrix(crossbill[,c("det991", "det992", "det993")]),
  siteCovs=crossbill[,c("ele", "forest")],
  obsCovs=list(date=crossbill[,c("date991", "date992", "date993")]))
sc <- scale(siteCovs(umf))
siteCovs(umf) <- sc
head(umf)


(fm.occu <- occu(~date ~ele + I(ele^2) + forest, umf))


library(lattice)
data(Switzerland)

print(levelplot(elevation ~ x + y, Switzerland, aspect="iso",
                xlab="Easting (m)", ylab="Northing (m)",
                col.regions=terrain.colors(100)))
?levelplot




###################
## Simulating data https://hmecology.github.io/unmarked/articles/simulate.html
###################

library(unmarked)
set.seed(123)

# sites
M <- 300

# Sampling occasions
J <- 8  

# Empty matrix
y <- matrix(NA, M, J)

# Covariates (elevation). In this case, elev is normally distributed, mean=0, SD=1. Could make it realistic if we sample from a refuge
site_covs <- data.frame(elev = rnorm(M))

# Create unmarked data frame with NAs and site covariates
umf <- unmarkedFrameOccu(y = y, siteCovs = site_covs)

# Specify the model for unmarked
model <- occu  # Single season occupancy model (occu)

# Specify other arguments for the model function
form <- ~1 ~ elev  # Model formula argument in occu function

# Specify the parameter values for the simulation function
simulate(umf, model = model, formula = form)  # Still need to specify "state" and "det", on the inverse link scale (logit in this case for detection) 

# Make our coefficients (coeffs) list
cf <- list(state = c(0, -0.4), det = 0)

# Run simulation
out <- simulate(umf, model = occu, formula = ~1 ~ elev, coefs = cf)
out[[1]]

# Check: Fit a simple single season occupancy model
(sim_mod <- occu(~1 ~ elev, data = out[[1]]))  # Occupancy is a function of elevation, Detection is a null model



######################
## Map species occurrence probability
######################


library(unmarked)
data(crossbill)

umf <- unmarkedFrameOccu(
  y = as.matrix(crossbill[ ,c("det991", "det992", "det993")]),
  siteCovs = crossbill[ ,c("ele", "forest")],
  obsCovs = list(date = crossbill[ ,c("date991", "date992", "date993")]))

sc <- scale(siteCovs(umf))
siteCovs(umf) <- sc
head(umf)

library(raster)
elevation <- rasterFromXYZ(Switzerland[,c("x","y","elevation")],
                           crs="+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")

forest <- rasterFromXYZ(Switzerland[,c("x","y","forest")],
                        crs="+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs")

attr(sc, "scaled:center")
attr(sc, "scaled:scale")

# Calculate standardized coefficients
ele.s <- (elevation - 1189) / 640
forest.s <- (forest-34.7)/27.7
# Create a raster stack
ef <- stack(ele.s, forest.s)
names(ef) <- c("ele", "forest")
plot(ef, col = terrain.colors(100))

(beta <- coef(fm.occu, type = "state"))

# If measures of uncertainty are not required, the following code can be used to quickly produce the species distribution map
logit.psi <- beta[1] + beta[2]*ele.s + beta[3]*ele.s^2 + beta[4]*forest.s
psi <- exp(logit.psi) / (1 + exp(logit.psi))
print(spplot(psi, col.regions=terrain.colors(100)))

# Map of expected values of the parameter of interest, the standard errors, and the upper and lower confidence intervals.
E.psi <- predict(fm.occu, type = "state", newdata = ef)
plot(E.psi, axes=FALSE, col=terrain.colors(100))
