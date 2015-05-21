if (F) {
  library(devtools)
  install_github("statguy/SpaceTime")
}
library(SpaceTime)

infections <- read.csv("stat_patch2012corr.txt", sep="\t", fileEncoding="ISO-8859-1")
complete <- complete.cases(infections[,c("PLM2_Sept2012","AA_F2012","Distance_to_shore","PA_2011")])
infections <- infections[complete,]

mesh <- NonConvexHullMesh$new(knots=infections[,c("Longitude","Latitude")], knotsScale=1e5)
mesh$construct(cutoff=1e3, maxEdge=c(2.2e3, 1e5), convex=0.1)
mesh$getINLAMesh()$n
mesh$plot()

model <- ContinuousSpaceModel$new()
model$setSpatialMesh(mesh)
model$setSpatialPrior()
model$setLikelihood("nbinomial")

# Intercept-only model
model$setSmoothingModel()
model$addObservationStack(response=infections$number_MLG)
model$estimate()
model$summary() # WAIC = 1486.21

# Full model
model$setCovariatesModel(~ 1 + PLM2_Sept2012 + AA_F2012 + Distance_to_shore + PA_2011, covariates=infections)
model$clearStack()$addObservationStack(response=infections$number_MLG, covariates=infections)
model$estimate()
model$summary() # WAIC = 1458.55

model$setCovariatesModel(~ 1 + PLM2_Sept2012 + AA_F2012 + PA_2011, covariates=infections)
model$clearStack()$addObservationStack(response=infections$number_MLG, covariates=infections)
model$estimate()
model$summary() # WAIC = 1457.05

model$setCovariatesModel(~ 1 + AA_F2012 + PA_2011, covariates=infections)
model$clearStack()$addObservationStack(response=infections$number_MLG, covariates=infections)
model$estimate()
model$summary() # WAIC = 1458.94

# Best fitting model (lowest WAIC)
model$setCovariatesModel(~ 1 + PLM2_Sept2012 + AA_F2012, covariates=infections)
model$clearStack()$addObservationStack(response=infections$number_MLG, covariates=infections)
model$estimate()
model$summary() # WAIC = 1455.63

model$setCovariatesModel(~ 1 + AA_F2012, covariates=infections)
model$clearStack()$addObservationStack(response=infections$number_MLG, covariates=infections)
model$estimate()
model$summary() # WAIC = 1457.46
