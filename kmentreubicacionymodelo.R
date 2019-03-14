
install.packages("geosphere")
library("geosphere")

#Punta Galea
distm(c(-3.03608,43.3752), c(-3,43.5), fun = distHaversine)[,1]/1000#Boya Bilbao Bizkaia
#Alegria
distm(c(-2.524022, 42.842736), c(-2.25,42.75), fun = distHaversine)[,1]/1000#Boya Bilbao Bizkaia

