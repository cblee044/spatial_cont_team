# sedchemdata <- read.csv("~/Documents/Class/MATH 539/Exam 2/bight2018_sedchem.csv")
# mloedata <- read.csv("~/Documents/Class/MATH 539/Exam 2/Bight_MLOE_98-18.csv")

sedchem <- sedchemdata[,c(1,2,3,4,5,7,14,17:22,30,33,34,36)]

# index 1707 - do not have corresponding key
# sedchem <- sedchem[!(sedchemdata$stationid == sedchemdata$stationid[1707]|
# 										 	sedchemdata$stationid == sedchemdata$stationid[3418]),]

# Make mloe key
mloe_key <- mloedata[mloedata$LOE == 'Chemistry', c(1,7,8)]

sc_stat_id <- sedchem$stationid

mloe_transfer <- matrix(0, length(sc_stat_id), 2)

match 	= F
key_ind = 1
n 			= nrow(mloe_key)
# Check each entry on sed chem
for(i in 1:length(sc_stat_id)) {
	# Determine true key
	if(!match) key_ind = c(1:n)[sc_stat_id[i] == mloe_key]
	
	if(length(key_ind) == 0) {mloe_transfer[i,] = c(NA,NA); match = F}
	else if(sc_stat_id[i] == mloe_key[key_ind,1]) mloe_transfer[i,] = t(mloe_key[key_ind,-1])
	else {match = F; key_ind = 1; i = i-1}
}
