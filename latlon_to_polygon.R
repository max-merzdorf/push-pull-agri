library(sf)

row <- fd_ethiopia[1,]

st_as_sf(fd_ethiopia, coords = c(2,3,6,7,10,11,15,16))
sum(is.na(fd_ethiopia_noNA$Corner4_Lat))
