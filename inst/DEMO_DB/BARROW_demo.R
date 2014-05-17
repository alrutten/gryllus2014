
# POPULATE TABLES IN ...\BARROW_demo.sqlite

require(pecs)
database_path =  "M:\\PROJECTS\\SOFTWARE\\R\\PACKAGES\\pecs\\pecs11\\calidris\\inst\\DEMO_DB\\BARROW_demo.sqlite"
con = dbConnect(dbDriver("SQLite"), dbname= database_path )

{ # GPS TABLE

sqliQuery("delete from GPS")

set.seed(12)

gps = spsample(.maps[[1]], 10, "random")

gps = data.frame(coordinates(gps))
names(gps) = c("longit", "latit")
gps$point = c(1:5, 1:5)
gps$gps = rep(10:11, each = 5)
gps$altit = round(rnorm(10), 1)
gps$date_time = Sys.time()+runif(10, 3600, 3600*48)

gps = gps[, c("gps", "point", "latit", "longit", "altit", "date_time")]
gps$date_time =as.character(gps$date_time)

sqliSave(gps, "GPS") 
} 
 
 
{ # NESTS TABLE
sqliQuery("delete from NESTS")

# found
n = data.frame(
	nest = c(paste("P10", 1:5, sep = ""), paste("P20", 1:5, sep = "") ),
	author = c(rep("AB", 5), rep("XY", 5) ), 
	date_time = gps$date_time,
	point = c(1:5, 1:5),
	gps = rep(10:11, each = 5),
	clutchSize = round(runif(10, 2, 4)),
	female_identity = "NOBA"
	)
n$species = "PESA"
n$bird_incubate = NA 
n$eggs_collected  = NA 
n$dev1 = NA 
n$dev2 = NA 
n$dev3 = NA 
n$dev4 = NA 
n$state = NA 
n$female_ID = NA 
n$male_identity = NA 
n$male_ID= NA 
n$remarks= NA 

n = n[, names(dbReadTable(con, "NESTS"))]


dbWriteTable(con, "NESTS", n, append = TRUE, row.names = FALSE) 




} 

