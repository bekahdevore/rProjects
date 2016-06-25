qcewGetAreaData <- function(year, qtr, area) {
        url <- "http://data.bls.gov/cew/data/api/YEAR/QTR/area/AREA.csv"
        url <- sub("YEAR", year, url, ignore.case=FALSE)
        url <- sub("QTR", qtr, url, ignore.case=FALSE)
        url <- sub("AREA", area, url, ignore.case=FALSE)
        read.csv(url, header = TRUE, sep = ",", quote="\"", dec=".", na.strings=" ", skip=0)
}
louisville <- qcewGetAreaData("2015", "a", "C3114") 
nashville <- qcewGetAreaData("2015", "a", "C3498")
charlotte <- qcewGetAreaData("2015", "a", "C1674")
dayton <- qcewGetAreaData("2015", "a", "C1938")
austin <- qcewGetAreaData("2015", "a", "C1242")
portland <- qcewGetAreaData("2015", "a", "C3890")

        



"http://www.bls.gov/cew/data/api/2015/a/area/C3114.csv"

