library(R.utils)
library(dplyr)
setwd ("c://Users/Shilo/Desktop/Course_Materials/Data_Course/Exam_1/")
gunzip ("DNA_Conc_by_Extraction_Date.csv.gz")



datPCR = read.csv("DNA_Conc_by_Extraction_Date.csv")
datPCRKaty = select(datPCR, -DNA_Concentration_Ben)
datPCRBen = select(datPCR, -DNA_Concentration_Katy, -Extract.Number, -Extract.Code, - Date_Collected, -Lab)

years = c(datPCRKaty$Year_Collected)
concentrationBen = (datPCRBen$DNA_Concentration_Ben)

# convert to a factor


#check it out
plot(years, concentrationBen) 
plot(concentrationBen)





jpeg(filename = "C://Users/Shilo/Desktop/Data_Course_Crook/CROOKPLOT2.jpeg")
hist(years, concentrationBen, breaks = 12, main = "Ben's Extractions", xlab = "YEAR", ylab = "DNA Concentration")
dev.off()

jpeg(filename = "C://Users/Shilo/Desktop/Data_Course_Crook/CROOKPLOT2.jpeg")
hist(datPCRKaty$Year_Collected, datPCRKaty$DNA_Concentration_Katy, breaks = 12, freq = FALSE, main = "Katy's Extractions", xlab = "YEAR", ylab = "DNA Concentration")
dev.off()
