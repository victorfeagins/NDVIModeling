
Data = c("Bull_Shoals", "36.563", "-93.067",
         "Russell_Sage", "32.457", "-91.974",
         "Duke", "35.974", "-79.100",
         "Marcell", "47.514", "-93.469",
         "Missouri_Ozarks", "38.744", "-92.200",
         "UMBS", "45.560", "-84.714",
         "Coweeta", "35.060", "-83.428",
         "Shenandoah", "38.617", "-78.350",
         "Shining_Rock", "35.390", "-82.775",
         "Havard_Forest", "42.538", "-72.172",
         "Green_Ridge", "39.691", "-78.407",
         "Morgan_Monroe", "39.323", "-86.413",
         "Bartlett", "44.065", "-71.288",
         "Willow_Creek", "45.806", "-90.079",
         "Hubbard_Brook", "43.927", "-71.741",
         "Steigerwaldt_Land_Services", "45.509", "-89.586",
         "The_University_of_Kansas_Field_Station", "39.040", "-95.192",
         "Great_Smoky_Mountains_National_Park", "35.689", "-83.502",
         "Dead_Lake", "32.542", "-87.804",
         "National_Grassland", "33.401", "-97.570")


df <- as.data.frame(matrix(Data, ncol = 3, byrow = TRUE))

colnames(df) <- c("SiteName", "Latitude", "Longitude")

write.csv(df, "SiteCodeBook.csv", row.names = FALSE)
