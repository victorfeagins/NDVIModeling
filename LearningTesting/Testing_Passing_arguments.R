library(stringr)

fileindex = as.numeric(commandArgs(TRUE)[1]) #Comes from job array
files = commandArgs(TRUE)[2:length(commandArgs(TRUE))] #Comes from vector of files

# missingfiles <- files %>% 
#   str_split(" ") %>% 
#   unlist()


length(files)
files[fileindex]

