
##########

# Import and process data
#http://stackoverflow.com/questions/11433432/importing-multiple-csv-files-into-r

listFiles = list.files(getwd(),pattern ="stockPrices*") #get list of fileNames

myfiles = lapply(listFiles, function(filename) read.csv(filename,header = T, stringsAsFactors = F))

n = nrow(myfiles[[1]])
mydata = data.frame(i=seq(1,n-1))
listNames = c('AAPL','IBM','SP500')
i = 1

for (file in myfiles){
  
  
  # Calculate the percentage changes in the adjusted (for dividends and any stock splits) 
  # closing prices from one month to the next.
  
  mydata[listNames[i]] = file[1:(n-1),"Adj.Close"] - file[2:n,"Adj.Close"]
  
  i=i+1
}

drops <- c('i')
mydata = mydata[,!(names(mydata) %in% drops)]


###############

# Import data
path <- "\\\\nas1/labuser169/MSIA_401_Statistical Methods for Data Mining/Homework/"
files <- list.files(getwd(),pattern ="stockPrices*")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

# Look at data
head(stockPrices_AAPL)

###############

createListData = function(path,pattern){
  list_files = list.files(path,pattern = pattern)
  listData = list()
  for (file in list_files){
    mydata = read.csv(file,header = T, stringsAsFactors = F)
    n = which(strsplit(file, "")[[1]]==".") #position of period
    filename = gsub(" ","",substr(file, 1, n-1))  #get string before period and delete spaces
    listData[filename] = mydata #store data with filename in list
  }
  
  return (listData)
}

l = createListData(getwd(),"stockPrices*")

################