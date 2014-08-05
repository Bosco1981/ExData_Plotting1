plot2 <- function() {
        #First we use load_trim to load the data and clean adjust it to what we need to plot
        file <- "household_power_consumption.txt"
        begin<- 65000
        length <- 10000
        dates <- as.Date(c("1/2/2007","2/2/2007"),"%d/%m/%Y")
        data <- load_trim(file,begin,length,dates)
        
        
        #We open a graphics device and plot the histogram 
        png(file="figure/plot2.png",width=480,height=480)
        plot(data$Chrono,data$Global_active_power,type ="l",xlab="",
             main = "",ylab = "Global Active Power (kilowatts)")
       
        dev.off()         
}

load_trim <-function(file,begin,length,dates){
        #First load length lines from begin in file and then subset to the dates
        result <- read.table(file,skip=begin,nrows=length,
                             na.strings="?",sep=";")
        names <- read.table(file,nrows=1,sep=";",stringsAsFactors =FALSE)
        colnames(result) <- names
        
        #create Chrono column to become the date time field
        result$Chrono <- strptime(paste(result$Date,result$Time),
                                  "%d/%m/%Y %H:%M:%S")
        
        #subset the dates of interest
        result$Date <- as.Date(result$Date,"%d/%m/%Y")
        ind <- result$Date %in% dates
        result <- result[ind,]
}
