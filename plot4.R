plot4 <- function() {
        #First we use load_trim to load the data and clean adjust it to what we need to plot
        file <- "household_power_consumption.txt"
        begin<- 65000
        length <- 10000
        dates <- as.Date(c("1/2/2007","2/2/2007"),"%d/%m/%Y")
        data <- load_trim(file,begin,length,dates)
        
        
        #We open a graphics device and plot the histogram 
        png(file="figure/plot4.png",width=480,height=480)
        
        par(mfrow = c(2,2))
        #Top Left Graph
        plot(data$Chrono,data$Global_active_power,lty=1,pch=".",xlab="",
             main = "",ylab = "Global Active Power")
        lines(data$Chrono,data$Global_active_power)
        #Top Right Graph
        plot(data$Chrono,data$Voltage,lty=1,pch=".",xlab="datetime",
             main = "",ylab = "Voltage")
        lines(data$Chrono,data$Voltage)
        #Bottom Left Graph
        plot(data$Chrono,data$Sub_metering_1,lty=1,pch=".",xlab="",
             main = "",ylab = "Energy sub metering")
        lines(data$Chrono,data$Sub_metering_1)
        lines(data$Chrono,data$Sub_metering_2,col="red")
        lines(data$Chrono,data$Sub_metering_3,col="blue")
        legend("topright",c("Sub_metering_1","Sub_metering_2",
                            "Sub_metering_3"),lty = c(1,1,1),
               col=c("black","red","blue"),bty = "n")
        #Bottom Right Graph
        plot(data$Chrono,data$Global_reactive_power,lty=1,pch=".",xlab="datetime",
             main = "",ylab = "Global_reactive_power")
        lines(data$Chrono,data$Global_reactive_power)
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