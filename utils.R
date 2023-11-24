ggplot_PLS<-function(PLS_results)
{
  library(ggplot2)
  PLS_gg<-PLS_results$PLS_summary
  PLS_gg[,"Month"]<-trunc(PLS_gg$Date/100)
  PLS_gg[,"Day"]<-PLS_gg$Date-PLS_gg$Month*100
  PLS_gg[,"Date"]<-ISOdate(2002,PLS_gg$Month,PLS_gg$Day)
  PLS_gg[which(PLS_gg$JDay<=0),"Date"]<-
    ISOdate(2001,
            PLS_gg$Month[which(PLS_gg$JDay<=0)],
            PLS_gg$Day[which(PLS_gg$JDay<=0)])
  PLS_gg[,"VIP_importance"]<-PLS_gg$VIP>=0.8
  PLS_gg[,"VIP_Coeff"]<-factor(sign(PLS_gg$Coef)*PLS_gg$VIP_importance)
  
  VIP_plot<- ggplot(PLS_gg,aes(x=Date,y=VIP)) +
    geom_bar(stat='identity',aes(fill=VIP>0.8)) +
    scale_fill_manual(name="VIP", 
                      labels = c("<0.8", ">0.8"), 
                      values = c("FALSE"="grey", "TRUE"="blue")) +
    theme_bw(base_size=15) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank() )
  
  coeff_plot<- ggplot(PLS_gg,aes(x=Date,y=Coef)) +
    geom_bar(stat='identity',aes(fill=VIP_Coeff)) +
    scale_fill_manual(name="Effect direction", 
                      labels = c("Advancing", "Unimportant","Delaying"), 
                      values = c("-1"="red", "0"="grey","1"="dark green")) +
    theme_bw(base_size=15) +
    ylab("PLS coefficient") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank() )
  
  temp_plot<- ggplot(PLS_gg) +
    geom_ribbon(aes(x=Date,ymin=Tmean-Tstdev,ymax=Tmean+Tstdev),
                fill="grey") +
    geom_ribbon(aes(x=Date,ymin=Tmean-Tstdev*(VIP_Coeff==-1),
                    ymax=Tmean+Tstdev*(VIP_Coeff==-1)),
                fill="red") +
    geom_ribbon(aes(x=Date,ymin=Tmean-Tstdev*(VIP_Coeff==1),
                    ymax=Tmean+Tstdev*(VIP_Coeff==1)),
                fill="dark green") +
    geom_line(aes(x=Date,y=Tmean)) +
    theme_bw(base_size=15) +
    ylab(expression(paste(T[mean]," (°C)")))
  
  library(patchwork)
  plot<- (VIP_plot +
            coeff_plot +
            temp_plot +
            plot_layout(ncol=1,
                        guides = "collect")
  ) & theme(legend.position = "right",
            legend.text = element_text(size=8),
            legend.title = element_text(size=10),
            axis.title.x=element_blank())
  
  plot
}

custom_color_scale <- function(legend_title) {
  custom_colors <- c("blue", "green", "yellow", "red")
  custom_values <- c(0, 0.25, 0.5, 1)
  
  custom_scale <- scale_fill_gradientn(
    colors = custom_colors,
    values = custom_values,
    guide = guide_colorbar(
      title = legend_title))
  return(custom_scale)}

plotly_annotater <- function(calib,valid) {
  annotation_txt <- paste("Calib (",length(calib[[1]]$pheno),"):\nAbs. Mean: ", round(calib[[2]], 3),"\n",
                          "RMSE: ", round(calib[[3]],3),"\n",
                          "RPIQ: ", round(calib[[4]],3),
                          "\nValid (",length(valid[[1]]$pheno),"):\nAbs. Mean: ", round(valid[[2]], 3),"\n",
                          "RMSE: ", round(valid[[3]],3),"\n",
                          "RPIQ: ", round(valid[[4]],3),sep="")
  return(annotation_txt)
}






triple_digit2DOY <- function(x, year) {
  require(lubridate)
  x <- as.character(x)
  if (nchar(x) == 3) {
    month <- substring(x,1,1)
    day <- substring(x,2,3)
    date <- paste(year,"/","0",month,"/",day,sep ="")
    DOY <- yday(date)
  
  }
  else if (nchar(x) == 4) {

    month <- substring(x,1,2)
    day <- substring(x,3,4)
    date <- paste(year,"/",month,"/",day,sep ="")
    DOY <- yday(date)

  }
  else if (x=="0") {
    DOY = NaN
  }
  else {
    print("Wrong format")
  }
  return(DOY)
}

# Random-split
split_list_randomly <- function(input_list, train_split,valid_split) {
  if (typeof(train_split) != typeof(valid_split)) {
    stop("Input has two be the same type.")}
  shuffled_list <- sample(input_list)
  if (train_split < 1) {
    if (train_split + valid_split > 1) {
      stop("Ratios do not add up to one.")}
    group1 <- shuffled_list[1:(train_split*length(shuffled_list))]
    group2 <- shuffled_list[(train_split*length(shuffled_list) + 1):(train_split*length(shuffled_list)+(train_split*length(shuffled_list)))]
  }
  if (train_split > 1) {
    if (train_split + valid_split > length(input_list)) {
      stop("Sizes do not add up to the length of the input list.")}
    group1 <- shuffled_list[1:train_split]
    group2 <- shuffled_list[(train_split + 1):(train_split+valid_split)]
  }
  
  return(list(Train = group1, Validation = group2))
}



# Load and prep data
data <- read.csv2("data/cherry_blossom_japan_edited.csv", check.names = FALSE)
row.names(data) <- data$Name
data <- data[,-1]
doy_data <- data

# Applying the DOY function to the whole df (by indexing)
for (c in colnames(data)) {
  year <- as.character(c)
  for (r in 1:length(rownames((data)))) {
    doy_data[r,c] <- triple_digit2DOY(data[r,c],year)
  }
}

# Rownames to own coloumn
library(tibble)
doy_data <- doy_data %>%
  rownames_to_column(var = "Locations")
row.names(doy_data) <- NULL


# Switching rows and coloumns
doy_data_t <- data.frame(t(doy_data[-1]))
colnames(doy_data_t) <- doy_data[, 1]

write.csv2(doy_data_t, "data/cherry_phenology_doy.csv")


