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



