library(RColorBrewer)
library(dplyr)
library(ggpubr)
library(cowplot)
library(lubridate)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ## Set working directory to source file
eval_indice_RF = read.csv('./Model_Output_data/error_index_rf_df_1')
eval_indice_RF <- eval_indice_RF %>% mutate(Model = 'RF', Corr.k = sqrt(R2))
names(eval_indice_RF)[names(eval_indice_RF) == 'site_no'] <- 'STAID'

watershed <- read.csv('watershed_characteristics.csv', check.names = F)
watershed$REGIME <- factor(watershed$REGIME, levels = c('Rainfall dominant','Transient','Snowmelt dominant'))
eval_indice_RF$Regime<- watershed$REGIME
head(eval_indice_RF)
eval_indice_mlr = read.csv('./Model_Output_data/error_index_mlr_df_1')
names(eval_indice_mlr)[names(eval_indice_mlr) == 'site_no'] <- 'STAID'
eval_indice_mlr <- eval_indice_mlr %>% mutate(Model = 'MLR', Corr.k = sqrt(R2)) 


colr = c('#1F78B4','#FF7F00','#E31A1C') ## Select color theme for three hydrologic regimes

#### ----- Compare the model against persistence and MLR using R2---- ####
## Read in raw streamflow data
folder = './Raw_Streamflow_Data/'
streamflow = list()
study_period = data.frame(seq(ymd(20090101),ymd(20181231), by = 'day'))
colnames(study_period) <- 'Date'
persistence = data.frame(matrix(0,nrow = nrow(watershed), ncol = 2))
colnames (persistence) <- c('STAID','Corr.k')
for (i in 1:nrow(watershed)){
  dat = read.csv(paste(folder,watershed$STAID[i], sep=''), stringsAsFactors = F)
  colnames(dat) <- c('STAID','Date','daily_flow')
  dat$Date = ymd(dat$Date) ## Convert the date into ymd format
  dat$daily_flow = as.numeric(dat$daily_flow)
  dat = left_join(study_period,dat, by = 'Date')  ## To make sure every station has the same number of obs, NA's will be introduced with missing data
  dat = dat[dat$Date >= ymd(20160101),] ## Extract the data for the validation period
  corr.k = cor(dat$daily_flow, lag(dat$daily_flow,1), use = 'complete.obs')
  persistence$STAID[i] = dat$STAID[1]
  persistence$Corr.k[i] = corr.k
}

persistence = persistence %>% mutate(Model = 'Pers')

pers_comp_df = rbind(persistence,eval_indice_RF[,c('STAID','Corr.k','Model')],
                     eval_indice_mlr[,c('STAID','Corr.k','Model')] )
pers_comp_df = left_join(pers_comp_df, watershed[c('STAID','REGIME')])
##----- Plot using ggplot ------##

RF_Naive.p <- ggplot() + 
  geom_point(aes( x = pers_comp_df[pers_comp_df$Model == 'Pers', c('Corr.k')], 
                  y = pers_comp_df[ pers_comp_df$Model == 'RF', c('Corr.k')],
                  col = pers_comp_df[ pers_comp_df$Model == 'Pers', c('REGIME')]), 
             size = 4) +
  ggtitle("") +  xlab( expression('Naive Model Corr. Coef.' ) ) + 
  ylab( expression('Random Forest Corr. Coef. ' )) +
  theme_bw()  + theme(
    axis.text = element_text(size = 14, color = 'black'),
    axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 7, b = 0, l = 0)),
    axis.title.x = element_text(margin = ggplot2::margin(t = 7, r = 0, b = 0, l = 0)),
    axis.title = element_text(size = 14),
    axis.ticks =  element_blank(),
    legend.text = element_text(size = 14),
    legend.title=element_text(size = 14),
    plot.margin = unit(c(0.5,1,0.5,0.5), "cm") ) + 
  scale_x_continuous(limits = c(0.65,1)) +  scale_y_continuous(limits = c(0.65,1)) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + 
  annotate("text", x = 0.72, y = 0.66, label = '1-1 line', size= 4)
RF_Naive.p <- RF_Naive.p + scale_color_manual(values= colr, name = 'Regime')
RF_Naive.p

RF_MLR.p <- ggplot() + 
  geom_point(aes( x = pers_comp_df[pers_comp_df$Model == 'MLR', c('Corr.k')], 
                  y = pers_comp_df[ pers_comp_df$Model == 'RF', c('Corr.k')],
                  col = pers_comp_df[ pers_comp_df$Model == 'MLR', c('REGIME')]), 
             size = 4) +
  ggtitle("") +  xlab( expression('MLR Corr. Coef.' ) ) + 
  ylab( expression('Random Forest Corr. Coef. ' )) +
  theme_bw()  + theme(axis.text = element_text(size = 14, color = 'black'),
                      axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 7, b = 0, l = 0)),
                      axis.title.x = element_text(margin = ggplot2::margin(t = 7, r = 0, b = 0, l = 0)),
                      axis.title = element_text(size = 14),
                      legend.text = element_text(size = 14),
                      legend.title=element_text(size = 14),
                      plot.margin = unit(c(0.5,1,0.5,0.5), "cm") ) + 
  scale_x_continuous(limits = c(0.65,1)) +  scale_y_continuous(limits = c(0.65,1)) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + 
  annotate("text", x = 0.72, y = 0.66, label = '1-1 line', size= 4)
RF_MLR.p <- RF_MLR.p + scale_color_manual(values= colr, name = 'Regime')
RF_MLR.p

MLR_Naive.p <- ggplot() + 
  geom_point(aes( x = pers_comp_df[pers_comp_df$Model == 'Pers', c('Corr.k')], 
                  y = pers_comp_df[ pers_comp_df$Model == 'MLR', c('Corr.k')],
                  col = pers_comp_df[ pers_comp_df$Model == 'Pers', c('REGIME')]), 
             size = 4) +
  ggtitle("") +  xlab( expression('Naive Model Corr. Coef.' ) ) + 
  ylab( expression('MLR Corr. Coef. ' )) +
  theme_bw()  + theme(axis.text = element_text(size = 14, color = 'black'),
                      axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 7, b = 0, l = 0)),
                      axis.title.x = element_text(margin = ggplot2::margin(t = 7, r = 0, b = 0, l = 0)),
                      axis.title = element_text(size = 14),
                      legend.text = element_text(size = 14),
                      legend.title=element_text(size = 14),
                      plot.margin = unit(c(0.5,1,0.5,0.5), "cm") ) + 
  scale_x_continuous(limits = c(0.65,1)) +  scale_y_continuous(limits = c(0.65,1)) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed') + 
  annotate("text", x = 0.72, y = 0.66, label = '1-1 line', size= 4)
MLR_Naive.p <- MLR_Naive.p + scale_color_manual(values= colr, name = 'Regime')
MLR_Naive.p


##-------- Figure 05: Benchmark RF against Multiple Linear Regression and Naïve models ------ ###
ggarrange(RF_Naive.p,RF_MLR.p,MLR_Naive.p, ncol =3, nrow =1, labels = c("(a)", "(b)","(c)"),
          common.legend = TRUE, legend = "bottom",
          font.label = list (size = 15, face ='bold'))

#ggsave(filename = 'RF_MLR_Naive.eps', height = 6, width = 16, dpi = 300, unit = 'in')

##-------- Evaluate RF model using KGE, R2, RMSE, and MAE metrics ------- ##

MAE.p <- ggplot(eval_indice_RF, aes(x = Regime, y = MAE, color = Regime )) +  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2) , cex = 1.5) 

MAE.p <- MAE.p +  scale_color_manual(values = colr) + 
  theme_bw() + theme(legend.position = "right", 
                     axis.title.x = element_blank(),
                     axis.text.x  = element_blank(),
                     axis.ticks.x = element_blank(),
                     axis.text.y  = element_text(size = 14, color = 'black'),
                     axis.title.y = element_text(size = 14, color = 'black'),
                     plot.margin = unit(c(0.5,0.5,1,0.5), "cm"),
                     legend.text = element_text(size=14),
                     legend.title = element_text(size = 14),
                     legend.key.size = unit(1.5,"line") )
MAE.p      

RMSE.p <- ggplot(eval_indice_RF, aes(x = Regime, y = RMSE, color = Regime )) +  geom_boxplot() + 
  geom_jitter(position=position_jitter(0.2) , cex = 1.5) 

RMSE.p <- RMSE.p +  scale_color_manual(values = colr) + 
  theme_bw() + theme(legend.position = "right", 
                     axis.title.x = element_blank(),
                     axis.text.x  = element_blank(),
                     axis.ticks.x = element_blank(),
                     axis.text.y  = element_text(size = 14, color = 'black'),
                     axis.title.y = element_text(size = 14, color = 'black'),
                     plot.margin = unit(c(1,0.5,1,0.5), "cm"),
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14),
                     legend.key.size = unit(1.5,"line") )
RMSE.p
ggarrange(MAE.p, RMSE.p, common.legend = TRUE, 
          legend = "bottom", nrow = 1, ncol = 2,align = "v",
          widths = c(1, 1),
          font.label = list (size = 20, face ='bold') )


R2_KGE <- ggscatterhist(eval_indice_RF, x = "R2", y = "KGE", color = 'Regime',
                        size = 3, alpha = 0.6, linetype = 'solid', xlab = expression(R^2),
                        palette = colr, xlim = c(0.55, 1), ylim = c(0.55,1),
                        margin.plot = "boxplot",
                        ggtheme = theme_bw(), legend = '"none"',
                        margin.params = list(fill = "Regime", color = colr, size = 0.2),
                        font.xtickslab =  c(14, "black"),
                        font.ytickslab =  c(14, "black"),
                        font.x = c(14), font.y = c(14)
) 
R2_KGE <- R2_KGE + theme(plot.margin = unit(c(0.5,2,0.5,2), "cm"))
R2_KGE

##---------- Figure 06: RF model performance across three hydrologic regimes ------ ##
ggarrange(R2_KGE, ggarrange(MAE.p, RMSE.p, common.legend = TRUE, align = 'hv', 
                            legend = "bottom", nrow = 1, ncol = 2, labels = c('(b)','(c)'),
                            font.label = list (size = 15, face ='bold'), hjust = -0.8, vjust = 1.1)
          , labels = c('(a)'),   nrow = 2, hjust = -1.0, 
          font.label = list (size = 15, face ='bold'))

#ggsave("R2_KGE_MAE_RMSE.png",  dpi = 300, width = 7, height = 10) 

#### --------- Analysis of the Variable Importance ------- ####
##-------- Calculate the importance of variables as boxplot--------##
var_import_list = read.csv('./Model_output_data//var_import_1', check.names = F)
names(var_import_list)[names(var_import_list) == 'site_no'] <- 'STAID'
var_import_list = inner_join(var_import_list,watershed[,c('STAID','REGIME')], by='STAID')

var_import_list$class = NULL
var_import_list = var_import_list %>% group_by(STAID) %>% mutate(rel_IncNPurity = 100 * IncNodePurity/sum(IncNodePurity))
levels(var_import_list$Predictors)
levels(var_import_list$Predictors) <-  c('Pentad','Precipitation','SWE','Snowmelt','Streamflow','3-d Precipitation','Tmax', 'Tmin')
RAIN_var = var_import_list[var_import_list$REGIME == 'Rainfall dominant',]
TRAN_var = var_import_list[var_import_list$REGIME == 'Transient',]
SNOW_var = var_import_list[var_import_list$REGIME == 'Snowmelt dominant',]
n_each_class = table(watershed$REGIME)
#colnames(varImport_list_MDA) <- c('Pentad','Precipitation','SWE','Snowmelt','Streamflow','3-d Precipitation','Tmax', 'Tmin')
par(mfrow = c(2,3), mai = c(0.8, 1.3, 0.5, 0.4))
#par(mar = c(4,10,4,4))


#### ----- Now plot the same information above but using only median values -----  ####
RAIN_var = RAIN_var %>% group_by(Predictors) %>% summarize( median_IncMSE = median(`%IncMSE`), median_rel_IncNPurity = median(rel_IncNPurity),
                                                            min_IncMSE = min(`%IncMSE`), max_IncMSE = max(`%IncMSE`),
                                                            min_rel_IncNPurity = min (rel_IncNPurity), max_rel_IncNPurity = max(rel_IncNPurity))
TRAN_var = TRAN_var %>% group_by(Predictors) %>% summarize( median_IncMSE = median(`%IncMSE`), median_rel_IncNPurity = median(rel_IncNPurity),
                                                            min_IncMSE = min(`%IncMSE`), max_IncMSE = max(`%IncMSE`),
                                                            min_rel_IncNPurity = min (rel_IncNPurity), max_rel_IncNPurity = max(rel_IncNPurity))
SNOW_var = SNOW_var %>% group_by(Predictors) %>% summarize( median_IncMSE = median(`%IncMSE`), median_rel_IncNPurity = median(rel_IncNPurity), 
                                                            min_IncMSE = min(`%IncMSE`), max_IncMSE = max(`%IncMSE`),
                                                            min_rel_IncNPurity = min (rel_IncNPurity), max_rel_IncNPurity = max(rel_IncNPurity))

par(mfrow = c(2,3), mai = c(0.7, 1.3, 0.5, 0.4))
##----- Mean Decrease in Accuracy -----##
RAIN_var = RAIN_var[order(RAIN_var$median_IncMSE, decreasing = F),] ## Rearrange the order based on the median value

TRAN_var = TRAN_var[order(TRAN_var$median_IncMSE, decreasing = F),] ## Rearrange the order based on the median value

SNOW_var = SNOW_var[order(SNOW_var$median_IncMSE, decreasing = F),] ## Rearrange the order based on the median value


##------- Mean Decrease in Node Impurity (%) -------- ##
RAIN_var = RAIN_var[order(RAIN_var$median_rel_IncNPurity, decreasing = F),] ## Rearrange the order


TRAN_var = TRAN_var[order(TRAN_var$median_rel_IncNPurity, decreasing = F),] ## Rearrange the order


SNOW_var = SNOW_var[order(SNOW_var$median_rel_IncNPurity, decreasing = F),] ## Rearrange the order

##------ Plot the same thing using ggplot
MDA_1<-ggplot(RAIN_var) + ggtitle(paste('Rainfall dominant'))+
  geom_bar( aes(x= reorder(Predictors,median_IncMSE), y= median_IncMSE),
            stat="identity", fill="dodgerblue4", alpha=0.7, position=position_dodge()) + 
  coord_flip() +  xlab("") + ylab("Mean Decrease in Accuracy (%)") +
  geom_errorbar(aes(x=reorder(Predictors,median_IncMSE),ymin=min_IncMSE , ymax=max_IncMSE), width=.2,
                position=position_dodge(.9)) + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold", size = 13),
        axis.text=element_text(size=12), axis.title=element_text(size=12),
        plot.margin = unit(c(0.5,1,0.5,.3),"cm"),
        panel.background = element_rect(fill = "gray95", colour = "white" ) ) 

MDA_2<-ggplot(TRAN_var) + ggtitle(paste('Transient '))+
  geom_bar( aes(x= reorder(Predictors,median_IncMSE), y=median_IncMSE), stat="identity", fill="#4393C3", alpha=0.7,
            position=position_dodge()) + 
  coord_flip() +  xlab("") + ylab("Mean Decrease in Accuracy (%)") +
  geom_errorbar(aes(x=reorder(Predictors,median_IncMSE),ymin=min_IncMSE , ymax=max_IncMSE), width=.2,
                position=position_dodge(.9)) + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold", size = 13),
        axis.text=element_text(size=12), axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.position="bottom", plot.margin = unit(c(0.5,1,0.5,.3),"cm"),
        panel.background = element_rect(fill = "gray95", colour = "white" )) 
MDA_3<- ggplot(SNOW_var) + ggtitle(paste('Snowmelt dominant'))+
  geom_bar( aes(x=reorder(Predictors,median_IncMSE), y=median_IncMSE), stat="identity", 
            fill="steelblue1", alpha=0.7,
            position=position_dodge()) + 
  coord_flip() +  xlab("") + ylab("Mean Decrease in Accuracy (%)") +
  geom_errorbar(aes(x=reorder(Predictors,median_IncMSE),ymin=min_IncMSE , ymax=max_IncMSE), width=.2,
                position=position_dodge(.9)) + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold",size = 13),
        axis.text=element_text(size=12), axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.position="bottom", plot.margin = unit(c(0.5,1,0.5,.3),"cm"),
        panel.background = element_rect(fill = "gray95", colour = "white" )) 
MDI_1 <-  ggplot(RAIN_var) + ggtitle(paste('                  ')) + 
  geom_bar( aes(x= reorder(Predictors,median_rel_IncNPurity), y=median_rel_IncNPurity), 
            stat="identity", fill="dodgerblue4", alpha=0.7,  position=position_dodge()) + 
  coord_flip() +  xlab("") + ylab("Mean Decrease in Node Impurity (%)") +
  geom_errorbar(aes(x=reorder(Predictors,median_rel_IncNPurity),ymin=min_rel_IncNPurity , ymax=max_rel_IncNPurity), width=.2,
                position=position_dodge(.9)) + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text=element_text(size=12), axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.position="bottom", plot.margin = unit(c(0.5,1,0.5,.3),"cm"),
        panel.background = element_rect(fill = "gray95", colour = "white" )) 
MDI_2 <-  ggplot(TRAN_var) + ggtitle(paste('                ')) + 
  geom_bar( aes(x=reorder(Predictors,median_rel_IncNPurity), y=median_rel_IncNPurity), stat="identity", 
            fill="#4393C3", alpha=0.7,position=position_dodge()) + 
  coord_flip() +  xlab("") + ylab("Mean Decrease in Node Impurity (%)") +
  geom_errorbar(aes(x=reorder(Predictors,median_rel_IncNPurity),ymin=min_rel_IncNPurity , ymax=max_rel_IncNPurity), width=.2,
                position=position_dodge(.9)) + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text=element_text(size=12), axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.position="bottom", plot.margin = unit(c(0.5,1,0.5,.3),"cm"),
        panel.background = element_rect(fill = "gray95", colour = "white" )) 
MDI_3 <-  ggplot(SNOW_var) + ggtitle(paste('                   ')) + 
  geom_bar( aes(x=reorder(Predictors,median_rel_IncNPurity), y=median_rel_IncNPurity), stat="identity", 
            fill="steelblue1", alpha=0.7, position=position_dodge()) + 
  coord_flip() +  xlab("") + ylab("Mean Decrease in Node Impurity (%)") +
  geom_errorbar(aes(x=Predictors,ymin=min_rel_IncNPurity , ymax=max_rel_IncNPurity), width=.2,
                position=position_dodge(.9)) + theme_bw()+
  theme(plot.title = element_text(hjust = 0.5, size = 13),
        axis.text=element_text(size = 12), axis.title=element_text(size=12),
        legend.text=element_text(size=12),axis.title.x = element_text(colour = "black"),
        legend.position="bottom", plot.margin =  unit(c(0.5,.9,0.5,.1),"cm"),
        panel.background = element_rect(fill = "gray95", colour = "white" ))

##------- Figure 08: Variable importance: Mean Decrease in Accuracy and Mean Decrease in Node Impurity -------##
ggarrange(MDA_1, MDA_2, MDA_3,
          MDI_1, MDI_2, MDI_3, labels= c("(a)", "(b)","(c)","(d)","(e)","(f)"),
          common.legend = TRUE, legend = "bottom", nrow = 2, ncol = 3,
          font.label = list (size = 15, face ='bold')
)

#ggsave("Importance_of_variables_bar.png", dpi = 300, height = 9, width = 12)
#ggsave("Importance_of_variables_bar.eps", dpi = 300, height = 8, width = 12)

##------------------ Analysis basin characteristics on model performance --------------- ## 
## Relationship between KGE score and percentage of sand for the watersheds
watershed_cha = inner_join(watershed,eval_indice_RF, by = 'STAID')
p <- ggscatter(watershed_cha, x = "SANDAVE", y = "KGE",
               color = "Regime", size = 2,xlab = 'Percentage of Sand',
               palette = colr,add = c("reg.line"),
               ggtheme = theme_bw(), legend = 'bottom',
               font.xtickslab =  c(14, "black"),
               font.ytickslab =  c(14, "black"),
               font.x = c(14), font.y = c(14),
               font.legend = c(14)
               
) 
##----- Figure 09: Scatter plot of KGE scores plotted against average percentage of sand in soil  ---- ##
p <- p + theme( axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)),
                axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)),)
#ggsave("KGE_SAND.png", dpi = 300, width = 8, height = 5)
#ggsave("KGE_SAND.eps", dpi = 300, width = 8, height = 5)



####---- Calculate Pearson's correlation for other basin characteristics ---- ####
basin_characteristics = c('SLOPE_PCT','ASPECT_EASTNESS','DRAIN_SQKM','BAS_COMPACTNESS',
                          'STREAMS_KM_SQ_KM','SANDAVE','FORESTNLCD06')
basin_cha_cor = data.frame(matrix(0, nrow = length(basin_characteristics), ncol = 4))
basin_cha_cor = list()
for (i in 1: length(basin_characteristics)){
  basin_cha_df = list()
  for (j in c(1:3)){ ## Going through 3 classes
    dat = watershed_cha[watershed_cha$REGIME == levels(watershed_cha$REGIME)[j],]
    cor_test = cor.test(dat[,c('KGE')],dat[, basin_characteristics[i]],method=c("pearson"))
    corr.k = cor_test$estimate[[1]]
    p_val = cor_test$p.value[[1]]
    basin_cha_df  [[j]] = data.frame(basin_cha = basin_characteristics[i],
                                     class = j,
                                     corr.k = round(corr.k,2),
                                     p_val = round(p_val,2),
                                     p_val_5_sign = p_val<0.05)
  }
  basin_cha_df = bind_rows(basin_cha_df)
  basin_cha_cor [[i]] = basin_cha_df
}
basin_cha_cor = bind_rows(basin_cha_cor)

##----- Table 05: Pearson correlation coefficient between KGE scores and selected basin variables ---- ##
basin_cha_cor
