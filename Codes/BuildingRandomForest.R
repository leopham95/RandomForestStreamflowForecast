library(randomForest)
library(hydroGOF)
library(dplyr)
set.seed(123)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ## Set working directory to source file
## Last modified: 11/12/2019
####--------------Adjust this ------------####
version = 'Ver_11_12_19.002'
note = 'In this version: use the Parameter_optimization_lowest_MAE.csv to optimize mtry for each gage, set ntree = 2000
        Split 70/30 training/validation, 
        1-day, 
        8 predictors,
        use pre-processed train data set Ver_11_01_19.002
        '
####--------------Adjust this ------------####
dir.create(paste(getwd(),'/','Model_output/',version, sep = ''))
q = paste(getwd(),'/','Model_output/',version,'/', sep = '')
write(note, file = paste(q,'ReadMe.txt',sep = ''))
usgs_ID_lon_lat <- read.csv('/media/leo/Extra Drive 1/My_Passport_Summer_2019/Summer_2019/CSV Files/usgs_ID_lon_lat_86.csv')
folder = './Predictor_Data_Processed/'
lead_time = c(1)
opt_df = read.csv('/media/leo/Extra Drive 1/My_Passport_Summer_2019/Summer_2019/CSV Files/Parameter_optimization_lowest_MAE.csv')
for ( u in 1:length(lead_time )){
         n_day_prediction = lead_time[u]
         var_import_list = list()
         rf_predicted_results = list()
         mlr_predicted_results = list()
         error_index_rf_df = list()
         error_index_mlr_df = list()
         training_mse = list()
        for ( i in (1: nrow(usgs_ID_lon_lat))){
          set.seed(123+i)
          current_gage = usgs_ID_lon_lat$site_no [i]
          ####------------Supply mtry and ntree -----####
          m_try = opt_df[usgs_ID_lon_lat$site_no == current_gage, ]$mtry 
          input_df = data.frame(read.csv(paste(folder,current_gage,'.txt', sep = ''), stringsAsFactors = F))
          head(input_df)
          
          train.dat = input_df[input_df$status =='train',] %>% dplyr::select(-status,-Date)
          valid.dat = input_df[input_df$status =='valid',] %>% dplyr::select(-status,-Date) 
          ## Train the modeling for Random Forest
          rf_stream <- randomForest(
            formula = str_ave ~ .
            , data    = train.dat
            , ntree = 2000
            , importance = T
            , mtry = m_try
          )
          #print(paste(i,'watershed at prediction lead time',n_day_prediction,',mtry = ',m,'out of 8, ntree = ',n_tree_vect[n],' out of 5'))
          print(paste(i,'watershed at prediction lead time',n_day_prediction))
          
          ## Store MSE during training
          training_mse[[i]] = data.frame(site_no = current_gage, training_MSE = mean(rf_stream$mse), class = usgs_ID_lon_lat$classification_df[i])
          # Store the importance of variables
          var_import_RF = data.frame(importance(rf_stream), check.names = F, site_no = current_gage, class = usgs_ID_lon_lat$classification_df[i])
          var_import_RF$Predictors = rownames(var_import_RF)
          var_import_list [[i]] = var_import_RF
           
          prediction_rf <- predict( rf_stream, valid.dat )
          actual = valid.dat$str_ave
          error_rf = prediction_rf - actual
          rf_predicted_results[[ i ]]= data.frame(  Date          = input_df %>% filter(status == 'valid') %>% dplyr::select(Date)
                                                    , prediction_rf = prediction_rf
                                                    , actual        = actual
                                                    , error_rf      = error_rf)
          # Make prediction and store the output for MLR
          mlr.fit = lm(str_ave~. , data = train.dat )
          prediction_mlr <- predict(mlr.fit,valid.dat)
          error_mlr = prediction_mlr - actual
          mlr_predicted_results [[i]] = data.frame( Date             = input_df %>% filter(status == 'valid') %>% dplyr::select(Date)
                                                    , prediction_mlr = prediction_mlr
                                                    , actual         = valid.dat$str_ave
                                                    , error_mlr      = error_mlr )

          ## Calculate error indices
           error_index_rf_df [[i]] = error_index_calculator(rf_predicted_results[[ i ]])
           error_index_mlr_df[[i]] = error_index_calculator (mlr_predicted_results [[i]])
        }
        error_index_rf_df_summary = bind_rows(error_index_rf_df)
        #summary(error_index_rf_df_summary)
        error_index_rf_df_summary = error_index_rf_df_summary %>% mutate (site_no = usgs_ID_lon_lat$site_no, class = usgs_ID_lon_lat$classification_df)
        #write.csv(error_index_rf_df_summary, file = paste(q, 'error_index_rf_df_',n_day_prediction,'_day_pred_mtry_',m,'_ntree_',n_tree_vect[n], sep = ''), row.names = F     )
        write.csv(error_index_rf_df_summary, file = paste(q, 'error_index_rf_df_',n_day_prediction,sep = ''), row.names = F     )
        
        
        error_index_mlr_df_summary = bind_rows(error_index_mlr_df)
        error_index_mlr_df_summary = error_index_mlr_df_summary %>% mutate (site_no = usgs_ID_lon_lat$site_no, class = usgs_ID_lon_lat$classification_df)
        #write.csv(error_index_mlr_df_summary, file = paste(q, 'error_index_mlr_df_',n_day_prediction,'_day_pred_mtry_',m,'_ntree_',n, sep = ''), row.names = F     )
        write.csv(error_index_mlr_df_summary, file = paste(q, 'error_index_mlr_df_',n_day_prediction, sep = ''), row.names = F     )
        
        training_mse = bind_rows(training_mse) 
        #write.csv(training_mse, file = paste(q, 'training_mse_',n_day_prediction,'_day_pred_mtry_',m,'_ntree_',n_tree_vect[n], sep = ''), row.names = F     )
        write.csv(training_mse, file = paste(q, 'training_mse_',n_day_prediction,sep = ''), row.names = F     )
        
        var_import_list = bind_rows(var_import_list)
        #write.csv(var_import_list, file = paste(q, 'var_import_',n_day_prediction,'_day_pred_mtry_',m,'_ntree_',n_tree_vect[n], sep = ''), row.names = F     )
        write.csv(var_import_list, file = paste(q, 'var_import_',n_day_prediction, sep = ''), row.names = F     )
        
        for (i in (1:length(rf_predicted_results))){
          write.csv(rf_predicted_results[[i]], file = paste(q,usgs_ID_lon_lat$site_no[i],'_rf_',n_day_prediction,'_d.txt',sep = ''), row.names = F)
          write.csv(mlr_predicted_results[[i]], file = paste(q,usgs_ID_lon_lat$site_no[i],'_mlr_',n_day_prediction,'_d.txt',sep = ''), row.names = F)
        }
}

