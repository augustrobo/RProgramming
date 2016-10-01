pollutantmean = function(directory, pollutant, id = 1:332){
        files_list = list.files(directory, full.names = TRUE)
        data = data.frame()
        for (i in id){
                data = rbind(data, read.csv(files_list[i]))
        }
        realdata = data[pollutant]
        return(mean(realdata[[1]], na.rm = TRUE))       
}

pollutantmean("specdata", "sulfate", 1:10) # [1] 4.064
pollutantmean("specdata", "nitrate", 70:72) # [1] 1.706
pollutantmean("specdata", "nitrate", 23) # [1] 1.281
pollutantmean("specdata", "nitrate", 70:72) # [1] 1.706
pollutantmean("specdata", "sulfate", 34) # [1] 1.477
pollutantmean("specdata", "nitrate") # [1] 1.703


###################################################################
complete = function(directory, id = 1:332){
        files_list = list.files(directory, full.names = TRUE)
        data = data.frame()
        for (i in id){
                csvdata = read.csv(files_list[i])
                index = is.na(csvdata$sulfate) | is.na(csvdata$nitrate)
                dat_subset = csvdata[!index, ]
                data = rbind(data, c(i, nrow(dat_subset)))
        }
        names(data) = c("id", "nobs")
        data
}

complete("specdata", 1)
##   id nobs
## 1  1  117
complete("specdata", c(2, 4, 8, 10, 12))
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96
complete("specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463
complete("specdata", 3)
##   id nobs
## 1  3  243

complete("specdata", c(6, 10, 20, 34, 100, 200, 310))$nobs
# [1] 228 148 124 165 104 460 232

complete("specdata", 54)$nobs
# [1] 219

set.seed(42)
complete("specdata", 332:1)$nobs[sample(332, 10)]
# [1] 711 135  74 445 178  73  49   0 687 237


###################################################################
corr = function(directory, threshold = 0){
#         files_list = list.files(directory, full.names = TRUE)
#         data = complete(directory)
#         idlist = data$id[data$nobs > threshold]
#         correlation = c()
#         for (i in idlist){
#                 csvdata = read.csv(files_list[i])
#                 correlation = c(correlation, cor(csvdata$sulfate,csvdata$nitrate, use = "pairwise.complete.obs"))
#         }
#         correlation
        
        files_list = list.files(directory, full.names = TRUE)
        correlation = c()
        for (i in 1:332){
                csvdata = read.csv(files_list[i])
                if (sum(complete.cases(csvdata)) > threshold){
                        correlation = c(correlation, 
                                        cor(csvdata$sulfate,csvdata$nitrate, 
                                            use = "pairwise.complete.obs"))   
                }
        }
        correlation
}

cr = corr("specdata", 400)
head(cr)
# [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
summary(cr)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630

cr = corr("specdata", 150)
head(cr)
# [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
summary(cr)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630

cr = sort(corr("specdata"))              
set.seed(868)                
round(cr[sample(length(cr), 5)], 4)
# [1]  0.2688  0.1127 -0.0085  0.4586  0.0447

cr = sort(corr("specdata", 129))               
n = length(cr)              
set.seed(197)                
c(n, round(cr[sample(n, 5)], 4))
# [1] 243.0000   0.2540   0.0504  -0.1462  -0.1680   0.5969

cr = corr("specdata", 2000)                
n = length(cr)                
cr = sort(corr("specdata", 1000))              
c(n, round(cr, 4))
# [1]  0.0000 -0.0190  0.0419  0.1901


