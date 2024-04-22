ffs.path.2010=paste0("data/input/ffs-costs/aged10.csv")
ffs.path.2011=paste0("data/input/ffs-costs/aged11.csv")
ffs.path.2012=paste0("data/input/ffs-costs/aged12.csv")
ffs.path.2013=paste0("data/input/ffs-costs/aged13.csv")
ffs.path.2014=paste0("data/input/ffs-costs/aged14.csv")
ffs.path.2015=paste0("data/input/ffs-costs/FFS15.xlsx")

drops=array(dim=c(6,2))
drops[,1]=c(2010:2015)
drops[,2]=c(7,2,2,2,2,2)



## Years 2010-2014
for (y in 2010:2014){
  d=drops[which(drops[,1]==y),2]
  ffs.data=read_csv(get(paste0("ffs.path.",y)),
                    skip=d,
                    col_names=FALSE, na="*")
  ffs.data=ffs.data[,1:15]
  names(ffs.data) = c("ssa","state","county_name","parta_enroll",
              "parta_reimb","parta_percap","parta_reimb_unadj",
              "parta_percap_unadj","parta_ime","parta_dsh",
              "parta_gme","partb_enroll",
              "partb_reimb","partb_percap",
              "mean_risk")  
 
  ffs.costs <- ffs.data %>%
    select(ssa,state,county_name,parta_enroll,parta_reimb,
           partb_enroll,partb_reimb,mean_risk) %>%
    mutate(year=y,
           ssa=as.numeric(ssa)) %>%
    mutate_at(vars(parta_enroll, parta_reimb, partb_enroll, partb_reimb, mean_risk),~str_replace_all(.,",",""))  %>%      
    mutate_at(vars(parta_enroll, parta_reimb, partb_enroll, partb_reimb, mean_risk),as.numeric)
  
  assign(paste("ffs.costs.",y,sep=""),ffs.costs)
  
}

## 2015
d=drops[which(drops[,1]==2015),2]
ffs.data=read_xlsx(get(paste0("ffs.path.",2015)),
                   skip=d,
                   col_names=c("ssa","state","county_name","parta_enroll",
                               "parta_reimb","parta_percap","parta_reimb_unadj",
                               "parta_percap_unadj","parta_ime","parta_dsh",
                               "parta_gme","partb_enroll",
                               "partb_reimb","partb_percap",
                               "mean_risk"), na="*")

                  
ffs.costs <- ffs.data %>%
  select(ssa,state,county_name,parta_enroll,parta_reimb,
         partb_enroll,partb_reimb,mean_risk) %>%
  mutate(year=2015,
         ssa=as.numeric(ssa)) %>%
  mutate_at(vars(parta_enroll, parta_reimb, partb_enroll, partb_reimb, mean_risk),~str_replace_all(.,",",""))  %>%  
  mutate_at(vars(parta_enroll, parta_reimb, partb_enroll, partb_reimb, mean_risk),as.numeric)  

assign(paste("ffs.costs.",2015,sep=""),ffs.costs)

ffs.costs.final=rbind(ffs.costs.2010, ffs.costs.2011, ffs.costs.2012,
                      ffs.costs.2013, ffs.costs.2014, ffs.costs.2015)

write_rds(ffs.costs.final,"data/output/ffs_costs.rds")