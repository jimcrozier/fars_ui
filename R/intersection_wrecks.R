library(mapproj)
library(maps)

mod_dat1 <- readRDS("./data/mod_dat_all_1.rds")
mod_dat2 <- readRDS("./data/mod_dat_all_2.rds")
mod_dat3 <- readRDS("./data/mod_dat_all_3.rds")
mod_dat = rbind(mod_dat1,mod_dat2, mod_dat3)
mod_dat = mod_dat %>% filter(!is.na(fatality_ind))

asdf = mod_dat %>% group_by(latitude,longitud) %>% summarise(cnt = n()) %>% filter(cnt>10, longitud<0)
m <- map("usa", plot=FALSE)
map("usa", project="albers", par=c(39, 45))

asdf$col = cut(asdf$cnt,breaks = c(0,10,20,1000),labels=F)
for (i in 1:NROW(asdf)){
points(mapproject(list(y=asdf$latitude[i], x=asdf$longitud[i])), col=ifelse(asdf$cnt[i]>20, "red", "blue"), pch="x", cex=2)	
}
