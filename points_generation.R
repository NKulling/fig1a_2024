
library(pxR)
library(dplyr)
library(terra)
library(data.table)

# Loading data ------------------------------------------------------------

setwd("C:/Users/kuelling/Documents/Conferences/Fig1a/2023/code")
file<-"C:/Users/kuelling/Documents/Conferences/Fig1a/2023/cheptel/px-x-0702000000_104.px"
mun<-vect("C:/swissboundaries3d_2023-01_2056_5728.shp/swissBOUNDARIES3D_1_4_TLM_HOHEITSGEBIET.shp") # municipalities

wolf<-rast("C:/Canis.lupus_reg_covariate_ensemble.tif") # wolf raster map
evals<-fread("C:/evals_reg_covariate.csv") # wolf map binarization data

cntons<-vect("C:/swissBOUNDARIES3D_1_3_TLM_KANTONSGEBIET.shp") # cantonal data 


# Processing OFS data -----------------------------------------------------

### OFS data

data = read.px(file, na.strings = c('"."','".."','"..."','"...."','"....."','"....."','":"'))
table= as.data.frame(data)

##- Selection 

colnames(table)[3] <- "comm"

subset_df <- table%>%
  filter(str_starts(str_trim(comm), "......"),
         Jahr=="2020",                                 # Year
         Beobachtungseinheit %in% c("Tiere - Rinder", #  Beef
                                    "Tiere - Ziege",  # goat
                                    "Tiere - Schafe"), # sheep
         Betriebssystem == "Betriebssystem - Total",  # Bio and conventional
         !str_detect(comm, "^>>"), #keeping municipalities only
         !str_detect(comm, "^-"),
         !str_detect(comm, "^Schweiz")
         )

# reworking municipality string

ndf<- subset_df%>%
  mutate(mun_id = as.numeric(str_extract(comm, "\\d+")))

#  Summing all bestioles

ndf2 <- ndf %>%
  group_by(mun_id) %>%
  summarize(total_chept = sum(value, na.rm = TRUE))


# Combining OFS and spatial municipality data -----------------------------

t<-merge(mun,ndf2,by.x="BFS_NUMMER", by.y= "mun_id")

plot(t, col=t$total_chept, type="continuous")



# Generate random points --------------------------------------------------
# in each municipality extent, based on the amount of bestioles present. 

pointzz<-spatSample(t[1],t[1]$total_chept)

for(i in 2:nrow(t)){
  
  nb_pt<-t[i]$total_chept
  
  if(nb_pt <1){next}
  else{
  poinz<-spatSample(t[i],nb_pt)
  
  pointzz<-rbind(poinz,pointzz)
  
  print(t[i]$NAME)
  }
  gc()
}

writeVector(pointzz, "C:/points_cheptel.shp" )

print("done")


# Processing wolf map -----------------------------------------------------

thr<-(evals[evals$species == "Canis lupus",]$Threshold+0.2) * 100 #increasing threshold by 0.2 to be conservative

m<-c(-Inf,thr,NA,
     thr,Inf,1)

m1<-matrix(m, byrow=T, ncol=3)

wolf_bi<-classify(wolf, m1)


# generating wolf points over the range -----------------------------------

wolfz<-300 #swiss confederation estimation

wolfs<-spatSample(wolf_bi,wolfz,xy=T, na.rm=T)

wolfs_points<-vect(wolfs[,c(1,2)], geom= c("x","y"), crs="epsg:2056")


# generating wolf pack points ---------------------------------------------

packs<-32 #https://www.swissinfo.ch/eng/sci-tech/new-wolf-pack-identified-in-switzerland/48301408

set.seed(12)

packz<-spatSample(wolf_bi,packs,xy=T,na.rm=T, method="stratified")

packs_points<-vect(packz[,c(1,2)], geom= c("x","y"), crs="epsg:2056")

packs_buff<-buffer(packs_points, 100)

a<-spatSample(packs_buff[1], wolfz/nrow(packs_buff), method= "random")

for(i in 2:nrow(packs_buff)){
  
  b<-spatSample(packs_buff[i], wolfz/nrow(packs_buff), method= "random")
  
  a<-rbind(a,b)
  
}

writeVector(a, "C:/wolfpacks.shp", overwrite=T)


