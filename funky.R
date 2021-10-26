HEI<-function(df9, df4){
  dat<-readdat(df9, df4)
  dat<-adequacy(dat)
  dat<-moderation(dat)
  dat<-totals(dat)
  dat<-final_hei(dat)
  return(dat)
}


readdat<-function(df9, df4){
  dat9<-read.delim(df9, header = T, sep = "\t")
  dat9 <- dat9[-c(1), ]
  Var_int9<-c("Participant.ID","Date.of.Intake", "VEG0100","VEG0200","VEG0300","VEG0400","VEG0800","VEG0450","VEG0700","VEG0600","VEG0900","VEG0500", "VEG0100","VEG0700", 
              "FRU0100","FRU0200","FRU0300","FRU0400","FRU0500","FRU0600","FRU0700",
              "FRU0300","FRU0400","FRU0500","FRU0600","FRU0700",
              "DMF0100","DMR0100","DML0100","DMN0100","DMF0200","DMR0200",
              "DML0200","DML0300","DML0400","DCF0100","DCR0100","DCL0100","DCN0100",
              "DYF0100","DYR0100","DYL0100","DYF0200","DYR0200","DYL0200","DYN0100" , "DOT0100" , "DOT0300","DOT0400" , "DOT0500","DOT0600" , "DML0500" , "DYF0300" , "DYR0300",
              "MRF0100","MRL0100","MRF0200","MRL0200",
              "MRF0300","MRL0300","MRF0400","MRL0400","MCF0200","MCL0200","MRF0500",
              "MPF0100","MPL0100","MPF0200","MFF0100","MFL0100","MFF0200","MSL0100",
              "MSF0100","MCF0100","MCL0100","MOF0100","MOF0200","MOF0300","MOF0400",
              "MOF0500","MOF0600","MOF0700","VEG0700","MFF0100", "MFL0100","MFF0200","MSL0100","MSF0100","MOF0500","MOF0600","MOF0700","VEG0700")
  dat9<-dat9[,Var_int9]
  dat4 <- read.delim(df4, header=TRUE, sep="\t")
  Var_int4<-c("Participant.ID","Date.of.Intake", 
              "Whole.Grains..ounce.equivalents.",
              "Sodium..mg.","Refined.Grains..ounce.equivalents.","Added.Sugars..by.Available.Carbohydrate...g.",
              "Total.Saturated.Fatty.Acids..SFA...g.","Total.Monounsaturated.Fatty.Acids..MUFA...g.","Total.Polyunsaturated.Fatty.Acids..PUFA...g.","Energy..kcal.")
  dat4<-dat4[,Var_int4]
  dat <- merge(dat4,dat9,by=c("Participant.ID","Date.of.Intake"))
  dat[3:100] <- sapply( dat[3:100], as.numeric )
  return(dat)
}

adequacy<-function(dat){
  #ADEQUACY
  ## TOTAL VEGETABLES COMPONENT
  dat$hei_totveg=(dat$VEG0100+dat$VEG0200+dat$VEG0300+dat$VEG0400+dat$VEG0800+dat$VEG0450+dat$VEG0700+
                    dat$VEG0600+dat$VEG0900+dat$VEG0500)/2 
  
  ## GREENS AND BEANS COMPONENT
  dat$hei_greensbeans=(dat$VEG0100+dat$VEG0700)/2
  
  ## TOTAL FRUITS COMPONENT
  dat$hei_totfruit=(dat$FRU0100+dat$FRU0200+dat$FRU0300+dat$FRU0400+dat$FRU0500+dat$FRU0600+dat$FRU0700)/2
  
  ##WHOLE FRUITS COMPONENT
  dat$hei_wholefruit=(dat$FRU0300+dat$FRU0400+dat$FRU0500+dat$FRU0600+dat$FRU0700)/2
  
  ##WHOLE GRAINS COMPONENT
  dat$hei_wholegrains=dat$Whole.Grains..ounce.equivalents.
  
  ## DAIRY COMPONENT INCLUDE DAIRY GROUPS ADDED IN 2019
  dat$hei_dairy=dat$DMF0100+dat$DMR0100+dat$DML0100+dat$DMN0100+dat$DMF0200+dat$DMR0200+ dat$DML0200+dat$DML0300+dat$DML0400+dat$DCF0100+dat$DCR0100+dat$DCL0100+dat$DCN0100+ dat$DYF0100+dat$DYR0100+dat$DYL0100+dat$DYF0200+dat$DYR0200+dat$DYL0200+dat$DYN0100 + ((dat$DOT0100)/3) + dat$DOT0300+dat$DOT0400 + dat$DOT0500+dat$DOT0600 + dat$DML0500 + dat$DYF0300 + dat$DYR0300 
  
  #removed DYL0300 + DOT0900
  
  ## TOTAL PROTEIN FOODS COMPONENT
  dat$hei_totproteins=(dat$MRF0100+dat$MRL0100+dat$MRF0200+dat$MRL0200+
                         dat$MRF0300+dat$MRL0300+dat$MRF0400+dat$MRL0400+dat$MCF0200+dat$MCL0200+dat$MRF0500+
                         dat$MPF0100+dat$MPL0100+dat$MPF0200+dat$MFF0100+dat$MFL0100+dat$MFF0200+dat$MSL0100+
                         dat$MSF0100+dat$MCF0100+dat$MCL0100+dat$MOF0100+dat$MOF0200+dat$MOF0300+dat$MOF0400+
                         dat$MOF0500+dat$MOF0600+dat$MOF0700+(dat$VEG0700*2))
  
  ##SEAFOOD AND PLANT PROTEINS COMPONENT
  dat$hei_seafoodplantprot=(dat$MFF0100+ dat$MFL0100+dat$MFF0200+dat$MSL0100+dat$MSF0100+dat$MOF0500+dat$MOF0600+dat$MOF0700+
                              (dat$VEG0700*2))
  return(dat)
}


moderation<-function(dat){
  #MODERATION
  ## SODIUM COMPONENT
  
  
  dat$hei_sodium = dat$Sodium..mg./1000
  
  ## REFINED GRAINS COMPONENT
  dat$hei_refinedgrains=dat$Refined.Grains..ounce.equivalents.
  
  ## ADDED SUGARS COMPONENT
  dat$hei_addedsugars=dat$Added.Sugars..by.Available.Carbohydrate...g.*4 
  dat$addsug_perc = 100*((dat$hei_addedsugars)/dat$Energy..kcal.)
  
  ## Saturated fat
  dat$sfa_perc = 100*((dat$Total.Saturated.Fatty.Acids..SFA...g.*9)/dat$Energy..kcal.)
  dat$fatratio = (dat$Total.Monounsaturated.Fatty.Acids..MUFA...g. + dat$Total.Polyunsaturated.Fatty.Acids..PUFA...g.)/dat$Total.Saturated.Fatty.Acids..SFA...g.
  return(dat)
}



totals<-function(dat){
  # total veggies
  dat$vegden <- dat$hei_totveg / (dat$Energy..kcal./1000)
  dat$heiveg <- 5*(dat$vegden/1.1)
  dat$heiveg <- ifelse(dat$heiveg > 5, 5, dat$heiveg)
  
  # heibngrn
  # beans and greens
  dat$bngrden <- dat$hei_greensbeans / (dat$Energy..kcal./1000)
  dat$heibngrn <- 5*(dat$bngrden/0.2)
  dat$heibngrn  <- ifelse(dat$heibngrn  > 5, 5, dat$heibngrn)
  
  # heitotfr
  # total fruit
  dat$frtden <- dat$hei_totfruit/(dat$Energy..kcal./1000)
  dat$heitotfrt <- 5*(dat$frtden/0.8)
  dat$heitotfrt <- ifelse(dat$heitotfrt > 5, 5, dat$heitotfrt)
  
  # heiwholefrt
  # whole fruit
  dat$wholefrtden <- dat$hei_wholefruit/(dat$Energy..kcal./1000)
  dat$heiwholefrt <- 5*(dat$wholefrtden/0.4)
  dat$heiwholefrt <- ifelse(dat$heiwholefrt > 5, 5, dat$heiwholefrt)
  
  # heiwholegrain
  # whole grain
  dat$wholegrainden <- dat$hei_wholegrains/(dat$Energy..kcal./1000)
  dat$heiwholegrain <- 10*(dat$wholegrainden/1.5)
  dat$heiwholegrain <- ifelse(dat$heiwholegrain > 10, 10, dat$heiwholegrain)
  
  # heidairy
  # dairy
  dat$dairyden <- dat$hei_dairy/(dat$Energy..kcal./1000)
  dat$heidairy <- 10*(dat$dairyden/1.3)
  dat$heidairy <- ifelse(dat$heidairy > 10, 10, dat$heidairy)
  
  # heitotpro
  # total protein
  dat$totproden <- dat$hei_totproteins/(dat$Energy..kcal./1000)
  dat$heitotpro <- 5*(dat$totproden/2.5)
  dat$heitotpro <- ifelse(dat$heitotpro > 5, 5, dat$heitotpro)
  
  # heiseaplantpro
  # seaplant protein
  dat$seaplantden <- dat$hei_seafoodplantprot/(dat$Energy..kcal./1000)
  dat$heiseaplantpro <- 5*(dat$seaplantden/0.8)
  dat$heiseaplantpro <- ifelse(dat$heiseaplantpro > 5, 5, dat$heiseaplantpro)
  
  # heifattyacid
  # fatty acid
  dat$faratio <- ifelse(dat$Total.Saturated.Fatty.Acids..SFA...g. > 0,
                        dat$fatratio/ dat$Total.Saturated.Fatty.Acids..SFA...g.,
                        0)
  
  dat$heifattyacid <- ifelse(dat$Total.Saturated.Fatty.Acids..SFA...g. == 0 & dat$fatratio == 0,
                             10,
                             NA)
  
  dat$heifattyacid <- ifelse(dat$Total.Saturated.Fatty.Acids..SFA...g. == 0 & dat$fatratio > 0,
                             10,
                             dat$heifattyacid)
  
  dat$heifattyacid <- ifelse(dat$faratio >= 2.5,
                             10,
                             dat$heifattyacid)
  
  dat$heifattyacid <- ifelse(dat$faratio <= 1.2,
                             0,
                             heifattyacid)
  
  dat$heifattyacid <- ifelse(is.na(dat$heifattyacid),
                             10*((dat$faratio-1.2)/(2.5-1.2)),
                             dat$heifattyacid)
  
  
  dat$heisfa <- ifelse(dat$sfa_perc <= 8,
                       10,
                       NA)
  
  dat$heisfa <- ifelse(dat$sfa_perc >= 16,
                       0,
                       dat$heisfa)
  
  dat$heisfa <- ifelse(is.na(dat$heisfa),
                       10 - (10*(dat$sfa_perc - 8)/(16-8)),
                       dat$heisfa)
  summary(dat$heisfa)
  
  # heisodi
  # sodium
  head(dat$hei_)
  dat$sodden <- dat$hei_sodium / dat$Energy..kcal.
  
  dat$heisodi <- ifelse(dat$sodden <= 1.1,
                        10,
                        NA)
  dat$heisodi <- ifelse(dat$sodden >= 2.0,
                        0,
                        dat$heisodi)
  dat$heisodi <- ifelse(is.na(dat$heisodi),
                        10 - (10*(dat$sodden - 1.1)/(2.0-1.1)),
                        dat$heisodi)
  
  #addsug_perc
  dat$heiaddsug <- ifelse(dat$addsug_perc <= 6.5,
                          10,
                          NA)
  dat$heiaddsug <- ifelse(dat$addsug_perc >= 26,
                          0,
                          dat$heiaddsug)
  
  dat$heiaddsug <- ifelse(is.na(dat$heiaddsug),
                          10 - (10*(dat$addsug_perc - 6.5)/(26-6.5)),
                          dat$heiaddsug)
  
  # heirefgrain
  # refined grain
  dat$refgrainnden <- dat$hei_refinedgrains / (dat$Energy..kcal./1000)
  
  dat$heirefgrain <- ifelse(dat$refgrainnden <= 1.8,
                            10,
                            NA)
  dat$heirefgrain <- ifelse(dat$refgrainnden >= 4.3,
                            0,
                            dat$heirefgrain)
  
  dat$heirefgrain <- ifelse(is.na(dat$heirefgrain),
                            10 - (10*(dat$refgrainnden - 1.8)/(4.3-1.8)),
                            dat$heirefgrain)
  return(dat)
}

final_hei<-function(dat){
  dat$HEI <- dat$heiveg + dat$heibngrn + dat$heitotfrt + dat$heiwholefrt + dat$heiwholegrain + dat$heidairy + dat$heitotpro + dat$heiseaplantpro + dat$heifattyacid + dat$heirefgrain + dat$heisodi + dat$heisfa +dat$heiaddsug
  return(dat)
}