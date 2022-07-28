#-----# [2] 연면적 부여

library(data.table)
library(tidyverse)



#----- columns 정의
mart_djy_col <- readRDS("/hdd02/mart/mart_djy_col.RDS")
mart_djy_01_colclass <- mart_djy_col[[1]]$datatype_inR
mart_djy_02_colclass <- mart_djy_col[[2]]$datatype_inR
mart_djy_03_colclass <- mart_djy_col[[3]]$datatype_inR
mart_djy_04_colclass <- mart_djy_col[[4]]$datatype_inR
mart_djy_05_colclass <- mart_djy_col[[5]]$datatype_inR



#----- 층별개요 mart_djy_04
mart_djy_04 <- fread("mart_djy_04.txt", colClasses = mart_djy_04_colclass)

names(mart_djy_04) <- mart_djy_col[[4]]$col_eng
names(mart_djy_04) <- tolower(names(mart_djy_04))

#--- pnu
mart_djy_04 <-
    mart_djy_04 %>%
    mutate(
        pnu = paste0(
            sigungucd,
            bjdongcd,
            as.numeric(platgbcd) + 1,
            sprintf("%04d", as.numeric(bun)),
            sprintf("%04d", as.numeric(ji))
        )
    )

#--- 19자리 pnu 필터링
mart_djy_04_1 <- mart_djy_04[nchar(pnu) == 19]

#--- 면적제외 필터링
# unique(mart_djy_04_1$areaexctyn)
mart_djy_04_2 <- mart_djy_04_1[areaexctyn != 1]

#--- 층별개요(mart_djy_04) - pnu별로 group_by한 면적값 - 연면적
mart_djy_04_totarea <-
    mart_djy_04_2 %>%
    group_by(pnu) %>%
    summarise(totarea_new = sum(area, na.rm = TRUE))

saveRDS(mart_djy_04_totarea, "mart_djy_04_totarea.rds")



#----- 부속지번
mart_djy_05 <- readRDS("mart_djy_05_area.rds")



#----- 표제부
mart_djy_03 <- readRDS("mart_djy_03_fin.rds")

#--- 층별개요(04)에서 pnu별로 group_by한 면적값(연면적)을 부여
mart_djy_03$totarea_new <- mart_djy_04_totarea$totarea_new[match(mart_djy_03$pnu, mart_djy_04_totarea$pnu)]
summary(mart_djy_03$totarea_new)

#--- 부여한 면적값이 NA인경우 표제부의 연면적을 부여
mart_djy_03$totarea_new[is.na(mart_djy_03$totarea_new)] <- 
    mart_djy_03$totarea[is.na(mart_djy_03$totarea_new)]

#--- 부여한 면적값이 표제부 연면적보다 작은경우 표제부의 연면적을 부여
mart_djy_03$totarea_new[mart_djy_03$totarea > mart_djy_03$totarea_new] <-
    mart_djy_03$totarea[mart_djy_03$totarea > mart_djy_03$totarea_new]

#--- 부여한 면적값이 0인경우 표제부의 연면적을 부여
mart_djy_03$totarea_new[mart_djy_03$totarea_new == 0] <- mart_djy_03$totarea[mart_djy_03$totarea_new == 0]
summary(mart_djy_03$totarea_new)

#--- 저장
saveRDS(mart_djy_03, "mart_djy_03_fin.rds")

