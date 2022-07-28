#-----# [3] 노후도 부여

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

#--- 면적제외 층 필터링
mart_djy_04_2 <- mart_djy_04[areaexctyn != 1]

#--- 주건축물 필터링
mart_djy_04_3 <- mart_djy_04_2[mainatchgbcdnm == "주건축물"]

#--- 기본개요와 층별개요 건축물대장PK(mgmbldrgstpk) 매칭해 상위건축물대장PK(mgmupbldrgstpk) 부여
mart_djy_01 <- readRDS("mart_djy_01.rds")
mart_djy_04_3$mgmupbldrgstpk <- mart_djy_01$mgmupbldrgstpk[match(mart_djy_04_3$mgmbldrgstpk, mart_djy_01$mgmbldrgstpk)]

#--- 새로운 관리번호 new_pk 부여 - 건축물대장PK 부여, 상위건축물대장PK가 있는 경우에는 상위건축물대장PK 부여
mart_djy_04_4 <- mart_djy_04_3
mart_djy_04_4$new_pk <- mart_djy_04_4$mgmbldrgstpk
mart_djy_04_4$new_pk[mart_djy_04_4$mgmupbldrgstpk != ""] <- mart_djy_04_4$mgmupbldrgstpk[mart_djy_04_4$mgmupbldrgstpk != ""]

#--- 건물별 면적 GROUP_BY
mart_djy_04_bld_area <-
    mart_djy_04_4 %>%
    group_by(new_pk) %>%
    summarise(sum_area = sum(area))

saveRDS(mart_djy_04_bld_area, "mart_djy_04_bld_area.rds")

mart_djy_04_bld_area <- readRDS("mart_djy_04_bld_area.rds")



#----- 표제부 mart_djy_03
mart_djy_03 <- fread("mart_djy_03_utf8.txt", colClasses = mart_djy_03_colclass)
names(mart_djy_03) <- mart_djy_col[[3]]$col_eng
names(mart_djy_03) <- tolower(names(mart_djy_03))

#--- pnu
mart_djy_03 <-
    mart_djy_03 %>%
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
mart_djy_03_clean <- mart_djy_03 %>% filter(nchar(pnu) == 19)
saveRDS(mart_djy_03_clean, "mart_djy_03.rds")

mart_djy_03 <- readRDS("mart_djy_03.rds")



#----- 노후도 부여
#--- 사용승인일, 착공일, 허가일 순으로 사용
mart_djy_03_oldyear <- mart_djy_03 %>% 
    select(
        pnu, mgmbldrgstpk, mainatchgbcdnm, mainpurpscdnm, useaprday, pmsday, stcnsday
    )

#--- 새로운 관리번호 new_pk 부여 - 건축물대장PK 부여, 상위건축물대장PK가 있는 경우에는 상위건축물대장PK 부여
mart_djy_01 <- readRDS("mart_djy_01.rds")
mart_djy_03_oldyear$mgmupbldrgstpk <- mart_djy_01$mgmupbldrgstpk[match(mart_djy_03_oldyear$mgmbldrgstpk, mart_djy_01$mgmbldrgstpk)]
mart_djy_03_oldyear$new_pk <- mart_djy_03_oldyear$mgmbldrgstpk
mart_djy_03_oldyear$new_pk[mart_djy_03_oldyear$mgmupbldrgstpk != ""] <- mart_djy_03_oldyear$mgmupbldrgstpk[mart_djy_03_oldyear$mgmupbldrgstpk != ""]

#--- 주건축물 필터링
mart_djy_03_oldyear <- mart_djy_03_oldyear[mainatchgbcdnm == "주건축물"]

mart_djy_03_oldyear <- 
    mart_djy_03_oldyear %>% 
    mutate(
        useaprday = lubridate::ymd(useaprday),
        pmsday = lubridate::ymd(pmsday),
        stcnsday = lubridate::ymd(stcnsday)
    )

#--- 사용승인일
mart_djy_03_oldyear$oldyear <- mart_djy_03_oldyear$useaprday
# summary(mart_djy_03_oldyear$oldyear)

#--- 착공일
mart_djy_03_oldyear$oldyear[is.na(mart_djy_03_oldyear$oldyear)] <- 
    mart_djy_03_oldyear$stcnsday[is.na(mart_djy_03_oldyear$oldyear)]
# summary(mart_djy_03_oldyear$oldyear)

#--- 허가일
mart_djy_03_oldyear$oldyear[is.na(mart_djy_03_oldyear$oldyear) & is.na(mart_djy_03_oldyear$stcnsday)] <- 
    mart_djy_03_oldyear$pmsday[is.na(mart_djy_03_oldyear$oldyear) & is.na(mart_djy_03_oldyear$stcnsday)]
# summary(mart_djy_03_oldyear$oldyear)

#--- 정렬
mart_djy_03_oldyear <- mart_djy_03_oldyear[order(new_pk)]
mart_djy_04_bld_area <- mart_djy_04_bld_area[order(new_pk)]


#--- 면적 부여
#--- 표제부와 층별개요의 건축물대장pk 매칭
mart_djy_03_oldyear$area <- 
    mart_djy_04_bld_area$sum_area[match(mart_djy_03_oldyear$mgmbldrgstpk, mart_djy_04_bld_area$new_pk)]

#--- 경과연수 계산
mart_djy_03_oldyear2 <- 
    mart_djy_03_oldyear %>% 
    mutate(today = Sys.Date()) %>% 
    mutate(
        oldyear_new = as.numeric(lubridate::as.period(lubridate::interval(oldyear, today)), "years")
    )

#--- PNU 별로 집계
mart_djy_03_oldyear3 <- 
    mart_djy_03_oldyear2 %>% 
    group_by(pnu, mainpurpscdnm) %>% 
    summarise(
        sum_area = sum(area, na.rm = TRUE),
        oldyear_new = mean(oldyear_new, na.rm = TRUE)
    ) %>% 
    ungroup() %>% 
    group_by(pnu) %>% 
    summarise(
        maxuse = mainpurpscdnm[which.max(sum_area)],
        oldyear_new = mean(oldyear_new, na.rm = TRUE)
    )

mart_djy_03_fin <- readRDS("mart_djy_03_fin.rds")

mart_djy_03_fin$oldyear <- 
    mart_djy_03_oldyear3$oldyear_new[match(mart_djy_03_fin$pnu, mart_djy_03_oldyear3$pnu)]

mart_djy_03_fin$mainpurpscdnm <- 
    mart_djy_03_oldyear3$maxuse[match(mart_djy_03_fin$pnu, mart_djy_03_oldyear3$pnu)]

sum(is.na(mart_djy_03_fin$oldyear))



#----- DB 적재
mart_djy_summary <- 
    mart_djy_03_fin %>% 
    select(pnu, mainpurpscdnm, platarea_new, atch_platarea, platarea_sum, archarea_new, vlratarea_new, totarea_new, oldyear) %>% 
    rename(platarea_main = platarea_new,
           platarea_atch = atch_platarea,
           platarea = platarea_sum,
           archarea = archarea_new,
           vlratestmarea = vlratarea_new,
           totarea = totarea_new)

saveRDS(mart_djy_summary, "mart_djy_summary.rds")


library(RPostgreSQL)
library(pool)

drv <- dbDriver("PostgreSQL")

pool.attr <- dbPool(
    drv = drv
    ,dbname = "attribute_table"
    ,host = "192.168.0.6"
    ,user = "akidor"
    ,password = "!@wodnr876"
)

con.attr <- dbConnect(
    drv
    ,dbname = 'attribute_table'
    ,host = '192.168.0.6'
    ,port = 5432
    ,user = 'akidor'
    ,password = '!@wodnr876'
)

max(nchar(mart_djy_summary$mainpurpscdnm))

field_type <- c("VARCHAR(19)", "VARCHAR(100)",
                "NUMERIC", "NUMERIC", "NUMERIC", "NUMERIC", "NUMERIC",
                "NUMERIC", "NUMERIC")

names(field_type) <- names(mart_djy_summary)

mart_djy_summary <- as.data.frame(mart_djy_summary)

dbWriteTable(conn = pool.attr
             ,name = "mart_djy_summary"
             ,value = mart_djy_summary
             ,row.names = FALSE
             ,append = T
             ,field.types = field_type)

dbSendQuery(conn = con.attr,'CREATE index mart_djy_summary_pnu_idx on mart_djy_summary("pnu");')
