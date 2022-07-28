#-----# [1] 층별표제부 pnu + 대지면적 + 건축면적 + 용적률산정용연면적 + 연면적

library(data.table)
library(tidyverse)



#----- columns 정의
mart_djy_col <- readRDS("/hdd02/mart/mart_djy_col.RDS")
mart_djy_01_colclass <- mart_djy_col[[1]]$datatype_inR
mart_djy_02_colclass <- mart_djy_col[[2]]$datatype_inR
mart_djy_03_colclass <- mart_djy_col[[3]]$datatype_inR
mart_djy_04_colclass <- mart_djy_col[[4]]$datatype_inR
mart_djy_05_colclass <- mart_djy_col[[5]]$datatype_inR



#----- 기본개요 mart_djy_01
mart_djy_01 <- fread("mart_djy_01_utf8.txt", colClasses = mart_djy_01_colclass)

# head(mart_djy_01)
# names(mart_djy_01)

mart_djy_01_col <- mart_djy_col[[1]]
names(mart_djy_01) <- mart_djy_col[[1]]$col_eng
names(mart_djy_01) <- tolower(names(mart_djy_01))

# head(mart_djy_01$regstrgbcdnm) # 인코딩 확인

#--- PNU 부여
mart_djy_01 <-
    mart_djy_01[, `:=`(
        pnu = paste0(
            sigungucd,
            bjdongcd,
            as.numeric(platgbcd) + 1,
            sprintf("%04d", as.numeric(bun)),
            sprintf("%04d", as.numeric(ji))
        )
    )]

# head(mart_djy_01$pnu)
# max(nchar(mart_djy_01$pnu))

# mart_djy_01_clean <- mart_djy_01 %>% filter(nchar(pnu) == 19)

#--- 19자리 pnu 필터링
mart_djy_01_clean <- mart_djy_01[nchar(pnu) == 19]
# mart_djy_01_tmp <- mart_djy_01[nchar(pnu) != 19]

saveRDS(mart_djy_01_clean, "mart_djy_01.rds")



#----- 총괄표제부 mart_djy_02
gc()
mart_djy_02 <- fread("mart_djy_02_utf8.txt", colClasses = mart_djy_02_colclass)

names(mart_djy_02) <- mart_djy_col[[2]]$col_eng
names(mart_djy_02) <- tolower(names(mart_djy_02))

#--- pnu
mart_djy_02 <-
    mart_djy_02 %>%
    mutate(
        pnu = paste0(
            sigungucd,
            bjdongcd,
            as.numeric(platgbcd) + 1,
            sprintf("%04d", as.numeric(bun)),
            sprintf("%04d", as.numeric(ji))
        )
    )

# head(mart_djy_02$newoldregstrgbcdnm) # 인코딩 확인

#--- 19자리 pnu 필터링
mart_djy_02_clean <- mart_djy_02 %>% filter(nchar(pnu) == 19)
# mart_djy_02_tmp <- mart_djy_02 %>% filter(nchar(pnu) != 19)

saveRDS(mart_djy_02_clean, "mart_djy_02.rds")



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

# head(mart_djy_04$mainatchgbcdnm) # 인코딩 확인

#--- 19자리 pnu 필터링
mart_djy_04_1 <- mart_djy_04[nchar(pnu) == 19]
mart_djy_04_tmp <- mart_djy_04[nchar(pnu) != 19]
# names(mart_djy_04_1)


gc()

# mart_djy_01 <- readRDS("mart_djy_01.rds")
# mart_djy_03 <- readRDS("mart_djy_03.rds")
# mart_djy_04 <- readRDS("mart_djy_04.rds")

#--- 면적제외 층 필터링
# unique(mart_djy_04_1$areaexctyn)
mart_djy_04_2 <- mart_djy_04_1[areaexctyn != 1]
unique(mart_djy_04_2$areaexctyn)
summary(mart_djy_04_2$area)

saveRDS(mart_djy_04_2, "mart_djy_04_2.rds")


# #--- OPTION 1
# #--- 주건축물 필터링?
# #--- Q.용적률산정용연면적에서 부속건축물을 제외해도 되는걸까요? A.안됨
# mart_djy_04_2 <- readRDS("mart_djy_04_2.rds")
# mart_djy_01 <- readRDS("mart_djy_01.rds")
# # unique(mart_djy_04_2$mainatchgbcdnm)
# # mart_djy_04_3 <- mart_djy_04_2[mainatchgbcdnm != "부속건축물"]
# mart_djy_04_3 <- mart_djy_04_2[mainatchgbcdnm == "주건축물"]
# saveRDS(mart_djy_04_3, "mart_djy_04_3.rds")
#
# head(mart_djy_04_3)
#
# #--- 기본개요와 층별개요 건축물대장PK(mgmbldrgstpk) 매칭해 상위건축물대장PK(mgmupbldrgstpk) 부여
# mart_djy_04_3$mgmupbldrgstpk <- mart_djy_01$mgmupbldrgstpk[match(mart_djy_04_3$mgmbldrgstpk, mart_djy_01$mgmbldrgstpk)]
# # head(mart_djy_04_3$mgmupbldrgstpk)
# # head(mart_djy_04_3)
#
# #--- 대장구분코드명 부여
# mart_djy_04_3$regstrkindcdnm <- mart_djy_01$regstrkindcdnm[match(mart_djy_04_3$mgmbldrgstpk, mart_djy_01$mgmbldrgstpk)]
# # head(mart_djy_04_3$regstrkindcdnm) # 인코딩 확인
#
# #--- 새로운 관리번호 new_pk 부여 - 건축물대장PK 부여, 상위건축물대장PK가 있는 경우에는 상위건축물대장PK 부여
# mart_djy_04_4 <- mart_djy_04_3
# mart_djy_04_4$new_pk <- mart_djy_04_4$mgmbldrgstpk
# mart_djy_04_4$new_pk[mart_djy_04_4$mgmupbldrgstpk != ""] <- mart_djy_04_4$mgmupbldrgstpk[mart_djy_04_4$mgmupbldrgstpk != ""]
#
# #--- 건물별 면적 GROUP_BY
# mart_djy_04_5 <- mart_djy_04_4 %>% filter(flrgbcdnm == "지상")
#
# mart_djy_04_5$area <- abs(mart_djy_04_5$area)
# # mart_djy_04_5_check <- mart_djy_04_5 %>% filter(area == 0) # 19511개
# # mart_djy_04_5_check2 <- mart_djy_04_5 %>% filter(is.na(area)) # 0개
#
# # summary(mart_djy_04_5$area)
#
# mart_djy_04_5_opt1 <-
#     mart_djy_04_5 %>%
#     group_by(new_pk) %>%
#     summarise(sum_area = sum(area))
#
# saveRDS(mart_djy_04_5_opt1, "mart_djy_04_fin.rds")
#
#
# #--- OPTION 2
# #--- 주건축물, 부속건축물 함께 필터링 - 나머지도 해야함(Opt3)
# mart_djy_04_2 <- readRDS("mart_djy_04_2.rds")
# mart_djy_01 <- readRDS("mart_djy_01.rds")
# mart_djy_04_3 <- mart_djy_04_2[mainatchgbcdnm %in% c("주건축물", "부속건축물")] # 주건축물 필터링 안하는 경우
#
# head(mart_djy_04_3)
#
# #--- 기본개요와 층별개요 건축물대장PK(mgmbldrgstpk) 매칭해 상위건축물대장PK(mgmupbldrgstpk) 부여
# mart_djy_04_3$mgmupbldrgstpk <- mart_djy_01$mgmupbldrgstpk[match(mart_djy_04_3$mgmbldrgstpk, mart_djy_01$mgmbldrgstpk)]
# head(mart_djy_04_3$mgmupbldrgstpk)
#
# #--- 대장구분코드명 부여
# mart_djy_04_3$regstrkindcdnm <- mart_djy_01$regstrkindcdnm[match(mart_djy_04_3$mgmbldrgstpk, mart_djy_01$mgmbldrgstpk)]
#
# #--- 새로운 관리번호 new_pk 부여 - 건축물대장PK 부여, 상위건축물대장PK가 있는 경우에는 상위건축물대장PK 부여
# mart_djy_04_4 <- mart_djy_04_3
# mart_djy_04_4$new_pk <- mart_djy_04_4$mgmbldrgstpk
# mart_djy_04_4$new_pk[mart_djy_04_4$mgmupbldrgstpk != ""] <- mart_djy_04_4$mgmupbldrgstpk[mart_djy_04_4$mgmupbldrgstpk != ""]
#
# #--- 건물별 면적 GROUP_BY
# mart_djy_04_5 <- mart_djy_04_4 %>% filter(flrgbcdnm == "지상")
#
# mart_djy_04_5$area <- abs(mart_djy_04_5$area)
#
# mart_djy_04_5_opt2 <-
#     mart_djy_04_5 %>%
#     group_by(new_pk) %>%
#     summarise(sum_area = sum(area))
#
# saveRDS(mart_djy_04_5_opt2, "mart_djy_04_fin2.rds")


#--- OPTION 3
#--- 모든 주부속건축물 포함
mart_djy_04_2 <- readRDS("mart_djy_04_2.rds")
mart_djy_01 <- readRDS("mart_djy_01.rds")

head(mart_djy_04_2)

# #--- 기본개요와 층별개요 건축물대장PK(mgmbldrgstpk) 매칭해 상위건축물대장PK(mgmupbldrgstpk) 부여
# mart_djy_04_2$mgmupbldrgstpk <- mart_djy_01$mgmupbldrgstpk[match(mart_djy_04_2$mgmbldrgstpk, mart_djy_01$mgmbldrgstpk)]
# head(mart_djy_04_2$mgmupbldrgstpk)
# 
# #--- 대장구분코드명 부여
# mart_djy_04_2$regstrkindcdnm <- mart_djy_01$regstrkindcdnm[match(mart_djy_04_2$mgmbldrgstpk, mart_djy_01$mgmbldrgstpk)]
# 
# #--- 새로운 관리번호 new_pk 부여 - 건축물대장PK 부여, 상위건축물대장PK가 있는 경우에는 상위건축물대장PK 부여
# mart_djy_04_2$new_pk <- mart_djy_04_2$mgmbldrgstpk
# mart_djy_04_2$new_pk[mart_djy_04_2$mgmupbldrgstpk != ""] <- mart_djy_04_2$mgmupbldrgstpk[mart_djy_04_2$mgmupbldrgstpk != ""]

#--- 건물별 면적 GROUP_BY
mart_djy_04_2 <- mart_djy_04_2[flrgbcdnm == "지상"]

mart_djy_04_2$area <- abs(mart_djy_04_2$area) # area: 층별 면적

saveRDS(mart_djy_04_2, "mart_djy_04_2.rds")

# mart_djy_04_2_opt3 <-
#     mart_djy_04_2 %>%
#     group_by(new_pk) %>%
#     summarise(sum_area = sum(area)) # 건물별로 집계
#
# saveRDS(mart_djy_04_2_opt3, "mart_djy_04_fin3.rds")


#--- 층별개요(mart_djy_04) - pnu별로 group_by한 면적값
mart_djy_04_2 <- readRDS("mart_djy_04_2.rds")
mart_djy_04_area <-
    mart_djy_04_2 %>%
    group_by(pnu) %>%
    summarise(area = sum(area, na.rm = TRUE))

saveRDS(mart_djy_04_area, "mart_djy_04_area.rds")



#----- 부속지번 데이터 mart_djy_05
mart_djy_05 <- fread("mart_djy_05.txt", colClasses = mart_djy_05_colclass)
names(mart_djy_05) <- mart_djy_col[[5]]$col_eng
names(mart_djy_05) <- tolower(names(mart_djy_05))

#--- pnu
mart_djy_05 <-
    mart_djy_05 %>%
    mutate(
        pnu = paste0(
            sigungucd,
            bjdongcd,
            as.numeric(platgbcd) + 1,
            sprintf("%04d", as.numeric(bun)),
            sprintf("%04d", as.numeric(ji))
        )
    )

mart_djy_05 <- mart_djy_05[nchar(pnu) == 19]

mart_djy_05 <-
    mart_djy_05 %>%
    mutate(
        atch_pnu = paste0(
            atchsigungucd,
            atchbjdongcd,
            as.numeric(ifelse(atchplatgbcd != "", atchplatgbcd, platgbcd)) + 1,
            sprintf("%04d", as.numeric(atchbun)),
            sprintf("%04d", as.numeric(atchji))
        )
    )

mart_djy_05 <- mart_djy_05[nchar(atch_pnu) == 19]
tmp <- mart_djy_05[nchar(atch_pnu) != 19]

mart_djy_05 <- mart_djy_05[order(pnu)]
head(mart_djy_05)

saveRDS(mart_djy_05, "mart_djy_05.rds")
mart_djy_05 <- readRDS("mart_djy_05.rds")



#----- 토지대장 데이터
#--- 전체 필지 데이터
# setwd("~/f003")
# 
# ls.files <- list.files(pattern = ".csv")
# landregi <- lapply(ls.files, function(x) {
#     x <- fread(x, colClasses = "character")
#     return(x)
# })

# head(landregi[[1]])

# landregi <- do.call(bind_rows, landregi)
# saveRDS(landregi, "~/f003/landregi.rds")

# head(landregi)



#--- 주필지(03)와 부속필지(05) 엮기
mart_djy_03 <- readRDS("mart_djy_03.rds")
mart_djy_05 <- readRDS("mart_djy_05.rds")

landregi <- readRDS("~/f003/landregi.rds")

gc()

# sample test
# landregi_test <- landregi %>% filter(`법정동명` == c("서울특별시 종로구 청운동")) # 모든 필지 존재
# mart_djy_03_test <- mart_djy_03 %>% filter(str_sub(pnu, 1, 10) == "1111010100") # 여기에는 주필지 데이터만 존재
# mart_djy_04_test <- mart_djy_04 %>% filter(str_sub(pnu, 1, 10) == "1111010100") # group_by(new_pk) 필지 내 존재하는 건물의 용적률산정용연면적 계산
# mart_djy_05_test <- mart_djy_05 %>% filter(str_sub(pnu, 1, 10) == "1111010100") # 주필지pnu, 부속필지pnu 존재
#
# mart_djy_05_test$platarea <- landregi_test$면적[match(mart_djy_05_test$atch_pnu, landregi_test$고유번호)]
#
# mart_djy_05_test2 <-
#     mart_djy_05_test %>%
#     group_by(pnu) %>%
#     summarise(platarea = sum(as.numeric(platarea), na.rm = TRUE))

#--- 부속지번 테이블에 면적 붙이기
mart_djy_05$platarea <- landregi$면적[match(mart_djy_05$atch_pnu, landregi$고유번호)]

head(mart_djy_05)
sum(is.na(mart_djy_05$platarea))

tmp <- mart_djy_05[is.na(mart_djy_05$platarea)] # 811238개

#--- pnu별로 집계 - 주 지번에 딸린 부속필지의 면적
mart_djy_05_area <-
    mart_djy_05 %>%
    group_by(pnu) %>%
    summarise(atch_platarea = sum(as.numeric(platarea), na.rm = TRUE))

saveRDS(mart_djy_05_area, "mart_djy_05_area.rds")

gc()



#----- 표제부(mart_djy_03) + 층별개요(mart_djy_04) + 부속지번(mart_djy_05)
#--- pnu - platarea - archarea - vlratestmtotarea

mart_djy_04_area <- readRDS("mart_djy_04_area.rds")
mart_djy_03 <- readRDS("mart_djy_03.rds")

mart_djy_03_2 <- mart_djy_03 %>%
    select(pnu, platarea, archarea, vlratestmtotarea, totarea)


#--- 표제부(mart_djy_03) - pnu별로 group_by한 면적값
mart_djy_03_2 <- mart_djy_03_2 %>%
    group_by(pnu) %>%
    summarise(
        platarea = sum(platarea),
        archarea = sum(archarea),
        vlratestmtotarea = sum(vlratestmtotarea),
        totarea = sum(totarea)
    ) %>%
    ungroup()

saveRDS(mart_djy_03_2, "mart_djy_03_2.rds")
mart_djy_03_2 <- readRDS("mart_djy_03_2.rds")
setDT(mart_djy_03_2)


#--- 층별개요(04)에서 pnu별로 group_by한 면적값(용적률 산정용 연면적)을 부여
mart_djy_03_2$vlratarea_new <- mart_djy_04_area$area[match(mart_djy_03_2$pnu, mart_djy_04_area$pnu)]

#--- 부여한 면적값이 NA인경우 표제부의 용적률 산정용 연면적을 부여
mart_djy_03_2$vlratarea_new[is.na(mart_djy_03_2$vlratarea_new)] <-
    mart_djy_03_2$vlratestmtotarea[is.na(mart_djy_03_2$vlratarea_new)]

#--- 부여한 면적값이 용적률 산정용 연면적보다 작은경우 표제부의 용적률 산정용 연면적을 부여
mart_djy_03_2$vlratarea_new[mart_djy_03_2$vlratestmtotarea > mart_djy_03_2$vlratarea_new] <-
    mart_djy_03_2$vlratestmtotarea[mart_djy_03_2$vlratestmtotarea > mart_djy_03_2$vlratarea_new]

#--- 부여한 면적값이 0인경우 표제부의 연면적을 부여
mart_djy_03_2$vlratarea_new[mart_djy_03_2$vlratarea_new == 0] <- mart_djy_03_2$totarea[mart_djy_03_2$vlratarea_new == 0]
summary(mart_djy_03_2$vlratarea_new)

#--- 절대값으로 변경(음수가 있는 경우)
summary(mart_djy_03_2$vlratarea_new)
mart_djy_03_2$vlratarea_new <- abs(mart_djy_03_2$vlratarea_new)


#--- 표제부(mart_djy_03) 테이블에 토지대장(landregi) 면적 부여
mart_djy_03_2$platarea_new <- landregi$면적[match(mart_djy_03_2$pnu, landregi$고유번호)]

#--- 부여한 면적이 NA인 경우 표제부의 면적 부여
tmp <- mart_djy_03_2 %>% filter(is.na(mart_djy_03_2$platarea_new)) # 결측치 287,603개 / 5,884,810개
mart_djy_03_2$platarea_new[is.na(mart_djy_03_2$platarea_new)] <- mart_djy_03_2$platarea[is.na(mart_djy_03_2$platarea_new)]

mart_djy_03_2 <- mart_djy_03_2 %>% mutate(platarea_new = as.numeric(platarea_new))
tmp <- mart_djy_03_2[platarea_new == 0] # 대지면적 없는 경우 234,387개 / 5,884,810개

#--- 대지면적이 0인 경우 연속지적도에서 불러오기
library(RPostgreSQL, lib.loc = "/Rlibpath")
library(pool, lib.loc = "/Rlibpath")

drv <- dbDriver("PostgreSQL")

con.shape <- dbConnect(
    drv
    ,dbname = 'spatial_data'
    ,host = '192.168.0.6'
    ,port = 5432
    ,user = 'akidor'
    ,password = '!@wodnr876'
)

library(sf)

tmp_q <- tmp$pnu

parcel <- 
    st_read(dsn = con.shape,
            query = paste0(
                "SELECT * FROM parcel20210617 WHERE a1 = '",
                tmp$pnu,
                "' ;"
            )
    ) # 해당없음?

# 해당되는 내용 없으면 결측치 무시
mart_djy_03_3 <- mart_djy_03_2[platarea_new != 0] # 5,650,423개



#--- 표제부(mart_djy_03) 테이블에 부속지번(mart_djy_05) 면적 부여
mart_djy_03_3$atch_platarea <- mart_djy_05_area$atch_platarea[match(mart_djy_03_3$pnu, mart_djy_05_area$pnu)]

#--- 표제부 원자료 대지면적(platarea), 토지대장 기반 대지면적(platarea_new + atch_platarea = platarea_sum)
mart_djy_03_3 <-
    mart_djy_03_3 %>%
    rowwise() %>%
    mutate(platarea_sum = sum(platarea_new, atch_platarea, na.rm = TRUE))

#--- 원자료의 대지면적(platarea)이 새로 계산한 대지면적(platarea_sum)보다 더 큰 경우 원자료 사용
mart_djy_03_3$platarea_sum[mart_djy_03_3$platarea > mart_djy_03_3$platarea_sum] <- 
    mart_djy_03_3$platarea[mart_djy_03_3$platarea > mart_djy_03_3$platarea_sum]


#--- 건축면적이 0인 경우 층별개요의 1층 면적 부여
mart_djy_04 <- readRDS("mart_djy_04.rds")
unique(mart_djy_04$flrgbcdnm)
mart_djy_04_flr <- mart_djy_04[flrno == 1]
mart_djy_04_flr <- mart_djy_04_flr %>%
    group_by(pnu) %>%
    summarise(archarea_new = sum(area, na.rm = TRUE)) # archarea_new = 층별개요의 1층 pnu별 group_by

mart_djy_03_3$archarea_new <- mart_djy_04_flr$archarea_new[match(mart_djy_03_3$pnu, mart_djy_04_flr$pnu)]

#--- 표제부에 건축면적 있는 경우 표제부 데이터 사용
str(mart_djy_03_3)
setDT(mart_djy_03_3)
tmp <- mart_djy_03_3[is.na(archarea_new)] # 결측치 확인 6,044개

mart_djy_03_3$archarea_new[is.na(mart_djy_03_3$archarea_new)] <- mart_djy_03_3$archarea[is.na(mart_djy_03_3$archarea_new)]
tmp <- mart_djy_03_3[archarea_new == 0] # 결측치 확인 2,961개

#--- 층별개요/표제부 모두 비어있는 경우 건물 도형 사용
library(RPostgreSQL, lib.loc = "/Rlibpath")
library(pool, lib.loc = "/Rlibpath")

drv <- dbDriver("PostgreSQL")

con.shape <- dbConnect(
    drv
    ,dbname = 'spatial_data'
    ,host = '192.168.0.6'
    ,port = 5432
    ,user = 'akidor'
    ,password = '!@wodnr876'
)

library(sf)

pnu_q <- tmp$pnu

# 여기부터
bld <- 
    st_read(dsn = con.shape,
            query = paste0(
                "SELECT * FROM tl_spbd_buld_2202 WHERE SUBSTR(bd_mgt_sn, 1, 19) = '",
                tmp$pnu,
                "' ;"
            )
    ) # 해당없음?

substr(bld$bd_mgt_sn, 1, 19)


# 결측치 check
summary(mart_djy_03_3)

sum(is.na(mart_djy_03_3$vlratarea_new)) # 0
sum(mart_djy_03_3$vlratarea_new == 0) # 559

sum(is.na(mart_djy_03_3$platarea_sum)) # 0
sum(mart_djy_03_3$platarea_sum == 0) # 0
# tmp <- mart_djy_03_3[mart_djy_03_3$platarea_sum == 0] # 0

sum(is.na(mart_djy_03_3$archarea_new)) # 0
sum(mart_djy_03_3$archarea_new == 0) # 2961
# tmp <- mart_djy_03_3[mart_djy_03_3$archarea_new == 0] # 2961



#----- 완성!
saveRDS(mart_djy_03_3, "mart_djy_03_fin.rds")



# check
sum(is.na(mart_djy_03_fin$vlratarea_new)) # 0
sum(mart_djy_03_fin$vlratarea_new == 0) # 559

sum(is.na(mart_djy_03_fin$platarea_sum)) # 0
sum(mart_djy_03_fin$platarea_sum == 0) # 0
# tmp <- mart_djy_03_fin[mart_djy_03_fin$platarea_sum == 0] # 0

sum(is.na(mart_djy_03_fin$archarea_new)) # 0
sum(mart_djy_03_fin$archarea_new == 0) # 2961
# tmp <- mart_djy_03_fin[mart_djy_03_fin$archarea_new == 0] # 2961



#----- DB 적재
getwd()
setwd("/home/songmin/mart_djy")
mart_djy_03_fin <- readRDS("mart_djy_03_fin.rds")

head(mart_djy_03_fin)

library(tidyverse)
mart_djy_clean <- 
    mart_djy_03_fin %>% 
    select(pnu, platarea_new, atch_platarea, platarea_sum, archarea_new, vlratarea_new) %>% 
    rename(platarea_main = platarea_new,
           platarea_atch = atch_platarea,
           platarea = platarea_sum,
           archarea = archarea_new,
           vlratestmarea = vlratarea_new)

head(mart_djy_clean)


library("RPostgreSQL")
library("pool")

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

str(mart_djy_clean)

nchar(mart_djy_clean$platarea_main) %>% max()
field_type <- c("VARCHAR(19)", "NUMERIC", "NUMERIC",
                "NUMERIC", "NUMERIC", "NUMERIC")
names(field_type) <- names(mart_djy_clean)


mart_djy_clean <- as.data.frame(mart_djy_clean)

dbWriteTable(conn = pool.attr
             ,name = "mart_djy_area"
             ,value = mart_djy_clean
             ,row.names = FALSE
             ,append = T
             ,field.types = field_type)

dbSendQuery(conn = con.attr,'CREATE index mart_djy_area_pnu_idx on mart_djy_area("pnu");')




