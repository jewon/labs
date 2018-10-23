## 기초 작업
kcity <- read.csv("korea_city.csv") # 파일 불러오기
kcity <- kcity[-c(1, 2),] # csv파일의 2번 행은 메타데이터, 3번 행은 전국 통계이므로 해당 행 삭제
kcity_df <- data.frame(kcity) # data.frame 변환
colnames(kcity_df)[1] <- "NAME" # 첫 열이름 "NAME"으로 변경
# "TYPE"이라는 열을 추가하고 해당 열에 "NAME"의 마지막 글자 삽입
kcity_df['TYPE'] <- substr(kcity_df$NAME, nchar(as.character(kcity_df$NAME)), nchar(as.character(kcity_df$NAME)))
kcity_df[kcity_df$TYPE != "시" & kcity_df$TYPE != "군", 'NAME'] # '시' 또는 '군'이 아닌 이름 출력해 검사 - 124개
kcity_df <- kcity_df[kcity_df$TYPE == "시" | kcity_df$TYPE == "군",] # "TYPE"이 '시' 또는 '군'인 것만 남기고 나머지 삭제
kcity_df <- kcity_df[kcity_df$NAME != "세종시"]

## 2010년만 구하기
kcity_2010 <- kcity_df[c('NAME', 'X2010', 'TYPE')] # 2010년 기준 필요한 데이터만 분리
kcity_2010 <- kcity_2010[kcity_2010$X2010 != '-',] # 값이 없는 행 삭제(당시에 존재하지 않는 행정구역)
kcity_2010$X2010 <- as.numeric(as.character((kcity_2010$X2010))) # factor자료형을 일반 숫자형으로 변환
kcity_2010 <- kcity_2010[order(-kcity_2010$X2010),] # 인구 내림차순 정렬
kcity_2010['RANK'] <- 1:nrow(kcity_2010) # RANK부여
kcity_2010['LOG_Pr'] <- log(kcity_2010$X2010, base = 10) # logPr 열 생성
kcity_2010_logP1 <- log(kcity_2010[1, 'X2010'], base = 10)
kcity_2010['LOG_Pr'] <- log(kcity_2010$X2010, base = 10)
kcity_2010['LOG_r'] <- log(kcity_2010$RANK, base = 10)
kcity_2010_reg <- lm((kcity_2010[1, 'LOG_Pr'] - kcity_2010$LOG_Pr)~log(kcity_2010$RANK, base = 10)+ 0, data = kcity_2010)
kcity_2010_reg

# 1992~2017년 회귀분석 결과값 모두구하여 연도별 그래프로 나타내기
result <- c() # 1992~2017년 회귀계수 벡터
n_city <- c()
for (i in 2:27){
  kcity_XXXX <- kcity_df[,c(1, i, 28)] # XXXX년 기준 필요한 데이터만 분리
  kcity_XXXX <- kcity_XXXX[kcity_XXXX[,2] != '-',] # 값이 없는 행 삭제(당시에 존재하지 않는 행정구역)
  kcity_XXXX[,2] <- as.numeric(as.character((kcity_XXXX[,2]))) # factor자료형을 일반 숫자형으로 변환
  kcity_XXXX <- kcity_XXXX[order(-kcity_XXXX[,2]),] # 인구 내림차순 정렬
  kcity_XXXX['RANK'] <- 1:nrow(kcity_XXXX) # RANK부여
  kcity_XXXX['LOG_Pr'] <- log(kcity_XXXX[,2], base = 10) # logPr 열 생성
  kcity_XXXX['LOG'] <- kcity_XXXX[1, 'LOG_Pr'] - kcity_XXXX$LOG_Pr # LOG열에 LogP1 - LogPr 저장
  kcity_XXXX['LOG_r'] <- log(kcity_XXXX$RANK, base = 10) # Logr 열 생성
  kcity_XXXX_reg <- lm(LOG~LOG_r+0, data = kcity_XXXX) # LOG~LOGr 식으로 회귀
  
  print(colnames(kcity_XXXX)[2])
  print(kcity_XXXX_reg)
  
  result <- c(result, kcity_XXXX_reg$coefficients)
  n_city <- c(n_city, nrow(kcity_XXXX))
}
names(result) <- 1992:2017 # 회귀계수
plot(result, type = "o", xaxt="n") # 회귀계수 변화 그래프
axis(1, at = 1:26, labels = names(result))

# 2010과 2017 비교
kcity_com <- kcity_df[c('NAME', 'TYPE', 'X2010', 'X2017')]
kcity_com$X2010 <- as.numeric(as.character((kcity_com$X2010)))
kcity_com$X2017 <- as.numeric(as.character((kcity_com$X2017)))
kcity_com <- kcity_com[!is.na(kcity_com$X2010) | !is.na(kcity_com$X2017),] # 둘다 무효인 것 삭제
kcity_com[is.na(kcity_com$X2010) | is.na(kcity_com$X2017),]
kcity_com['RANK_2010'] <- rank(-kcity_com$X2010, na.last = "keep")
kcity_com['RANK_2017'] <- rank(-kcity_com$X2017, na.last = "keep")
kcity_com_t <- kcity_com[!is.na(kcity_com$X2010) & !is.na(kcity_com$X2017),]
kcity_com_t <- kcity_com_t[kcity_com_t$RANK_2010 != kcity_com_t$RANK_2017, ]
kcity_com_t['RANK_DIFF'] <- kcity_com_t$RANK_2017 - kcity_com_t$RANK_2010
kcity_com['POP_DIFF'] <- kcity_com$X2017 - kcity_com$X2010
kcity_com['175', 1] <- "고성군1" # 한국에는 '고성군'이 두 개 있어 이름을 그대로 rowname으로 사용 불가함
kcity_com['354', 1] <- "고성군2"
rownames(kcity_com) <- kcity_com$NAME
kcity_com['Q_2010'] <- (log(kcity_com['서울특별시', 'X2010'], base = 10) - log(kcity_com[, 'X2010'], base = 10)) / log(kcity_com[, 'RANK_2010'], base = 10)
kcity_com_2010 <- kcity_com[order(kcity_com$RANK_2010, na.last = TRUE),]
plot(kcity_com_2010$Q_2010, pch = 16, cex = 0.5)
abline(h = result['2010'])
text(kcity_com_2010[c(1:10),'RANK_2010'], kcity_com_2010[c(1:10), 'Q_2010'], kcity_com_2010[c(1:10), 'NAME'], cex = 0.5, pos = 4)
kcity_com['Q_2017'] <- (log(kcity_com['서울특별시', 'X2017'], base = 10) - log(kcity_com[, 'X2017'], base = 10)) / log(kcity_com[, 'RANK_2017'], base = 10)
kcity_com_2017 <- kcity_com[order(kcity_com$RANK_2017, na.last = TRUE),]
plot(kcity_com_2017$Q_2017, pch = 16, cex = 0.5)
abline(h = result['2017'])
abline(h = result['2010'], col = 'gray')
text(kcity_com_2017[c(1:10),'RANK_2017'], kcity_com_2017[c(1:10), 'Q_2017'], kcity_com_2017[c(1:10), 'NAME'], cex = 0.5, pos = 4)
plot(kcity_com_2010$Q_2010, pch = 16, cex = 0.5, xlab = "RANK", ylab = "q-value of city")
points(kcity_com_2017$Q_2017, pch = 16, cex = 0.5, col = 'red')
