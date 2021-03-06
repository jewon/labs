# init
library(ggplot2)
data <- read.csv("all_1.csv")
data <- rbind(data, read.csv("all_2.csv"))
data <- rbind(data, read.csv("all_3.csv"))
data <- rbind(data, read.csv("all_4.csv"))
data <- rbind(data, read.csv("all_5.csv"))
data <- rbind(data, read.csv("korea.csv"))
df <- data.frame(data)
colnames(df) <- c("DONG", "YEAR", "SEX", "AGE", "POP")

# 피라미드 함수
pyramid <- function (dong_name, set_year) {
  temp_df <- subset(df, DONG == dong_name & SEX != "총인구수 (명)" & YEAR == set_year)
  total <- sum(temp_df[temp_df$AGE == "계", 5])
  temp_df <- temp_df[temp_df$AGE != "계",]
  temp_df['POP_RATIO'] <- temp_df$POP / total * 100
  temp_df[temp_df$SEX == "남자인구수 (명)" , 'POP_RATIO'] <- (-1) * temp_df[temp_df$SEX == "남자인구수 (명)" , 'POP_RATIO']
  ages <- factor(temp_df[temp_df$SEX == "남자인구수 (명)", 4])
  
  p <- ggplot(data = temp_df, aes(x = AGE, y = POP_RATIO, fill = SEX))
  p <- p + geom_bar(data = subset(temp_df, SEX = "여자총인구수 (명)"), stat = "identity")
  p <- p + geom_col(width = .95)
  p <- p + scale_fill_manual(values = c("steelblue1", "indianred1"))
  p <- p + scale_x_discrete(limits = ages)
  p <- p + scale_y_continuous(breaks = seq(-100, 100, by = 2), labels = abs(seq(-100, 100, by = 2)))
  p <- p + expand_limits(y = c(-6, 6))
  p <- p + coord_flip()
  p <- p + ggtitle(paste(dong_name, "(", total, "명, ", set_year, "년)"))
  p
  ## 해당 지역 전체 인구에 대한 비율로 출력
}

## ex) pyramid("반포본동", 2017)
