library(dplyr)
library(tidyr)
library(ggplot2)


data <- read.csv('JKim.csv')

result_1 <- data %>%
  filter(MS_r == 1) %>%
  summarise(
    across(
      everything(),  # 모든 열에 대해 계산
      list(
        Mean = ~ mean(.x, na.rm = TRUE),  # 평균 계산
        SE = ~ sd(.x, na.rm = TRUE) / sqrt(n())  # 표준 오차 계산
      ),
      .names = "{col}_{fn}"  # 새로운 열 이름 지정: 변수명_통계명
    )
  )

result_0 <- data %>%
  filter(MS_r == 0) %>%
  summarise(
    across(
      everything(),  # 모든 열에 대해 계산
      list(
        Mean = ~ mean(.x, na.rm = TRUE),  # 평균 계산
        SE = ~ sd(.x, na.rm = TRUE) / sqrt(n())  # 표준 오차 계산
      ),
      .names = "{col}_{fn}"  # 새로운 열 이름 지정: 변수명_통계명
    )
  )


plot_data <- rbind(result_0, result_1)
names(plot_data) <- gsub('Ex','', names(plot_data ))
names(plot_data) <- gsub('_Mean','Mean', names(plot_data ))
names(plot_data) <- gsub('_SE','SE', names(plot_data ))

final_data <- plot_data %>%
  pivot_longer(
    cols = -c(MS_rMean, MS_rSE),
    names_to = c("Stage", ".value"),
    names_sep = "_"
  )




# 2. 화살표 그래프 생성
ggplot(final_data, aes(x = CIMean, y = PCWPMean, color = factor(MS_rMean))) +
  # 포인트 추가
  geom_point(size = 1.6) +  
  
  # 화살표 추가 (Base -> Peak)
  geom_line(aes(group = MS_rMean), 
            arrow = arrow(type = "closed", length = unit(0.2, "cm"))) +
  
  # 색상 설정
  scale_color_manual(values = c("blue", "red")) +  
  
  # 에러바 추가 (각 데이터 포인트에 대해)
  geom_segment(aes(x = CIMean[1], xend = CIMean[1], 
                   y = PCWPMean[1] - PCWPSE[1], yend = PCWPMean[1]), 
               color = "blue") +
  geom_segment(aes(x = CIMean[1] - 0.07, xend = CIMean[1] + 0.07, 
                   y = PCWPMean[1] - PCWPSE[1], yend = PCWPMean[1] - PCWPSE[1]), 
               color = "blue") +
  geom_segment(aes(x = CIMean[1] - CISE[1], xend = CIMean[1], 
                   y = PCWPMean[1], yend = PCWPMean[1]), 
               color = "blue") +
  geom_segment(aes(x = CIMean[1] - CISE[1], xend = CIMean[1] - CISE[1], 
                   y = PCWPMean[1] - 0.3, yend = PCWPMean[1] + 0.3), 
               color = "blue") +
  
  # 동일한 방식으로 2, 3, 4번째 데이터 포인트에 대해 에러바 추가
  geom_segment(aes(x = CIMean[2], xend = CIMean[2], 
                   y = PCWPMean[2] + PCWPSE[2], yend = PCWPMean[2]), 
               color = "blue") +
  geom_segment(aes(x = CIMean[2] + 0.07, xend = CIMean[2] - 0.07, 
                   y = PCWPMean[2] + PCWPSE[2], yend = PCWPMean[2] + PCWPSE[2]), 
               color = "blue") +
  geom_segment(aes(x = CIMean[2] + CISE[2], xend = CIMean[2], 
                   y = PCWPMean[2], yend = PCWPMean[2]), 
               color = "blue") +
  geom_segment(aes(x = CIMean[2] + CISE[2], xend = CIMean[2] + CISE[2], 
                   y = PCWPMean[2] - 0.3, yend = PCWPMean[2] + 0.3), 
               color = "blue") +
  
  geom_segment(aes(x = CIMean[3], xend = CIMean[3], 
                   y = PCWPMean[3] - PCWPSE[3], yend = PCWPMean[3]), 
               color = "red") +
  geom_segment(aes(x = CIMean[3] - 0.07, xend = CIMean[3] + 0.07, 
                   y = PCWPMean[3] - PCWPSE[3], yend = PCWPMean[3] - PCWPSE[3]), 
               color = "red") +
  geom_segment(aes(x = CIMean[3] - CISE[3], xend = CIMean[3], 
                   y = PCWPMean[3], yend = PCWPMean[3]), 
               color = "red") +
  geom_segment(aes(x = CIMean[3] - CISE[3], xend = CIMean[3] - CISE[3], 
                   y = PCWPMean[3] - 0.3, yend = PCWPMean[3] + 0.3), 
               color = "red") +
  
  geom_segment(aes(x = CIMean[4], xend = CIMean[4], 
                   y = PCWPMean[4] + PCWPSE[4], yend = PCWPMean[4]), 
               color = "red") +
  geom_segment(aes(x = CIMean[4] + 0.07, xend = CIMean[4] - 0.07, 
                   y = PCWPMean[4] + PCWPSE[4], yend = PCWPMean[4] + PCWPSE[4]), 
               color = "red") +
  geom_segment(aes(x = CIMean[4] + CISE[4], xend = CIMean[4], 
                   y = PCWPMean[4], yend = PCWPMean[4]), 
               color = "red") +
  geom_segment(aes(x = CIMean[4] + CISE[4], xend = CIMean[4] + CISE[4], 
                   y = PCWPMean[4] - 0.3, yend = PCWPMean[4] + 0.3), 
               color = "red") +
  
  # 격자 표시 및 축과 글씨 스타일 조정
  theme_classic() +  # 외부 격자를 포함한 스타일 적용
  theme(
    axis.line = element_line(size = 1.2),  # 축 선 굵게 설정
    axis.text = element_text(size = 16),  # 축 눈금 글씨 크기 확대
    axis.title = element_text(size = 18),  # 축 제목 글씨 크기 확대
    legend.text = element_text(size = 14),  # 범례 글씨 크기 확대
    legend.position = "right",  # 범례 위치 설정
    plot.title = element_blank()  # 제목 제거
  ) +
  
  # 축 이름 설정
  labs(
    x = "Cardiac Index (CI)",
    y = "Pulmonary Capillary Wedge Pressure (PCWP)",
    color = "Mitral Stenosis"
  )
