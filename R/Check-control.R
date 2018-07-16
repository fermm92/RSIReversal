dfReturn
dfHomeBound.t
dfNHB_old

reverseSummary <-
(dfReturn %>%
  group_by(RSI_Site) %>%
  summarise(sumTH = sum(PHI)))

reverseSummary$sumFH <-
as.matrix(dfHomeBound.t %>%
  group_by(RSI_Site) %>%
  summarise(sumFH = sum(Trips)) %>%
  select(sumFH))

reverseSummary$sumNH <-
as.matrix(dfNHB_old %>%
  group_by(RSI_Site) %>%
  summarise(sumNH = n())%>%
  select(sumNH))

reverseSummary$total <- reverseSummary$sumTH + reverseSummary$sumFH + reverseSummary$sumNH


#Normal direction

observedSummary <- 
df %>%
  filter(Vehicle_Type == "CAR") %>%
  group_by(RSI_Site) %>%
  summarise(countFH = sum(Factor*control_factor))

observedSummary$countTH <-
as.matrix(dfHomeBound %>%
  filter(Vehicle_Type == "CAR") %>%
  group_by(RSI_Site) %>%
  summarise(countTH = sum(Factor*control_factor))%>%
  select(countTH))

observedSummary$countNH <-
  as.matrix(dfNHB_old %>%
              filter(Vehicle_Type == "CAR") %>%
              group_by(RSI_Site) %>%
              summarise(countNH = sum(Factor*control_factor))%>%
              select(countNH))

observedSummary$total <- observedSummary$countFH + observedSummary$countTH +observedSummary$countNH
print(kable(reverseSummary))
print(sum(reverseSummary$total))


observed_inbound <- read.csv("./data/outbound_observed_flow.csv")

observedSummary$actual <- observed_inbound$InboundFlow

observedSummary$control_factor <- observedSummary$actual/observedSummary$total

observedSummary

print(kable(observedSummary))
print(sum(observedSummary$total))

#write_csv( observedSummary[,c("RSI_Site","control_factor")],  "./data/control_return_factor.csv")