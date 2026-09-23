#' @title Relative abundance indecies
#' @description
#' Generate abundance plots and datasets
#'
#' @param d data.frame
#'
#' @export
OysterPop<- function(d, global.line = c("lm", "loess", "both"), river.line= c("lm", "loess", "both")){
  if (is.data.frame(d) == F) {return(
    "Warning: Dataset must be of class data.frame or tibble")}
  else{
# Rename Misentered Obs
d$River_name[which(d$River_name == "Mobjack")] <- "Mobjack Bay"
d$River_name[which(d$River_name == "Elizabeth River ")] <- "Elizabeth River"
d$River_name[which(d$River_name == "Great Wicomico")] <- "Great Wicomico River"

# Counts of Strata
nStrata<- d |> group_by(Year) |> count(Station_ID) |> rename(StrataObs = n)
nTowYear <- d |>  group_by(Year) |> count() |> rename(TotalYearTows = n)

# Grouped Data of oysters per station and year
groupedD<- d|> select(Year, Station_ID, River_name, Spat, Small, Market) |>
  group_by(Station_ID, Year) |>
  summarise(Spat = sum(Spat), Small = sum(Small), Market = sum(Market), River_name = unique(River_name))
nOyster <- groupedD |>
  left_join(nStrata, join_by(Year, Station_ID)) |>
  left_join(nTowYear, by = "Year") |>
  mutate(TotalCount = rowSums(across(c(Spat, Small,Market))),
         AvgCount= rowMeans(across(c(Spat, Small, Market))),
         OysterPerTow = TotalCount/StrataObs) |>
  rowwise() |>
  mutate(Var = var(c(Spat, Small, Market))) |>
  arrange(Year, Station_ID)

# Yearly Relative abundance index
yearSum <- nOyster |> group_by(Year) |>
  summarise(yearSum= sum(AvgCount * StrataObs))  |>
  left_join(nTowYear, by = "Year") |> ungroup() |>
  mutate(WeightedMean = yearSum/TotalYearTows)

# Calculate weighted variance
nOyster <- nOyster |> group_by(Year) |>
  mutate(Weights = StrataObs/TotalYearTows,
         yearVar = ((Weights^2) * Var/StrataObs))

#Yearly error in abundance index
yearVar <- nOyster |> summarise(yearVar  = sum(yearVar)) |>
  mutate(SEYear = sqrt(yearVar), CI95 = SEYear * 1.96)

# Bay-wide abundance data
yearlyOysterData<- left_join(yearSum, yearVar, by = "Year")

# Plot Abundance
g<- ggplot(data = yearlyOysterData, aes (x = Year, y = WeightedMean)) +
  theme_classic() +
  ylab(expression(paste("Weighted Mean Abudnance (No. grab"^"-1",")"))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.93)) +
  scale_x_continuous("Year", labels = as.character(yearlyOysterData$Year), breaks = yearlyOysterData$Year) +
  geom_line(lwd =1) +
  geom_point(size = 2.5, stroke = NA) +
  geom_errorbar(aes(ymin = WeightedMean - CI95, ymax = WeightedMean + CI95), linewidth = 1)
if (missing(global.line)) {
  g<- g
}
else{
  g<- g +
  {if(global.line == "lm")  geom_line(stat = "smooth", method = "lm", linetype = "dashed", linewidth = 1.3, se = F, colour = "#3366FF")} +
  {if(global.line=="loess") geom_line(stat = "smooth",method = "loess", linetype = "dashed", linewidth = 1.3, se = F, colour = "#3366FF")} +
  {if(global.line == "both")geom_line(stat = "smooth",method = "lm", linetype = "dashed", linewidth = 1.3, se = F, colour = "#3366FF")} +
  {if(global.line == "both")geom_line(stat = "smooth",method = "loess", linetype = "dashed", linewidth = 1.3, se = F,
                                        color = "firebrick")}
}
# Plots for output
oysterPlots<- list()
riverPlots<- list()
jointPlots<- list()
oysterPlots[[1]] <- plot(g + ggtitle("Bay-wide Average Oyster Abundance"))
names(oysterPlots)[[1]] <- "Global"

# River Abundances
nStrataRiver<- d |> group_by(Year,River_name) |> count(Station_ID) |> rename(StrataObs = n)
nTowRiverYear <- d |>  group_by(Year, River_name) |> count() |> rename(TotalYearTows = n)

nOysterRiver <- groupedD |>
  left_join(nStrataRiver, join_by(Year, Station_ID, River_name)) |>
  left_join(nTowRiverYear, join_by(Year, River_name)) |>
  mutate(TotalCount = rowSums(across(c(Spat, Small,Market))),
         AvgCount= rowMeans(across(c(Spat, Small, Market))),
         OysterPerTow = TotalCount/StrataObs) |>
  rowwise() |>
  mutate(Var = var(c(Spat, Small, Market))) |>
  arrange(Year, Station_ID, River_name)
yearSumRiver <- nOysterRiver |> group_by(Year, River_name) |>
  summarise(yearSum= sum(AvgCount * StrataObs))  |>
  left_join(nTowRiverYear, join_by(Year, River_name)) |> ungroup() |>
  mutate(WeightedMean = yearSum/TotalYearTows)
nOysterRiver <- nOysterRiver |> group_by(Year, River_name) |>
  mutate(Weights = StrataObs/TotalYearTows,
         yearVar = ((Weights^2) * Var/StrataObs))
yearVarRiver <- nOysterRiver |> summarise(yearVar  = sum(yearVar)) |>
  mutate(SEYear = sqrt(yearVar), CI95 = SEYear * 1.96)

# Bay-wide abundance data
yearlyRiverOysterData<- left_join(yearSumRiver, yearVarRiver, join_by(Year, River_name))

for (i in 1:length(unique(yearlyRiverOysterData$River_name))) {
  r<-
    ggplot(data = yearlyRiverOysterData |> filter(River_name == unique(yearlyRiverOysterData$River_name)[i]),
         aes (x = Year, y = WeightedMean)) +
    theme_classic() +
    ylab(expression(paste("Weighted Mean Abudnance (No. grab"^"-1",")"))) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 0.93)) +
    scale_x_continuous("Year", labels = as.character(yearlyRiverOysterData$Year), breaks = yearlyRiverOysterData$Year) +
    geom_line(lwd =1) +
    geom_point(size = 2.5, stroke = NA) +
    geom_errorbar(aes(ymin = WeightedMean - CI95, ymax = WeightedMean + CI95), linewidth = 1) +
    ggtitle(paste(unique(yearlyRiverOysterData$River_name)[i], "Average Oyster Abundance"))
  if (missing(river.line)) {
    r<- r
  }
  else{
    r<- r +
      {if(river.line == "lm")  geom_line(stat = "smooth", method = "lm", linetype = "dashed", linewidth = 1.3, se = F, colour = "#3366FF")} +
      {if(river.line=="loess") geom_line(stat = "smooth",method = "loess", linetype = "dashed", linewidth = 1.3, se = F, colour = "#3366FF")} +
      {if(river.line == "both")geom_line(stat = "smooth",method = "lm", linetype = "dashed", linewidth = 1.3, se = F, colour = "#3366FF")} +
      {if(river.line == "both")geom_line(stat = "smooth",method = "loess", linetype = "dashed", linewidth = 1.3, se = F,
                                          color = "firebrick")}
  }
  riverPlots[[i]]<- plot(r)
  names(riverPlots)[[i]] <- unique(yearlyRiverOysterData$River_name)[i]

gr <- g + ggtitle(paste(unique(yearlyRiverOysterData$River_name)[i], "and Bay-wide Average Oyster Abundance")) +
  geom_line(data = yearlyRiverOysterData |> filter(River_name == unique(yearlyRiverOysterData$River_name)[i]),
                aes(x = Year, y = WeightedMean), lwd =1, inherit.aes = F) +
  geom_point(data = yearlyRiverOysterData |> filter(River_name == unique(yearlyRiverOysterData$River_name)[i]),
             aes(x = Year, y = WeightedMean), size = 2.5, stroke = NA, inherit.aes = F) +
  geom_errorbar(data = yearlyRiverOysterData |> filter(River_name == unique(yearlyRiverOysterData$River_name)[i]),
                aes (x = Year, y = WeightedMean, ymin = WeightedMean - CI95, ymax = WeightedMean + CI95), linewidth = 1, inherit.aes = F)
  if (missing(river.line)) {
    gr<- gr
  }
  else{
    gr<- gr +
      {if(river.line == "lm")  geom_line(data = yearlyRiverOysterData |> filter(River_name == unique(yearlyRiverOysterData$River_name)[i]),
                                         aes(x = Year, y = WeightedMean), stat = "smooth", method = "lm", linetype = "dashed", linewidth = 1.3,
                                         se = F, colour = "#3366FF", inherit.aes = F)} +
      {if(river.line=="loess") geom_line(data = yearlyRiverOysterData |> filter(River_name == unique(yearlyRiverOysterData$River_name)[i]),
                                         aes(x = Year, y = WeightedMean), stat = "smooth",method = "loess", linetype = "dashed", linewidth = 1.3,
                                         se = F, colour = "#3366FF", inherit.aes = F)} +
      {if(river.line == "both")geom_line(data = yearlyRiverOysterData |> filter(River_name == unique(yearlyRiverOysterData$River_name)[i]),
                                         aes(x = Year, y = WeightedMean), stat = "smooth",method = "lm", linetype = "dashed", linewidth = 1.3,
                                         se = F, colour = "#3366FF", inherit.aes = F)} +
      {if(river.line == "both")geom_line(data = yearlyRiverOysterData |> filter(River_name == unique(yearlyRiverOysterData$River_name)[i]),
                                         aes(x = Year, y = WeightedMean), stat = "smooth",method = "loess", linetype = "dashed", linewidth = 1.3,
                                         se = F, color = "firebrick", inherit.aes = F)}
  }

# Base Plot for river indexes
p<- ggplot_build(gr)
for (j in 1:length(ggplot_build(g)$data)) {
  p$data[[j]]$alpha <- 0.3
  }
p<- ggplot_gtable(p)
jointPlots[[i]]<- as.ggplot(p)
names(jointPlots)[[i]] <- paste(unique(yearlyRiverOysterData$River_name)[i], "Joint", sep = " ")
}

oysterPlots[[2]] <- riverPlots
oysterPlots[[3]] <- jointPlots
oysterPlots[[4]] <- plot_grid(plotlist = jointPlots)

names(oysterPlots)[2:4] <- c("River Abundances", "Comparisons", "Comparison Plot")

globalList<- list(nOyster, yearlyOysterData)
names(globalList) <- c("Oyster Data", "Weighted Mean")
riverList<-  list(nOysterRiver, yearlyRiverOysterData)
names(riverList) <- c("Oyster Data per River", "Weighted Mean per River")
dataList<- list(globalList, riverList)
names(dataList) <- c("Bay-wide aggregate Data", "Per River aggregate Data")
funList<- list(dataList, oysterPlots)
names(funList)<- c("Data","Plots")
return(funList)
  }
} |> suppressMessages()



