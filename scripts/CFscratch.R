mean(wind2012[which(wind2012$X2024_140725.Energy..MWh. > 0), "X2024_140725.Energy..MWh."]/(8760*wind2012[which(wind2012$X2024_140725.Energy..MWh. > 0), "Nameplate..MW."]))
mean(solar2012[which(solar2012$X2024_140725.Energy..MWh. > 0), "X2024_140725.Energy..MWh."]/(8760*solar2012[which(solar2012$X2024_140725.Energy..MWh. > 0), "Nameplate..MW."]))


length(which(solar2012$X2024_140725.Energy..MWh. > 0))
length(which(wind2012$X2024_140725.Energy..MWh. > 0))
