new_data <- data.frame(CellSize=seq(from=0.352, to=0.664, length=100)) 
predictions<- predict(M1, newdata = new_data, type = "link", se.fit = TRUE) # the type="link" here predicted the fit and se on the log-linear scale.  
new_data$pred<- predictions$fit 
new_data$se<- predictions$se.fit 
new_data$upperCI<- new_data$pred+(new_data$se*1.96) 
new_data$lowerCI<- new_data$pred-(new_data$se*1.96) 

# Making the Plot  
p <- ggplot(new_data, aes(x=CellSize, y=plogis(pred)))+  
  geom_line(col="black")+ 
  geom_point(worker, mapping = aes(x=CellSize, y=Parasites), col="blue")+ 
  geom_ribbon(aes(ymin=plogis(lowerCI), ymax=plogis(upperCI), alpha=0.2), show.legend = FALSE)+  
  labs(y="Probability of Infection", x="Cell Size (cm)")+ 
  theme_classic() 

print(p)