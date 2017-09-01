## Images for Final Presentation and Poster


## Slide 10
## Question 1: Dissimilarity Metrics- Assay 1764
assay1764_RMSE_response_matrices %>% 
  image(x=1:458, y=1:458, .,
        cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
        main="Response Matrix Dissimilarity Using RMSE",
        xlab="Drug Combinations in the Assay", ylab="Drug Combinations in the Assay")


assay1764_syrjala_response_matrices %>% 
  image(x=1:458, y=1:458, .,
        cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
        main="Response Matrix Dissimilarity Using Syrjala Test",
        xlab="Drug Combinations in the Assay", ylab="Drug Combinations in the Assay")



## Slide 11
assay1764_RMSE_bliss_matrices %>% 
  image(x=1:458, y=1:458, .,
        cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
        main="Δ Bliss Matrix Dissimilarity Using RMSE",
        xlab="Drug Combinations in the Assay", ylab="Drug Combinations in the Assay")

assay1764_syrjala_bliss_matrices %>% 
  image(x=1:458, y=1:458, .,
        cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
        main="Δ Bliss Matrix Dissimilarity Using Syrjala Test",
        xlab="Drug Combinations in the Assay", ylab="Drug Combinations in the Assay")



## not used anymore
plot( as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_syrjala_response_matrices), 
      pch=16, cex=0.15,
      cex.lab=1.5, cex.axis=1.25, cex.main=2, cex.sub=1,
      main="Correlation Between Response and Δ Bliss",
      xlab="Syrjala Test- Reponse Matrix Dissimilarities", ylab="Syrjala Test- Δ Bliss Matrix Dissimilarities" )
# make a linear model
assay1764_lm_response_bliss <- lm(as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_syrjala_response_matrices))
# adding it to the graphic
assay1764_lm_response_bliss %>% abline(., col="red")
assay1764_lm_response_bliss %>% summary()



## Slide 15
assay1764_agent_distance %>% 
  image(x=1:458, y=1:458, ., 
        cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
        main="Agent Dissimilarity",
        xlab="Drug Combinations in the Assay", ylab="Drug Combinations in the Assay")

## Slide 15
plot( as.vector(assay1764_syrjala_response_matrices) ~ as.vector(assay1764_agent_distance), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
      main="Correlation Between Agents and Responses",
      xlab="Agent Dissimilarity", ylab="Syrjala Test- Response Matrix Dissimilarity" )
# make a linear model
assay1764_agent_lm2 <- lm(as.vector(assay1764_syrjala_response_matrices) ~ as.vector(assay1764_agent_distance))
# adding it to the graphic
assay1764_agent_lm2 %>% abline(., col="red")
assay1764_agent_lm2 %>% summary()


## Slide 16 
par(mfrow=c(1,3))
plot( as.vector(assay1764_syrjala_response_matrices) ~ as.vector(assay1764_agent_distance), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.25, cex.main=2, cex.sub=1,
      main="Assay 1764 Structure and Combination Response",
      xlab="Agent Structure Dissimilarity", ylab="Syrjala Test- Response Matrices" )
# make a linear model
#assay1764_agent_lm2 <- lm(as.vector(assay1764_syrjala_response_matrices) ~ as.vector(assay1764_agent_distance))
# adding it to the graphic
assay1764_agent_lm2 %>% abline(., col="red")

plot( as.vector(assay1763_syrjala_response_matrices) ~ as.vector(assay1763_agent_distance), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.25, cex.main=2, cex.sub=1,
      main="Assay 1763 Structure and Combination Response",
      xlab="Agent Structure Dissimilarity", ylab="Syrjala Test- Response Matrices" )
# make a linear model
#assay1763_agent_lm2 <- lm(as.vector(assay1763_syrjala_response_matrices) ~ as.vector(assay1763_agent_distance))
# adding it to the graphic
assay1763_agent_lm2 %>% abline(., col="red")

plot( as.vector(assay1761_syrjala_response_matrices) ~ as.vector(assay1761_agent_distance), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.25, cex.main=2, cex.sub=1,
      main="Assay 1761 Structure and Combination Response",
      xlab="Agent Structure Dissimilarity", ylab="Syrjala Test- Response Matrices" )
# make a linear model
#assay1761_agent_lm2 <- lm(as.vector(assay1761_syrjala_response_matrices) ~ as.vector(assay1761_agent_distance))
# adding it to the graphic
assay1761_agent_lm2 %>% abline(., col="red")



## Slide 17
plot( as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_agent_distance), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
      main="Correlation Between Agents and Δ Bliss",
      xlab="Agent Dissimilarity", ylab="Syrjala Test- Δ Bliss Matrix Dissimilarity" )
# make a linear model
assay1764_agent_lm <- lm(as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_agent_distance))
# adding it to the graphic
assay1764_agent_lm %>% abline(., col="red")
assay1764_agent_lm %>% summary()

## Slide 18
par(mfrow=c(1,3))
plot( as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_agent_distance), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.25, cex.main=2, cex.sub=1,
      main="Assay 1764 Structure and Δ Bliss",
      xlab="Agent Structure Dissimilarity", ylab="Syrjala Test- Δ Bliss Matrices" )
# make a linear model
assay1764_agent_lm3 <- lm(as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_agent_distance))
# adding it to the graphic
assay1764_agent_lm3 %>% abline(., col="red")

plot( as.vector(assay1763_syrjala_bliss_matrices) ~ as.vector(assay1763_agent_distance), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.25, cex.main=2, cex.sub=1,
      main="Assay 1763 Structure and Δ Bliss",
      xlab="Agent Structure Dissimilarity", ylab="Syrjala Test- Δ Bliss Matrices" )
# make a linear model
assay1763_agent_lm3 <- lm(as.vector(assay1763_syrjala_bliss_matrices) ~ as.vector(assay1763_agent_distance))
# adding it to the graphic
assay1763_agent_lm3 %>% abline(., col="red")

plot( as.vector(assay1761_syrjala_bliss_matrices) ~ as.vector(assay1761_agent_distance), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.25, cex.main=2, cex.sub=1,
      main="Assay 1761 Structure and Δ Bliss",
      xlab="Agent Structure Dissimilarity", ylab="Syrjala Test- Δ Bliss Matrices" )
# make a linear model
assay1761_agent_lm3 <- lm(as.vector(assay1761_syrjala_bliss_matrices) ~ as.vector(assay1761_agent_distance))
# adding it to the graphic
assay1761_agent_lm3 %>% abline(., col="red")




## Slide 22
assay1764_dose_jitter_inverse <-  1 - assay1764_dose_jitter

## Slide 22
assay1764_dose_jitter_inverse %>% 
  image(x=1:458, y=1:458, .,
        cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
        main="Dose Response Dissimilarity",
        xlab="Drug Combinations in the Assay", ylab="Drug Combinations in the Assay")


## Slide 22
plot( as.vector(assay1764_syrjala_response_matrices) ~ as.vector(assay1764_dose_jitter_inverse), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.5, cex.main=1.75, cex.sub=1.5,
      main="Correlation Between Dose Response and Response",
      xlab="Dose Response Dissimilarity", ylab="Syrjala Test- Response Matrix Dissimilarity" )
# make a linear model
assay1764_dose_response_lm <- lm(as.vector(assay1764_syrjala_response_matrices) ~ as.vector(assay1764_dose_jitter_inverse))
# adding it to the graphic
assay1764_dose_response_lm %>% abline(., col="red")
assay1764_dose_response_lm %>% summary()


## Slide 23
assay1763_sub_last <- good_subset(assay1763, malaria_good, keep_last = TRUE)
assay1763_dose_jitter <- dose_list_jitter(assay1763_sub_last)
assay1763_dose_jitter_inverse <-  1 - assay1763_dose_jitter

assay1761_sub_last <- good_subset(assay1761, malaria_good, keep_last = TRUE)
assay1761_dose_jitter <- dose_list_jitter(assay1761_sub_last)
assay1761_dose_jitter_inverse <-  1 - assay1761_dose_jitter

## Slide 23
par(mfrow=c(1,3))

plot( as.vector(assay1764_syrjala_response_matrices) ~ as.vector(assay1764_dose_jitter_inverse), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.5, cex.main=1.75, cex.sub=1.5,
      main="Assay 1764 Dose Response and Combination Response",
      xlab="Dose Response Dissimilarity", ylab="Syrjala Test- Response Matrix Dissimilarity" )
# make a linear model
assay1764_dose_response_lm <- lm(as.vector(assay1764_syrjala_response_matrices) ~ as.vector(assay1764_dose_jitter_inverse))
# adding it to the graphic
assay1764_dose_response_lm %>% abline(., col="red")

plot( as.vector(assay1763_syrjala_response_matrices) ~ as.vector(assay1763_dose_jitter_inverse), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.5, cex.main=1.75, cex.sub=1.5,
      main="Assay 1763 Dose Response and Combination Response",
      xlab="Dose Response Dissimilarity", ylab="Syrjala Test- Response Matrix Dissimilarity" )
# make a linear model
assay1763_dose_response_lm <- lm(as.vector(assay1763_syrjala_response_matrices) ~ as.vector(assay1763_dose_jitter_inverse))
# adding it to the graphic
assay1763_dose_response_lm %>% abline(., col="red")

plot( as.vector(assay1761_syrjala_response_matrices) ~ as.vector(assay1761_dose_jitter_inverse), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.5, cex.main=1.75, cex.sub=1.5,
      main="Assay 1761 Dose Response and Combination Response",
      xlab="Dose Response Dissimilarity", ylab="Syrjala Test- Response Matrix Dissimilarity" )
# make a linear model
assay1761_dose_response_lm <- lm(as.vector(assay1761_syrjala_response_matrices) ~ as.vector(assay1761_dose_jitter_inverse))
# adding it to the graphic
assay1761_dose_response_lm %>% abline(., col="red")




## Slide 24
plot( as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_dose_jitter_inverse), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
      main="Correlation Between Dose Response and Δ Bliss",
      xlab="Dose Response Dissimilarity", ylab="Syrjala Test- Δ Bliss Matrix Dissimilarity" )
# make a linear model
assay1764_dose_lm <- lm(as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_dose_jitter_inverse))
# adding it to the graphic
assay1764_dose_lm %>% abline(., col="red")
assay1764_dose_lm %>% summary()



## Slide 25
par(mfrow=c(1,3))

plot( as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_dose_jitter_inverse), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
      main="Assay 1764 Dose Response and Δ Bliss",
      xlab="Dose Response Dissimilarity", ylab="Syrjala Test- Δ Bliss Matrix Dissimilarity" )
# make a linear model
assay1764_dose_lm <- lm(as.vector(assay1764_syrjala_bliss_matrices) ~ as.vector(assay1764_dose_jitter_inverse))
# adding it to the graphic
assay1764_dose_lm %>% abline(., col="red")

plot( as.vector(assay1763_syrjala_bliss_matrices) ~ as.vector(assay1763_dose_jitter_inverse), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
      main="Assay 1763 Dose Response and Δ Bliss",
      xlab="Dose Response Dissimilarity", ylab="Syrjala Test- Δ Bliss Matrix Dissimilarity" )
# make a linear model
assay1763_dose_lm <- lm(as.vector(assay1763_syrjala_bliss_matrices) ~ as.vector(assay1763_dose_jitter_inverse))
# adding it to the graphic
assay1763_dose_lm %>% abline(., col="red")

plot( as.vector(assay1761_syrjala_bliss_matrices) ~ as.vector(assay1761_dose_jitter_inverse), 
      pch=16, cex=0.05,
      cex.lab=1.5, cex.axis=1.5, cex.main=2, cex.sub=1.5,
      main="Assay 1761 Dose Response and Δ Bliss",
      xlab="Dose Response Dissimilarity", ylab="Syrjala Test- Δ Bliss Matrix Dissimilarity" )
# make a linear model
assay1761_dose_lm <- lm(as.vector(assay1761_syrjala_bliss_matrices) ~ as.vector(assay1761_dose_jitter_inverse))
# adding it to the graphic
assay1761_dose_lm %>% abline(., col="red")


## Slide 26 SPP
par(mfrow=c(1,1))

plot( assay1764_dose_vector_inverse ~ assay1764_agent_vector, 
      pch=16, cex=1,
      cex.lab=1.5, cex.axis=1.5, cex.main=1.75, cex.sub=1.5,
      main="Correlation Between Agents and Dose Response",
      xlab="Agent Dissimilarity", ylab="Dose Response Dissimilarity" )
# make a linear model
assay1764_lm_agent_dose <- lm( assay1764_dose_vector_inverse ~ assay1764_agent_vector)
# adding it to the graphic
assay1764_lm_agent_dose %>% abline(., col="red")
assay1764_lm_agent_dose %>% summary()




## Slide 27 clustering

assay1764_syrjala_response_matrices %>% 
  heatmap(., main="Clustering of Dissimilarity of Combination Response",
          cex.main=2)

assay1764_RMSE_response_matrices %>% heatmap()

assay1764_dose_jitter_inverse %>% heatmap()





assay1764_syrjala_bliss_matrices %>% heatmap()

assay1764_agent_distance %>% heatmap()





