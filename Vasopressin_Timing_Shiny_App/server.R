
library(tidyverse)
library(rms)
library(psych)
library(shiny)
library(plotly)

# Loading and importing the data
mimic <- read.csv("C:/Users/sachag/OneDrive - Cleveland Clinic/Desktop/Gretchen R Code/MSHDS/HDSC 824/AVP Data/MIMIC-IV_finaldf.csv") %>% 
  select(-icuid, -time0, -vasoactivestart, -vasoactivestop, -avpstart, -avpstop) %>%
  mutate_if(is.character, as.factor)

eicu <- read.csv("C:/Users/sachag/OneDrive - Cleveland Clinic/Desktop/Gretchen R Code/MSHDS/HDSC 824/AVP Data/eICU_finaldf.csv") %>% 
  select(-time0, -vasoactivestart, -vasoactivestop, -avpstart, -avpstop) %>%
  mutate_if(is.character, as.factor)

# Combining data and limiting to just patients with septic shock via modified sepsis 3 definition
fulldf <- rbind(mimic,eicu) %>% 
  mutate(diabetes = if_else(diabetes %in% c("Yes", "yes"), "Yes", "No"),
         cirrhosis = if_else(cirrhosis %in% c("Yes", "yes"), "Yes", "No"),
         immune_suppression = if_else(immune_suppression %in% c("Yes", "yes"), "Yes", "No"),
         copd = if_else(copd %in% c("Yes", "yes"), "Yes", "No"),
         dialysis = if_else(dialysis %in% c("Yes", "yes"), "Yes", "No")) %>% 
  mutate(iculocation= if_else(iculocation %in% c("Neurosciences ICU", "NICU"), "NICU", iculocation)) %>%
  mutate_if(is.character, as.factor) %>% 
  mutate(sofa_change = sofa_48hr-sofa_baseline) %>% 
  mutate(avpstartdose_category = ifelse(avpstartdose == 0.03, "0.03", 
                                        ifelse(avpstartdose == 0.04, "0.04", "other"))) %>% 
  relocate(avpstartdose_category, .after = avpstartdose) %>%
  mutate(neqquantile = ifelse(neqdoseatavp <= 14.3,"<=14.3", 
                              ifelse(neqdoseatavp >14.3 & neqdoseatavp <=26.4,"14.3-26.4",
                                     ifelse(neqdoseatavp >26.4 & neqdoseatavp <=40.9, "26.4-40.9",">40.9"))),
         lactatequantile = ifelse(lactateatavp <= 2.5, "<=2.5", 
                                  ifelse(lactateatavp >2.5 & lactateatavp <=3.8, "2.5-3.8", 
                                         ifelse(lactateatavp >3.8 & lactateatavp <=6.4,"3.8-6.4", ">6.4"))),
         timingquantile = ifelse(timefromshockonset_hr <= 2.0, "<=2.0", 
                                 ifelse(timefromshockonset_hr >2.0 & timefromshockonset_hr <=5.6, "2.0-5.6", 
                                        ifelse(timefromshockonset_hr >5.6 & timefromshockonset_hr <=13.8,"5.6-13.8", ">13.8")))) %>%
  mutate(akimva = ifelse(akimva == "No AKI", "No AKI", 
                         ifelse(akimva == "AKI", "AKI", 
                                ifelse(akimva == "ESRD", "No AKI", "")))) %>%
  filter(include_exclude_sepsis3nomapbosch == "include") %>%
  mutate_if(is.character, as.factor) %>% 
  select(1:43,56:58) %>% select(-no_chronic_health) %>% 
  mutate(neqdose_bin = cut(neqdoseatavp, breaks = c(-Inf, 10, seq(20, 400, by = 10)), 
                           labels = c("<10", paste(seq(10, 390, by = 10), seq(20, 400, by = 10), sep = "-")),
                           right = FALSE)) %>%
  mutate(lactate_bin = cut(lactateatavp, breaks = c(0.7, seq(5, 30, by = 5), Inf), 
                           labels = c("<5", paste(seq(5, 30, by = 5), seq(10, 35, by = 5), sep = "-")),
                           right = FALSE)) %>%
  mutate(time_bin = cut(timefromshockonset_hr, breaks = c(-Inf, seq(5, 50, by = 5), Inf), 
                        labels = c("<5", paste(seq(5, 49, by = 5), seq(10, 50, by = 5), sep = "-"), ">35"),
                        right = FALSE))


# Define Server - the code within the app
server <- function(input, output) {
  observe({
    
    # NEQ ####
    if(input$var_choice == "NEQ"){
      ## Figure 1a: NEQ Timing Histogram ####
      output$figure1 <- renderPlotly({
        
        # Total count for the bin summary 
        fulldf %>% filter(neqdoseatavp < 100) %>% nrow()
        
        bin_summary <- fulldf %>%
          filter(neqdoseatavp < 100) %>%
          group_by(neqdose_bin) %>%
          summarise(count = n(), percent = (count / 1339) * 100) %>%
          ungroup()
        
        plot_data <- fulldf %>%
          filter(neqdoseatavp < 100) %>%
          left_join(bin_summary, by = "neqdose_bin")
        
        plot <- ggplot(data = plot_data, aes(x=neqdose_bin)) + 
          geom_bar(aes(text = paste0("NEQ Dose ", neqdose_bin, " mcg/min", "<br>Count: ", count, "<br>Percent: ", round(percent, 1), "%")), 
                   color = "black", fill = "lightblue", alpha = 0.5) + 
          scale_y_continuous(breaks = seq(0, 310, by = 50), limits = c(0, 310)) + 
          xlab('Norepinephrine-equivalent dose at vasopressin initiation, mcg/min') + 
          ylab('Frequency')+
          ggtitle("Norepinephrine-Equivalent Dose at Vasopressin Initiation") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))  
        ggplotly(plot, tooltip = "text")  
      })
      
      ## Figure 2a: NEQ Cubic Splines Regression ####
      output$figure2 <- renderPlotly({
        
        fulldfmodsepsis3 <- fulldf
        fulldfmodsepsis3$inhospital_mortality <- droplevels(fulldfmodsepsis3$inhospital_mortality) # dropping unused level "Unknown"
        fulldfmodsepsis3$gender <- droplevels(fulldfmodsepsis3$gender) # dropping unused level "Unknown"
        ddist <- datadist(fulldfmodsepsis3)
        options(datadist= ddist)
        
        cubavptime <- lrm(inhospital_mortality~ rcs(neqdoseatavp,3) + lactateatavp + timefromshockonset_hr + 
                            age + weight + mv_baseline + akimva + sofa_baseline + totalfluidbolus + fluidbalance + 
                            gender + race + immune_suppression + database + iculocation + hydrocortuse, 
                          data = fulldfmodsepsis3, x=TRUE, y=TRUE)
        
        pred.neq <- as.data.frame(Predict(cubavptime, type = "predictions", neqdoseatavp, lactateatavp = 3.7, timefromshockonset_hr = 5.6, age=65, weight=80, akimva = "No AKI", database = "MIMIC-IV", sofa_baseline=10, totalfluidbolus = 1000, fluidbalance=8500, immune_suppression="No", iculocation = "MICU", hydrocortuse = "Yes", mv_baseline = "Yes", race = "White", gender = "Male"))
        pred.neq$prob <- logistic(pred.neq$yhat)
        pred.neq$prob.lower <- logistic(pred.neq$lower)
        pred.neq$prob.upper <- logistic(pred.neq$upper)
        
        plot <- ggplot(pred.neq, aes(x = neqdoseatavp)) +
          geom_line(aes(y = prob), color = "black", linewidth = 0.5) +
          geom_line(aes(y = prob.lower), color = "lightblue", linewidth = 0.5) +
          geom_line(aes(y = prob.upper), color = "lightblue", linewidth = 0.5) +
          geom_point(aes(y = prob,
                         text = paste0("Predicted Mortality: ", scales::percent(prob, accuracy = 0.1), 
                                       "<br>95% CI: ", scales::percent(prob.lower, accuracy = 0.1), 
                                       "-", scales::percent(prob.upper, accuracy = 0.1))), 
                     size = 0.1, color = "transparent") +
          xlab('Norepinephrine-equivalent dose at vasopressin initiation, mcg/min') + 
          ylab('Predicted incidence of in-hospital mortality, %')+
          ggtitle("Predicted Incidence of In-Hospital Mortality by NEQ Dose") +
          scale_x_continuous(breaks=seq(0,160,by=20), limits=c(0,160)) +
          scale_y_continuous(breaks=seq(.2,1,by=.1), limits=c(.2,1), labels = scales::percent_format(accuracy = 1)) + 
          theme_light() + 
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        ggplotly(plot, tooltip = "text")
      })
      
      ## Figure 3a1: NEQ then Time Quantile Plot ####
      output$figure3a <- renderPlotly({
        quantileplotneqtime <- fulldf %>% 
          select(timingquantile, neqquantile, inhospital_mortality) %>%
          group_by(timingquantile, neqquantile) %>% 
          mutate(prop_mortality = mean(inhospital_mortality == "Yes")) %>% ungroup() %>%
          distinct(timingquantile,neqquantile,.keep_all = TRUE) %>%
          select(-inhospital_mortality) %>%
          arrange(neqquantile,timingquantile) %>%
          mutate(combined_quantiles = as.factor(paste("NEQ",neqquantile,"& Time", timingquantile)))
        
        quantileplotneqtime$combined_quantiles <- factor(quantileplotneqtime$combined_quantiles, 
                                                         levels = c("NEQ <=14.3 & Time <=2.0",
                                                                    "NEQ <=14.3 & Time 2.0-5.6",
                                                                    "NEQ <=14.3 & Time 5.6-13.8",
                                                                    "NEQ <=14.3 & Time >13.8",
                                                                    "NEQ 14.3-26.4 & Time <=2.0",
                                                                    "NEQ 14.3-26.4 & Time 2.0-5.6",
                                                                    "NEQ 14.3-26.4 & Time 5.6-13.8",
                                                                    "NEQ 14.3-26.4 & Time >13.8",
                                                                    "NEQ 26.4-40.9 & Time <=2.0",
                                                                    "NEQ 26.4-40.9 & Time 2.0-5.6",
                                                                    "NEQ 26.4-40.9 & Time 5.6-13.8",
                                                                    "NEQ 26.4-40.9 & Time >13.8",
                                                                    "NEQ >40.9 & Time <=2.0",
                                                                    "NEQ >40.9 & Time 2.0-5.6",
                                                                    "NEQ >40.9 & Time 5.6-13.8",
                                                                    "NEQ >40.9 & Time >13.8"))
        
        
        plot <- ggplot(quantileplotneqtime, aes(y = combined_quantiles, x = prop_mortality, color = neqquantile, 
                                                text = paste0("Mortality: ", scales::percent(prop_mortality), 
                                                              "<br>Quantiles: ", combined_quantiles))) +
          geom_point(size = 3) +
          geom_vline(xintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "gray") +
          scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
          labs(x = "In-hospital Mortality (%)", y = "", color = "neqatavp") +
          ggtitle("NEQ Dose and Time Duration from Shock Onset") +
          theme_bw() +
          scale_y_discrete(limits = c(rev(levels(quantileplotneqtime$combined_quantiles)))) + # adding line breaks
          theme(axis.text.x = element_text(color = "black"), # making axis labels/text black
                axis.text.y = element_text(color = "black", size = 10),
                axis.title.x = element_text(size = 14),
                plot.title = element_text(face = "bold"),
                panel.border = element_blank()) + 
          guides(color = "none") + # removing legend
          scale_colour_manual(values=c("blue", "firebrick1", "forestgreen", "darkviolet"))
        
        ggplotly(plot, tooltip = "text")
      })
      
      ## Figure 3b1: NEQ then Lactate Quantile Plot ####
      output$figure3b <- renderPlotly({
        quantileplotneqlac <- fulldf %>% 
          select(neqquantile, lactatequantile, inhospital_mortality) %>%
          group_by(neqquantile, lactatequantile) %>% 
          mutate(prop_mortality = mean(inhospital_mortality == "Yes")) %>% ungroup() %>%
          distinct(neqquantile,lactatequantile,.keep_all = TRUE) %>%
          select(-inhospital_mortality) %>%
          arrange(neqquantile,lactatequantile) %>%
          mutate(combined_quantiles = as.factor(paste("NEQ", neqquantile,"& Lactate", lactatequantile)))
        
        quantileplotneqlac$combined_quantiles <- factor(quantileplotneqlac$combined_quantiles, 
                                                        levels = c("NEQ <=14.3 & Lactate <=2.5",
                                                                   "NEQ <=14.3 & Lactate 2.5-3.8",
                                                                   "NEQ <=14.3 & Lactate 3.8-6.4",
                                                                   "NEQ <=14.3 & Lactate >6.4",
                                                                   "NEQ 14.3-26.4 & Lactate <=2.5",
                                                                   "NEQ 14.3-26.4 & Lactate 2.5-3.8",
                                                                   "NEQ 14.3-26.4 & Lactate 3.8-6.4",
                                                                   "NEQ 14.3-26.4 & Lactate >6.4",
                                                                   "NEQ 26.4-40.9 & Lactate <=2.5",
                                                                   "NEQ 26.4-40.9 & Lactate 2.5-3.8",
                                                                   "NEQ 26.4-40.9 & Lactate 3.8-6.4",
                                                                   "NEQ 26.4-40.9 & Lactate >6.4",
                                                                   "NEQ >40.9 & Lactate <=2.5",
                                                                   "NEQ >40.9 & Lactate 2.5-3.8",
                                                                   "NEQ >40.9 & Lactate 3.8-6.4",
                                                                   "NEQ >40.9 & Lactate >6.4"))
        
        plot <- ggplot(quantileplotneqlac, aes(y = combined_quantiles, x = prop_mortality, color = neqquantile, 
                                               text = paste0("Mortality: ", scales::percent(prop_mortality), 
                                                             "<br>Quantiles: ", combined_quantiles))) +
          geom_point(size = 3) +
          geom_vline(xintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "gray") +
          scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
          labs(x = "In-hospital Mortality (%)", y = "", color = "neqatavp") +
          ggtitle("NEQ Dose and Lactate") +
          theme_bw() +
          scale_y_discrete(limits = c(rev(levels(quantileplotneqlac$combined_quantiles)))) + # adding line breaks
          theme(axis.text.x = element_text(color = "black"), # making axis labels/text black
                axis.text.y = element_text(color = "black", size = 10),
                axis.title.x = element_text(size = 14),
                plot.title = element_text(face = "bold"),
                panel.border = element_blank()) + 
          guides(color = "none") + # removing legend
          scale_colour_manual(values=c("blue", "firebrick1", "forestgreen", "darkviolet"))
        
        ggplotly(plot, tooltip = "text")
      })
      
      # LACTATE ####
    } else if (input$var_choice == "Lactate") {
      
      ## Figure 1b: Lactate Timing Histogram ####
      output$figure1 <- renderPlotly({
        
        lacbin_summary <- fulldf %>%
          group_by(lactate_bin) %>%
          summarise(count = n(), percent = (count / 1409) * 100) %>%
          ungroup()
        
        plot_data <- fulldf %>%
          group_by(lactate_bin) %>%
          left_join(lacbin_summary, by = "lactate_bin")
        
        plot <- ggplot(data = plot_data, aes(x=lactate_bin)) + 
          geom_bar(aes(text = paste0("Lactate ", lactate_bin, " mmol/L", "<br>Count: ", count, "<br>Percent: ", round(percent, 1), "%")),
                   color = "black", fill = "lightgreen", alpha = 0.5) + 
          scale_y_continuous(breaks = seq(0, 950, by = 100), limits = c(0, 950)) + 
          xlab('Lactate Concentration at Vasopressin Initiation, mmol/L') + 
          ylab('Frequency')+
          ggtitle("Lactate Concentration at Vasopressin Initiation") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        ggplotly(plot, tooltip = "text")
      })
      
      ## Figure 2b: Lactate Cubic Splines Regression ####
      output$figure2 <- renderPlotly({
        
        fulldfmodsepsis3 <- fulldf
        fulldfmodsepsis3$inhospital_mortality <- droplevels(fulldfmodsepsis3$inhospital_mortality) # dropping unused level "Unknown"
        fulldfmodsepsis3$gender <- droplevels(fulldfmodsepsis3$gender) # dropping unused level "Unknown"
        ddist <- datadist(fulldfmodsepsis3)
        options(datadist=ddist)
        
        cubavptime <- lrm(inhospital_mortality~ rcs(neqdoseatavp,3) + lactateatavp + timefromshockonset_hr + 
                            age + weight + mv_baseline + akimva + sofa_baseline + totalfluidbolus + fluidbalance + 
                            gender + race + immune_suppression + database + iculocation + hydrocortuse, 
                          data = fulldfmodsepsis3, x=TRUE, y=TRUE)
        
        pred.lac <- as.data.frame(Predict(cubavptime, type = "predictions", lactateatavp, neqdoseatavp = 28.4, timefromshockonset_hr = 5.6, age=65, weight=80, akimva = "No AKI", database = "MIMIC-IV", sofa_baseline=10, totalfluidbolus = 1000, fluidbalance=8500, immune_suppression="No", iculocation = "MICU", hydrocortuse = "Yes", mv_baseline = "Yes", race = "White", gender = "Male"))
        pred.lac$prob <- logistic(pred.lac$yhat)
        pred.lac$prob.lower <- logistic(pred.lac$lower)
        pred.lac$prob.upper <- logistic(pred.lac$upper)
        
        plot <- ggplot(pred.lac, aes(x=lactateatavp)) +
          geom_line(aes(y = prob)) +
          geom_line(aes(y = prob.lower), color = "lightgreen", linewidth = 0.5) +
          geom_line(aes(y = prob.upper), color = "lightgreen", linewidth = 0.5) +
          geom_point(aes(y = prob,
                         text = paste0("Predicted Mortality: ", scales::percent(prob, accuracy = 0.1), 
                                       "<br>95% CI: ", scales::percent(prob.lower, accuracy = 0.1), 
                                       "-", scales::percent(prob.upper, accuracy = 0.1))), 
                     size = 0.1, color = "transparent") + 
          xlab('Lactate concentration at vasopressin initiation, mmol/L') + 
          ylab('Predicted incidence of in-hospital mortality, %')+
          ggtitle("Predicted Incidence of In-Hospital Mortality by Lactate") +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
          theme_light() + 
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        ggplotly(plot, tooltip = "text")
      })
      
      ## Figure 3a2: Lactate then NEQ Quantile Plot ####
      output$figure3a <- renderPlotly({
        
        quantileplotlacneq <- fulldf %>% 
          select(neqquantile, lactatequantile, inhospital_mortality) %>%
          group_by(neqquantile, lactatequantile) %>% 
          mutate(prop_mortality = mean(inhospital_mortality == "Yes")) %>% ungroup() %>%
          distinct(neqquantile,lactatequantile,.keep_all = TRUE) %>%
          select(-inhospital_mortality) %>%
          arrange(lactatequantile,neqquantile) %>%
          mutate(combined_quantiles = as.factor(paste("Lactate",lactatequantile,"& NEQ", neqquantile)))
        
        quantileplotlacneq$combined_quantiles <- factor(quantileplotlacneq$combined_quantiles, 
                                                        levels = c("Lactate <=2.5 & NEQ <=14.3",
                                                                   "Lactate <=2.5 & NEQ 14.3-26.4",
                                                                   "Lactate <=2.5 & NEQ 26.4-40.9",
                                                                   "Lactate <=2.5 & NEQ >40.9",
                                                                   "Lactate 2.5-3.8 & NEQ <=14.3",
                                                                   "Lactate 2.5-3.8 & NEQ 14.3-26.4",
                                                                   "Lactate 2.5-3.8 & NEQ 26.4-40.9",
                                                                   "Lactate 2.5-3.8 & NEQ >40.9",
                                                                   "Lactate 3.8-6.4 & NEQ <=14.3",
                                                                   "Lactate 3.8-6.4 & NEQ 14.3-26.4",
                                                                   "Lactate 3.8-6.4 & NEQ 26.4-40.9",
                                                                   "Lactate 3.8-6.4 & NEQ >40.9",
                                                                   "Lactate >6.4 & NEQ <=14.3",
                                                                   "Lactate >6.4 & NEQ 14.3-26.4",
                                                                   "Lactate >6.4 & NEQ 26.4-40.9",
                                                                   "Lactate >6.4 & NEQ >40.9"))
        
        
        plot <- ggplot(quantileplotlacneq, aes(y = combined_quantiles, x = prop_mortality, color = lactatequantile, 
                                               text = paste0("Mortality: ", scales::percent(prop_mortality), 
                                                             "<br>Quantiles: ", combined_quantiles))) +
          geom_point(size = 3) +
          geom_vline(xintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "gray") +
          scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
          labs(x = "In-hospital Mortality (%)", y = "", color = "neqatavp") +
          ggtitle("Lactate and NEQ Dose") +
          theme_bw() +
          scale_y_discrete(limits = c(rev(levels(quantileplotlacneq$combined_quantiles)))) + # adding line breaks
          theme(axis.text.x = element_text(color = "black"), # making axis labels/text black
                axis.text.y = element_text(color = "black", size = 10),
                axis.title.x = element_text(size = 14),
                plot.title = element_text(face = "bold"),
                panel.border = element_blank()) + 
          guides(color = "none") + # removing legend
          scale_colour_manual(values=c("blue", "firebrick1", "forestgreen", "darkviolet"))
        
        ggplotly(plot, tooltip = "text")
      })
      
      ## Figure 3b2: Lactate then Time Quantile Plot ####
      output$figure3b <- renderPlotly({
        
        quantileplotlactime <- fulldf %>% 
          select(timingquantile, lactatequantile, inhospital_mortality) %>%
          group_by(timingquantile, lactatequantile) %>% 
          mutate(prop_mortality = mean(inhospital_mortality == "Yes")) %>% ungroup() %>%
          distinct(timingquantile,lactatequantile,.keep_all = TRUE) %>%
          select(-inhospital_mortality) %>%
          arrange(lactatequantile,timingquantile) %>%
          mutate(combined_quantiles = as.factor(paste("Lactate",lactatequantile,"& Time", timingquantile)))
        
        quantileplotlactime$combined_quantiles <- factor(quantileplotlactime$combined_quantiles, 
                                                         levels = c("Lactate <=2.5 & Time <=2.0",
                                                                    "Lactate <=2.5 & Time 2.0-5.6",
                                                                    "Lactate <=2.5 & Time 5.6-13.8",
                                                                    "Lactate <=2.5 & Time >13.8",
                                                                    "Lactate 2.5-3.8 & Time <=2.0",
                                                                    "Lactate 2.5-3.8 & Time 2.0-5.6",
                                                                    "Lactate 2.5-3.8 & Time 5.6-13.8",
                                                                    "Lactate 2.5-3.8 & Time >13.8",
                                                                    "Lactate 3.8-6.4 & Time <=2.0",
                                                                    "Lactate 3.8-6.4 & Time 2.0-5.6",
                                                                    "Lactate 3.8-6.4 & Time 5.6-13.8",
                                                                    "Lactate 3.8-6.4 & Time >13.8",
                                                                    "Lactate >6.4 & Time <=2.0",
                                                                    "Lactate >6.4 & Time 2.0-5.6",
                                                                    "Lactate >6.4 & Time 5.6-13.8",
                                                                    "Lactate >6.4 & Time >13.8"))
        
        plot <- ggplot(quantileplotlactime, aes(y = combined_quantiles, x = prop_mortality, color = lactatequantile, 
                                                text = paste0("Mortality: ", scales::percent(prop_mortality), 
                                                              "<br>Quantiles: ", combined_quantiles))) +
          geom_point(size = 3) +
          geom_vline(xintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "gray") +
          scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
          labs(x = "In-hospital Mortality (%)", y = "", color = "neqatavp") +
          ggtitle("Lactate and Time Duration from Shock Onset") +
          theme_bw() +
          scale_y_discrete(limits = c(rev(levels(quantileplotlactime$combined_quantiles)))) + # adding line breaks
          theme(axis.text.x = element_text(color = "black"), # making axis labels/text black
                axis.text.y = element_text(color = "black", size = 10),
                axis.title.x = element_text(size = 14),
                plot.title = element_text(face = "bold"),
                panel.border = element_blank()) + 
          guides(color = "none") + # removing legend
          scale_colour_manual(values=c("blue", "firebrick1", "forestgreen", "darkviolet"))
        
        ggplotly(plot, tooltip = "text")
      })
      
      # SHOCK DURATION ####
    } else if (input$var_choice == "ShockDuration") {
      
      ## Figure 1c: Shock Duration Histogram ####
      output$figure1 <- renderPlotly({
        
        timebin_summary <- fulldf %>%
          group_by(time_bin) %>%
          summarise(count = n(), percent = (count / 1409) * 100) %>%
          ungroup()
        
        plot_data <- fulldf %>%
          group_by(time_bin) %>%
          left_join(timebin_summary, by = "time_bin")
        
        plot <- ggplot(plot_data, aes(x=time_bin)) + 
          geom_bar(aes(text = paste0("Shock Duration ", time_bin, " hours", "<br>Count: ", count, "<br>Percent: ", round(percent, 1), "%")),
                   color = "black", fill = "pink", alpha = 0.5) + 
          scale_y_continuous(breaks = seq(0, 700, by = 100), limits = c(0, 700)) + 
          xlab('Shock Duration before Vasopressin Initiation, hours') + 
          ylab('Frequency')+
          ggtitle("Shock Duration Before Vasopressin Initiation") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        ggplotly(plot, tooltip = "text")
      })
      
      ## Figure 2c: Shock Duration Cubic Splines Regression ####
      output$figure2 <- renderPlotly({
        
        fulldfmodsepsis3 <- fulldf
        fulldfmodsepsis3$inhospital_mortality <- droplevels(fulldfmodsepsis3$inhospital_mortality) # dropping unused level "Unknown"
        fulldfmodsepsis3$gender <- droplevels(fulldfmodsepsis3$gender) # dropping unused level "Unknown"
        ddist <- datadist(fulldfmodsepsis3)
        options(datadist=ddist)
        
        cubavptime <- lrm(inhospital_mortality~ rcs(neqdoseatavp,3) + lactateatavp + timefromshockonset_hr + 
                            age + weight + mv_baseline + akimva + sofa_baseline + totalfluidbolus + fluidbalance + 
                            gender + race + immune_suppression + database + iculocation + hydrocortuse, 
                          data = fulldfmodsepsis3, x=TRUE, y=TRUE)
        
        pred.time <- as.data.frame(Predict(cubavptime, type = "predictions", timefromshockonset_hr, lactateatavp=3.7, neqdoseatavp = 28.4, age=65, weight=80, akimva = "No AKI", database = "MIMIC-IV", sofa_baseline=10, totalfluidbolus = 1000, fluidbalance=8500, immune_suppression="No", iculocation = "MICU", hydrocortuse = "Yes", mv_baseline = "Yes", race = "White", gender = "Male"))
        pred.time$prob <- logistic(pred.time$yhat)
        pred.time$prob.lower <- logistic(pred.time$lower)
        pred.time$prob.upper <- logistic(pred.time$upper)
        
        plot <- ggplot(pred.time, aes(x=timefromshockonset_hr)) +
          geom_line(aes(y = prob)) +
          geom_line(aes(y = prob.lower), color = "pink", linewidth = 0.5) +
          geom_line(aes(y = prob.upper), color = "pink", linewidth = 0.5) +
          geom_point(aes(y = prob,
                         text = paste0("Predicted Mortality: ", scales::percent(prob, accuracy = 0.1), 
                                       "<br>95% CI: ", scales::percent(prob.lower, accuracy = 0.1), 
                                       "-", scales::percent(prob.upper, accuracy = 0.1))), 
                     size = 0.1, color = "transparent") + 
          xlab('Time from shock onset, hours') + 
          ylab('Predicted incidence of in-hospital mortality, %') +
          ggtitle("Predicted Incidence of In-Hospital Mortality by Shock Duration") +
          scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
          theme_light() + 
          theme(plot.title = element_text(hjust = 0.5, face = "bold"))
        ggplotly(plot, tooltip = "text")
      })
      
      ## Figure 3a3: Time then NEQ Quantile Plot ####
      output$figure3a <- renderPlotly({
        
        quantileplottimeneq <- fulldf %>% 
          select(timingquantile, neqquantile, inhospital_mortality) %>%
          group_by(timingquantile, neqquantile) %>% 
          mutate(prop_mortality = mean(inhospital_mortality == "Yes")) %>% ungroup() %>%
          distinct(timingquantile,neqquantile,.keep_all = TRUE) %>%
          select(-inhospital_mortality) %>%
          arrange(timingquantile,neqquantile) %>%
          mutate(combined_quantiles = as.factor(paste("Time",timingquantile,"& NEQ", neqquantile)))
        
        quantileplottimeneq$combined_quantiles <- factor(quantileplottimeneq$combined_quantiles, 
                                                         levels = c("Time <=2.0 & NEQ <=14.3",
                                                                    "Time <=2.0 & NEQ 14.3-26.4",
                                                                    "Time <=2.0 & NEQ 26.4-40.9",
                                                                    "Time <=2.0 & NEQ >40.9",
                                                                    "Time 2.0-5.6 & NEQ <=14.3",
                                                                    "Time 2.0-5.6 & NEQ 14.3-26.4",
                                                                    "Time 2.0-5.6 & NEQ 26.4-40.9",
                                                                    "Time 2.0-5.6 & NEQ >40.9",
                                                                    "Time 5.6-13.8 & NEQ <=14.3",
                                                                    "Time 5.6-13.8 & NEQ 14.3-26.4",
                                                                    "Time 5.6-13.8 & NEQ 26.4-40.9",
                                                                    "Time 5.6-13.8 & NEQ >40.9",
                                                                    "Time >13.8 & NEQ <=14.3",
                                                                    "Time >13.8 & NEQ 14.3-26.4",
                                                                    "Time >13.8 & NEQ 26.4-40.9",
                                                                    "Time >13.8 & NEQ >40.9"))
        
        
        
        
        plot <- ggplot(quantileplottimeneq, aes(y = combined_quantiles, x = prop_mortality, color = timingquantile, 
                                                text = paste0("Mortality: ", scales::percent(prop_mortality), 
                                                              "<br>Quantiles: ", combined_quantiles))) +
          geom_point(size = 3) +
          geom_vline(xintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "gray") +
          scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
          labs(x = "In-hospital Mortality (%)", y = "", color = "neqatavp") +
          ggtitle("Time Duration from Shock Onset and NEQ Dose") +
          theme_bw() +
          scale_y_discrete(limits = c(rev(levels(quantileplottimeneq$combined_quantiles)))) + # adding line breaks
          theme(axis.text.x = element_text(color = "black"), # making axis labels/text black
                axis.text.y = element_text(color = "black", size = 10),
                axis.title.x = element_text(size = 14),
                plot.title = element_text(face = "bold"),
                panel.border = element_blank()) + 
          guides(color = "none") + # removing legend
          scale_colour_manual(values=c("blue", "firebrick1", "forestgreen", "darkviolet"))
        
        ggplotly(plot, tooltip = "text")
      })
      
      ## Figure 3b3: Time then Lactate Quantile Plot ####
      output$figure3b <- renderPlotly({
        
        quantileplottimelac <- fulldf %>% 
          select(timingquantile, lactatequantile, inhospital_mortality) %>%
          group_by(timingquantile, lactatequantile) %>% 
          mutate(prop_mortality = mean(inhospital_mortality == "Yes")) %>% ungroup() %>%
          distinct(timingquantile,lactatequantile,.keep_all = TRUE) %>%
          select(-inhospital_mortality) %>%
          arrange(timingquantile, lactatequantile) %>%
          mutate(combined_quantiles = as.factor(paste("Time",timingquantile,"& Lactate", lactatequantile)))
        
        quantileplottimelac$combined_quantiles <- factor(quantileplottimelac$combined_quantiles, 
                                                         levels = c("Time <=2.0 & Lactate <=2.5",
                                                                    "Time <=2.0 & Lactate 2.5-3.8",
                                                                    "Time <=2.0 & Lactate 3.8-6.4",
                                                                    "Time <=2.0 & Lactate >6.4",
                                                                    "Time 2.0-5.6 & Lactate <=2.5",
                                                                    "Time 2.0-5.6 & Lactate 2.5-3.8",
                                                                    "Time 2.0-5.6 & Lactate 3.8-6.4",
                                                                    "Time 2.0-5.6 & Lactate >6.4",
                                                                    "Time 5.6-13.8 & Lactate <=2.5",
                                                                    "Time 5.6-13.8 & Lactate 2.5-3.8",
                                                                    "Time 5.6-13.8 & Lactate 3.8-6.4",
                                                                    "Time 5.6-13.8 & Lactate >6.4",
                                                                    "Time >13.8 & Lactate <=2.5",
                                                                    "Time >13.8 & Lactate 2.5-3.8",
                                                                    "Time >13.8 & Lactate 3.8-6.4",
                                                                    "Time >13.8 & Lactate >6.4"))
        
        
        plot <- ggplot(quantileplottimelac, aes(y = combined_quantiles, x = prop_mortality, color = timingquantile, 
                                                text = paste0("Mortality: ", scales::percent(prop_mortality), 
                                                              "<br>Quantiles: ", combined_quantiles))) +
          geom_point(size = 3) +
          geom_vline(xintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "gray") +
          scale_x_continuous(labels = scales::percent, breaks = seq(0, 1, by = 0.1)) +
          labs(x = "In-hospital Mortality (%)", y = "", color = "neqatavp") +
          ggtitle("Time Duration from Shock Onset and Lactate") +
          theme_bw() +
          scale_y_discrete(limits = c(rev(levels(quantileplottimelac$combined_quantiles)))) + # adding line breaks
          theme(axis.text.x = element_text(color = "black"), # making axis labels/text black
                axis.text.y = element_text(color = "black", size = 10),
                axis.title.x = element_text(size = 14),
                plot.title = element_text(face = "bold"),
                panel.border = element_blank()) + 
          guides(color = "none") + # removing legend
          scale_colour_manual(values=c("blue", "firebrick1", "forestgreen", "darkviolet"))
        
        ggplotly(plot, tooltip = "text")
      })
      
    }
  })
}

