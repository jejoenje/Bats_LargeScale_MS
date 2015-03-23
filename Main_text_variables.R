### Calculate variables for text:

lme4.v <- as.vector(unlist(sessionInfo()[[6]]$lme4[2]))
MuMIn.v <- sessionInfo()[[6]]$MuMIn$Version
arm.v<- as.vector(unlist(sessionInfo()[[6]]$arm[2]))
n_sites <- nlevels(bats_nona$SITE)
n_turb_min <- min(tapply(bats_nona$NOTURB, bats_nona$SITE, mean))
n_turb_max <- max(tapply(bats_nona$NOTURB, bats_nona$SITE, mean))
n_turb_mn <- round(mean(tapply(bats_nona$NOTURB, bats_nona$SITE, mean)),1)
date_min_13 <- min(as.Date(bats_nona$DATE, format='%Y-%m-%d')[format(as.Date(bats_nona$DATE, format='%Y-%m-%d'),'%Y')=='2013'])
date_max_13 <- max(as.Date(bats_nona$DATE, format='%Y-%m-%d')[format(as.Date(bats_nona$DATE, format='%Y-%m-%d'),'%Y')=='2013'])
date_min_14 <- min(as.Date(bats_nona$DATE, format='%Y-%m-%d')[format(as.Date(bats_nona$DATE, format='%Y-%m-%d'),'%Y')=='2014'])
date_max_14 <- max(as.Date(bats_nona$DATE, format='%Y-%m-%d')[format(as.Date(bats_nona$DATE, format='%Y-%m-%d'),'%Y')=='2014'])
date_min_13 <- format(date_min_13, '%d %B')
date_max_13 <- format(date_max_13, '%d %B %Y')
date_min_14 <- format(date_min_14, '%d %B')
date_max_14 <- format(date_max_14, '%d %B %Y')
n_pass <- sum(bats_nona$PASSES)
n_pass_pip <- sum(bats_nona$ALL_PIPS)
n_pass_pip_prop <- (n_pass_pip/n_pass)*100
n_total <- nrow(bats_nona)
n_surveys <- nlevels(bats_nona$SURVEYID)
n_sections <- nlevels(bats_nona$id)
n_surveys_p_site_min2 <- sum(tapply(bats_nona$SURVEYID, bats_nona$SITE, function(x) length(unique(x)))>=2)
n_surveys_p_site_1 <- sum(tapply(bats_nona$SURVEYID, bats_nona$SITE, function(x) length(unique(x)))<2)
n_turb_single <- table(tapply(bats_nona$TURB, bats_nona$SITE, unique))[[1]]
n_turb_multiple <- table(tapply(bats_nona$TURB, bats_nona$SITE, unique))[[2]]