require(MASS)
set.seed(1)
n = 500
mu1 = 0
mu2 = 0
mu3 = 0
sigma1 = 1
sigma2 = 1
sigma3 = 1

#covariance matrix
ssigma = matrix(c(sigma1^2,0,0,0,sigma2^2,0,
                 0,0,sigma3^2),ncol=3,nrow=3,byrow = T)

#generate Z = (Z1, Z2, Z3)
Z = mvrnorm(n, mu = c(mu1, mu2, mu3),Sigma = ssigma)

# storing and rounding the simulated values in three variables
Z1 = Z[,1]
Z2 = Z[,2]
Z3 = Z[,3]

Y1 = 1 + Z1
Y2 = 5 + 2 * Z1 + Z2

#Question 3a
Y2_3a_miss = Y2[2*(Y1-1)+Z3<0]

Y2_3a_obs = Y2[2*(Y1-1)+Z3>=0]


plot(density(Y2), lwd = 2, col = "blue", main = "Question 3a:MAR", ylim = c(0,0.25))
lines(density(Y2_3a_obs), lwd = 2, col = "red")
lines(density(Y2_3a_miss), lwd = 2, col = "darkgreen")
legend(x = 8,y= 0.25,legend = c("Complete data", "Observed data", "Missing data"),
       col = c("blue", "red", "darkgreen"), lty = c(1, 1, 1), lwd = c(2, 2, 2))

#Question 3b
Y2_3b = Y2
Y2_3b[2*(Y1-1)+Z3<0]= NA
data_3b = data.frame(Y1,Y2_3b)
fit = lm(formula = Y2_3b ~ Y1, data = data_3b)
summary(fit)
fit$coefficients

set.seed(1)
predicted_sri = predict(fit,newdata = data_3b)+ rnorm(nrow(data_3b),0,sigma(fit))
Y2_3b_sri = ifelse(is.na(data_3b$Y2_3b),predicted_sri, data_3b$Y2_3b)

plot(density(data_3b$Y2_3b[-which(is.na(data_3b$Y2_3b)==TRUE)]), col = "blue",
     main = "Question:3b",xlab = "Y2")
lines(density(Y2_3b_sri),col="red")
legend(8,0.2,legend = c("Observed","Completed"),
       col = c("blue","red"),lty = c(1,1),bty = "n")

#Question 3c
Y2_3c_miss = Y2[2*(Y2-5)+Z3<0]

Y2_3c_obs = Y2[2*(Y2-5)+Z3>=0]

plot(density(Y2), lwd = 2, col = "blue", main = "Question 3c:MNAR", ylim = c(0,0.3))
lines(density(Y2_3c_obs), lwd = 2, col = "red")
lines(density(Y2_3c_miss), lwd = 2, col = "darkgreen")
legend(x = 8,y= 0.25,legend = c("Complete data", "Observed data", "Missing data"),
       col = c("blue", "red", "darkgreen"), lty = c(1, 1, 1), lwd = c(2, 2, 2))


#Question 3d
Y2_3d = Y2
Y2_3d[2*(Y2-5)+Z3<0]= NA
data_3d = data.frame(Y1,Y2_3d)
fit_3d = lm(formula = Y2_3d ~ Y1, data = data_3d)
summary(fit_3d)
fit_3d$coefficients

set.seed(1)
predicted_sri_3d = predict(fit_3d,newdata = data_3d)+ rnorm(nrow(data_3d),0,sigma(fit_3d))
Y2_3d_sri = ifelse(is.na(data_3d$Y2_3d),predicted_sri_3d, data_3d$Y2_3d)

plot(density(data_3d$Y2_3d[-which(is.na(data_3d$Y2_3d)==TRUE)]), col = "blue",
     main = "Question:3d",xlab = "Y2")
lines(density(Y2_3d_sri),col="red")
legend(8,0.2,legend = c("Observed","Completed"),
       col = c("blue","red"),lty = c(1,1),bty = "n")

#Question:4a
mean=mean(databp$recovtime, na.rm = TRUE)
standard_deviation_4a = sd(databp$recovtime, na.rm = TRUE)
sum(!is.na(databp$recovtime))
se4a = standard_deviation_4a/sqrt(sum(!is.na(databp$recovtime)))
se4a
cov4a = cov(databp,use="complete")
cov4a
cov4a_recoverytime_dose = cov4a[1,3]
cov4a_recoverytime_bloodpressure = cov4a[3,2]

#Question:4b
databp_b = databp
recovtime_mi = ifelse(is.na(databp_b$recovtime),mean,databp_b$recovtime)
databp_b$recovtime = recovtime_mi


mean_4b=mean(databp_b$recovtime)
standard_deviation_4b = sd(databp_b$recovtime)

se4b = standard_deviation_4b/sqrt(sum(!is.na(databp_b$recovtime)))
se4b
cov4b = cov(databp_b)
cov4b
cov4b_recoverytime_dose = cov4b[1,3]
cov4b_recoverytime_bloodpressure = cov4b[3,2]
#Question:4c
databp_c = databp
fit_4c = lm(formula=recovtime~logdose+bloodp, data = databp_c)
summary(fit_4c)
fit_4c$coefficients
predicted_ri = predict(fit_4c,newdata = databp_c)
recovtime_ri = ifelse(is.na(databp_c$recovtime),predicted_ri,databp_c$recovtime)
databp_c$recovtime = recovtime_ri
mean_4c = mean(recovtime_ri)
se_4c = sd(databp_c$recovtime)/sqrt(sum(!is.na(databp_c$recovtime)))
cov_4c = cov(databp_c)
cor_recovtime_dose_4c = cov_4c[1,3]
cor_recovtime_bloodp_4c = cov_4c[3,2]
#Question:4d
databp_4d = databp
predicted_sri_4c = predict(fit_4c,newdata = databp_4d)+
  rnorm(nrow(databp_4d),0,sigma(fit_4c))
recovtime_sri = ifelse(is.na(databp_4d$recovtime),
                       predicted_sri_4c,databp_4d$recovtime)
databp_4d$recovtime=recovtime_sri
mean_4d = mean(recovtime_sri)
se_4d = sd(databp_4d$recovtime)/sqrt(sum(!is.na(databp_4d$recovtime)))
cov_4d = cov(databp_4d)
cor_recovtime_dose_4d = cov_4d[1,3]
cor_recovtime_bloodp_4d = cov_4d[3,2]

#Question:4e
donars = which(is.na(databp$recovtime)==FALSE)
missings = which(is.na(databp$recovtime)==TRUE)
recovtime_ri[missings]
donar_values = databp$recovtime[donars]
donar_4 = donar_values[which.min((recovtime_ri[missings][1]-donar_values)**2)]
donar_10 = donar_values[which.min((recovtime_ri[missings][2]-donar_values)**2)]
donar_22 = donar_values[which.min((recovtime_ri[missings][3]-donar_values)**2)]
databp_4e = databp
databp_4e$recovtime[missings] = c(donar_4,donar_10,donar_22)
databp_4e
mean_4e = mean(databp_4e$recovtime)
se_4e = sd(databp_4e$recovtime)/sqrt(sum(!is.na(databp_4e$recovtime)))
cov_4e = cov(databp_4e)
cor_recovtime_dose_4e = cov_4e[1,3]
cor_recovtime_bloodp_4e = cov_4e[3,2]
