T_K  <- 273.15     # Kelvin
Temp <- seq(from = 0, to = 35, by = 0.01) + T_K; # Kelvin
T_A  <- 10000 # K Arrhenius temperature

# Temperatura de referencia 1
T_ref1 <- 20 + T_K
c_T1   <- exp(T_A / T_ref1 - T_A / Temp) # Arrhenius factor
DEB_var1 <- 0.002 # Variable DEB
DEB_var1_T <- DEB_var1 * c_T1

# Temperatura de referencia 2
T_ref2 <- 16 + T_K

DEB_var2 <- DEB_var1 * exp(T_A / T_ref1 - T_A / T_ref2)
c_T2 <- exp(T_A / T_ref2 - T_A / Temp)
DEB_var2_T <- DEB_var2 * c_T2 # Arrhenius factor

plot(Temp - T_K, DEB_var1_T, type = 'l', xlab = 'Temperature', ylab = 'DEB var value')
lines(Temp - T_K, DEB_var2_T, col = 'red')

a <- DEB_var1_T[which(Temp - T_K == T_ref1 - T_K)]
b <- DEB_var2_T[which(Temp - T_K == T_ref2 - T_K)]

print(paste('Temperatura de Referencia Inicial =', T_ref1-T_K, 'ºC'))
print(paste('Temperatura de Referencia Final =', T_ref2-T_K, 'ºC'))
print(paste('Valor de la Variable DEB =', a, 'a temperatura de referencia', T_ref1-T_K, 'ºC')) 
print(paste('Valor de la Variable DEB =', b, 'a temperatura de referencia', T_ref2-T_K, 'ºC')) 



