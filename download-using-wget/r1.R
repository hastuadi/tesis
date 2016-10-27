# download using wget
#system('wget http://www.ogimet.com/display_synops2.php?lang=en&lugar=96745&tipo=ALL&ord=REV&nil=SI&fmt=txt&ano=2016&mes=07&day=01&hora=00&anof=2016&mesf=07&dayf=11&horaf=00&send=send')

rangex1 = 15.7 * (10^4)
rangex2 = 5.7 * (10^4)
rangex3 = 15.7 * (10^4)

pangkat = c(1:20)

bit = 2 ^ pangkat

b1 = which(bit > rangex1)
b1 = b1[1]

b2 = which(bit > rangex2)
b2 = b2[1]

b3 = which(bit > rangex3)
b3 = b3[1]

total = b1 + b2 + b3
