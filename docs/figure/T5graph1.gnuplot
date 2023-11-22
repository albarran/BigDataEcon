#set term x11 enhanced
#
#set xrange [-3:3]
#set yrange [0:1]
#
#set dummy z
#set xlabel 'z'
#
##set xtics 0
#set ytics 0.5
#set grid
#
##plot cos(x) with linespoints title 'cos', \
##     sin(x) with lines       title 'sin'
##G(x)=exp(x)/(1+exp(x))
##plot G(x)
#plot exp(z)/(1+exp(z))

###################################################################
reset

#set terminal svg font "Bitstream Vera Sans,18" size 600,400
#set output "Logistic-curve.svg"
#set term x11 enhanced
set term png
set output "T5clogistic.png"

set xrange [-4:4]
set yrange [-0.01:1.01]
set xzeroaxis linetype -1
set yzeroaxis linetype -1
set xtics axis nomirror
#set ytics axis nomirror 0,0.5,1
set ytics  0,0.5,1
set key off
#set key top left
#set label 1 "G(z)= exp(z)/(1 + exp(z))" at -4,0.95 left  font "Helvetica,20"
#set label 1 "G(z)= exp(z)/(1 + exp(z))" at -4,0.95 left  font "Vera,18"
set label 1 "G(z)= exp(z)/(1 + exp(z))" at -4,0.95 left 
set label 2 "z" at 4.2,-.01 left
#set xlabel "z"
set grid ytics
set border 0

set samples 400

set dummy z
plot exp(z)/(1 + exp(z)) with line linetype rgbcolor "red" linewidth 2 #title "G(z)= exp(z)/(1 + exp(z))"