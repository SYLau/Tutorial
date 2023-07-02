#set term wxt size 640, 480 font "12"

set xlabel "time"
set ylabel "soln"

pos(x) = 20*x - 5*x**2
vel(x) = 20-10*x

plot "proj_test2/result_parabola.dat" using 1:2 title "x(t)" w p pt 5 lc 8 \
, "proj_test2/result_parabola.dat" using 1:3 title "v(t)" w p pt 6 lc 7 \
, pos(x) title "x(t) Analytic" w l dt 3 lc 8 \
, vel(x) title "v(t) Analytic" w l dt 3 lc 7