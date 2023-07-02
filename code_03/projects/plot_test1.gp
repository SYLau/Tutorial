#set term wxt size 640, 480 font "12"

set xlabel "time"
set ylabel "soln"

plot "proj_test1/result_SHM.dat" using 1:2 title "x(t)" w l lc 8 \
, "proj_test1/result_SHM.dat" using 1:3 title "v(t)" w l lc 7