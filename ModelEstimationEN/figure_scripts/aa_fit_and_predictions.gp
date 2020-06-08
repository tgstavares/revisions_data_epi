set term postscript eps enhanced color round size 20cm,11cm #font 13
set output "figure_scripts/aa_fit_and_predictions.eps"
#set term dumb size 235,55

set datafile separator ','

dat1 = "data/exportfinal_002.csv"
dat11= "<awk -F: '{if($1 <= 68){print $0}}' data/exportfinal_002.csv"
dat2 = "data/exportfinal_132.csv"
dat22= "<awk -F: '{if($1 <= 68){print $0}}' data/exportfinal_132.csv"

set multiplot layout 2,2

set xtics in scale 0.00001
set ytics in
set xtics nomirror out
set xdata time
set timefmt "%Y-%m-%d"
#set xtics format "%b %d"

set key b r Left reverse
#set logscale y
set xrange ["2020-03-20":"2020-08-18"]
set title "Cumulative deaths by SARs-CoV-2 in NHS England"

# plot dat2 index 0 u 4:($1<=68  ? $6 : 1/0) pt 68 ps 1.0 lw 3 lc 'dark-green' title "total deaths (data with rev)", \
#      dat2 index 0 u 4:($1<=160 ? $3 : 1/0) w l lw 5 lc 'dark-green' dt (2,2) title "total deaths (model with rev)"

plot dat1 index 0 u 4:($1<=68  ? $5 : 1/0) pt 65 ps 1.0 lw 3 lc 'pink' title "total deaths (data no rev)", \
     dat1 index 0 u 4:($1<=160 ? $3 : 1/0) w l lw 5 lc 'pink' title "total deaths (model no rev)", \
     dat2 index 0 u 4:($1<=68  ? $6 : 1/0) pt 68 ps 1.0 lw 3 lc 'dark-green' title "total deaths (data with rev)", \
     dat2 index 0 u 4:($1<=160 ? $3 : 1/0) w l lw 5 lc 'dark-green' dt (2,2) title "total deaths (model with rev)"


unset logscale
array Z[200]
do for [i=1:200] { Z[i] = NaN }
diff(i, x) = (y = x - Z[i], Z[i] = x, y)
set key t r Right noreverse
set xdata
set xrange [29:160]
set title "Daily deaths by SARs-CoV-2 in NHS England (days after the begining of epidemics)"

# plot dat22 index 0 u ($1-0.2):(diff(1,$6)) with impulses lw 2.0 lc 'dark-green' dt (1,1) title "daily deaths (data with rev)", \
#      dat2 index 0 u 1:($1<=160 ? diff(1,$3) : NaN) w l lw 5 lc 'dark-green' dt (2,2) title "daily deaths (model with rev)"

plot dat11 index 0 u ($1+0.2):(diff(1,$5)) with impulses lw 2.0 lc 'pink' title "daily deaths (data no rev)", \
     dat1 index 0 u 1:($1<=160 ? diff(1,$3) : NaN) w l lw 5 lc 'pink' title "daily deaths (model no rev)", \
     dat22 index 0 u ($1-0.2):(diff(1,$6)) with impulses lw 2.0 lc 'dark-green' dt (1,1) title "daily deaths (data with rev)", \
     dat2 index 0 u 1:($1<=160 ? diff(1,$3) : NaN) w l lw 5 lc 'dark-green' dt (2,2) title "daily deaths (model with rev)"


unset multiplot
