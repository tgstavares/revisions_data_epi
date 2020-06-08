set term postscript eps enhanced color round size 20cm,11cm #font 13
set output "fig_mx_en.eps"
#set term dumb size 235,55

set datafile separator ','

dat1mx = "../ModelEstimationMX/data/exportfinal_002.csv"
dat11mx= "<awk -F: '{if($1 <= 66){print $0}}' ../ModelEstimationMX/data/exportfinal_002.csv"
dat2mx = "../ModelEstimationMX/data/exportfinal_132.csv"
dat22mx= "<awk -F: '{if($1 <= 66){print $0}}' ../ModelEstimationMX/data/exportfinal_132.csv"

dat1en = "../ModelEstimationEN/data/exportfinal_002.csv"
dat11en= "<awk -F: '{if($1 <= 83){print $0}}' ../ModelEstimationEN/data/exportfinal_002.csv"
dat2en = "../ModelEstimationEN/data/exportfinal_132.csv"
dat22en= "<awk -F: '{if($1 <= 83){print $0}}' ../ModelEstimationEN/data/exportfinal_132.csv"

set multiplot layout 2,2

set xtics in scale 0.00001
set ytics in
set xtics nomirror out

#### Mexico

set xdata time
set timefmt "%Y-%m-%d"
#set xtics format "%b %d"
set key t l Left reverse
#set logscale y
set xrange ["2020-03-28":"2020-07-18"]
set yrange[0:30000]
set title "Cumulative deaths by SARs-CoV-2 in Mexico"
plot dat1mx index 0 u 4:($1<=66  ? $5 : 1/0) pt 65 ps 1.0 lw 3 lc 'plum' title "total deaths (by date reported)", \
     dat2mx index 0 u 4:($1<=66  ? $6 : 1/0) pt 68 ps 1.0 lw 3 lc 'dark-green' title "total deaths (by date occurred)", \
     dat1mx index 0 u 4:($1<=160 ? $3 : 1/0) w l lw 5 lc 'plum' title "total deaths model (by date reported)", \
     dat2mx index 0 u 4:($1<=160 ? $3 : 1/0) w l lw 5 lc 'dark-green' dt (2,2) title "total deaths model (by date occurred)", \

unset logscale
array Z[200]
do for [i=1:200] { Z[i] = NaN }
diff(i, x) = (y = x - Z[i], Z[i] = x, y)
set key t r Right noreverse
set xdata
set xrange [24:160]
set yrange[0:1000]
set title "Daily deaths by SARs-CoV-2 in Mexico (days after the begining of epidemics)"
plot dat11mx index 0 u ($1+0.2):(diff(1,$5)) with impulses lw 2.0 lc 'plum' title "daily deaths (by date reported)", \
     dat22mx index 0 u ($1-0.2):(diff(1,$6)) with impulses lw 2.0 lc 'dark-green' title "daily deaths (by date occurred)", \
     dat1mx index 0 u 1:($1<=160 ? diff(1,$3) : NaN) w l lw 5 lc 'plum' title "daily deaths model (by date reported)", \
     dat2mx index 0 u 1:($1<=160 ? diff(1,$3) : NaN) w l lw 5 lc 'dark-green' dt (2,2) title "daily deaths model (by date occurred)"


#### England

set xdata time
set timefmt "%Y-%m-%d"
#set xtics format "%b %d"
set key b r Left reverse
#set logscale y
set xrange ["2020-03-28":"2020-07-18"]
set yrange[0:30000]
set title "Cumulative deaths by SARs-CoV-2 in England"
plot dat1en index 0 u 4:($1<=83  ? $5 : 1/0) pt 65 ps 1.0 lw 3 lc 'plum' notitle "total deaths (by date reported)", \
     dat2en index 0 u 4:($1<=83  ? $6 : 1/0) pt 68 ps 1.0 lw 3 lc 'dark-green' notitle "total deaths (by date occurred)", \
     dat1en index 0 u 4:($1<=160 ? $3 : 1/0) w l lw 5 lc 'plum' notitle "total deaths model (by date reported)", \
     dat2en index 0 u 4:($1<=160 ? $3 : 1/0) w l lw 5 lc 'dark-green' dt (2,2) notitle "total deaths model (by date occurred)"

unset logscale
array Z[200]
do for [i=1:200] { Z[i] = NaN }
diff(i, x) = (y = x - Z[i], Z[i] = x, y)
set key t r Right noreverse
set xdata
set xrange [29:160]
set yrange[0:1000]
set title "Daily deaths by SARs-CoV-2 in England (days after the begining of epidemics)"
plot dat11en index 0 u ($1+0.2):(diff(1,$5)) with impulses lw 2.0 lc 'plum' notitle "daily deaths (by date reported)", \
     dat22en index 0 u ($1-0.2):(diff(1,$6)) with impulses lw 2.0 lc 'dark-green' notitle "daily deaths (by date occurred)", \
     dat1en index 0 u 1:($1<=160 ? diff(1,$3) : NaN) w l lw 5 lc 'plum' notitle "daily deaths model (by date reported)", \
     dat2en index 0 u 1:($1<=160 ? diff(1,$3) : NaN) w l lw 5 lc 'dark-green' dt (2,2) notitle "daily deaths model (by date occurred)"


unset multiplot
