'reinit'
'open ref/gf_dataOut-9000.ctl'
'open   ./gf_dataOut-9000.ctl'

dummy = colors2()
'set display white'
'clear'

'set lon -56.6'
'set lat -9.'
'set z 1 60'

'set zlog off'
'set xlopts 1 8 0.14'
'set annot 1 8'
'set ylopts 1 8 0.14'
'set grads off'
'set cmark 0'
"set lwid 13 4";"set cthick 10"

icx=1
'set ccolor 'icx''
'd zup1'

icx=2
'set ccolor 'icx''
'set cmark 0'

'd zup1.2'

return

******************************************************
function getinfo(is,loc,prem)
"q file "is""
 name=sublin(result,1)
* name=subwrd(name,4)
 
"set strsiz 0.16 0.17"
"set string "is+2" l 7 0"
"draw string 2.5 "loc" "name" mean="prem" mm/h"
return
******************************************************
function getinfo2(is,loc)
"q file "is""
 name=sublin(result,1)
* name=subwrd(name,4)
 
"set strsiz 0.16 0.17"
"set string "is+2" l 7 0"
"draw string 3. "loc" "name""
return


*'gxyat -y 2000 -x 3000 fig.png'
******************************************
function colors
*karla
'set rgb  16    0    0   74'
'set rgb  17    0    0   90'
'set rgb  18    0    0  122'
'set rgb  19    0    0  150'
'set rgb  20    0    0  175'
'set rgb  21    0    0  200'
'set rgb  22    0    0  225'
'set rgb  23    0   10  255'
'set rgb  24    0   91  255'
'set rgb  25    0  120  255'
'set rgb  26  100  150  255'
'set rgb  27  120  160  255'
'set rgb  28  162  189  255'


'set rgb  30    0   80    0'
'set rgb  31    0  100    0'
'set rgb  32    0  116    0'
'set rgb  33    0  136    0'
'set rgb  34    0  150    0'
'set rgb  35    0  174    0'
'set rgb  36    0  200    0'
'set rgb  37    0  220    0'
'set rgb  38    0  255    0'

'set rgb  58  30     0    0'
'set rgb  59  60     0    0'
'set rgb  60  90     0    0'
'set rgb  61  124    0    0'
'set rgb  62  180    0    0'
'set rgb  63  200    0    0'
'set rgb  64  220   10    0'
'set rgb  65  255   20   0'
'set rgb  66  255   45   15'
'set rgb  68  255   80   30'
'set rgb  75  255  110   60'
'set rgb  77  255  150   75'
'set rgb  78  255  180   99'
'set rgb  79  255  210   99'
'set rgb  80  255  255   80'

return
******************************************
function colors2
*karla
*light yellow to dark red
'set rgb 21 255 250 170'
'set rgb 24 255 160   0'
'set rgb 25 255  96   0'
'set rgb 26 255  50   0'
'set rgb 27 225  20   0'
'set rgb 28 192   0   0'
'set rgb 29 165   0   0'
*
*light green to dark green
'set rgb 31 230 255 225'
'set rgb 32 200 255 190'
'set rgb 33 180 250 170'
'set rgb 34 150 245 140'
'set rgb 35 120 245 115'
'set rgb 36  80 240  80'
'set rgb 37  55 210  60'
'set rgb 38  30 180  30'
'set rgb 39  15 160  15'
*set rgb 39   5 150   5
*
*light blue to dark blue
'set rgb 41 225 255 255'
'set rgb 42 180 240 250'
'set rgb 43 150 210 250'
'set rgb 44 120 185 250'
'set rgb 45  80 165 245'
'set rgb 46  60 150 245'
'set rgb 47  40 130 240'
'set rgb 48  30 110 235'
'set rgb 49  20 100 210'

'set rgb  16  0    0  255 80 '
'set rgb  17  55   55  255 '
'set rgb  18  0    160  0   '
'set rgb  19  165  165  255 '
'set rgb  20  244  223  66 '
'set rgb  21  0    100  255 '
'set rgb  22  200  0 0 '
'set rgb  23  34   180 34 '
'set rgb  24  255  0 0 '
'set rgb  25  0    200  255 '
'set rgb  26  34   255 34 '
'set rgb  27  66   244 212 '
'set rgb  28  101 66 244 '
'set rgb  29  135 132 59 '
'set rgb  30 119 115 115'
*brown
'set rgb  50  142 72 72'
*brown2
'set rgb  51  200 168 137'
*green clear
'set rgb  52  102 255 51'
*blue clear
'set rgb  54  102 179 255'
'set rgb 56 204 204 51'
'set rgb 58 200 137 200'

return
