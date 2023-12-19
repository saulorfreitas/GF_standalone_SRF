'reinit'
'open CPoff_pgi.ctl'
*'open CPUM0_pgi.ctl'
*'open CPUM2_pgi.ctl'
'open CPUM22_pgi.ctl'
'open CPUM22_F_pgi.ctl'
'open CPUM22_Fx_pgi.ctl'
'open CPUM22_yy_pgi.ctl'




'define x1=hc1-heso1'
'define x2=hc1.2-heso1.2'
'define x3=hc1.3-heso1.3'
'define x4=hc1.4-heso1.4'
'define x5=hc1.5-heso1.5'
*'q pos'
'clear'
'set z 1'
'set axlim 0 5'
'd precip1'
'd precip1.2'
'd precip1.3'
'd precip1.4'
'set ccolor 9'

'd precip1.5'

'q pos'
'reset'
'set lat 90'
'd zup1'
'd zup1.2'
'd zup1.3'
'd zup1.4'
'set ccolor 9'
'd zup1.5'



return
*'set lat 30 80'
'set cint 0.2'
'd x1'
'set cint 0.2'
'd x2'
'set cint 0.2'
'd x3'
'set cint 0.2'
'd x4'
'set cint 0.2'
'd x5'


return

