#!/bin/bash

#DIRHOME=/Users/saulo.freitas/work/models/GF_standalone_SRF/
DIRHOME=/home/sfreitas/models/GF_standalone_SRF

#DIRHOME=$PWD
SCRIPTS=${DIRHOME}/scripts
DATAOUT=${DIRHOME}/dataout
DATAIN=${DIRHOME}/datain
SRC=${DIRHOME}/src
BIN=${DIRHOME}/bin
#echo $BIN; exit

#---------------------------create the executable
#rm -f gf.x
# Verificando o argumento de entrada
COMPILER=${1:-"gnu"}
if [ -z "${1}" ]
then
  echo "Compiler is not set: gnu or intel or pgi"
  echo "$COMPILER is set by default" 
fi
  
echo "COMPILER=$COMPILER"

cd ${BIN}
#/bin/rm gf.x
make clean
/bin/cp Makefile_3D Makefile
echo "Compilando"
#comando="make clean; make $COMPILER"
comando="make $COMPILER"
echo $comando; eval $comando

#mk_${1}
#
#rm -f ref_${1}.gra
#
#---------------------------create gf.inp namelist
cat << Eof1 > ${DATAIN}/gf.inp

 &run
  runname   = "closW_on_KI2",  
  runlabel  = "ref",  
  version   =  4,  ! v=1 GATE , VERSION =4 GEOS5
  KLEV_SOUND = 91,
  
 !rundata    = "ocea.dat",
 !rundata    = "land.dat",
 !rundata    = "mid_land.dat",
  rundata    = "GATE.dat",
   land      =.FALSE.,
 
 &end
Eof1

#---------------------------create GF namelist
cat << Eof0 > ${DATAIN}/GF_ConvPar_nml
&GF_NML  
  output_sound     = 2,
  icumulus_gf      = 1,1,1, != trimodal plume (deep ,shallow ,congestus)
  
  closure_choice   = 10,10,3, != closure for the mass flux at the cloud base
  
  cum_entr_rate    =6.3e-4, 1.e-3, 5.e-4, != initial gross entrainment rate for 
                                          != deep, shallow, congestus
  
  dicycle          = 1,            != 0/1/2:  diurnal cycle closure, default = 1
                                   != 2 adds Qadv closure (Becker et al 2021) 
  cum_t_star = 4., -99., -99., != scale temperature for 
                                   !diurnal cycle closure, 
  rh_dicycle       = 0,            != 0/1: controls of RH on the diurnal cycle (see Tian et al 2022 GRL) 
                                   ! default = 0

  use_scale_dep    = 1,     != 0/1: turn ON/OFF the scale dependence approach
  sig_factor       = 0.22,  != exponential factor for the sigma determination (orig = 0.1)

  convection_tracer = 1,
  add_coldpool_prop = 3,
  add_coldpool_clos = 1,
  add_coldpool_trig = 2,
  add_coldpool_diff = 3,

  tau_ocea_cp       =7200.,
  tau_land_cp       =7200.,
  mx_buoy1          = 250.5,   ! J/kg
  mx_buoy2          = 20004.0, ! J/kg
  use_memory        = 22,
  use_gustiness     = 0,

  sgs_w_timescale  = 1,     != 0/1: uses vertical velocity for determination of tau_ecmwf
  tau_deep         = 3600., != timescales for instability removal, only for sgs_w_timescale = 0
  tau_mid          =1200., 

  moist_trigger    = 0,     != 0/1: relative humidity effects on the cap_max trigger function
  adv_trigger      = 0,     != 0/1/3:  1 => Kain (2004), 3 => dcape trigger  Xie et al (2019)
  dcape_threshold  = 60.,   != CAPE time rate threshold for triggering convection (adv_trigger = 3)
                            != typical range is [-200,200] J/kg/hour

  lcl_trigger      = 0,     != only for shallow: lcl_trigger > 0 activates the LCL trigger which  
                            != requires the lcl height be lower than the pbl height. 0 turn it off.

  cap_maxs         = 50.,   != max- distance (hPa) the air parcel is allowed to go up looking for the LFC
!---
!--- controls rainfall evaporation
  use_rebcb            = 1, != 0/1: turn ON/OFF rainfall evap below cloud base

  cum_MAX_EDT_LAND     = 0.9, 0.0, 0.2,   !-(deep ,shallow ,congestus)
  cum_MAX_EDT_OCEAN      = 0.9, 0.0, 0.2,
!----

!---- boundary condition specification
  cum_use_excess       =2,1,1,
  cum_ave_layer        = 50.,25.,25.,!- (deep ,shallow ,congestus)
!----

!---- for mass flux profiles - (deep ,shallow ,congestus)
  cum_HEI_UPDF_LAND    = 0.55, 0.1, 0.55,  != height of maximum Z_updraft
  cum_HEI_UPDF_OCEAN   = 0.55, 0.1, 0.55,

  cum_HEI_DOWN_LAND    = 0.40, 0.0, 0.35,  != height of maximum Z_downdraft 
  cum_HEI_DOWN_OCEAN   = 0.35, 0.0, 0.35, 

  use_random_num      = 0., != stochastic pertubation for the height of maximum Zu
  use_smooth_prof     = 1,  != 1 makes the normalized mass flux, entr and detraiment profiles smoother
  
  use_linear_subcl_mf = 1,  !-- for shallow convection only
  beta_sh             = 2.2,!-- for shallow convection only
!----

!---- the 'cloud microphysics'
  autoconv        = 4,
  qrc_crit        =6.0e-4,

   c0_deep         = 1.0e-3,
   c0_shal         = 0.0e-3,
   c0_mid          = 1.5e-3, 
   n_cldrop        = 50.,

!   c0_deep    = 2.e-3,
!   c0_mid     = 2.e-3, 
!   c0_shal    = 0.,
!----

!--- for momentum transport
  use_momentum_transp   = 1,
  lambau_deep      = 0.0,
  lambau_shdn      = 2.0,
!----

!--- for tracer transport
  use_tracer_transp = 0,
  use_tracer_scaven = 0,
  use_tracer_evap   = 0,
  apply_sub_mp      = 0,
  use_flux_form     = 1,
  use_fct           = 1,
  alp1              = 1, 
!----
  
!---- couplings w/ other parameterizations
  lightning_diag      = 0,
  liq_ice_number_conc = 0,
!----

!--- do not change below
  downdraft             = 1,
  use_smooth_tend       = 1, 
  use_cloud_dissipation = 0., 
  frac_modis            = 1, 
  overshoot             = 0,
  use_wetbulb           = 0,
  vert_discr            = 1, 
  clev_grid             = 1, 
  max_tq_tend        = 500., 
!-----

&end!-----

Eof0
#--



#-----------------------------run GF standalone
cd ${DATAIN}
/bin/rm gf.x
/bin/cp $BIN/gf.x .
./gf.x > gf.out

exit 

ls -ltr *ctl

exit
echo "compare --------"
cmp ${DATAOUT}/ref_$i.gra ${DIRHOME}/refs/ref_g.gra
)





