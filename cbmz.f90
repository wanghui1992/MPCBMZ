!!*********************************************************************
!!*** revised by feng fan, feb,11th,2015 
!!*** mbe solver, 76 species
!!*** modified backward euler (mbe)
!!*********************************************************************

      subroutine  cbmz(cppb,lprint,factcld)
      implicit real(a-h,o-z), integer(i-n) 
      include 'chm1.inc'
      include 'gas1.inc'

      dimension cppb(ngas_max)
      logical lprint


      
      
      
      call setrunparameters  !!!set tbeg_sec, tcur_sec and trun_sec 

      call setaircomposition

      call loadperoxyparameters	! aperox and bperox  

      call domassbalance	! initial elemental mass balance  




      dt_sec= min_stepsize   !!!!!! notice this, by feng fan
      tbeg_plus_trun_sec=tbeg_sec+trun_sec
      tpnt_sec = tcur_sec
      !if (lprint .eq. .true. ) &
      !call printresult(cppb)                       





       it = 1


!!------------------------------------------------------------------

!!----------------- main time-loop begins...  ----------------------



     do while (tcur_sec<tbeg_plus_trun_sec)

       it = it+1 

       call updatetime

      if(msolar.eq.1)then
       call solarzenithangle
      endif

      !call updatemetfields     !at present, "call updatemetfields" is of no use.

      !call updateemissions     !at present, "call updateemissions" is of no use.


      call integratechemistry(factcld)   !!! solver is in this


      call domassbalance       

      !if ( lprint .eq. .true. .and. (tcur_sec-tpnt_sec)>=300) then 
      !call printresult(cppb)
      !tpnt_sec = tcur_sec
      !endif

 

    end do






  return
  end            







!!**********************************************************************
!!*********************  functions *************************************


!!------------------------------------------------------
!!------------------------------------------------------

      function arr(aa,bb)
      include 'chm1.inc'	! for te
      arr = aa*exp(bb/te)
      return
      end


!!------------------------------------------------------
!!------------------------------------------------------
	integer function nbllen( str )

!!   returns the position of the last non-blank character in str

	character*(*) str

	j = len(str)

	if (j .gt. 0) then
1000	    if (str(j:j) .eq. ' ') then
		j = j - 1
		if (j .gt. 0) goto 1000
	    end if
	end if
	nbllen = max0( j, 0 )

	return
	end

!!------------------------------------------------------
!!------------------------------------------------------
      function troe(cair_mlc,te,rk0,rnn,rki,rmm)
      rk0 = rk0*cair_mlc*(te/300.)**(-rnn)
      rki = rki*(te/300.)**(-rmm)
      expo= 1./(1. + (alog10(rk0/rki))**2)
      troe  = (rk0*rki/(rk0+rki))*exp(expo*alog(.6))
      return
      end


!!------------------------------------------------------
!!------------------------------------------------------
!!--  function fuchs(kn, a)   !!!(used to compute rk_  )!!!!

      function fuchs(kn, a)
      real kn

      fuchs = 0.75*a*(1. + kn)/(kn**2 + kn + 0.283*kn*a + 0.75*a)

      return
      end

!!********************************************************************
!! function watervapor
!! purpose: calculates concentration of h2o using the method given in 
!!          seinfeld's book, pg. 181
!!---------------------------------------------------------------------

      function watervapor(rh, cair_mlc, te, pr_atm)

      t_steam = 373.15			! steam temperature  [k]
      pr_std   = 1.0			! standard pressure  [atm]

      a      = 1.0 - t_steam/te
      arg    = (((-.1299*a -.6445)*a -1.976)*a +13.3185)*a
      pr_h2o = pr_std*exp(arg)  			! [atm]
      watervapor = rh*(pr_h2o/pr_atm)*cair_mlc/100.	! [molec/cc]

      return
      end







!!**********************************************************************
!!*********************  subroutines ***********************************


      subroutine readinputfile(lin)
      include 'chm1.inc'
      include 'gas1.inc'

      character*40 dword

      read(lin,*)dword

!!----------- begin time from 12:00 (noon) march 21 [min]
      read(lin,*)dword, tbeg_dd, tbeg_hh, tbeg_mm, tbeg_ss, dword
      read(lin,*)dword, trun_dd, trun_hh, trun_mm, trun_ss, dword
      read(lin,*)dword, dt_min,dword ! transport time-step [min]
      read(lin,*)dword, rlon,  dword ! longitude [deg]
      read(lin,*)dword, rlat,  dword ! latitude [deg]
      read(lin,*)dword, zalt_m,dword ! altitude  above mean sea level [m]
      read(lin,*)dword, rh,    dword ! relative humidity [%]
      read(lin,*)dword, te,    dword ! temperature [k]
      read(lin,*)dword, pr_atm,dword ! pressure [atm]
      read(lin,*)dword, msolar,dword ! msolar flag
      read(lin,*)dword, mphoto,dword ! mphoto flag
      read(lin,*)dword, iprint,dword ! freq of output

!!----------- read index, species name, initial conc, and emissions
      read(lin,*)dword
      read(lin,*)dword

      read(lin,*)dword ! gas

      do i=1, ngas_max
        read(lin,*)k, species(k), cnn(k), emission(k)   ! k?
      
      enddo

      write(6,*)'finished reading all inputs...'
      return
      end




!!**********************************************************************
!!***********************************************************************
      subroutine integratechemistry(factcld)
      include 'chm1.inc'
      include 'gas1.inc'


      t_in = told_sec

      call gaschemistry(t_in,factcld)     !!! odesolver(ntot,stot,t_in) is in this

      return
      end



!!***********************************************************************
!!********************************************************************
      subroutine gaschemistry(t_in,factcld)
      include 'chm1.inc'
      include 'gas1.inc'
      dimension stot(nmax)		! local species array

      call selectgasregime(ntot)	! selects iregime and calculates ntot

      call peroxyrateconstants

      call gasrateconstants(factcld)

      call setgasindices		! set gas indices for selected iregime 

      call mapgasspecies(stot,0)	! map cnn into stot for selected iregime

      call odesolver(ntot,stot,t_in)

      call mapgasspecies(stot,1)	! map stot back into cnn

      
      return
      end



!!**********************************************************************
!!**********************************************************************
      subroutine updateemissions
      include 'chm1.inc'

!! update emission fluxes here

      return
      end



!!**********************************************************************
!!***********************************************************************
      subroutine updatemetfields
      include 'chm1.inc'

!! update temperature, pressure, cloud flag, etc. here

      return
      end






!!**********************************************************************
!!***********************************************************************
      subroutine updatetime

      include 'chm1.inc'

     
      told_sec = tcur_sec

      tcur_sec = tcur_sec + dt_sec
      tcur_min = tcur_sec/60.
      tcur_hrs = tcur_min/60.
      tmid_sec = told_sec + 0.5*dt_sec

      return
      end




!!*****************************************************************
!!     subroutine for printing output at iprint time steps
!!***************************************************************** 
      subroutine printresult(cppb)
      implicit real(a-h,o-z), integer(i-n)
      include 'chm1.inc'
      include 'gas1.inc'
      dimension cppb(ngas_max)


!=================================================================
!
!     converting (molecules/cc) to (ppb)
!-----------------------------------------------------------------
      do l=1,ngas_max
        cppb(l) = (cnn(l)/cair_mlc)*ppb
      enddo

!=================================================================
!
!     gas-phase species
!-----------------------------------------------------------------
!                                                            comm by lijie
!      if(it.eq.0)write(20,770)

!      write(20,7)tcur_hrs,cppb(ko3),cppb(kno),cppb(kno2),
!     &                    cppb(khno3),cppb(khono),cppb(kpan),
!     &                    cppb(konit),cppb(kh2o2),cppb(kolet),
!     &                    cppb(kolei),cppb(kpar),cppb(kald2),
!     &                    cppb(kisop)

!
!===================================================================
!
!     formats
!-------------------------------------------------------------------

!7     format(f6.1, 13(2x,f7.3)) ! original

!!!!!----- comm by feng fan ----------------------------------
!!7       format(f6.1, 13(2x,f10.3)) !lijie modify
!!770   format('  time     o3       no       no2      hno3' &
!!            '     hono     pan     onit      h2o2    olet' &
!!            '     olei     par      ald2     isop')
!!!!!----------------------------------------------------------
      
      write(20,*)tcur_hrs,dt_sec,iregime,cppb(ko3),cppb(kno),cppb(kno2),cppb(kso2),&
      cppb(kh2o2),cppb(kco),cppb(ko1d),cppb(ko3p),cppb(kh2so4)
      return
      end









!!***********************************************************************
!!***********************************************************************
      subroutine setaircomposition
      include 'chm1.inc'
      include 'gas1.inc'

!!-----------------------------------------------------------------------
!! set bulk air composition in (molec/cc)
      cair_mlc = avogad*pr_atm/(82.056*te)	! air conc [molec/cc]
!!      write(6,*)' air concentraton = ', cair_mlc, ' molec/cc'
      o2       = 0.21*cair_mlc
      h2       = 0.58*1.e-6*cair_mlc
      h2o      = watervapor(rh, cair_mlc, te, pr_atm)

!!      write(6,*)'h2o = ', h2o

!!-----------------------------------------------------------------------
!! conversion factor for converting [ppb] to [molec/cc]
!!
      ppb = 1.e+9
!!
!!-------------------------------------------------------------
!! converting gas-phase con!! from [ppb] to [molec/cc]
      do l=1, ngas_max
        cnn(l) = cnn(l)*cair_mlc/ppb
      enddo

!! convert from ppb/hr to molec/cc/s
      do l=1, ngas_max
        emission(l) = emission(l)*cair_mlc/ppb/3600.
      enddo

      return
      end            








!!************************************************************************
!! subroutine peroxyrateconstants: calculates parameterized thermal rate 
!!                     constants for the alkylperoxy radical permutation 
!!                     reactions for the entire mechanism.
!! nomenclature:
!! rk_param  = parameterized reaction rate constants (1/s)
!! rk_perox  = individual permutation reaction rate constants (mole!!c!!s)
!! te        = ambient atmospheri!! temperature (k)
!! 
!! author: rahul a. zaveri
!! date  : june 1998
!!
!!------------------------------------------------------------------------
      subroutine peroxyrateconstants
      include 'chm1.inc'
      include 'gas1.inc'

      dimension sperox(nperox), rk_perox(nperox,nperox)

      sperox(jch3o2)  = cnn(kch3o2)
      sperox(jethp)   = cnn(kethp)
      sperox(jro2)    = cnn(kro2)
      sperox(jc2o3)   = cnn(kc2o3)
      sperox(jano2)   = cnn(kano2)
      sperox(jnap)    = cnn(knap)
      sperox(jisopp)  = cnn(kisopp)
      sperox(jisopn)  = cnn(kisopn)
      sperox(jisopo2) = cnn(kisopo2)
      sperox(jxo2)    = cnn(kxo2)


!! initialize to zero
      do i = 1, nperox
      rk_param(i) = 0.0
      enddo

      do i = 1, nperox
      do j = 1, nperox
      rk_perox(i,j) = arr(aperox(i,j),bperox(i,j))
      rk_param(i) = rk_param(i) + rk_perox(i,j)*sperox(j)
      enddo
      enddo

      return
      end



!!************************************************************************
!! subroutine loadperoxyparameters: loads thermal rate coefficients 
!!                                  for peroxy-peroxy permutation reactions 
!! nomenclature:
!! aperox  = pre-exponential factor (mole!!c!!s)
!! bperox  = activation energy (-e/r)  (k)
!! 
!! author: rahul a. zaveri
!! date  : june 1998
!!------------------------------------------------------------------------
      subroutine loadperoxyparameters
      include 'chm1.inc'
      include 'gas1.inc'

      aperox(jch3o2,jch3o2)   = 2.5e-13
      aperox(jethp,jethp)     = 6.8e-14
      aperox(jc2o3,jc2o3)     = 2.9e-12
      aperox(jano2,jano2)     = 8.0e-12
      aperox(jnap,jnap)       = 1.0e-12
      aperox(jro2,jro2)       = 5.3e-16
      aperox(jisopp,jisopp)   = 3.1e-14
      aperox(jisopn,jisopn)   = 3.1e-14
      aperox(jisopo2,jisopo2) = 3.1e-14
      aperox(jxo2,jxo2)       = 3.1e-14

      bperox(jch3o2,jch3o2)   = 190.
      bperox(jethp,jethp)     = 0.0
      bperox(jc2o3,jc2o3)     = 500.
      bperox(jano2,jano2)     = 0.0
      bperox(jnap,jnap)       = 0.0
      bperox(jro2,jro2)       = 1980.
      bperox(jisopp,jisopp)   = 1000.
      bperox(jisopn,jisopn)   = 1000.
      bperox(jisopo2,jisopo2) = 1000.
      bperox(jxo2,jxo2)       = 1000.

      do i = 1, nperox
      do j = 1, nperox
        if(i.ne.j)then
          aperox(i,j) = 2.0*sqrt(aperox(i,i)*aperox(j,j))
          bperox(i,j) = 0.5*(bperox(i,i) + bperox(j,j))
        endif
      enddo
      enddo

!! except for
      aperox(jc2o3,jch3o2) = 1.3e-12
      aperox(jch3o2,jc2o3) = 1.3e-12
      bperox(jc2o3,jch3o2) = 640.
      bperox(jch3o2,jc2o3) = 640.

      return
      end












!!***********************************************************************
!! subroutine photoconstants_solar: calculates photochemical rate constants (1/s)
!!
!! input: cos_sza (cosine of solar zenith angle from solarzenithangle.f)
!!        zalt_m (altitude above sea level in meters)
!!
!!------------------------------------------------------------------------
 
      subroutine photoconstants_solar(factcld)
      include 'chm1.inc'
      include 'gas1.inc'
      parameter (sza_cut = 89.00)		! cutoff solar zenith angle
      parameter (cos_sza_cut = 0.017452406)	! cos of sza_cut

     ! factcld = 1.0

      if(cos_sza .ge. cos_sza_cut)then	! daytime

         idaytime = 1	

         if(mphoto.eq.1)then
           call photoparam1
         elseif(mphoto.eq.2)then
           call photoparam2
         endif

!! apply cloudiness correction factor
         do jphoto = 1, nphoto
           rk_photo(jphoto) = rk_photo(jphoto)*factcld
         enddo

      else				! nighttime

         idaytime = 0

         do jphoto = 1, nphoto
           rk_photo(jphoto) = 0.0
         enddo

      endif
      return
      end






      subroutine setrunparameters
      include 'chm1.inc'


      tbeg_sec = ((tbeg_dd*24.+tbeg_hh)*60.+tbeg_mm)*60.+tbeg_ss
      trun_sec = ((trun_dd*24.+trun_hh)*60.+trun_mm)*60.+trun_ss


      dt_sec   = 60.*dt_min	! time step [seconds]
     

      tcur_sec = tbeg_sec	! initialize current time to tbeg_sec
      tcur_min = tcur_sec/60.
      tcur_hrs = tcur_min/60.
      it       = 0		! initialize step counter to zero


! convert rlon and rlat to radians
      rlon = rlon*deg2rad
      rlat = rlat*deg2rad       

      if(msolar .eq. 2)then
        write(6,*)' you have selected fixed photolysis rates'
        write(6,*)' for the entire simulation (msolar = 2).'
        write(6,*)' set the rate constants in file:'
        write(6,*)' photoconstants_fixed.f'
      endif


      return
      end















!!*****************************************************************
!!      subroutine solar - calculates cosine of zenith angle
!!                         for use in photochemical rate coefficient
!!                         calculations.
!!
!!      nomenclature:
!!  
!!      tmid_sec = time in seconds from greenich noon march 21
!!
!!      cos_za = cosine of zenith angle
!!
!!      rlon   = local longitude, w hemis. values are negative (radians)
!!                   
!!      rlat   = local latitude, s hemis. values are negative (radians)
!!
!!*****************************************************************
 
       subroutine solarzenithangle
       include 'chm1.inc'
 
       tlocal=tmid_sec                                                  
       tdec=0.4092797*sin(1.992385e-7*tlocal)                           
       sidec=sin(tdec)                                                  
       codec=cos(tdec)                                                  
       tloc=7.272205e-5*tlocal                                           
       thou=cos(rlon+tloc)                                      
       cos_sza=sin(rlat)*sidec+cos(rlat)*codec*thou         
 
       return
       end





!!******************************************************************
!!******************************************************************
      subroutine domassbalance
      implicit real(a-h,o-z), integer(i-n) 
      include 'chm1.inc'


!!=================================================================
!! mass balances
!!=================================================================
!!     initialize...
      tots = 0.0
      totn = 0.0
      totcl= 0.0
!!--------------------------------------------------
!! sulfur balance

      do i=kdms,ksulfhox
      tots = tots + cnn(i)
      enddo
      tots = tots + cnn(kso2) + cnn(kh2so4)
 
!!--------------------------------------------------
!! nitrogen balance

      do i=kno,khno4
      totn = totn + cnn(i)
      enddo
      totn = totn + cnn(kn2o5)+cnn(kpan)+cnn(konit)+cnn(knap)+cnn(kisopn)

!!--------------------------------------------------
!! chlorine balance

      totcl = cnn(khcl)

!!--------------------------------------------------
!!  initial total elements (n,s,cl)
      if(it.eq.0) then

          if(totn.gt.1.e-30)then
          tni = totn
          tnid= totn
          else
          tni = totn
          tnid= 1.0
          endif

          if(tots.gt.1.e-30)then
          tsi = tots
          tsid= tots
          else
          tsi = tots
          tsid= 1.0
          endif

          if(totcl.gt.1.e-30)then
          tcli = totcl
          tclid= totcl
          else
          tcli = totcli
          tclid= 1.0
          endif

      end if


!!  calculate percent deviation in elemental mass balance
      dn = 100.*(totn-tni)/tnid
      ds = 100.*(tots-tsi)/tsid
      dcl= 100.*(totcl-tcli)/tclid
 
      return
      end





!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!





!!************************************************************************
      subroutine photoconstants_fixed
      include 'chm1.inc'
      include 'gas1.inc'

      rk_photo(1)   = 0.0	! no2 + hv    --> no + o(3p)
      rk_photo(2)   = 0.0	! no3 + hv    --> .89no2 + .89o(3p) + .11no
      rk_photo(3)   = 0.0	! hono + hv   --> oh + no
      rk_photo(4)   = 0.0	! hno3 + hv   --> oh + no2
      rk_photo(5)   = 0.0	! hno4 + hv   --> ho2 + no2
      rk_photo(6)   = 0.0	! n2o5 + hv   --> no2 + no3
      rk_photo(7)   = 0.0	! o3 + hv     --> o(3p)
      rk_photo(8)   = 0.0	! o3 + hv     --> o(1d)
      rk_photo(9)   = 0.0	! h2o2 + hv   --> 2oh
      rk_photo(10)   = 0.0	! hcho + hv   --> 2ho2 + co
      rk_photo(11)  = 0.0	! hcho + hv   --> co
      rk_photo(12)  = 0.0	! ch3ooh + hv --> hcho + ho2 + oh
      rk_photo(13)  = 0.0	! ethooh + hv --> ald2 + ho2 + oh
      rk_photo(14)  = 0.0	! ald2 + hv   --> 
      rk_photo(15)  = 0.0	! aone + hv   --> 
      rk_photo(16)  = 0.0	! mgly + hv   --> 
      rk_photo(17)  = 0.0	! open + hv   --> 
      rk_photo(18)  = 0.0	! rooh + hv   -->
      rk_photo(19)  = 0.0	! onit + hv   --> 	
      rk_photo(20)  = 0.0	! isoprd + hv -->

      return
      end






!!***********************************************************************
!! subroutine photoparam1: calculates photochemical rate constants (1/s)
!!
!! input: cos_sza (cosine of solar zenith angle from solarzenithangle.f)
!!        zalt_m (altitude above sea level in meters)
!!
!!------------------------------------------------------------------------

      subroutine photoparam1
      include 'chm1.inc'
      include 'gas1.inc'

      z = min(1.e-4*zalt_m, 1.1)	! zalt in meters

!! no2 + hv --> no + o3p
      alpha0=2.10223e-2-1.6e-3*(z-1.15)**2.
      alpha1=-12.2e-2+alpha0**.5
      beta0=1.258-.16*(z-2.15)**2.
      beta1=-1.2+beta0**.5
      rk_photo(jphoto_no2)    = alpha1*exp(beta1/cos_sza)
!! no3 + hv --> .89 no2 + .89 o3p + .11 no
      rk_photo(jphoto_no3)    = 23.8*rk_photo(jphoto_no2)

!! hono + hv --> oh + no
      rk_photo(jphoto_hono)   = 0.197*rk_photo(jphoto_no2)

!! hno3 + hv --> oh + no2
      rk_photo(jphoto_hno3)   = 3.3e-5*rk_photo(jphoto_no2)

!! hno4 + hv --> ho2 + no2
      rk_photo(jphoto_hno4)   = 5.9e-4*rk_photo(jphoto_no2)

!! n2o5 + hv --> no2 + no3
      rk_photo(jphoto_n2o5)   = 0.0*rk_photo(jphoto_no2)

!! o3 + hv --> o3p
      rk_photo(jphoto_o3a)    = 0.053*rk_photo(jphoto_no2)

!! o3 + hv --> o1d
      alpha0=2.69924e-7-4.0e-8*(z-.375)**2.
      alpha7=-3.25e-4+sqrt(alpha0)
      beta0=4.173-.64*(z-2.0)**2.
      beta7=-3.2+sqrt(beta0)
      rk_photo(jphoto_o3b)    = alpha7*exp(beta7/cos_sza)

!! h2o2 + hv --> 2 oh
      alpha0=2.540534e-9-4.e-11*(z-0.75)**2.
      alpha8=-3.e-5+sqrt(alpha0)
      beta0=.539284-.16*(z-1.75)**2.
      beta8=-1+sqrt(beta0)
      rk_photo(jphoto_h2o2)   = alpha8*exp(beta8/cos_sza)

!! hcho + hv ---> 2 ho2 + co
      alpha0=7.1747e-9-1.6e-9*(z-.75)**2.
      alpha49=-4.e-5+sqrt(alpha0)
      beta0=.7631144-.16*(z-2.)**2.
      beta49=-1.2+sqrt(beta0)
      rk_photo(jphoto_hchoa)  = alpha49*exp(beta49/cos_sza)

!! hcho + hv ---> co
      alpha0=1.693813e-7-4.e-8*(z-.875)**2.
      alpha50=-2.5e-4+sqrt(alpha0)
      beta0=.7631144-.16*(z-1.875)**2.
      beta50=-1.1+sqrt(beta0)
      rk_photo(jphoto_hchob)  = alpha50*exp(beta50/cos_sza)

      rk_photo(jphoto_ch3ooh) = 0.7   *rk_photo(jphoto_h2o2)
      rk_photo(jphoto_ethooh) = 0.7   *rk_photo(jphoto_h2o2)
      rk_photo(jphoto_ald2)   = 4.6e-4*rk_photo(jphoto_no2)
      rk_photo(jphoto_aone)   = 7.8e-5*rk_photo(jphoto_no2)
      rk_photo(jphoto_mgly)   = 9.64  *rk_photo(jphoto_hchoa)
      rk_photo(jphoto_open)   = 9.04  *rk_photo(jphoto_hchoa)
      rk_photo(jphoto_rooh)   = 0.7   *rk_photo(jphoto_h2o2)
      rk_photo(jphoto_onit)   = 1.0e-4*rk_photo(jphoto_no2)
      rk_photo(jphoto_isoprd) = .025  *rk_photo(jphoto_hchob)


!!check if rk_photo is negative   (by feng fan)

      do jdum=1,nphoto
         if(rk_photo(jdum)<0)then
           write(*,*)"subroutine photoparam1 rk_photo(",jdum,")<0"
           stop
          end if
      end do 

    
      return
      end




!!***********************************************************************
!! subroutine photoparam2: calculates photochemical rate constants (1/s)
!!
!! input: cos_sza (cosine of solar zenith angle from solarzenithangle.f)
!!        zalt_m (altitude above sea level in meters)
!!
!!------------------------------------------------------------------------

      subroutine photoparam2
      include 'chm1.inc'
      include 'gas1.inc'

      real kr(3,nphoto)	! kr(level, species #):

      cz = cos_sza
        !initialize
        kr = 0.

!! surface photolysis rates:
!! no2
	kr(1,jphoto_no2)=-1.0184e-3+cz*1.8542e-2-cz**2*9.5368e-3+cz**3*1.8165e-3
!! no3       
	kr(1,jphoto_no3)=4.3945e-3+0.556446*cz-0.71996*cz**2+0.34253*cz**3
!! o3      
        kr(1,jphoto_o3b)=(-3.2168e-8+cz*2.4588e-6-cz**2*2.6274e-5+1.2005e-4*cz**3-7.6216e-5*cz**4+1.4628e-5*cz**5)
!! hono
	kr(1,jphoto_hono)=-1.7863e-4+3.2272e-3*cz-8.5989e-4*cz**2-1.8987e-4*cz**3
!! hno3      ->test 
	kr(1,jphoto_hno3)=1.9592e-8-2.8147e-7*cz+1.3533e-6*cz**2-4.2010e-7*cz**3
!! h2o2     
	kr(1,jphoto_h2o2)=-2.1672e-7+4.0070e-6*cz+1.1189e-5*cz**2-6.4306e-6*cz**3
!! hno4     ->test 
	kr(1,jphoto_hno4)=2.1392e-8-2.0854e-7*cz+1.6131e-5*cz**2-7.2297e-6*cz**3
!! hchob     ->test 
	kr(1,jphoto_hchoa)=-5.4160e-8+9.4694e-7*cz+6.4697e-5*cz**2-3.2594e-5*cz**3
!! hchoa      
	kr(1,jphoto_hchob)=-2.6855e-6+4.9102e-5*cz+5.7086e-5*cz**2-4.3040e-5*cz**3


!! photolysis rates at 4 km:
!! no2	
  	kr(2,jphoto_no2)=-1.3136e-3+2.4948e-2*cz-1.9513e-2*cz**2+6.611e-3*cz**3
!! no3	       
	kr(2,jphoto_no3)=1.59e-2+0.54202*cz-0.72079*cz**2+0.34898*cz**3 
!! o3
        kr(2,jphoto_o3b)=(1.6295e-7+cz*4.9940e-7-cz**2*2.9055e-5+1.8187e-4*cz**3-1.5627e-4*cz**4+4.5975e-5*cz**5)
!! hono       
	kr(2,jphoto_hono)=-2.6339e-4+4.6997e-3*cz-2.9408e-3*cz**2+7.4996e-4*cz**3
!! hno3      -->test 
	kr(2,jphoto_hno3)=2.2106e-8-3.4422e-7*cz+1.8449e-6*cz**2-6.7994e-7*cz**3
!! h2o2       
	kr(2,jphoto_h2o2)=-4.73e-7+7.4881e-6*cz+9.7183e-6*cz**2-6.4955e-6*cz**3 
!! hno4      -->test 
	kr(2,jphoto_hno4)=-1.0672e-7+1.1698e-6*cz+1.9044e-5*cz**2-9.4072e-6*cz**3
!! hchob     --test  
	kr(2,jphoto_hchoa)=-7.4493e-7+8.7149e-6*cz+7.1885e-5*cz**2-3.9526e-5*cz**3 
!! hchoa       
	kr(2,jphoto_hchob)=-5.1681e-6+8.4398e-5*cz+2.6478e-5*cz**2-3.4452e-5*cz**3 


!! photolysis rates at 8 km:
!! no2
       kr(3,jphoto_no2)=-1.3748e-3+2.9757e-2*cz-2.8355e-2*cz**2+1.1168e-2*cz**3
!! no3
       kr(3,jphoto_no3)=2.80132e-2+0.51381*cz-0.68839*cz**2+0.33448*cz**3
!! o3
       kr(3,jphoto_o3b)=(1.6295e-7+cz*4.9940e-7-cz**2*2.9055e-5+1.8187e-4*cz**3-1.5627e-4*cz**4+4.5975e-5*cz**5)
!! hono
       kr(3,jphoto_hono)=-3.1944e-4+6.0983e-3*cz-5.2694e-3*cz**2+1.9111e-3*cz**3
!! hno3
       kr(3,jphoto_hno3)=1.9176e-8-3.4083e-7*cz+2.1560e-6*cz**2-8.7941e-7*cz**3
!! h2o2
       kr(3,jphoto_h2o2)=-7.6642e-7+1.1717e-5*cz+5.3611e-6*cz**2-4.9358e-6*cz**3
!! hno4
       kr(3,jphoto_hno4)=-3.2131e-7+3.6898e-6*cz+1.8481e-5*cz**2-9.826e-6*cz**3
!! hchob
       kr(3,jphoto_hchoa)=-1.7563e-6+2.0714e-5*cz+6.5668e-5*cz**2-3.9386e-5*cz**3
!! hchoa
       kr(3,jphoto_hchob)=-7.9124e-6+1.258e-4*cz-2.8767e-5*cz**2-1.0505e-5*cz**3
!! force all the kr to be non-negative
	do jdum = 1, nphoto
	do idum = 1, 3
	    kr(idum,jdum) = max( 0., kr(idum,jdum) )
	end do
	end do

!!--- force all negative zalt_m to be 1 (by feng fan 2015.1.22)
!!--- if zalt_m < 0, then rk_photo(i)<0. however, rk_photo(i)should be >=0 

if (zalt_m<0)then
   zalt_m=1 
end if
!!------------------------------------------------------------





!! above 8km, use values at 8km
	if (zalt_m .ge. 8000) then
	 alpha = 1
	 k = 3
	 km1 = 3
!! linear interpolation 
	else if (zalt_m .lt. 8000. .and. zalt_m .ge. 4000.) then
	 alpha = (8000. - zalt_m)/4000.
	 k = 3
	 km1 = 2
	else if (zalt_m .lt. 4000) then
	 alpha = (4000. - zalt_m)/4000
	 k = 2
	 km1 = 1
	end if

        a = alpha
        a1= 1. - alpha

      rk_photo(jphoto_no2)   = a1*kr(k,jphoto_no2)   +  a*kr(km1,jphoto_no2)
      rk_photo(jphoto_no3)   = a1*kr(k,jphoto_no3)   +  a*kr(km1,jphoto_no3)
      rk_photo(jphoto_o3a)   = 0.053*rk_photo(jphoto_no2)

      rk_photo(jphoto_o3b)   = a1*kr(k,jphoto_o3b)   +  a*kr(km1,jphoto_o3b)
      rk_photo(jphoto_hono)  = a1*kr(k,jphoto_hono)  +  a*kr(km1,jphoto_hono)
      rk_photo(jphoto_hno3)  = a1*kr(k,jphoto_hno3)  +  a*kr(km1,jphoto_hno3)
      rk_photo(jphoto_h2o2)  = a1*kr(k,jphoto_h2o2)  +  a*kr(km1,jphoto_h2o2)
      rk_photo(jphoto_hno4)  = a1*kr(k,jphoto_hno4)  +  a*kr(km1,jphoto_hno4)
      rk_photo(jphoto_hchoa) = a1*kr(k,jphoto_hchoa) +  a*kr(km1,jphoto_hchoa)
      rk_photo(jphoto_hchob) = a1*kr(k,jphoto_hchob) +  a*kr(km1,jphoto_hchob)

      rk_photo(jphoto_n2o5)   = 0.0   *rk_photo(jphoto_no2)
      rk_photo(jphoto_ch3ooh) = 0.7   *rk_photo(jphoto_h2o2)
      rk_photo(jphoto_ethooh) = 0.7   *rk_photo(jphoto_h2o2)
      rk_photo(jphoto_ald2)   = 4.6e-4*rk_photo(jphoto_no2)
      rk_photo(jphoto_aone)   = 7.8e-5*rk_photo(jphoto_no2)
      rk_photo(jphoto_mgly)   = 9.64  *rk_photo(jphoto_hchoa)
      rk_photo(jphoto_open)   = 9.04  *rk_photo(jphoto_hchoa)
      rk_photo(jphoto_rooh)   = 0.7   *rk_photo(jphoto_h2o2)
      rk_photo(jphoto_onit)   = 1.0e-4*rk_photo(jphoto_no2)
      rk_photo(jphoto_isoprd) = .025  *rk_photo(jphoto_hchob)


!!check if rk_photo is negative   (by feng fan)

      do jdum=1,nphoto
         if(rk_photo(jdum)<0)then
           write(*,*)"subroutine photoparam2 rk_photo(",jdum,")<0"
           stop
          end if
      end do    

      return
      end





             









!!************************************************************************
!! subroutine gasrateconstants_bio: generates thermal rate coefficients 
!!                   for the selected mechanism
!! nomenclature:
!! rk_bio    = reaction rate constants for hc2 mechanism    (mole!!c!!s)
!! te        = ambient atmospheri!! temperature (k)
!! 
!! author: rahul a. zaveri
!! date  : february 1996
!!------------------------------------------------------------------------
      subroutine gasrateconstants_bio
      include 'chm1.inc'
      include 'gas1.inc'

      rk_bio(1)  = arr(2.6e-11, 409.)
      rk_bio(2)  = arr(1.2e-14, -2013.)
      rk_bio(3)  = arr(3.0e-12, -446.)
      rk_bio(4)  = rk_photo(jphoto_isoprd)
      rk_bio(5)  = 3.3e-11
      rk_bio(6)  = 7.0e-18
      rk_bio(7)  = 1.0e-15
      rk_bio(8)  = 4.0e-12
      rk_bio(9)  = 4.0e-12
      rk_bio(10) = 4.0e-12
      rk_bio(11) = arr(1.7e-13, 1300.)
      rk_bio(12) = arr(1.7e-13, 1300.)
      rk_bio(13) = arr(1.7e-13, 1300.)
      rk_bio(14) = rk_param(jisopp)
      rk_bio(15) = rk_param(jisopn)
      rk_bio(16) = rk_param(jisopo2)
      rk_bio(17) = 3.563e-11
      rk_bio(18) = arr(6.712e-11,-449.)
      rk_bio(19) = arr(7.44e-17,821.)
      rk_bio(20) = arr(6.642e-12,-175.)

      return
      end





!!************************************************************************
!! subroutine gasrateconstants_com: generates thermal rate coefficients 
!!                   for the selected mechanism
!! nomenclature:
!! rk_com    = reaction rate constants for common mechanism (mole!!c!!s)
!! te        = ambient atmospheri!! temperature (k)
!! iregime = selected mechanism for the current chemical regime (1-6) 
!! 
!! author: rahul a. zaveri
!! date  : february 1996
!!
!!------------------------------------------------------------------------
      subroutine gasrateconstants_com
      include 'chm1.inc'
      include 'gas1.inc'

      rk_com(1) = rk_photo(jphoto_no2)                                                         
      rk_com(2) = rk_photo(jphoto_no3)                                                         
      rk_com(3) = rk_photo(jphoto_hono)                                                        
      rk_com(4) = rk_photo(jphoto_hno3)                                                        
      rk_com(5) = rk_photo(jphoto_hno4)                                                       
      rk_com(6) = rk_photo(jphoto_n2o5)                                                        
      rk_com(7) = rk_photo(jphoto_o3a)                                                         
      rk_com(8) = rk_photo(jphoto_o3b)                                                         
      rk_com(9) = rk_photo(jphoto_h2o2)                                                        
      rk_com(10) = arr(3.2e-11, 70.)                                                
      rk_com(11) = arr(1.8e-11, 110.)                                               
      rk_com(12) = 2.2e-10                                                          
      rk_com(13) = cair_mlc*6.e-34*(te/300.)**(-2.3)                                                        
      rk_com(14) = arr(8.0e-12, -2060.)                                             
      rk_com(15) = arr(6.5e-12, -120.)                 

      rk0 = 9.0e-32
      rnn = 2.0
      rki = 2.2e-11
      rmm = 0.0                             
      rk_com(16) = troe(cair_mlc,te,rk0,rnn,rki,rmm)   

      rk0 = 9.0e-32
      rnn = 1.5
      rki = 3.0e-11
      rmm = 0.0                                 
      rk_com(17) = troe(cair_mlc,te,rk0,rnn,rki,rmm) 
                                   
      rk_com(18) = arr(2.0e-12, -1400.)                                             
      rk_com(19) = arr(1.2e-13, -2450.)                                             
      rk_com(20) = arr(1.6e-12, -940.)                                              
      rk_com(21) = arr(1.1e-14, -500.)                                              
      rk_com(22) = arr(5.5e-12, -2000.)  
                                           
      rk0 = 7.0e-31
      rnn = 2.6
      rki = 3.6e-11
      rmm = 0.1
      rk_com(23) = troe(cair_mlc,te,rk0,rnn,rki,rmm)   

      rk0 = 2.5e-30
      rnn = 4.4
      rki = 1.6e-11
      rmm = 1.7
      rk_com(24) = troe(cair_mlc,te,rk0,rnn,rki,rmm)                                    
      rk_com(25) = 2.2e-11                                                          
      rk_com(26) = arr(1.8e-11, -390.)

             rko = 7.2e-15 * exp(785./te)
             rk2 = 4.1e-16 * exp(1440./te)
             rk3 = 1.9e-33 * exp(725./te)*cair_mlc
      rk_oh_hno3 = rko + rk3/(1.+rk3/rk2)                                          
      rk_com(27) = rk_oh_hno3                                                       
      rk_com(28) = arr(1.3e-12, 380.)                                               
      rk_com(29) = arr(4.8e-11, 250.)                                               
      rk_com(30) = arr(2.9e-12, -160.)

      rk_2ho2    = 2.3e-13 * exp(600./te) + 1.7e-33 * exp(1000./te)*cair_mlc	          ! ho2 + ho2 --> h2o2
      rk_com(31) = rk_2ho2

      p3 = 1.4e-21 * exp(2200./te)
      rk_2ho2_h2o= rk_2ho2*p3!1.4e-21*exp(2200./te)! ho2 + ho2 + h2o --> h2o2                                                          
      rk_com(32) = rk_2ho2_h2o
                                      
      rk_com(33) = arr(3.5e-12, 250.)     

      rk0 = 1.8e-31
      rnn = 3.2
      rki = 4.7e-12
      rmm = 1.4                                          
      rk_com(34) = troe(cair_mlc,te,rk0,rnn,rki,rmm)    
                                
      rk_com(35) = 5.0e-16                                                          
      rk_com(36) = rk_com(34)*arr(4.8e26, -10900.)                                  
      rk_com(37) = arr(1.5e-11, 170.)                                               
      rk_com(38) = arr(4.5e-14, -1260.)     

      rk0 = 2.2e-30
      rnn = 3.9
      rki = 1.5e-12
      rmm = 0.7                  
      rk_com(39) = troe(cair_mlc,te,rk0,rnn,rki,rmm)         
                           
      rk_com(40) = arr(8.5e-13, -2450.)                                             
      rk_com(41) = 3.5e-12                                                          
      rk_com(42) = 2.0e-21                                                          
      rk_com(43) = rk_com(39)*arr(3.7e26, -11000.)     

      rk_co_oh   = 1.5e-13 * (1.+8.18e-23*te*cair_mlc) ! co + oh --> ho2                             
      rk_com(44) = rk_co_oh                   
                                      
      rk0 = 3.0e-31
      rnn = 3.3
      rki = 1.5e-12
      rmm = 0.0
      rk_com(45) = troe(cair_mlc,te,rk0,rnn,rki,rmm)  
                                  
      rk_com(46) = te**.667*arr(2.8e-14, -1575.)                                    
      rk_com(47) = te**2*arr(1.5e-17, -492.)                                        
      rk_com(48) = arr(6.7e-12, -600.)                                              
      rk_com(49) = rk_photo(jphoto_hchoa)                                                      
      rk_com(50) = rk_photo(jphoto_hchob)                                                      
      rk_com(51) = 1.0e-11                                                          
      rk_com(52) = arr(3.4e-13, -1900.)                                             
      rk_com(53) = rk_photo(jphoto_ch3ooh)                                                    
      rk_com(54) = rk_photo(jphoto_ethooh)                                                    
      rk_com(55) = arr(3.8e-12, 200.)                                               
      rk_com(56) = arr(3.8e-12, 200.)                                               
      rk_com(57) = arr(3.0e-12, 280.)                                               
      rk_com(58) = arr(2.6e-12, 365.)                                               
      rk_com(59) = 1.1e-12                                                          
      rk_com(60) = 2.5e-12                                                          
      rk_com(61) = arr(3.8e-13, 800.)                                               
      rk_com(62) = arr(7.5e-13, 700.)                                               
      rk_com(63) = rk_param(jch3o2)                                                 
      rk_com(64) = rk_param(jethp)                                                  
      rk_com(65) = arr(7.0e-12, -235.)                                              
      rk_com(66) = rk_photo(jphoto_ald2)                                               
      rk_com(67) = arr(5.6e-12, 270.)                                               
      rk_com(68) = arr(1.4e-12, -1900.)       

      rk0 = 9.7e-29
      rnn = 5.6
      rki = 9.3e-12
      rmm = 1.5                                      
      rk_com(69) = troe(cair_mlc,te,rk0,rnn,rki,rmm)          
                          
      rk_com(70) = rk_com(69)*arr(1.1e28, -14000.)                                  
      rk_com(71) = arr(5.3e-12, 360.)                                               
      rk_com(72) = 4.0e-12                                                          
      rk_com(73) = arr(4.5e-13, 1000.)                                              
      rk_com(74) = rk_param(jc2o3)  
    
!     lijie add reactions for no3 +vocs at 20130327
      rk_com(75) = 1.0e-17   ! ethane + no3
      rk_com(76) = arr(9.4e-13,-2650.) ! ch3oh+no3
      !!!!!!!debugging
      return
      end





!!!! to comment all lines in this subroutine and add a new fron lijie
!!!             for heterogeneous chemistry  !!!
!!************************************************************************
!! subroutine gasrateconstants_het: generates thermal rate coefficients 
!!                   for the selected mechanism
!! 
!! rk_het    = reaction rate constants for heterogeneous rxns (1/s)
!! 
!! 
!! revised by li jie
!!------------------------------------------------------------------------
      subroutine gasrateconstants_het
      include 'chm1.inc'
      include 'gas1.inc'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! to comment all lines in this subroutine and add a new form (by lijie)

         rk_het(1) = rk_het(1)  !1  n2o5 + aso4 -> 2hno3 1/s
         rk_het(2) = rk_het(2)  !2  no2  + bc   -> 0.5hono+0.5hno3
         rk_het(3) = rk_het(3)  !3  no3  + aso4 -> hno3
         rk_het(4) = rk_het(4)  !4  ho2  + aso4 -> 0.5h2o2
         rk_het(5) = rk_het(5)  !5  hcho + aso4 -> products
         rk_het(6) = rk_het(6)  !6  oh   + aso4 -> products
         rk_het(7) = rk_het(7)  !7  o3   + bc   -> products
         rk_het(8) = rk_het(8)  !8  no2  + bc   -> hono
         rk_het(9) = rk_het(9)  !9  hno3 + bc   -> no2
         rk_het(10) = rk_het(10)    ! 10  n2o5 + bc   -> 2hno3
         rk_het(11) = rk_het(11)    ! 11  o3   + dust -> products
         rk_het(12) = rk_het(12)    ! 12  hno3 + dust -> ano3 + products
         rk_het(13) = rk_het(13)    ! 13  no2  + dust -> 0.5hono + 0.5hno3
         rk_het(14) = rk_het(14)    ! 14  no3  + dust -> hno3
         rk_het(15) = rk_het(15)    ! 15  n2o5 + dust -> 2hno3
         rk_het(16) = rk_het(16)    ! 16  oh   + dust -> products
         rk_het(17) = rk_het(17)    ! 17  ho2  + dust -> 0.5h2o2
         rk_het(18) = rk_het(18)    ! 18  h2o2 + dust -> products
         rk_het(19) = rk_het(19)    ! 19  so2  + dust -> aso4 
         rk_het(20) = rk_het(20)    ! 20  ch3cooh + dust -> products
         rk_het(21) = rk_het(21)    ! 21  ch3oh   + dust -> products
         rk_het(22) = rk_het(22)    ! 22  hcho    + dust -> products
         rk_het(23) = rk_het(23)    ! 23  n2o5 + ssa  -> 2hno3
         rk_het(24) = rk_het(24)    ! 24  no3  + ssa  -> hno3
         rk_het(25) = rk_het(24)    ! 25  ho2  + ssa  -> 0.5hono
         rk_het(26) = rk_het(26)    ! 26  so2  + ssa  -> aso4
         rk_het(27) = rk_het(27)    ! 27  no3  + ssa  -> ano3
         rk_het(28) = rk_het(28)    ! 28  hno3 + ssa  -> ano3



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! the following values are temperary values only used for test. the above 28 lines are written by li jie
!         rk_het(1) = 1.1451681e-27  
!         rk_het(2) = 9.2583208e-29
!         rk_het(3) = 2.3924208e-27
!         rk_het(4) = 2.7327207e-25
!         rk_het(5) = 2.5221690e-26
!         rk_het(6) = 3.0459142e-25
!         rk_het(7) = 5.6911365e-30
!         rk_het(8) = 3.0552454e-28
!         rk_het(9) = 6.3289386e-27
!         rk_het(10) = 3.0211289e-27
!         rk_het(11) = 0.0
!         rk_het(12) = 0.0
!         rk_het(13) = 0.0
!         rk_het(14) = 0.0
!         rk_het(15) = 0.0
!         rk_het(16) = 0.0
!         rk_het(17) = 0.0
!         rk_het(18) = 0.0
!         rk_het(19) = 0.0
!         rk_het(20) = 0.0
!         rk_het(21) = 0.0
!         rk_het(22) = 0.0
!         rk_het(23) = 0.0
!         rk_het(24) = 0.0
!         rk_het(25) = 0.0
!         rk_het(26) = 0.0
!         rk_het(27) = 0.0
!         rk_het(28) = 0.0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      return
      end





!!************************************************************************
!! subroutine gasrateconstants_mar: generates thermal rate coefficients 
!!                   for the selected mechanism
!! nomenclature:
!! rk_mar    = reaction rate constants for marine mechanism (mole!!c!!s)
!! te        = ambient atmospheri!! temperature (k)
!! 
!! author: rahul a. zaveri
!! date  : february 1996
!!------------------------------------------------------------------------
      subroutine gasrateconstants_mar
      include 'chm1.inc'
      include 'gas1.inc'

!! abstraction reaction
!! hynes et al. (1986)
      rk_tot_num =       te * exp(-234./te) +  &
                   8.46e-10 * exp(7230./te) +  &
                   2.68e-10 * exp(7810./te)
      rk_tot_den = 1.04e+11 * te + 88.1 * exp(7460./te)
      rk_tot	 = rk_tot_num/rk_tot_den

      rk_mar(1)   = 9.60e-12 * exp(-234./te) ! ch3sch3 + oh --> ch3sch2
      babs       = rk_mar(1)/rk_tot
      badd	 = 1. - babs
      rk_mar(2)   = 1.40e-13 * exp(500./te)  ! ch3sch3 + no3 --> 
      rk_mar(3)   = 1.26e-11 * exp(409./te)  ! ch3sch3 + o3p --> 

!! addition reaction
      rk_mar(4)   = badd*rk_tot		     ! ch3sch3 + oh --> ch3s(oh)ch3
      rk_mar(5)   = 8.0e-12
      rk_mar(6)   = 1.8e-13
      rk_mar(7)   = 5.8e-11
      rk_mar(8)   = 1.0e-14
      rk_mar(9)   = 5.0e-12
      rk_mar(10)  = 1.8e-13
      rk_mar(11)  = 1.0e-15
      rk_mar(12)  = 1.0e-13
      rk_mar(13)  = 1.0e-15
      rk_mar(14)  = 1.6e-11
      rk_mar(15)  = 1.0e-13
      rk_mar(16)  = 2.5e+13 * exp(-8686./te) ! ch3so2 --> so2 + ch3o2
      rk_mar(17)  = 1.0e-14
      rk_mar(18)  = 5.0e-15
      rk_mar(19)  = 2.5e-13
      rk_mar(20)  = 2.5e-13
      rk_mar(21)  = 5.0e-11
      rk_mar(22)  = 2.6e-18
      rk_mar(23)  = 3.3
      rk_mar(24)  = 1.0e-11
      rk_mar(25)  = 5.5e-12
      rk_mar(26)  = 2.0e+17 * exp(-12626./te) ! ch3so3 --> h2so4 + ch3o2
      rk_mar(27)  = 3.0e-15
      rk_mar(28)  = 3.0e-15
      rk_mar(29)  = 5.0e-11
      rk_mar(30)  = 1.6e-15

      rk_mar(31)  = 2.5e-13	! ch3sch2oo + ch3so2 --> ch3so3 + ch3so2
      rk_mar(32)  = 8.6e-14	! 2ch3sch2oo --> .15mtf + 1.85ch3so2

!! dry deposition
      rk_mar(33)  = 0.0 ! 2.0e-5	! 1/s
      rk_mar(34)  = 0.0 ! 2.0e-5
      rk_mar(35)  = 0.0 ! 2.0e-5

      return
      end
!!************************************************************************
!! subroutine gasrateconstants_urb: generates thermal rate coefficients 
!!                   for the selected mechanism
!! nomenclature:
!! rk_urb    = reaction rate constants for hc1 mechanism    (mole!!c!!s)
!! te        = ambient atmospheri!! temperature (k)
!! 
!! author: rahul a. zaveri
!! date  : february 1996
!!------------------------------------------------------------------------
      subroutine gasrateconstants_urb
      include 'chm1.inc'
      include 'gas1.inc'

      rk_urb(1) = 8.1e-13                                                           
      rk_urb(2) = rk_photo(jphoto_aone)                                                
      rk_urb(3) = te**2*arr(5.3e-18, -230.)                                         
      rk_urb(4) = rk_photo(jphoto_mgly)                                                  
      rk_urb(5) = 1.7e-11                                                           
      rk_urb(6) = arr(1.4e-12, -1900.)                                              
      rk_urb(7) = arr(1.2e-14, -2630.)      

      rk0 = 1.0e-28
      rnn = 0.8
      rki = 8.8e-12
      rmm = 0.0                                        
      rk_urb(8) = troe(cair_mlc,te,rk0,rnn,rki,rmm)    
                                 
      rk_urb(9) = arr(4.2e-15, -1800.)                                              
      rk_urb(10) = arr(8.9e-16, -392.)                                              
      rk_urb(11) = arr(5.8e-12, 478.)                                               
      rk_urb(12) = arr(2.9e-11, 255.)                                               
      rk_urb(13) = arr(3.1e-13, -1010.)                                             
      rk_urb(14) = 2.5e-12                                                          
      rk_urb(15) = arr(2.1e-12, 322.)                                               
      rk_urb(16) = arr(1.7e-11, 116.)                                               
      rk_urb(17) = 8.1e-12                                                          
      rk_urb(18) = 4.1e-11                                                          
      rk_urb(19) = 2.2e-11                                                          
      rk_urb(20) = 1.4e-11                                                          
      rk_urb(21) = 3.0e-11                                                          
      rk_urb(22) = rk_photo(jphoto_open)                                                 
      rk_urb(23) = arr(5.4e-17, -500.)                                              
      rk_urb(24) = rk_photo(jphoto_rooh)                                                    
      rk_urb(25) = arr(3.8e-12, 200.)                                               
      rk_urb(26) = arr(1.6e-11, -540.)                                              
      rk_urb(27) = rk_photo(jphoto_onit)                                               
      rk_urb(28) = 4.0e-12                                                          
      rk_urb(29) = 4.0e-12                                                          
      rk_urb(30) = 4.0e-12                                                          
      rk_urb(31) = 4.0e-12                                                          
      rk_urb(32) = 2.5e-12                                                          
      rk_urb(33) = 1.2e-12                                                          
      rk_urb(34) = 4.0e-12                                                          
      rk_urb(35) = 2.5e-12                                                          
      rk_urb(36) = arr(1.7e-13, 1300.)                                              
      rk_urb(37) = arr(1.2e-13, 1300.)                                              
      rk_urb(38) = arr(1.7e-13, 1300.)                                              
      rk_urb(39) = arr(1.7e-13, 1300.)                                              
      rk_urb(40) = rk_param(jro2)                                                   
      rk_urb(41) = rk_param(jano2)                                                  
      rk_urb(42) = rk_param(jnap)                                                   
      rk_urb(43) = rk_param(jxo2)                                                   
      rk_urb(44) = 1.0e-11   

! move from rk_com to rk_urb by feng fan

      rk_urb(45) = 7.0e-17   ! par+no3
      rk_urb(46) = arr(3.3e-12,-2880.) ! eth+no3=no2+xo2+2hcho
      rk_urb(47) = 7.0e-17   ! tol+no3
      rk_urb(48) = 3.0e-17   ! anoe+no3
      rk_urb(49) = 5.0e-16   ! xyl+no3
 

      return
      end





!!!-----------------------------------------------------------------
!!!------------------------------------------------------------------
      subroutine gasrates_com(s)
      include 'chm1.inc'
      include 'gas1.inc'
      dimension s(nmax)

      r_com(1) = rk_com(1)*s(ino2)
      r1_com(1) = rk_com(1)
     
                                                      
      r_com(2) = rk_com(2)*s(ino3)
      r1_com(2) = rk_com(2) 
                                                     
      r_com(3) = rk_com(3)*s(ihono)
      r1_com(3) = rk_com(3)
                                                     
      r_com(4) = rk_com(4)*s(ihno3)
      r1_com(4) = rk_com(4)
                                                     
      r_com(5) = rk_com(5)*s(ihno4)
      r1_com(5) = rk_com(5) 
                                                    
      r_com(6) = rk_com(6)*s(in2o5)
      r1_com(6) = rk_com(6)
                                                     
      r_com(7) = rk_com(7)*s(io3)
      r1_com(7) = rk_com(7) 
                                                      
      r_com(8) = rk_com(8)*s(io3)
      r1_com(8) = rk_com(8)
                                                       
      r_com(9) = rk_com(9)*s(ih2o2)
      r1_com(9) = rk_com(9) 
                                                    
      r_com(10) = rk_com(10)*s(io1d)*.21*cair_mlc 
      r1_com(10) = rk_com(10)*.21*cair_mlc
                                                
      r_com(11) = rk_com(11)*s(io1d)*.79*cair_mlc
      r1_com(11) = rk_com(11)*.79*cair_mlc 
                                                
      r_com(12) = rk_com(12)*s(io1d)*h2o
      r1_com(12) = rk_com(12)*h2o  
                                              
      r_com(13) = rk_com(13)*s(io3p)*.21*cair_mlc
      r1_com(13) = rk_com(13)*.21*cair_mlc  
                                               
      r_com(14) = rk_com(14)*s(io3p)*s(io3)
      r1_com(14) = rk_com(14)*s(io3p)
      r2_com(14) = rk_com(14)*s(io3)  
                                           
      r_com(15) = rk_com(15)*s(io3p)*s(ino2)
      r1_com(15) = rk_com(15)*s(io3p)
      r2_com(15) = rk_com(15)*s(ino2) 
                                           
      r_com(16) = rk_com(16)*s(io3p)*s(ino2)
      r1_com(16) = rk_com(16)*s(io3p)
      r2_com(16) = rk_com(16)*s(ino2) 
                                           
      r_com(17) = rk_com(17)*s(io3p)*s(ino)
      r1_com(17) = rk_com(17)*s(io3p)
      r2_com(17) = rk_com(17)*s(ino) 
                                            
      r_com(18) = rk_com(18)*s(io3)*s(ino)
      r1_com(18) = rk_com(18)*s(io3)
      r2_com(18) = rk_com(18)*s(ino) 
                                             
      r_com(19) = rk_com(19)*s(io3)*s(ino2) 
      r1_com(19) = rk_com(19)*s(io3)
      r2_com(19) = rk_com(19)*s(ino2)
                                            
      r_com(20) = rk_com(20)*s(io3)*s(ioh)
      r1_com(20) = rk_com(20)*s(io3)
      r2_com(20) = rk_com(20)*s(ioh) 
                                             
      r_com(21) = rk_com(21)*s(io3)*s(iho2)
      r1_com(21) = rk_com(21)*s(io3)
      r2_com(21) = rk_com(21)*s(iho2) 
                                            
      r_com(22) = rk_com(22)*s(ioh)*h2 
      r1_com(22) = rk_com(22)*h2 
                                                
      r_com(23) = rk_com(23)*s(ioh)*s(ino)
      r1_com(23) = rk_com(23)*s(ioh)
      r2_com(23) = rk_com(23)*s(ino)  
                                            
      r_com(24) = rk_com(24)*s(ioh)*s(ino2)
      r1_com(24) = rk_com(24)*s(ioh)
      r2_com(24) = rk_com(24)*s(ino2)  
                                           
      r_com(25) = rk_com(25)*s(ioh)*s(ino3)
      r1_com(25) = rk_com(25)*s(ioh)
      r2_com(25) = rk_com(25)*s(ino3) 
                                            
      r_com(26) = rk_com(26)*s(ioh)*s(ihono)
      r1_com(26) = rk_com(26)*s(ioh)
      r2_com(26) = rk_com(26)*s(ihono) 
                                           
      r_com(27) = rk_com(27)*s(ioh)*s(ihno3) 
      r1_com(27) = rk_com(27)*s(ioh)
      r2_com(27) = rk_com(27)*s(ihno3)
                                           
      r_com(28) = rk_com(28)*s(ioh)*s(ihno4)
      r1_com(28) = rk_com(28)*s(ioh)
      r2_com(28) = rk_com(28)*s(ihno4) 
                                           
      r_com(29) = rk_com(29)*s(ioh)*s(iho2)
      r1_com(29) = rk_com(29)*s(ioh)
      r2_com(29) = rk_com(29)*s(iho2)  
                                           
      r_com(30) = rk_com(30)*s(ioh)*s(ih2o2)
      r1_com(30) = rk_com(30)*s(ioh)
      r2_com(30) = rk_com(30)*s(ih2o2) 
                                           
      r_com(31) = rk_com(31)*s(iho2)*s(iho2)
      r1_com(31) = rk_com(31)*s(iho2) 
                                           
      r_com(32) = rk_com(32)*s(iho2)*s(iho2)*h2o
      r1_com(32) = rk_com(32)*s(iho2)*h2o  
                                      
      r_com(33) = rk_com(33)*s(iho2)*s(ino)
      r1_com(33) = rk_com(33)*s(iho2)
      r2_com(33) = rk_com(33)*s(ino)  
                                           
      r_com(34) = rk_com(34)*s(iho2)*s(ino2)
      r1_com(34) = rk_com(34)*s(iho2)
      r2_com(34) = rk_com(34)*s(ino2)  
                                          
      r_com(35) = rk_com(35)*s(iho2)*s(ino2)
      r1_com(35) = rk_com(35)*s(iho2)
      r2_com(35) = rk_com(35)*s(ino2) 
                                           
      r_com(36) = rk_com(36)*s(ihno4)
      r1_com(36) = rk_com(36)    
                                               
      r_com(37) = rk_com(37)*s(ino3)*s(ino)
      r1_com(37) = rk_com(37)*s(ino3)
      r2_com(37) = rk_com(37)*s(ino) 
                                            
      r_com(38) = rk_com(38)*s(ino3)*s(ino2)
      r1_com(38) = rk_com(38)*s(ino3) 
      r2_com(38) = rk_com(38)*s(ino2)
                                           
      r_com(39) = rk_com(39)*s(ino3)*s(ino2)
      r1_com(39) = rk_com(39)*s(ino3)
      r2_com(39) = rk_com(39)*s(ino2) 
                                           
      r_com(40) = rk_com(40)*s(ino3)*s(ino3)
      r1_com(40) = rk_com(40)*s(ino3)  
                                          
      r_com(41) = rk_com(41)*s(ino3)*s(iho2)
      r1_com(41) = rk_com(41)*s(ino3)
      r2_com(41) = rk_com(41)*s(iho2)  
                                          
      r_com(42) = rk_com(42)*s(in2o5)*h2o
      r1_com(42) = rk_com(42)*h2o   
                                            
      r_com(43) = rk_com(43)*s(in2o5)
      r1_com(43) = rk_com(43)  
                                                 
      r_com(44) = rk_com(44)*s(ico)*s(ioh)
      r1_com(44) = rk_com(44)*s(ico)
      r2_com(44) = rk_com(44)*s(ioh) 
                                             
      r_com(45) = rk_com(45)*s(iso2)*s(ioh)
      r1_com(45) = rk_com(45)*s(iso2)
      r2_com(45) = rk_com(45)*s(ioh) 
                                            
      r_com(46) = rk_com(46)*s(ich4)*s(ioh)
      r1_com(46) = rk_com(46)*s(ich4)
      r2_com(46) = rk_com(46)*s(ioh)  
                                           
      r_com(47) = rk_com(47)*s(ic2h6)*s(ioh)
      r1_com(47) = rk_com(47)*s(ic2h6)
      r2_com(47) = rk_com(47)*s(ioh) 
                                           
      r_com(48) = rk_com(48)*s(ich3oh)*s(ioh)
      r1_com(48) = rk_com(48)*s(ich3oh)
      r2_com(48) = rk_com(48)*s(ioh)   
                                        
      r_com(49) = rk_com(49)*s(ihcho) 
      r1_com(49) = rk_com(49)  
                                                
      r_com(50) = rk_com(50)*s(ihcho)
      r1_com(50) = rk_com(50)    
                                               
      r_com(51) = rk_com(51)*s(ihcho)*s(ioh)
      r1_com(51) = rk_com(51)*s(ihcho)
      r2_com(51) = rk_com(51)*s(ioh)   
                                         
      r_com(52) = rk_com(52)*s(ihcho)*s(ino3)
      r1_com(52) = rk_com(52)*s(ihcho)
      r2_com(52) = rk_com(52)*s(ino3)  
                                         
      r_com(53) = rk_com(53)*s(ich3ooh) 
      r1_com(53) = rk_com(53) 
                                               
      r_com(54) = rk_com(54)*s(iethooh)
      r1_com(54) = rk_com(54)  
                                               
      r_com(55) = rk_com(55)*s(ich3ooh)*s(ioh)
      r1_com(55) = rk_com(55)*s(ich3ooh)
      r2_com(55) = rk_com(55)*s(ioh)   
                                       
      r_com(56) = rk_com(56)*s(iethooh)*s(ioh)
      r1_com(56) = rk_com(56)*s(iethooh)
      r2_com(56) = rk_com(56)*s(ioh)  
                                        
      r_com(57) = rk_com(57)*s(ich3o2)*s(ino)
      r1_com(57) = rk_com(57)*s(ich3o2)
      r2_com(57) = rk_com(57)*s(ino)  
                                         
      r_com(58) = rk_com(58)*s(iethp)*s(ino)
      r1_com(58) = rk_com(58)*s(iethp)
      r2_com(58) = rk_com(58)*s(ino)  
                                          
      r_com(59) = rk_com(59)*s(ich3o2)*s(ino3)
      r1_com(59) = rk_com(59)*s(ich3o2)
      r2_com(59) = rk_com(59)*s(ino3)
                                         
      r_com(60) = rk_com(60)*s(iethp)*s(ino3)
      r1_com(60) = rk_com(60)*s(iethp)
      r2_com(60) = rk_com(60)*s(ino3) 
                                          
      r_com(61) = rk_com(61)*s(ich3o2)*s(iho2)
      r1_com(61) = rk_com(61)*s(ich3o2)
      r2_com(61) = rk_com(61)*s(iho2)  
                                        
      r_com(62) = rk_com(62)*s(iethp)*s(iho2)
      r1_com(62) = rk_com(62)*s(iethp)
      r2_com(62) = rk_com(62)*s(iho2) 
                                          
      r_com(63) = rk_com(63)*s(ich3o2)
      r1_com(63) = rk_com(63)  
                                                
      r_com(64) = rk_com(64)*s(iethp) 
      r1_com(64) = rk_com(64)  
                                                
      r_com(65) = rk_com(65)*s(ianol)*s(ioh)
      r1_com(65) = rk_com(65)*s(ianol)
      r2_com(65) = rk_com(65)*s(ioh)  
                                          
      r_com(66) = rk_com(66)*s(iald2)
      r1_com(66) = rk_com(66)  
                                                 
      r_com(67) = rk_com(67)*s(iald2)*s(ioh)
      r1_com(67) = rk_com(67)*s(iald2)
      r2_com(67) = rk_com(67)*s(ioh) 
                                          
      r_com(68) = rk_com(68)*s(iald2)*s(ino3)
      r1_com(68) = rk_com(68)*s(iald2) 
      r2_com(68) = rk_com(68)*s(ino3) 
                                         
      r_com(69) = rk_com(69)*s(ic2o3)*s(ino2)
      r1_com(69) = rk_com(69)*s(ic2o3) 
      r2_com(69) = rk_com(69)*s(ino2)  
                                        
      r_com(70) = rk_com(70)*s(ipan)
      r1_com(70) = rk_com(70)   
                                                 
      r_com(71) = rk_com(71)*s(ic2o3)*s(ino)
      r1_com(71) = rk_com(71)*s(ic2o3)
      r2_com(71) = rk_com(71)*s(ino)  
                                          
      r_com(72) = rk_com(72)*s(ic2o3)*s(ino3)
      r1_com(72) = rk_com(72)*s(ic2o3)
      r2_com(72) = rk_com(72)*s(ino3)   
                                        
      r_com(73) = rk_com(73)*s(ic2o3)*s(iho2)
      r1_com(73) = rk_com(73)*s(ic2o3)
      r2_com(73) = rk_com(73)*s(iho2)  
                                         
      r_com(74) = rk_com(74)*s(ic2o3)
      r1_com(74) = rk_com(74)   

!  lijie change for add no3 sinks at 20130327
      r_com(75) = rk_com(75)*s(ino3)*s(ic2h6)
      r1_com(75) = rk_com(75)*s(ino3)
      r2_com(75) = rk_com(75)*s(ic2h6)

      
      r_com(76) = rk_com(76)*s(ino3)*s(ich3oh)
      r1_com(76) = rk_com(76)*s(ino3)
      r2_com(76) = rk_com(76)*s(ich3oh)


      return
      end


!!!-----------------------------------------------------------------
!!!------------------------------------------------------------------
      subroutine gasrates_bio(s)
      include 'chm1.inc'
      include 'gas1.inc'
      dimension s(nmax)

      r_bio(1)  = rk_bio(1)*s(iisop)*s(ioh)
      r1_bio(1)  = rk_bio(1)*s(iisop)
      r2_bio(1)  = rk_bio(1)*s(ioh)

      r_bio(2)  = rk_bio(2)*s(iisop)*s(io3)
      r1_bio(2)  = rk_bio(2)*s(iisop)
      r2_bio(2)  = rk_bio(2)*s(io3)

      r_bio(3)  = rk_bio(3)*s(iisop)*s(ino3)
      r1_bio(3)  = rk_bio(3)*s(iisop)
      r2_bio(3)  = rk_bio(3)*s(ino3)

      r_bio(4)  = rk_bio(4) *s(iisoprd)
      r1_bio(4)  = rk_bio(4)

      r_bio(5)  = rk_bio(5)*s(iisoprd)*s(ioh)
      r1_bio(5)  = rk_bio(5)*s(iisoprd)
      r2_bio(5)  = rk_bio(5)*s(ioh)

      r_bio(6)  = rk_bio(6)*s(iisoprd)*s(io3)
      r1_bio(6)  = rk_bio(6)*s(iisoprd)
      r2_bio(6)  = rk_bio(6)*s(io3)

      r_bio(7)  = rk_bio(7)*s(iisoprd)*s(ino3)
      r1_bio(7)  = rk_bio(7)*s(iisoprd)
      r2_bio(7)  = rk_bio(7)*s(ino3)

      r_bio(8)  = rk_bio(8)*s(iisopp)*s(ino)
      r1_bio(8)  = rk_bio(8)*s(iisopp)
      r2_bio(8)  = rk_bio(8)*s(ino)

      r_bio(9)  = rk_bio(9)*s(iisopn)*s(ino)
      r1_bio(9)  = rk_bio(9)*s(iisopn)
      r2_bio(9)  = rk_bio(9)*s(ino)

      r_bio(10) = rk_bio(10)*s(iisopo2)*s(ino)
      r1_bio(10) = rk_bio(10)*s(iisopo2)
      r2_bio(10) = rk_bio(10)*s(ino)

      r_bio(11) = rk_bio(11)*s(iisopp)*s(iho2)
      r1_bio(11) = rk_bio(11)*s(iisopp)
      r2_bio(11) = rk_bio(11)*s(iho2)

      r_bio(12) = rk_bio(12)*s(iisopn)*s(iho2)
      r1_bio(12) = rk_bio(12)*s(iisopn)
      r2_bio(12) = rk_bio(12)*s(iho2)

      r_bio(13) = rk_bio(13)*s(iisopo2)*s(iho2)
      r1_bio(13) = rk_bio(13)*s(iisopo2)
      r2_bio(13) = rk_bio(13)*s(iho2)

      r_bio(14) = rk_bio(14)*s(iisopp)
      r1_bio(14) = rk_bio(14)

      r_bio(15) = rk_bio(15)*s(iisopn)
      r1_bio(15) = rk_bio(15)

      r_bio(16) = rk_bio(16)*s(iisopo2)
      r1_bio(16) = rk_bio(16)

! li jie added r_bio(17)(18)(19)(20)
      r_bio(17) = rk_bio(17)*s(iterp)*s(io1d)
      r1_bio(17) = rk_bio(17)*s(iterp)
      r2_bio(17) = rk_bio(17)*s(io1d)

      r_bio(18) = rk_bio(18)*s(iterp)*s(ioh)
      r1_bio(18) = rk_bio(18)*s(iterp)
      r2_bio(18) = rk_bio(18)*s(ioh)

      r_bio(19) = rk_bio(19)*s(iterp)*s(io3)
      r1_bio(19) = rk_bio(19)*s(iterp)
      r2_bio(19) = rk_bio(19)*s(io3)

      r_bio(20) = rk_bio(20)*s(iterp)*s(ino3)
      r1_bio(20) = rk_bio(20)*s(iterp)
      r2_bio(20) = rk_bio(20)*s(ino3)
 

      return
      end










!!!-----------------------------------------------------------------
!!!------------------------------------------------------------------
      subroutine gasrates_het(s)
      include 'chm1.inc'
      include 'gas1.inc'
      dimension s(nmax)


!! heterogeneous chemistry

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! to comment all lines in this subroutine and add a new form (by li jie)


      r_het(1) = rk_het(1)*s(in2o5) ! n2o5 + aso4 -> 2hno3
      r1_het(1) = rk_het(1)

      r_het(2) = rk_het(2)*s(ino2)  ! no2  + bc   -> 0.5hono+0.5hno3
      r1_het(2) = rk_het(2)

      r_het(3) = rk_het(3)*s(ino3)  ! no3  + aso4 -> hno3
      r1_het(3) = rk_het(3)

      r_het(4) = rk_het(4)*s(iho2)  ! ho2  + aso4 -> 0.5h2o2
      r1_het(4) = rk_het(4)

      r_het(5) = rk_het(5)*s(ihcho) ! hcho + aso4 -> products
      r1_het(5) = rk_het(5)

      r_het(6) = rk_het(6)*s(ioh)   ! oh   + aso4 -> products
      r1_het(6) = rk_het(6)

      r_het(7) = rk_het(7)*s(io3)   ! o3   + bc   -> products
      r1_het(7) = rk_het(7)

      r_het(8) = rk_het(8)*s(ino2)      ! no2  + bc   -> hono
      r1_het(8) = rk_het(8)

      r_het(9) = rk_het(9)*s(ihno3)     ! hno3 + bc   -> no2
      r1_het(9) = rk_het(9)

      r_het(10) = rk_het(10)*s(in2o5)   ! n2o5 + bc   -> 2hno3
      r1_het(10) = rk_het(10)

      r_het(11) = rk_het(11)*s(io3)     ! o3   + dust -> products
      r1_het(11) = rk_het(11)

      r_het(12) = rk_het(12)*s(ihno3)   ! hno3 + dust -> ano3 
      r1_het(12) = rk_het(12)

      r_het(13) = rk_het(13)*s(ino2)    ! no2  + dust -> 0.5hono + 0.5hno3
      r1_het(13) = rk_het(13)

      r_het(14) = rk_het(14)*s(ino3)    ! no3  + dust -> hno3
      r1_het(14) = rk_het(14)

      r_het(15) = rk_het(15)*s(in2o5)   ! n2o5 + dust -> 2hno3
      r1_het(15) = rk_het(15)

      r_het(16) = rk_het(16)*s(ioh)     ! oh   + dust -> products
      r1_het(16) = rk_het(16)

      r_het(17) = rk_het(17)*s(iho2)    ! ho2  + dust -> 0.5h2o2
      r1_het(17) = rk_het(17)

      r_het(18) = rk_het(18)*s(ih2o2)   ! h2o2 + dust -> products
      r1_het(18) = rk_het(18)

      r_het(19) = rk_het(19)*s(iso2)    ! so2  + dust -> aso4
      r1_het(19) = rk_het(19)

!!!--- rooh belongs to "urb", not "com", should not write it here. (feng fan remarked)
      r_het(20) = rk_het(20)*s(irooh)   ! ch3cooh + dust -> products
      r1_het(20) = rk_het(20)           
!!!---------------------------------------------------------------------------------

      r_het(21) = rk_het(21)*s(ich3oh)   ! ch3oh + dust -> products
      r1_het(21) = rk_het(21)

      r_het(22) = rk_het(22)*s(ihcho)    ! hcho  + dust -> products
      r1_het(22) = rk_het(22)

      r_het(23) = rk_het(23)*s(in2o5)    ! n2o5  + ssa  -> 2hno3
      r1_het(23) = rk_het(23)

      r_het(24) = rk_het(24)*s(ino3)     ! no3   + ssa  -> hno3
      r1_het(24) = rk_het(24)

      r_het(25) = rk_het(25)*s(iho2)     ! ho2   + ssa  -> 0.5hono
      r1_het(25) = rk_het(25)

      r_het(26) = rk_het(26)*s(iso2)     ! so2   + ssa  -> aso4
      r1_het(26) = rk_het(26)

      r_het(27) = rk_het(27)*s(ino3)     ! no3   + ssa  -> ano3
      r1_het(27) = rk_het(27)

      r_het(28) = rk_het(28)*s(ihno3)    ! hno3  + ssa  -> ano3
      r1_het(28) = rk_het(28)


      return
      end

!!!-----------------------------------------------------------------
!!!------------------------------------------------------------------

      subroutine gasrates_mar(s)
      include 'chm1.inc'
      include 'gas1.inc'
      dimension s(nmax)

      r_mar(1) = rk_mar(1)*s(idms)*s(ioh)
      r1_mar(1) = rk_mar(1)*s(idms)
      r2_mar(1) = rk_mar(1)*s(ioh)

      r_mar(2) = rk_mar(2)*s(idms)*s(ino3)
      r1_mar(2) = rk_mar(2)*s(idms)
      r2_mar(2) = rk_mar(2)*s(ino3)

      r_mar(3) = rk_mar(3)*s(idms)*s(io3p)
      r1_mar(3) = rk_mar(3)*s(idms)
      r2_mar(3) = rk_mar(3)*s(io3p)

      r_mar(4) = rk_mar(4)*s(idms)*s(ioh)
      r1_mar(4) = rk_mar(4)*s(idms)
      r2_mar(4) = rk_mar(4)*s(ioh)

      r_mar(5) = rk_mar(5)*s(ich3sch2oo)*s(ino)
      r1_mar(5) = rk_mar(5)*s(ich3sch2oo)
      r2_mar(5) = rk_mar(5)*s(ino)

      r_mar(6) = rk_mar(6)*s(ich3sch2oo)*s(ich3o2)
      r1_mar(6) = rk_mar(6)*s(ich3sch2oo)
      r2_mar(6) = rk_mar(6)*s(ich3o2)

      r_mar(7) = rk_mar(7)*s(idmso)*s(ioh)
      r1_mar(7)  = rk_mar(7)*s(idmso)
      r2_mar(7) = rk_mar(7)*s(ioh)

      r_mar(8) = rk_mar(8)*s(idmso2)*s(ioh)
      r1_mar(8) = rk_mar(8)*s(idmso2)
      r2_mar(8) = rk_mar(8)*s(ioh)

      r_mar(9) = rk_mar(9)*s(ich3so2ch2oo)*s(ino)
      r1_mar(9) = rk_mar(9)*s(ich3so2ch2oo)
      r2_mar(9) = rk_mar(9)*s(ino)

      r_mar(10) = rk_mar(10)*s(ich3so2ch2oo)*s(ich3o2)
      r1_mar(10) = rk_mar(10)*s(ich3so2ch2oo)
      r2_mar(10) = rk_mar(10)*s(ich3o2)

      r_mar(11) = rk_mar(11)*s(ich3so2h)*s(iho2)
      r1_mar(11) = rk_mar(11)*s(ich3so2h)
      r2_mar(11) = rk_mar(11)*s(iho2)

      r_mar(12) = rk_mar(12)*s(ich3so2h)*s(ino3)
      r1_mar(12) = rk_mar(12)*s(ich3so2h)
      r2_mar(12) = rk_mar(12)*s(ino3)

      r_mar(13) = rk_mar(13)*s(ich3so2h)*s(ich3o2)
      r1_mar(13) = rk_mar(13)*s(ich3so2h)
      r2_mar(13) = rk_mar(13)*s(ich3o2)

      r_mar(14) = rk_mar(14)*s(ich3so2h)*s(ioh)
      r1_mar(14) = rk_mar(14)*s(ich3so2h)
      r2_mar(14) = rk_mar(14)*s(ioh)

      r_mar(15) = rk_mar(15)*s(ich3so2h)*s(ich3so3)
      r1_mar(15) = rk_mar(15)*s(ich3so2h)
      r2_mar(15) = rk_mar(15)*s(ich3so3)

      r_mar(16) = rk_mar(16)*s(ich3so2)
      r1_mar(16) = rk_mar(16) 
      

      r_mar(17) = rk_mar(17)*s(ich3so2)*s(ino2)
      r1_mar(17) = rk_mar(17)*s(ich3so2)
      r2_mar(17) = rk_mar(17)*s(ino2)

      r_mar(18) = rk_mar(18)*s(ich3so2)*s(io3)
      r1_mar(18) = rk_mar(18)*s(ich3so2)
      r2_mar(18) = rk_mar(18)*s(io3)

      r_mar(19) = rk_mar(19)*s(ich3so2)*s(iho2)
      r1_mar(19) = rk_mar(19)*s(ich3so2)
      r2_mar(19) = rk_mar(19)*s(iho2)

      r_mar(20) = rk_mar(20)*s(ich3so2)*s(ich3o2)
      r1_mar(20) = rk_mar(20)*s(ich3so2)
      r2_mar(20) = rk_mar(20)*s(ich3o2)

      r_mar(21) = rk_mar(21)*s(ich3so2)*s(ioh)
      r1_mar(21) = rk_mar(21)*s(ich3so2)
      r2_mar(21) = rk_mar(21)*s(ioh)

      r_mar(22) = rk_mar(22)*s(ich3so2)*o2
      r1_mar(22) = rk_mar(22)*o2

      r_mar(23) = rk_mar(23)*s(ich3so2oo)
      r1_mar(23) = rk_mar(23)  

      r_mar(24) = rk_mar(24)*s(ich3so2oo)*s(ino)
      r1_mar(24) = rk_mar(24)*s(ich3so2oo)
      r2_mar(24) = rk_mar(24)*s(ino)

      r_mar(25) = rk_mar(25)*s(ich3so2oo)*s(ich3o2)
      r1_mar(25) = rk_mar(25)*s(ich3so2oo)
      r2_mar(25) = rk_mar(25)*s(ich3o2)

      r_mar(26) = rk_mar(26)*s(ich3so3)
      r1_mar(26) = rk_mar(26)  

      r_mar(27) = rk_mar(27)*s(ich3so3)*s(ino2)
      r1_mar(27) = rk_mar(27)*s(ich3so3)
      r2_mar(27) = rk_mar(27)*s(ino2)

      r_mar(28) = rk_mar(28)*s(ich3so3)*s(ino)
      r1_mar(28) = rk_mar(28)*s(ich3so3)
      r2_mar(28) = rk_mar(28)*s(ino)

      r_mar(29) = rk_mar(29)*s(ich3so3)*s(iho2)
      r1_mar(29) = rk_mar(29)*s(ich3so3)
      r2_mar(29) = rk_mar(29)*s(iho2)

      r_mar(30) = rk_mar(30)*s(ich3so3)*s(ihcho)
      r1_mar(30) = rk_mar(30)*s(ich3so3)
      r2_mar(30) = rk_mar(30)*s(ihcho)

      r_mar(31) = rk_mar(31)*s(ich3sch2oo)*s(ich3so2)
      r1_mar(31) = rk_mar(31)*s(ich3sch2oo)
      r2_mar(31) = rk_mar(31)*s(ich3so2)

      r_mar(32) = rk_mar(32)*s(ich3sch2oo)*s(ich3sch2oo)
      r1_mar(32) = rk_mar(32)*s(ich3sch2oo)
      

      r_mar(33) = rk_mar(33)*s(iso2)
      r1_mar(33) = rk_mar(33) 

      r_mar(34) = rk_mar(34)*s(idmso)
      r1_mar(34) = rk_mar(34) 

      r_mar(35) = rk_mar(35)*s(idmso2)
      r1_mar(35) = rk_mar(35)

      return
      end
!!!-----------------------------------------------------------------
!!!------------------------------------------------------------------
      subroutine gasrates_urb(s)
      include 'chm1.inc'
      include 'gas1.inc'
      dimension s(nmax)

      r_urb(1) = rk_urb(1)*s(ipar)*s(ioh)
      r1_urb(1) = rk_urb(1)*s(ipar)
      r2_urb(1) = rk_urb(1)*s(ioh)  

                                             
      r_urb(2) = rk_urb(2)*s(iaone)
      r1_urb(2) = rk_urb(2)
     

                                                    
      r_urb(3) = rk_urb(3)*s(iaone)*s(ioh)
      r1_urb(3) = rk_urb(3)*s(iaone)
      r2_urb(3) = rk_urb(3)*s(ioh)    

                                            
      r_urb(4) = rk_urb(4)*s(imgly) 
      r1_urb(4) = rk_urb(4) 
   

                                                   
      r_urb(5) = rk_urb(5)*s(imgly)*s(ioh) 
      r1_urb(5) = rk_urb(5)*s(imgly)
      r2_urb(5) = rk_urb(5)*s(ioh)  

                                            
      r_urb(6) = rk_urb(6)*s(imgly)*s(ino3)
      r1_urb(6) = rk_urb(6)*s(imgly)
      r2_urb(6) = rk_urb(6)*s(ino3)  

                                           
      r_urb(7) = rk_urb(7)*s(ieth)*s(io3)
      r1_urb(7) = rk_urb(7)*s(ieth) 
      r2_urb(7) = rk_urb(7)*s(io3)   

                                             
      r_urb(8) = rk_urb(8)*s(ieth)*s(ioh) 
      r1_urb(8) = rk_urb(8)*s(ieth)
      r2_urb(8) = rk_urb(8)*s(ioh) 

                                             
      r_urb(9) = rk_urb(9)*s(iolet)*s(io3) 
      r1_urb(9) = rk_urb(9)*s(iolet)
      r2_urb(9) = rk_urb(9)*s(io3) 

                                            
      r_urb(10) = rk_urb(10)*s(iolei)*s(io3)  
      r1_urb(10) = rk_urb(10)*s(iolei)
      r2_urb(10) = rk_urb(10)*s(io3)

                                          
      r_urb(11) = rk_urb(11)*s(iolet)*s(ioh) 
      r1_urb(11) = rk_urb(11)*s(iolet)
      r2_urb(11) = rk_urb(11)*s(ioh) 

                                          
      r_urb(12) = rk_urb(12)*s(iolei)*s(ioh) 
      r1_urb(12) = rk_urb(12)*s(iolei)
      r2_urb(12) = rk_urb(12)*s(ioh)   

                                         
      r_urb(13) = rk_urb(13)*s(iolet)*s(ino3) 
      r1_urb(13) = rk_urb(13)*s(iolet)
      r2_urb(13) = rk_urb(13)*s(ino3)  

                                        
      r_urb(14) = rk_urb(14)*s(iolei)*s(ino3) 
      r1_urb(14) = rk_urb(14)*s(iolei)
      r2_urb(14) = rk_urb(14)*s(ino3)   

                                        
      r_urb(15) = rk_urb(15)*s(itol)*s(ioh)  
      r1_urb(15) = rk_urb(15)*s(itol) 
      r2_urb(15) = rk_urb(15)*s(ioh) 

                                           
      r_urb(16) = rk_urb(16)*s(ixyl)*s(ioh) 
      r1_urb(16) = rk_urb(16)*s(ixyl)
      r2_urb(16) = rk_urb(16)*s(ioh)  

                                          
      r_urb(17) = rk_urb(17)*s(ito2)*s(ino)  
      r1_urb(17) = rk_urb(17)*s(ito2)
      r2_urb(17) = rk_urb(17)*s(ino)  

                                         
      r_urb(18) = rk_urb(18)*s(icres)*s(ioh)  
      r1_urb(18) = rk_urb(18)*s(icres)
      r2_urb(18) = rk_urb(18)*s(ioh)  

                                         
      r_urb(19) = rk_urb(19)*s(icres)*s(ino3)  
      r1_urb(19) = rk_urb(19)*s(icres) 
      r2_urb(19) = rk_urb(19)*s(ino3) 

                                         
      r_urb(20) = rk_urb(20)*s(icro)*s(ino2)  
      r1_urb(20) = rk_urb(20)*s(icro)  
      r2_urb(20) = rk_urb(20)*s(ino2)  

                                          
      r_urb(21) = rk_urb(21)*s(iopen)*s(ioh)  
      r1_urb(21) = rk_urb(21)*s(iopen)
      r2_urb(21) = rk_urb(21)*s(ioh)   

                                         
      r_urb(22) = rk_urb(22)*s(iopen)   
      r1_urb(22) = rk_urb(22)

                                               
      r_urb(23) = rk_urb(23)*s(iopen)*s(io3)  
      r1_urb(23) = rk_urb(23)*s(iopen)
      r2_urb(23) = rk_urb(23)*s(io3)

                                          
      r_urb(24) = rk_urb(24)*s(irooh) 
      r1_urb(24) = rk_urb(24)  

                                                
      r_urb(25) = rk_urb(25)*s(irooh)*s(ioh)  
      r1_urb(25) = rk_urb(25)*s(irooh)
      r2_urb(25) = rk_urb(25)*s(ioh)
                                          
      r_urb(26) = rk_urb(26)*s(ionit)*s(ioh) 
      r1_urb(26) = rk_urb(26)*s(ionit)
      r2_urb(26) = rk_urb(26)*s(ioh) 

                                          
      r_urb(27) = rk_urb(27)*s(ionit)
      r1_urb(27) = rk_urb(27)

                                                
      r_urb(28) = rk_urb(28)*s(iro2)*s(ino) 
      r1_urb(28) = rk_urb(28)*s(iro2)
      r2_urb(28) = rk_urb(28)*s(ino)   

                                          
      r_urb(29) = rk_urb(29)*s(iano2)*s(ino) 
      r1_urb(29) = rk_urb(29)*s(iano2) 
      r2_urb(29) = rk_urb(29)*s(ino)   

                                          
      r_urb(30) = rk_urb(30)*s(inap)*s(ino)  
      r1_urb(30) = rk_urb(30)*s(inap)
      r2_urb(30) = rk_urb(30)*s(ino)   

                                          
      r_urb(31) = rk_urb(31)*s(ixo2)*s(ino) 
      r1_urb(31) = rk_urb(31)*s(ixo2)
      r2_urb(31) = rk_urb(31)*s(ino) 

                                          
      r_urb(32) = rk_urb(32)*s(iro2)*s(ino3) 
      r1_urb(32) = rk_urb(32)*s(iro2)
      r2_urb(32) = rk_urb(32)*s(ino3)  

                                         
      r_urb(33) = rk_urb(33)*s(iano2)*s(ino3) 
      r1_urb(33) = rk_urb(33)*s(iano2)
      r2_urb(33) = rk_urb(33)*s(ino3)    

                                       
      r_urb(34) = rk_urb(34)*s(inap)*s(ino3) 
      r1_urb(34) = rk_urb(34)*s(inap)
      r2_urb(34) = rk_urb(34)*s(ino3)     

                                       
      r_urb(35) = rk_urb(35)*s(ixo2)*s(ino3) 
      r1_urb(35) = rk_urb(35)*s(ixo2)
      r2_urb(35) = rk_urb(35)*s(ino3)    

                                        
      r_urb(36) = rk_urb(36)*s(iro2)*s(iho2) 
      r1_urb(36) = rk_urb(36)*s(iro2)
      r2_urb(36) = rk_urb(36)*s(iho2)   

                                         
      r_urb(37) = rk_urb(37)*s(iano2)*s(iho2) 
      r1_urb(37) = rk_urb(37)*s(iano2)
      r2_urb(37) = rk_urb(37)*s(iho2)   

                                        
      r_urb(38) = rk_urb(38)*s(inap)*s(iho2) 
      r1_urb(38) = rk_urb(38)*s(inap)
      r2_urb(38) = rk_urb(38)*s(iho2)   

                                         
      r_urb(39) = rk_urb(39)*s(ixo2)*s(iho2)  
      r1_urb(39) = rk_urb(39)*s(ixo2)
      r2_urb(39) = rk_urb(39)*s(iho2) 

                                          
      r_urb(40) = rk_urb(40)*s(iro2)  
      r1_urb(40) = rk_urb(40)
     
                                                
      r_urb(41) = rk_urb(41)*s(iano2) 
      r1_urb(41) = rk_urb(41)
     
                                              
      r_urb(42) = rk_urb(42)*s(inap) 
      r1_urb(42) = rk_urb(42)
   
                                                
      r_urb(43) = rk_urb(43)*s(ixo2)  
      r1_urb(43) = rk_urb(43)

                                                  
      r_urb(44) = rk_urb(44)*s(ipar)*s(ixpar)                                           
      r1_urb(44) = rk_urb(44)*s(ipar)
      r2_urb(44) = rk_urb(44)*s(ixpar)   



!! move from r_com to r_urb by feng fan on 20140822

      r_urb(45) = rk_urb(45)*s(ino3)*s(ipar)
      r1_urb(45) = rk_urb(45)*s(ino3)
      r2_urb(45) = rk_urb(45)*s(ipar)

      r_urb(46) = rk_urb(46)*s(ino3)*s(ieth)
      r1_urb(46) = rk_urb(46)*s(ino3)
      r2_urb(46) = rk_urb(46)*s(ieth)

      r_urb(47) = rk_urb(47)*s(ino3)*s(itol)
      r1_urb(47) = rk_urb(47)*s(ino3)
      r2_urb(47) = rk_urb(47)*s(itol)

 
      r_urb(48) = rk_urb(48)*s(ino3)*s(iaone)
      r1_urb(48) = rk_urb(48)*s(ino3)
      r2_urb(48) = rk_urb(48)*s(iaone)

      r_urb(49) = rk_urb(49)*s(ino3)*s(ixyl)
      r1_urb(49) = rk_urb(49)*s(ino3)
      r2_urb(49) = rk_urb(49)*s(ixyl)                                                


      return
      end




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




!!*********************************************************************
!!*********************************************************************
      subroutine ode_com
      include 'chm1.inc'
      include 'gas1.inc'

      p_com(ih2so4)= r_com(45)                                                  
      l_com(ih2so4)= 0.0                                                            



      p_com(ihno3)= r_com(24)+.3*r_com(41)+r_com(42)+r_com(42)+r_com(52)+r_com(68)
                                                           
      l_com(ihno3)= r1_com(4)+r1_com(27)   

                                                   

      p_com(ihcl)= 0.0                                                              
      l_com(ihcl)= 0.0                                                              

      p_com(inh3)= 0.0                                                              
      l_com(inh3)= 0.0                                                              



      p_com(ino)= r_com(1)+0.11*r_com(2)+r_com(3)+r_com(15)+r_com(38)  
                                 
      l_com(ino)= r1_com(17)+r1_com(18)+r1_com(23)+r1_com(33)+r1_com(37)  &
                  +r1_com(57)+r1_com(58)+r1_com(71)                                                       



      p_com(ino2)= 0.89*r_com(2)+r_com(4)+r_com(5) &
             +r_com(6)+r_com(17)+r_com(18)     &                        
             +r_com(25)+r_com(26)+r_com(28)    &
             +r_com(33)+r_com(36)+r_com(37)    &                          
             +r_com(37)+r_com(38)+r_com(40)+r_com(40)+.7*r_com(41)   &                              
             +r_com(43)+r_com(57)+r_com(58)+r_com(59)                &
             +r_com(60)+r_com(70)+r_com(71)+r_com(72)
              


                                                  
      l_com(ino2)= r1_com(1)+r1_com(15)+r1_com(16)+r1_com(19)+r1_com(24)  &
                 +r1_com(34)+r1_com(35)+r1_com(38)+r1_com(39)+r1_com(69) 

                                         

      p_com(ino3)= r_com(6)+r_com(16)+r_com(19)+r_com(27)+r_com(43) 
                                    
      l_com(ino3)= r1_com(2)+r1_com(25)+r2_com(37)+r2_com(38)    &
             +r2_com(39)+r1_com(40)                            &       
             +r1_com(40)+r2_com(41)+r1_com(52)+r1_com(59)        &
             +r1_com(60)+r1_com(68)                            &                      
             +r1_com(72)    &                                                        
             +r2_com(75)+r2_com(76) !lijie add no3+vocs on 20130327  
               
                                                    




      p_com(in2o5)= r_com(39)                                                           
      l_com(in2o5)= r1_com(6)+r1_com(42)+r1_com(43)                                                

      p_com(ihono)= r_com(23)+r_com(35)                                                     
      l_com(ihono)= r1_com(3)+r1_com(26)                                                      

      p_com(ihno4)= r_com(34)                                                           
      l_com(ihno4)= r1_com(5)+r1_com(28)+r1_com(36)                                                




      p_com(io3)= r_com(13)+.4*r_com(73) 
                                                   
      l_com(io3)= r1_com(7)+r1_com(8)+r1_com(14)+r2_com(18)  &
                 +r2_com(19)+r2_com(20)+r2_com(21)                                                             




      p_com(io1d)= r_com(8)                                                             
      l_com(io1d)= r1_com(10)+r1_com(11)+r1_com(12)                                                




      p_com(io3p)= r_com(1)+0.89*r_com(2)+r_com(7)        &
                  +r_com(10)+r_com(11)

                                  
      l_com(io3p)= r1_com(13)+r2_com(14)+r2_com(15)+r2_com(16)+r2_com(17)


                                    

      p_com(ioh)= r_com(3)+r_com(4)+2*r_com(9)   &
            +2*r_com(12)+r_com(21)+r_com(33)     &                         
            +.7*r_com(41)+r_com(53)+r_com(54)    &
            +.3*r_com(55)+.5*r_com(56) 
                           
      l_com(ioh)= r1_com(20)+r1_com(22)+r2_com(23)        &
            +r2_com(24)+r2_com(25)+r2_com(26)             &                  
            +r2_com(27)+r2_com(28)+r2_com(29)+r2_com(30)   &
            +r1_com(44)+r1_com(45)                       &        
            +r1_com(46)+r1_com(47)+r1_com(48)             &
            +r1_com(51)+r1_com(55)+r1_com(56)             &                  
            +r1_com(65)+r1_com(67)                                                       



      p_com(iho2)= r_com(5)+r_com(20)+r_com(22)        &
             +r_com(25)+r_com(30)+r_com(36)            &                   
             +r_com(44)+r_com(45)+r_com(48)            &
             +2*r_com(49)+r_com(51)                    &              
             +r_com(52)+r_com(53)+r_com(54)            &
             +r_com(57)+r_com(58)+r_com(59)            &                  
             +r_com(60)+.32*r_com(63)+.6*r_com(64)     &
             +r_com(65)+r_com(66) 
                            
      l_com(iho2)= r1_com(21)+r1_com(29)+r1_com(31)    &
             +r1_com(31)+r1_com(32)+r1_com(32)         &                     
             +r2_com(33)+r2_com(34)+r2_com(35)         &
             +r1_com(41)+r1_com(61)+r1_com(62)         &                     
             +r1_com(73)                                                            



      p_com(ih2o2)= r_com(31)+r_com(32)                                                     
      l_com(ih2o2)= r1_com(9)+r1_com(30)                                                      

      p_com(ico)= r_com(49)+r_com(50)+r_com(51)     &
                 +r_com(52)+r_com(66)
                                     
      l_com(ico)= r2_com(44)                                                             



      p_com(iso2)= 0.0                                                              
      l_com(iso2)= r2_com(45)                                                            

      p_com(ich4)= 0.0                                                              
      l_com(ich4)= r2_com(46)                                                            

      p_com(ic2h6)= .2*r_com(64)                                                        
      l_com(ic2h6)= r2_com(47)                                                           

      p_com(ich3o2)= r_com(46)+.7*r_com(55)+r_com(66)   &
               +r_com(71)+r_com(72)                     &              
               +r_com(74) 
                                                         
      l_com(ich3o2)= r2_com(57)+r2_com(59)+r2_com(61)+r1_com(63) 

                                       

      p_com(iethp)= r_com(47)+.5*r_com(56) 
                                                 
      l_com(iethp)= r2_com(58)+r2_com(60)+r2_com(62)+r1_com(64) 

                                        

      p_com(ihcho)= r_com(48)+r_com(53)+.3*r_com(55)   &
              +r_com(57)+r_com(59)                     &           
              +.66*r_com(63)
               
                                                      
      l_com(ihcho)= r1_com(49)+r1_com(50)+r2_com(51)+r2_com(52)                                         



      p_com(ich3oh)= .34*r_com(63)                                                      
      l_com(ich3oh)= r2_com(48)                                                          

      p_com(ianol)= 0.0                                                             
      l_com(ianol)= r2_com(65)                                                           

      p_com(ich3ooh)= r_com(61)                                                         
      l_com(ich3ooh)= r1_com(53)+r2_com(55)                                                   

      p_com(iethooh)= r_com(62)                                                         
      l_com(iethooh)= r1_com(54)+r2_com(56)                                                   



      p_com(iald2)= r_com(54)+.5*r_com(56)+r_com(58) &
              +r_com(60)+.8*r_com(64)                &             
              +r_com(65)   
                                                        
      l_com(iald2)= r1_com(66)+r2_com(67)+r2_com(68)                                               



      p_com(ihcooh)= 0.0                                                            
      l_com(ihcooh)= 0.0                                                            

      p_com(ircooh)= .4*r_com(73)                                                       
      l_com(ircooh)= 0.0                                                            



      p_com(ic2o3)= r_com(67)+r_com(68)+r_com(70) 
                                              
      l_com(ic2o3)= r2_com(69)+r2_com(71)+r2_com(72) &
             +r2_com(73)+r1_com(74)    

                               

      p_com(ipan)= r_com(69)                                                            
      l_com(ipan)= r1_com(70)  


     !!! species dso4 and dno3 are added by li jie.  0.0 below is a temporary value. it should be changed.                   
      p_com(idso4)= 0.0                                         
      l_com(idso4)= 0.0   

      p_com(idno3)= 0.0                                                            
      l_com(idno3)= 0.0  


      return
      end


!!*********************************************************************
!!*********************************************************************
      subroutine ode_bio
      include 'chm1.inc'
      include 'gas1.inc'
   
      p_bio(ino)= 0.0
      l_bio(ino)= r1_bio(8)+r1_bio(9)+r1_bio(10)

      p_bio(ino2)= .91*r_bio(8)+1.2*r_bio(9)+r_bio(10)+0.47*r_bio(20) !(20)added by li jie
      l_bio(ino2)= 0.0

      p_bio(ino3)= 0.0
      l_bio(ino3)= r1_bio(3)+r1_bio(7)+r1_bio(20) !(20)added by li jie

      p_bio(ihno3)= .07*r_bio(7)
      l_bio(ihno3)= 0.0

      p_bio(io3)= 0.0
      l_bio(io3)= r1_bio(2)+r1_bio(6)+r1_bio(19) !(19)added by li jie

      p_bio(ioh)= .27*r_bio(2)+.27*r_bio(6)+0.57*r_bio(19) !(19)added by li jie
      l_bio(ioh)= r1_bio(1)+r1_bio(5)+r1_bio(18)     !(18)added by li jie

      p_bio(iho2)= .07*r_bio(2)+.33*r_bio(4)+.1*r_bio(6)+.93*r_bio(7)+.91*r_bio(8)+.8*r_bio(9)+r_bio(10) &
                   +.75*r_bio(18)+.07*r_bio(19)+.28*r_bio(20) !(18)(19)(20)added by li jie

      l_bio(iho2)= r1_bio(11)+r1_bio(12)+r1_bio(13)

      p_bio(ih2o2)= 0.0
      l_bio(ih2o2)= 0.0

      p_bio(ico)= .07*r_bio(2)+.33*r_bio(4)+.16*r_bio(6)+.64*r_bio(7)+.59*r_bio(10) &
                  +0.001*r_bio(19) !(19)added by li jie

      l_bio(ico)= 0.0

      p_bio(ihcho)= .6*r_bio(2)+.2*r_bio(4)+.15*r_bio(6)+.28*r_bio(7)+.63*r_bio(8)+.25*r_bio(10) &
                    +.28*r_bio(18)+.24*r_bio(19) !(18)(19)added by li jie

      l_bio(ihcho)= 0.0


      p_bio(iald2)= .15*r_bio(2)+.07*r_bio(4)+.02*r_bio(6)+.28*r_bio(7)+.8*r_bio(9)+.55*r_bio(10)+r_bio(15)+.5*r_bio(16) &
                    +.15*r_bio(17)+.47*r_bio(18)+.21*r_bio(19)+0.47*r_bio(20)  !(17)(18)(19)(20)added by li jie

      l_bio(iald2)= 0.0


      p_bio(ipar)= 1.86*r_bio(7)+.18*r_bio(8)+1.6*r_bio(9)+2*r_bio(12)+2*r_bio(15) &
                   +0.51*r_bio(17)+7.*r_bio(19) !(17)(19)added by li jie
      l_bio(ipar)= 0.0

      p_bio(iaone)= .03*r_bio(4)+.09*r_bio(6)+.63*r_bio(10)+.5*r_bio(16)
      l_bio(iaone)= 0.0

      p_bio(imgly)= .85*r_bio(6)+.34*r_bio(10)
      l_bio(imgly)= 0.0

      p_bio(ionit)= .93*r_bio(7)+.09*r_bio(8)+.8*r_bio(9)+r_bio(12)+r_bio(15) &
                    +0.53*r_bio(20) !(20)added by li jie
      l_bio(ionit)= 0.0

      p_bio(ircooh)= .39*r_bio(2)+.46*r_bio(6)
      l_bio(ircooh)= 0.0

      p_bio(irooh)= r_bio(11)+r_bio(13)
      l_bio(irooh)= 0.0

      p_bio(ich3o2)= .7*r_bio(4)+.05*r_bio(6)
      l_bio(ich3o2)= 0.0

      p_bio(ic2o3)= .2*r_bio(2)+.97*r_bio(4)+.5*r_bio(5)+.11*r_bio(6)+.07*r_bio(7)
      l_bio(ic2o3)= 0.0

      p_bio(ixo2)= .08*r_bio(1)+.2*r_bio(2)+.2*r_bio(5)+.07*r_bio(6)+.93*r_bio(7) &
                   +1.25*r_bio(18)+.76*r_bio(19)+.76*r_bio(20) !(18)(19)(20)added by li jie
      l_bio(ixo2)= 0.0

      p_bio(iisop)= 0.0
      l_bio(iisop)= r2_bio(1)+r2_bio(2)+r2_bio(3)

      p_bio(iisoprd)= .65*r_bio(2)+.91*r_bio(8)+.2*r_bio(9)+r_bio(14)
      l_bio(iisoprd)= r1_bio(4)+r2_bio(5)+r2_bio(6)+r2_bio(7)

      p_bio(iisopp)= r_bio(1)
      l_bio(iisopp)= r2_bio(8)+r2_bio(11)+r1_bio(14)

      p_bio(iisopn)= r_bio(3)
      l_bio(iisopn)= r2_bio(9)+r2_bio(12)+r1_bio(15)

      p_bio(iisopo2)= .5*r_bio(5)
      l_bio(iisopo2)= r2_bio(10)+r2_bio(13)+r1_bio(16)

!!! the following p_bio(io1d),l_bio(io1d),sv3,sv4,sv5,sv6,terp are added by li jie 

      p_bio(io1d)= 0.0
      l_bio(io1d)= r1_bio(17)

      p_bio(isv3) = .110681*r_bio(17)+.063304*r_bio(18)+.10998*r_bio(19)+.092287*r_bio(20)
      l_bio(isv3) = 0.0

      p_bio(isv4) = .4172*r_bio(17)+.324763*r_bio(18)+.389277*r_bio(19)+.395069*r_bio(20)
      l_bio(isv4) = 0.0

      p_bio(isv5) = .232*r_bio(1)
      l_bio(isv5) = 0.0

      p_bio(isv6) = .0288*r_bio(1)
      l_bio(isv6) = 0.0

      p_bio(iterp) = 0.0
      l_bio(iterp) = r2_bio(17)+r2_bio(18)+r2_bio(19)+r2_bio(20)

      return
      end









!!*********************************************************************
!!*********************************************************************
      subroutine ode_het
      include 'chm1.inc'
      include 'gas1.inc'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! to comment all lines in this subroutine and add a new form (by lijie)




       p_het(in2o5) = 0.0
       l_het(in2o5) = r1_het(1) + r1_het(10) + r1_het(15) + r1_het(23)

       p_het(ino2)  = r_het(9)
       l_het(ino2)  = r1_het(8) + r1_het(2) +r1_het(13)

       p_het(ino3)  = 0.0
       l_het(ino3)  = r1_het(3) + r1_het(14) + r1_het(24) + r1_het(27)

       p_het(iho2)  = 0.0
       l_het(iho2)  = r1_het(4) + r1_het(17) + r1_het(25)

       p_het(ihcho) = 0.0
       l_het(ihcho) = r1_het(5) + r1_het(22)

       p_het(ioh)   = 0.0
       l_het(ioh)   = r1_het(6) + r1_het(16)

       p_het(io3)   = 0.0
       l_het(io3)   = r1_het(7) + r1_het(11)


       p_het(ihno3) = 2.*r_het(1) + 0.5*r_het(2) + r_het(3) + 2.* r_het(10)  &
                    + 0.5*r_het(13) + r_het(14) + 2.*r_het(15)       &
                     + 2.*r_het(23) + r_het(24)

       l_het(ihno3) = r1_het(9) + r1_het(12) +r1_het(28)


       p_het(ihono) = 0.5*r_het(2) + r_het(8) + 0.5*r_het(13) + 0.5 * r_het(25)
                      
       l_het(ihono) = 0.0

       p_het(ih2o2) = 0.5 * r_het(4) + 0.5 * r_het(17)
       l_het(ih2o2) = r1_het(18)

       p_het(iso2) = 0.0
       l_het(iso2) = r1_het(19) + r1_het(26)

       p_het(irooh) = 0.0
       l_het(irooh) = r1_het(20)

       p_het(ich3oh) = 0.0
       l_het(ich3oh) = r1_het(21)

       p_het(idso4) = r_het(19) + r_het(26)
       l_het(idso4) = 0.0

       p_het(idno3) = r_het(12) + r_het(27)+r_het(28)
       l_het(idno3) = 0.0


            
      return
      end





!!*********************************************************************
!!*********************************************************************
      subroutine ode_mar
      include 'chm1.inc'
      include 'gas1.inc'


      a = 5.e+5/(5.e+5 + o2*3.e-12)
      b = 1.5e+7/(1.5e+7 + o2*1.2e-12)


      p_mar(ino)= r_mar(17) 
  
      l_mar(ino)= r1_mar(5)+r1_mar(9)+r1_mar(24)+r1_mar(28)



      p_mar(ino2)= r_mar(5)+r_mar(9)+r_mar(24)   

      l_mar(ino2)= r1_mar(17)+r1_mar(27)



      p_mar(ino3)=   0.0
      l_mar(ino3)= r1_mar(2)+r1_mar(12)   



      p_mar(ihono)= r_mar(28)   
      l_mar(ihono)=   0.0



      p_mar(ihno3)= r_mar(2)+r_mar(12)+r_mar(27)   
      l_mar(ihno3)=   0.0



      p_mar(io3)=   0.0
      l_mar(io3)= r1_mar(18)   



      p_mar(io3p)=   0.0
      l_mar(io3p)= r1_mar(3)    



      p_mar(ioh)= r_mar(19)  
 
      l_mar(ioh)= r1_mar(1)+r1_mar(4)+r1_mar(7)+r1_mar(8)+  &
                 r1_mar(14)+r1_mar(21)   



      p_mar(iho2)= (1.-a)*r_mar(4)+r_mar(6)+(1.-b)*r_mar(7)+  &
                  r_mar(10)+r_mar(20)+r_mar(25)+r_mar(30)   

      l_mar(iho2)= r1_mar(11)+r1_mar(19)+r1_mar(29)   



      p_mar(ih2o2)= r_mar(11)   
      l_mar(ih2o2)=   0.0



      p_mar(iso2)= r_mar(16)   
      l_mar(iso2)= r1_mar(33)



      p_mar(ih2so4)= r_mar(26)   
      l_mar(ih2so4)=   0.0



      p_mar(ich3o2)= r_mar(3)+a*r_mar(4)+b*r_mar(7)+r_mar(16)+   &
                   r_mar(26)   

      l_mar(ich3o2)= r1_mar(6)+r1_mar(10)+r1_mar(13)+r1_mar(20)+    &
                    r1_mar(25)   



      p_mar(ich3ooh)= r_mar(13)   
      l_mar(ich3ooh)=   0.0



      p_mar(ihcho)= r_mar(5)+r_mar(6)+r_mar(6)+r_mar(9)+     &
                   r_mar(10)+r_mar(10)+r_mar(20)+r_mar(25) 
  
      l_mar(ihcho)= r1_mar(30)   



      p_mar(idms)= 0.0
      l_mar(idms)= r2_mar(1)+r2_mar(2)+r2_mar(3)+r2_mar(4)    



      p_mar(imsa)= r_mar(15)+r_mar(21)+r_mar(27)+r_mar(28)+     &
                  r_mar(29)+r_mar(30)   
      l_mar(imsa)=   0.0



      p_mar(idmso)= (1.-a)*r_mar(4)   
 
      l_mar(idmso)= r2_mar(7) + r1_mar(34)



      p_mar(idmso2)= (1.-b)*r_mar(7)   
 
      l_mar(idmso2)= r2_mar(8) + r1_mar(35)



      p_mar(ich3so2h)= b*r_mar(7)   
 
      l_mar(ich3so2h)= r2_mar(11)+r2_mar(12)+r2_mar(13)+r2_mar(14)+    &
                      r2_mar(15)   



      p_mar(ich3sch2oo)= r_mar(1)+r_mar(2)

      l_mar(ich3sch2oo)= r2_mar(5)+r2_mar(6)+r2_mar(31)+2.*r1_mar(32)



      p_mar(ich3so2)= r_mar(3)+a*r_mar(4)+r_mar(5)+r_mar(6)+     &
                      r_mar(9)+r_mar(10)+r_mar(11)+r_mar(12)+    &
                      r_mar(13)+r_mar(14)+r_mar(15)+r_mar(23)+   &
                      r_mar(31)+1.85*r_mar(32)

      l_mar(ich3so2)= r1_mar(16)+r2_mar(17)+r2_mar(18)+r2_mar(19)+  &
                     r2_mar(20)+r2_mar(21)+r1_mar(22)+r1_mar(31)



      p_mar(ich3so3)= r_mar(17)+r_mar(18)+r_mar(19)+r_mar(20)+  &
                      r_mar(24)+r_mar(25)+r_mar(31)

      l_mar(ich3so3)= r1_mar(15)+r1_mar(26)+r2_mar(27)+r2_mar(28)+  &
                      r2_mar(29)+r2_mar(30)  

 

      p_mar(ich3so2oo)= r_mar(22) 
  
      l_mar(ich3so2oo)= r1_mar(23)+r2_mar(24)+r2_mar(25) 

  

      p_mar(ich3so2ch2oo)= r_mar(8) 
   
      l_mar(ich3so2ch2oo)= r2_mar(9)+r2_mar(10)



      p_mar(isulfhox)= 0.15*r_mar(32)
      l_mar(isulfhox)= 0.0



      return
      end




!!*********************************************************************
!!*********************************************************************
      subroutine ode_urb
      include 'chm1.inc'
      include 'gas1.inc'

      p_urb(ihno3)= r_urb(6)+r_urb(19)                                                      
      l_urb(ihno3)= 0.0                                                             

      p_urb(ino)= 0.0                                                               
      l_urb(ino)= r1_urb(17)+r1_urb(28)+r1_urb(29)+r1_urb(30)+r1_urb(31)    

                                 

      p_urb(ino2)= .95*r_urb(17)+r_urb(27)+.84*r_urb(28)+r_urb(29)  &                                 
             +1.5*r_urb(30)+r_urb(31)+r_urb(32)+r_urb(33)           &
             +1.5*r_urb(34)+r_urb(35)+.5*r_urb(42)                  &
             +r_urb(46)  !move from p_com(ino2) by feng fan on 20140822  
                                            
      l_urb(ino2)= r1_urb(20)                                                            



      p_urb(ino3)= 0.0                                                              
      l_urb(ino3)= r1_urb(6)+r1_urb(13)+r1_urb(14)+r1_urb(19)     &
             +r1_urb(32)+r1_urb(33)+r1_urb(34)+r1_urb(35)         &
             +r2_urb(45)+r2_urb(46)+r2_urb(47)+r2_urb(48)+r2_urb(49)  !move from l_com(ino3) by feng fan on 20140822

                                                    

      p_urb(io3)= 0.0                                                               
      l_urb(io3)= r1_urb(7)+r1_urb(9)+r1_urb(10)+r1_urb(23)    
                                         

      p_urb(ioh)= .12*r_urb(7)+.33*r_urb(9)+.6*r_urb(10)      &
            +.08*r_urb(23)+r_urb(24)+.23*r_urb(25) 
                                                  
      l_urb(ioh)= r1_urb(1)+r1_urb(3)+r1_urb(5)                      &
            +r1_urb(8)+r1_urb(11)+r1_urb(12)+r1_urb(15)               &              
            +r1_urb(16)+r1_urb(18)+r1_urb(21)+r1_urb(25)+r1_urb(26)                                     



      p_urb(iho2)= r_urb(4)+.22*r_urb(7)+r_urb(8)                 &
             +.26*r_urb(9)+.22*r_urb(10)                          &  
             +r_urb(11)+r_urb(12)+.2*r_urb(15)                    &
             +.55*r_urb(16)+.95*r_urb(17)                         &
             +.6*r_urb(18)+2*r_urb(21)+r_urb(22)+.76*r_urb(23)    &                            
             +.9*r_urb(24)+.9*r_urb(27)+.76*r_urb(28)             &                
             +.5*r_urb(30)+.9*r_urb(32)+.5*r_urb(34)              &
             +.54*r_urb(40)    
                                  
      l_urb(iho2)= r1_urb(36)+r1_urb(37)+r1_urb(38)+r1_urb(39)                                          



      p_urb(ico)= r_urb(4)+r_urb(6)+.24*r_urb(7)       &
            +.31*r_urb(9)+.3*r_urb(10)                 &             
            +2*r_urb(21)+r_urb(22)+.69*r_urb(23)                                           
      l_urb(ico)= 0.0                                                               



      p_urb(ich3o2)= r_urb(2)+.07*r_urb(9)+.1*r_urb(10)                                         
      l_urb(ich3o2)= 0.0                                                            



      p_urb(iethp)= .06*r_urb(9)+.05*r_urb(10)+.1*r_urb(24)    &
              +.1*r_urb(27)                                    &
              +.08*r_urb(28)+.1*r_urb(32)+.06*r_urb(40)                                    
      l_urb(iethp)= 0.0                                                             



      p_urb(ihcho)= r_urb(7)+1.56*r_urb(8)+.57*r_urb(9)        &
              +r_urb(11)+r_urb(21)                             &
              +.7*r_urb(23)+r_urb(29)+.5*r_urb(30)             &
              +r_urb(33)+.5*r_urb(34)                          &
              +.7*r_urb(41)+.5*r_urb(42)                       &
              + 2.0*r_urb(46) !move from p_com(ihcho) by feng fan on 20140822 
                                             
      l_urb(ihcho)= 0.0                                                             



      p_urb(ich3oh)= .03*r_urb(9)+.04*r_urb(10)                                             
      l_urb(ich3oh)= 0.0                                                            



      p_urb(iald2)= .22*r_urb(8)+.47*r_urb(9)+1.03*r_urb(10)    &
              +r_urb(11)                                        &
              +1.77*r_urb(12)+.03*r_urb(23)+.3*r_urb(24)        &
              +.04*r_urb(25)                                    &
              +.3*r_urb(27)+.25*r_urb(28)+.5*r_urb(30)          &
              +.3*r_urb(32)                                     &
              +.5*r_urb(34)+.21*r_urb(40)+.5*r_urb(42)                                     
      l_urb(iald2)= 0.0                                                             



      p_urb(ihcooh)= .52*r_urb(7)+.22*r_urb(9)                                              
      l_urb(ihcooh)= 0.0                                                            



      p_urb(ircooh)= .09*r_urb(9)+.16*r_urb(10)                                             
      l_urb(ircooh)= 0.0                                                            



      p_urb(ic2o3)= r_urb(2)+r_urb(4)+r_urb(5)+r_urb(6)         &
              +.13*r_urb(9)+.19*r_urb(10)                       &   
              +r_urb(21)+r_urb(22)+.62*r_urb(23)                &
              +r_urb(29)+r_urb(33)                              & 
              +.7*r_urb(41)                                                        
      l_urb(ic2o3)= 0.0                                                             



      p_urb(ipar)= 1.1*r_urb(16)    
                                                    
      l_urb(ipar)= r2_urb(1)+r2_urb(44)                                                       



      p_urb(iaone)= .07*r_urb(10)+.23*r_urb(12)             &
              +.74*r_urb(24)+.74*r_urb(27)                  &       
              +.62*r_urb(28)+.74*r_urb(32)                  &
              +.57*r_urb(40)+.15*r_urb(41) 
                        
      l_urb(iaone)= r1_urb(2)+r2_urb(3)                                                       



      p_urb(imgly)= .04*r_urb(9)+.07*r_urb(10)     &
              +.8*r_urb(16)+.2*r_urb(23)           &                 
              +.19*r_urb(25)+.15*r_urb(41)  
                                           
      l_urb(imgly)= r1_urb(4)+r2_urb(5)+r2_urb(6)  

                                                

      p_urb(ieth)= 0.0                                                              
      l_urb(ieth)= r2_urb(7)+r2_urb(8)   

                                                     
 
      p_urb(iolet)= 0.0                                                             
      l_urb(iolet)= r2_urb(9)+r2_urb(11)+r2_urb(13)   

                                             

      p_urb(iolei)= 0.0                                                             
      l_urb(iolei)= r2_urb(10)+r2_urb(12)+r2_urb(14)   

                                            

      p_urb(itol)= 0.0                                                              
      l_urb(itol)= r2_urb(15)   

                                                         

      p_urb(ixyl)= 0.0                                                              
      l_urb(ixyl)= r2_urb(16)   

                                                         

      p_urb(icres)= .12*r_urb(15)+.05*r_urb(16)  
                                           
      l_urb(icres)= r2_urb(18)+r2_urb(19)  

                                                   

      p_urb(ito2)= .8*r_urb(15)+.45*r_urb(16) 
                                              
      l_urb(ito2)= r2_urb(17) 

                                                           

      p_urb(icro)= .4*r_urb(18)+r_urb(19)   
                                                
      l_urb(icro)= r2_urb(20)  

                                                          

      p_urb(iopen)= .95*r_urb(17)+.3*r_urb(18)  
                                            
      l_urb(iopen)= r2_urb(21)+r1_urb(22)+r2_urb(23) 

                                              

      p_urb(ionit)= .05*r_urb(17)+r_urb(20)    &
              +.16*r_urb(28)+.5*r_urb(30)      &                        
              +.5*r_urb(34)+r_urb(38)+.5*r_urb(42)   
                                      
      l_urb(ionit)= r2_urb(26)+r1_urb(27)  
                        
                           

      p_urb(irooh)= r_urb(36)+r_urb(37)  
                                                   
      l_urb(irooh)= r1_urb(24)+r2_urb(25)  

                                                   

      p_urb(iro2)= r_urb(1)+.03*r_urb(9)+.09*r_urb(10) &
             +.77*r_urb(25)  
                              
      l_urb(iro2)= r2_urb(28)+r2_urb(32)+r2_urb(36)+r1_urb(40)  

                                        

      p_urb(iano2)= r_urb(3)+.11*r_urb(10)  
                                                
      l_urb(iano2)= r2_urb(29)+r2_urb(33)+r2_urb(37)+r1_urb(41)  

                                       

      p_urb(inap)= r_urb(13)+r_urb(14)+r_urb(26) 
                                               
      l_urb(inap)= r2_urb(30)+r2_urb(34)+r2_urb(38)+r1_urb(42) 

                                         

      p_urb(ixo2)= r_urb(5)+r_urb(8)+r_urb(11)+r_urb(12)          &
             +.08*r_urb(15)                                       &
             +.5*r_urb(16)+.6*r_urb(18)+r_urb(21)+.03*r_urb(23)   &                             
             +.4*r_urb(24)+.41*r_urb(27)+.34*r_urb(28)            &
             +.4*r_urb(32)+.24*r_urb(40)                          &
             +r_urb(46) ! move from p_com to p_urb by feng fan on 20140822
                                                    
      l_urb(ixo2)= r2_urb(31)+r2_urb(35)+r2_urb(39)+r1_urb(43) 

                                         

      p_urb(ixpar)= 1.06*r_urb(9)+2.26*r_urb(10)                  &
              +r_urb(11)+2.23*r_urb(12)                           &
              +1.98*r_urb(24)+.42*r_urb(25)+1.98*r_urb(27)        &                         
              +1.68*r_urb(28)+r_urb(30)+1.98*r_urb(32)+r_urb(34)  &                             
              +1.25*r_urb(40)+r_urb(42) 
                                               
      l_urb(ixpar)= r1_urb(44)     


!!! the following sv1,sv2 added by li jie

      p_urb(isv1) = r_urb(15)*0.071+r_urb(16)*0.038+r_urb(1)*0.0718+r_urb(18)*0.05
      l_urb(isv1) = 0.0

      p_urb(isv2) = r_urb(15)*0.138+r_urb(16)*0.167
      l_urb(isv2) = 0.0     
     

      return
      end



!!*********************************************************************
!! subroutine mapgas_com: maps cnn to and fro stot for the common 
!!                        gas-phase mechanism.
!!
!! nomenclature:
!! cnn       = full species concentration array.
!! stot      = subset of cnn. species concentration array to be supplied to
!!             lsodes. length of stot depends on the selected mechanism
!! iregime   = selected chemical regime (1-6)
!! imap      = 0 : map cnn to stot
!!           = 1 : map stot to cnn
!! 
!! author: rahul a. zaveri
!! date  : february 1996
!!
!!------------------------------------------------------------------------
      subroutine mapgas_com(stot,imap)
      include 'chm1.inc'
      include 'gas1.inc'
      dimension stot(nmax)

      emit(ih2so4)	=emission(kh2so4)
      emit(ihno3)	=emission(khno3)
      emit(ihcl)	=emission(khcl)
      emit(inh3)	=emission(knh3)
      emit(ino)		=emission(kno)
      emit(ino2)	=emission(kno2)
      emit(ino3)	=emission(kno3)
      emit(in2o5)	=emission(kn2o5)
      emit(ihono)	=emission(khono)
      emit(ihno4)	=emission(khno4)
      emit(io3)		=emission(ko3)
      emit(io1d)	=emission(ko1d)
      emit(io3p)	=emission(ko3p)
      emit(ioh)		=emission(koh)
      emit(iho2)	=emission(kho2)
      emit(ih2o2)	=emission(kh2o2)
      emit(ico)		=emission(kco)
      emit(iso2)	=emission(kso2)
      emit(ich4)	=emission(kch4)
      emit(ic2h6)	=emission(kc2h6)
      emit(ich3o2)	=emission(kch3o2)
      emit(iethp)	=emission(kethp)
      emit(ihcho)	=emission(khcho)
      emit(ich3oh)	=emission(kch3oh)
      emit(ianol)	=emission(kanol)
      emit(ich3ooh)	=emission(kch3ooh)
      emit(iethooh)	=emission(kethooh)
      emit(iald2)	=emission(kald2)
      emit(ihcooh)	=emission(khcooh)
      emit(ircooh)	=emission(krcooh)
      emit(ic2o3)	=emission(kc2o3)
      emit(ipan)	=emission(kpan)
      emit(idso4)       =emission(kdso4)
      emit(idno3)       =emission(kdno3)

      if(imap.eq.0)then    ! map cnn into stot
      stot(ih2so4)	=cnn(kh2so4)
      stot(ihno3)	=cnn(khno3)
      stot(ihcl)	=cnn(khcl)
      stot(inh3)	=cnn(knh3)
      stot(ino)		=cnn(kno)
      stot(ino2)	=cnn(kno2)
      stot(ino3)	=cnn(kno3)
      stot(in2o5)	=cnn(kn2o5)
      stot(ihono)	=cnn(khono)
      stot(ihno4)	=cnn(khno4)
      stot(io3)		=cnn(ko3)
      stot(io1d)	=cnn(ko1d)
      stot(io3p)	=cnn(ko3p)
      stot(ioh)		=cnn(koh)
      stot(iho2)	=cnn(kho2)
      stot(ih2o2)	=cnn(kh2o2)
      stot(ico)		=cnn(kco)
      stot(iso2)	=cnn(kso2)
      stot(ich4)	=cnn(kch4)
      stot(ic2h6)	=cnn(kc2h6)
      stot(ich3o2)	=cnn(kch3o2)
      stot(iethp)	=cnn(kethp)
      stot(ihcho)	=cnn(khcho)
      stot(ich3oh)	=cnn(kch3oh)
      stot(ianol)	=cnn(kanol)
      stot(ich3ooh)	=cnn(kch3ooh)
      stot(iethooh)	=cnn(kethooh)
      stot(iald2)	=cnn(kald2)
      stot(ihcooh)	=cnn(khcooh)
      stot(ircooh)	=cnn(krcooh)
      stot(ic2o3)	=cnn(kc2o3)
      stot(ipan)	=cnn(kpan)
      stot(idso4)       =cnn(kdso4)
      stot(idno3)       =cnn(kdno3)

 




      else                 ! map stot back into cnn
      cnn(kh2so4)	=stot(ih2so4)
      cnn(khno3)	=stot(ihno3)
      cnn(khcl)		=stot(ihcl)
      cnn(knh3)		=stot(inh3)
      cnn(kno)		=stot(ino)
      cnn(kno2)		=stot(ino2)
      cnn(kno3)		=stot(ino3)
      cnn(kn2o5)	=stot(in2o5)
      cnn(khono)	=stot(ihono)
      cnn(khno4)	=stot(ihno4)
      cnn(ko3)		=stot(io3)
      cnn(ko1d)		=stot(io1d)
      cnn(ko3p)		=stot(io3p)
      cnn(koh)		=stot(ioh)
      cnn(kho2)		=stot(iho2)
      cnn(kh2o2)	=stot(ih2o2)
      cnn(kco)		=stot(ico)
      cnn(kso2)		=stot(iso2)
      cnn(kch4)		=stot(ich4)
      cnn(kc2h6)	=stot(ic2h6)
      cnn(kch3o2)	=stot(ich3o2)
      cnn(kethp)	=stot(iethp)
      cnn(khcho)	=stot(ihcho)
      cnn(kch3oh)	=stot(ich3oh)
      cnn(kanol)	=stot(ianol)
      cnn(kch3ooh)	=stot(ich3ooh)
      cnn(kethooh)	=stot(iethooh)
      cnn(kald2)	=stot(iald2)
      cnn(khcooh)	=stot(ihcooh)
      cnn(krcooh)	=stot(ircooh)
      cnn(kc2o3)	=stot(ic2o3)
      cnn(kpan)		=stot(ipan)
      cnn(kdso4)        =stot(idso4)
      cnn(kdno3)        =stot(idno3)

      endif

      return
      end





!!***********************************************************************
!! subroutine mapgas_bio: maps cnn to and fro stot for the biogeni!! 
!!                        gas-phase mechanism.
!!
!! nomenclature:
!! cnn       = full species concentration array.
!! stot      = subset of cnn. species concentration array to be supplied to
!!             lsodes. length of stot depends on the selected mechanism
!! iregime   = selected chemical regime (1-6)
!! imap      = 0 : map cnn to stot
!!           = 1 : map stot to cnn
!! 
!! author: rahul a. zaveri
!! date  : february 1996
!!
!!-------------------------------------------------------------------------
      subroutine mapgas_bio(stot,imap)
      include 'chm1.inc'
      include 'gas1.inc'
      dimension stot(nmax)

      emit(iisop)	=emission(kisop)
      emit(iisoprd)	=emission(kisoprd)
      emit(iisopp)	=emission(kisopp)
      emit(iisopn)	=emission(kisopn)
      emit(iisopo2)	=emission(kisopo2)
      emit(iterp)       =emission(kterp)
      emit(isv3)        =emission(ksv3)
      emit(isv4)        =emission(ksv4)
      emit(isv5)        =emission(ksv5)
      emit(isv6)        =emission(ksv6)

      if(imap.eq.0)then    ! map cnn into stot
      stot(iisop)	=cnn(kisop)
      stot(iisoprd)	=cnn(kisoprd)
      stot(iisopp)	=cnn(kisopp)
      stot(iisopn)	=cnn(kisopn)
      stot(iisopo2)	=cnn(kisopo2)
      stot(iterp)       =cnn(kterp)
      stot(isv3)        =cnn(ksv3)
      stot(isv4)        =cnn(ksv4)
      stot(isv5)        =cnn(ksv5)
      stot(isv6)        =cnn(ksv6)


      else                 ! map stot back into cnn
      cnn(kisop)	=stot(iisop)
      cnn(kisoprd)	=stot(iisoprd)
      cnn(kisopp)	=stot(iisopp)
      cnn(kisopn)	=stot(iisopn)
      cnn(kisopo2)	=stot(iisopo2)
      cnn(kterp)        =stot(iterp)
      cnn(ksv3)         =stot(isv3)
      cnn(ksv4)         =stot(isv4)
      cnn(ksv5)         =stot(isv5)
      cnn(ksv6)         =stot(isv6) 

      endif

      return
      end










!!*********************************************************************
!! subroutine mapgas_mar: maps cnn to and fro stot for the marine
!!                        gas-phase mechanism.
!!
!! nomenclature:
!! cnn       = full species concentration array.
!! stot      = subset of cnn. species concentration array to be supplied to
!!             lsodes. length of stot depends on the selected mechanism
!! iregime   = selected chemical regime (1-6)
!! imap      = 0 : map cnn to stot
!!           = 1 : map stot to cnn
!! 
!! author: rahul a. zaveri
!! date  : february 1996
!!
!!------------------------------------------------------------------------
      subroutine mapgas_mar(stot,imap)
      include 'chm1.inc'
      include 'gas1.inc'
      dimension stot(nmax)

      emit(idms)	=emission(kdms)
      emit(imsa)	=emission(kmsa)
      emit(idmso)	=emission(kdmso)
      emit(idmso2)	=emission(kdmso2)
      emit(ich3so2h)	=emission(kch3so2h)
      emit(ich3sch2oo)	=emission(kch3sch2oo)
      emit(ich3so2)	=emission(kch3so2)
      emit(ich3so3)	=emission(kch3so3)
      emit(ich3so2oo)	=emission(kch3so2oo)
      emit(ich3so2ch2oo)=emission(kch3so2ch2oo)
      emit(isulfhox)	=emission(ksulfhox)

      if(imap.eq.0)then    ! map cnn into stot
      stot(idms)	=cnn(kdms)
      stot(imsa)	=cnn(kmsa)
      stot(idmso)	=cnn(kdmso)
      stot(idmso2)	=cnn(kdmso2)
      stot(ich3so2h)	=cnn(kch3so2h)
      stot(ich3sch2oo)	=cnn(kch3sch2oo)
      stot(ich3so2)	=cnn(kch3so2)
      stot(ich3so3)	=cnn(kch3so3)
      stot(ich3so2oo)	=cnn(kch3so2oo)
      stot(ich3so2ch2oo)=cnn(kch3so2ch2oo)
      stot(isulfhox)	=cnn(ksulfhox)


      else                 ! map stot back into cnn
      cnn(kdms)		=stot(idms)
      cnn(kmsa)		=stot(imsa)
      cnn(kdmso)	=stot(idmso)
      cnn(kdmso2)	=stot(idmso2)
      cnn(kch3so2h)	=stot(ich3so2h)
      cnn(kch3sch2oo)	=stot(ich3sch2oo)
      cnn(kch3so2)	=stot(ich3so2)
      cnn(kch3so3)	=stot(ich3so3)
      cnn(kch3so2oo)	=stot(ich3so2oo)
      cnn(kch3so2ch2oo)	=stot(ich3so2ch2oo)
      cnn(ksulfhox)	=stot(isulfhox)
      endif

      return
      end













!!*********************************************************************
!! subroutine mapgas_urb: maps cnn to and fro stot for the urban 
!!                        gas-phase mechanism.
!!
!! nomenclature:
!! cnn       = full species concentration array.
!! stot      = subset of cnn. species concentration array to be supplied to
!!             lsodes. length of stot depends on the selected mechanism
!! iregime   = selected chemical regime (1-6)
!! imap      = 0 : map cnn to stot
!!           = 1 : map stot to cnn
!! 
!! author: rahul a. zaveri
!! date  : february 1996
!!
!!------------------------------------------------------------------------
      subroutine mapgas_urb(stot,imap)
      include 'chm1.inc'
      include 'gas1.inc'
      dimension stot(nmax)

      emit(ipar)	=emission(kpar)
      emit(iaone)	=emission(kaone)
      emit(imgly)	=emission(kmgly)
      emit(ieth)	=emission(keth)
      emit(iolet)	=emission(kolet)
      emit(iolei)	=emission(kolei)
      emit(itol)	=emission(ktol)
      emit(ixyl)	=emission(kxyl)
      emit(icres)	=emission(kcres)
      emit(ito2)	=emission(kto2)
      emit(icro)	=emission(kcro)
      emit(iopen)	=emission(kopen)
      emit(ionit)	=emission(konit)
      emit(irooh)	=emission(krooh)
      emit(iro2)	=emission(kro2)
      emit(iano2)	=emission(kano2)
      emit(inap)	=emission(knap)
      emit(ixo2)	=emission(kxo2)
      emit(ixpar)	=emission(kxpar)
      emit(isv1)        =emission(ksv1)
      emit(isv2)        =emission(ksv2)

      if(imap.eq.0)then    ! map cnn into stot
      stot(ipar)	=cnn(kpar)
      stot(iaone)	=cnn(kaone)
      stot(imgly)	=cnn(kmgly)
      stot(ieth)	=cnn(keth)
      stot(iolet)	=cnn(kolet)
      stot(iolei)	=cnn(kolei)
      stot(itol)	=cnn(ktol)
      stot(ixyl)	=cnn(kxyl)
      stot(icres)	=cnn(kcres)
      stot(ito2)	=cnn(kto2)
      stot(icro)	=cnn(kcro)
      stot(iopen)	=cnn(kopen)
      stot(ionit)	=cnn(konit)
      stot(irooh)	=cnn(krooh)
      stot(iro2)	=cnn(kro2)
      stot(iano2)	=cnn(kano2)
      stot(inap)	=cnn(knap)
      stot(ixo2)	=cnn(kxo2)
      stot(ixpar)	=cnn(kxpar)
      stot(isv1)        =cnn(ksv1)
      stot(isv2)        =cnn(ksv2)

 
      else                 ! map stot back into cnn
      cnn(kpar)		=stot(ipar)
      cnn(kaone)	=stot(iaone)
      cnn(kmgly)	=stot(imgly)
      cnn(keth)		=stot(ieth)
      cnn(kolet)	=stot(iolet)
      cnn(kolei)	=stot(iolei)
      cnn(ktol)		=stot(itol)
      cnn(kxyl)		=stot(ixyl)
      cnn(kcres)	=stot(icres)
      cnn(kto2)		=stot(ito2)
      cnn(kcro)		=stot(icro)
      cnn(kopen)	=stot(iopen)
      cnn(konit)	=stot(ionit)
      cnn(krooh)	=stot(irooh)
      cnn(kro2)		=stot(iro2)
      cnn(kano2)	=stot(iano2)
      cnn(knap)		=stot(inap)
      cnn(kxo2)		=stot(ixo2)
      cnn(kxpar)	=stot(ixpar)
      cnn(ksv1)         =stot(isv1)
      cnn(ksv2)         =stot(isv2)

      endif

      return
      end







!!*********************************************************************
!! subroutine setgas_com: sets up gas-phase species indices for 
!! the selected mechanism.
!!
!! author: rahul a. zaveri
!! date  : february 1996
!!------------------------------------------------------------------------
      subroutine setgas_com(ilast)
      include 'chm1.inc'
      include 'gas1.inc'

      ih2so     = 1
      ihno3     = 2
      ihcl      = 3
      inh3      = 4
      ino       = 5
      ino2      = 6
      ino3      = 7
      in2o5     = 8
      ihono     = 9
      ihno4     = 10
      io3       = 11
      io1d      = 12
      io3p      = 13
      ioh       = 14
      iho2      = 15
      ih2o2     = 16

      ico       = 17
      iso2      = 18
      ich4      = 19
      ic2h6     = 20
      ich3o2    = 21
      iethp     = 22
      ihcho     = 23
      ich3oh    = 24
      ianol     = 25
      ich3ooh   = 26
      iethooh   = 27
      iald2     = 28
      ihcoh     = 29
      ircooh    = 30
      ic2o3     = 31
      ipan      = 32
      idso4     = 33
      idno3     = 34

      ilast	= idno3
 
      return
      end






!!*********************************************************************
!! subroutine setgas_bio: sets up gas-phase species indices for 
!! the selected mechanism.
!!
!! author: rahul a. zaveri
!! date  : february 1996
!!------------------------------------------------------------------------
      subroutine setgas_bio(ilast)
      include 'chm1.inc'
      include 'gas1.inc'

      iisop	= ilast + 1
      iisoprd	= ilast + 2
      iisopp	= ilast + 3
      iisopn	= ilast + 4
      iisopo2	= ilast + 5
      iterp     = ilast + 6
      isv3      = ilast + 7
      isv4      = ilast + 8
      isv5      = ilast + 9
      isv6      = ilast + 10
      
      ilast	= isv6

      return
      end








!!*********************************************************************
!! subroutine setgas_mar: sets up gas-phase species indices for 
!! the selected mechanism.
!!
!! author: rahul a. zaveri
!! date  : february 1996
!!------------------------------------------------------------------------
      subroutine setgas_mar(ilast)
      include 'chm1.inc'
      include 'gas1.inc'

      idms         = ilast + 1
      imsa         = ilast + 2
      idmso        = ilast + 3
      idmso2       = ilast + 4
      ich3so2h     = ilast + 5
      ich3sch2oo   = ilast + 6
      ich3so2      = ilast + 7
      ich3so3      = ilast + 8
      ich3so2oo    = ilast + 9
      ich3so2ch2oo = ilast + 10
      isulfhox     = ilast + 11

      ilast	   = isulfhox

      return
      end












!!*********************************************************************
!! subroutine setgas_urb: sets up gas-phase species indices for 
!! the selected mechanism.
!!
!! author: rahul a. zaveri
!! date  : february 1996
!!------------------------------------------------------------------------
      subroutine setgas_urb(ilast)
      include 'chm1.inc'
      include 'gas1.inc'

      ipar	= ilast + 1
      iaone	= ilast + 2
      imgly	= ilast + 3
      ieth	= ilast + 4
      iolet	= ilast + 5
      iolei	= ilast + 6
      itol	= ilast + 7
      ixyl	= ilast + 8
      icres	= ilast + 9
      ito2	= ilast + 10
      icro	= ilast + 11
      iopen	= ilast + 12
      ionit	= ilast + 13
      irooh	= ilast + 14
      iro2	= ilast + 15
      iano2	= ilast + 16
      inap	= ilast + 17
      ixo2	= ilast + 18
      ixpar	= ilast + 19
      isv1      = ilast + 20
      isv2      = ilast + 21
      
      ilast	= isv2

      return
      end



!!*****************************************************************
!!*****************************************************************
        block data

!!   place various constant or initial values into common

      include 'chm1.inc'
      include 'gas1.inc'

      common /eh0001/ mesflg, lunit
      common /ls0001/ rowns(209), &
        ccmax, el0, h, hmin, hmxi, hu, rc, tn, uround, &
        illin, init, lyh, lewt, lacor, lsavf, lwm, liwm, &
        mxstep, mxhnil, nhnil, ntrep, nslast, nyh, iowns(6), &
        icf, ierpj, iersl, jcur, jstart, kflag, l, meth, miter, &
        maxord, maxcor, msbp, mxncf, n, nq, nst, nfe, nje, nqu


!!-------------------------------------------------
!!     define fundamental constants...
      data pi		/3.141592654/
      data avogad	/6.02217e+23/
      data deg2rad	/0.017453293/

!!--------------------------------
!! define species indices

!! species in inorgani!! chemistry
      data kh2so4       /  1/
      data khno3        /  2/
      data khcl         /  3/
      data knh3         /  4/
      data kno          /  5/
      data kno2         /  6/
      data kno3         /  7/
      data kn2o5        /  8/
      data khono        /  9/
      data khno4        / 10/
      data ko3          / 11/
      data ko1d         / 12/
      data ko3p         / 13/
      data koh          / 14/
      data kho2         / 15/
      data kh2o2        / 16/
      data kco          / 17/
      data kso2         / 18/

!! species in methane, ethane, formaldehyde chemistry
      data kch4         / 19/
      data kc2h6        / 20/
      data kch3o2       / 21/
      data kethp        / 22/
      data khcho        / 23/
      data kch3oh       / 24/
      data kanol	/ 25/
      data kch3ooh      / 26/
      data kethooh	/ 27/
      data kald2        / 28/
      data khcooh	/ 29/
      data krcooh	/ 30/
      data kc2o3	/ 31/
      data kpan		/ 32/
 
!! species in hc1 mechanism. initialize indices to zero
      data kpar         / 33/
      data kaone        / 34/
      data kmgly        / 35/
      data keth         / 36/
      data kolet        / 37/
      data kolei        / 38/
      data ktol         / 39/
      data kxyl         / 40/
      data kcres        / 41/
      data kto2         / 42/
      data kcro         / 43/
      data kopen        / 44/
      data konit        / 45/
      data krooh	/ 46/
      data kro2         / 47/
      data kano2	/ 48/
      data knap		/ 49/
      data kxo2		/ 50/
      data kxpar	/ 51/

!! species in hc2 mechanism. initialize indices to zero
      data kisop	/ 52/
      data kisoprd	/ 53/
      data kisopp	/ 54/
      data kisopn	/ 55/
      data kisopo2	/ 56/

!! species in dms mechanism. initialize indices to zero
      data kdms         / 57/
      data kmsa         / 58/
      data kdmso        / 59/
      data kdmso2       / 60/
      data kch3so2h     / 61/
      data kch3sch2oo   / 62/
      data kch3so2      / 63/
      data kch3so3      / 64/
      data kch3so2oo    / 65/
      data kch3so2ch2oo / 66/
      data ksulfhox     / 67/
!! species in soa formation 2 species in urban, 5 species in biogenic
      data kterp        / 68/ 
      data ksv1         / 69/
      data ksv2         / 70/
      data ksv3         / 71/
      data ksv4         / 72/
      data ksv5         / 73/
      data ksv6         / 74/
!! species in aso4 and ano3 from heteorogeneous chemistry
      data kdso4        / 75/
      data kdno3        / 76/

!! regime-dependent chemistry definitions

      data iregime	/  1/

!!     gas

!! species in common chemistry
      data ih2so4       /  1/
      data ihno3        /  2/
      data ihcl         /  3/
      data inh3         /  4/
      data ino          /  5/
      data ino2         /  6/
      data ino3         /  7/
      data in2o5        /  8/
      data ihono        /  9/
      data ihno4        / 10/
      data io3          / 11/
      data io1d         / 12/
      data io3p         / 13/
      data ioh          / 14/
      data iho2         / 15/
      data ih2o2        / 16/
      data ico          / 17/
      data iso2         / 18/

!! species in methane, ethane, formaldehyde chemistry
      data ich4         / 19/
      data ic2h6        / 20/
      data ich3o2       / 21/
      data iethp        / 22/
      data ihcho        / 23/
      data ich3oh       / 24/
      data ianol	/ 25/
      data ich3ooh      / 26/
      data iethooh	/ 27/
      data iald2        / 28/
      data ihcooh	/ 29/
      data ircooh	/ 30/
      data ic2o3	/ 31/
      data ipan		/ 32/

!! species in hc1 mechanism. initialize indices to zero
      data ipar         / 33/
      data iaone        / 34/
      data imgly        / 35/
      data ieth         / 36/
      data iolet        / 37/
      data iolei        / 38/
      data itol         / 39/
      data ixyl         / 40/
      data icres        / 41/
      data ito2         / 42/
      data icro         / 43/
      data iopen        / 44/
      data ionit        / 45/
      data irooh	/ 46/
      data iro2         / 47/
      data iano2	/ 48/
      data inap		/ 49/
      data ixo2		/ 50/
      data ixpar	/ 51/

!! species in hc2 mechanism. initialize indices to zero
      data iisop	/ 52/
      data iisoprd	/ 53/
      data iisopp	/ 54/
      data iisopn	/ 55/
      data iisopo2	/ 56/

!! species in dms mechanism. initialize indices to zero
      data idms         / 57/
      data imsa         / 58/
      data idmso        / 59/
      data idmso2       / 60/
      data ich3so2h     / 61/
      data ich3sch2oo   / 62/
      data ich3so2      / 63/
      data ich3so3      / 64/
      data ich3so2oo    / 65/
      data ich3so2ch2oo / 66/
      data isulfhox     / 67/
!! intial values for soa formation
      data iterp        / 68/
      data isv1         / 69/
      data isv2         / 70/
      data isv3         / 71/
      data isv4         / 72/
      data isv5         / 73/
      data isv6         / 74/
!! intial values for aso4 and ano3 from heterogeneous chemistry
      data idso4        / 75/
      data idno3        / 76/

!! alkylperoxy radical indices for parameterized permutation reactions
      data jch3o2	/  1/
      data jethp	/  2/
      data jro2		/  3/
      data jc2o3	/  4/
      data jano2	/  5/
      data jnap		/  6/
      data jisopp	/  7/
      data jisopn	/  8/
      data jisopo2	/  9/
      data jxo2		/ 10/

!! photolyzing species indices
      data jphoto_no2	/  1/
      data jphoto_no3	/  2/
      data jphoto_hono	/  3/
      data jphoto_hno3	/  4/
      data jphoto_hno4	/  5/
      data jphoto_n2o5  /  6/
      data jphoto_o3a	/  7/
      data jphoto_o3b	/  8/
      data jphoto_h2o2	/  9/
      data jphoto_hchoa	/ 10/
      data jphoto_hchob	/ 11/
      data jphoto_ch3ooh/ 12/
      data jphoto_ethooh/ 13/
      data jphoto_ald2	/ 14/
      data jphoto_aone	/ 15/
      data jphoto_mgly	/ 16/
      data jphoto_open	/ 17/
      data jphoto_rooh	/ 18/
      data jphoto_onit	/ 19/
      data jphoto_isoprd/ 20/


!! lsodes parameters
      data illin/0/, ntrep/0/
      data mesflg/1/, lunit/6/

      end






!!************************************************************************
!! subroutine gasrateconstants: generates thermal rate coefficients 
!!                   for the selected mechanism
!! nomenclature:
!! rk_com    = reaction rate constants for common mechanism (mole!!c!!s)
!! rk_urb    = reaction rate constants for hc1 mechanism    (mole!!c!!s)
!! rk_bio    = reaction rate constants for hc2 mechanism    (mole!!c!!s)
!! rk_mar    = reaction rate constants for marine mechanism (mole!!c!!s)
!! te        = ambient atmospheri!! temperature (k)
!! iregime = selected mechanism for the current chemical regime (1-6) 
!! 
!! author: rahul a. zaveri
!! date  : february 1996
!!------------------------------------------------------------------------
      subroutine gasrateconstants(factcld)
      include 'chm1.inc'
      include 'gas1.inc'

      if(msolar.eq.1)then
       call solarzenithangle		! calculates cos_sza
       call photoconstants_solar(factcld)	! natural diurnal variation
      elseif(msolar.eq.2)then
       call photoconstants_fixed	! artificial as in a smog chamber
      endif

      call gasrateconstants_het

      goto (1,2,3,4,5,6), iregime

1     call gasrateconstants_com
      return

2     call gasrateconstants_com
      call gasrateconstants_urb
      return

3     call gasrateconstants_com
      call gasrateconstants_urb
      call gasrateconstants_bio
      return

4     call gasrateconstants_com
      call gasrateconstants_mar
      return
 
5     call gasrateconstants_com
      call gasrateconstants_urb
      call gasrateconstants_mar
      return

6     call gasrateconstants_com
      call gasrateconstants_urb
      call gasrateconstants_bio
      call gasrateconstants_mar
      return

      end




!!************************************************************************
!! subroutine gasrate: calculates reaction rates for the selected mechanism
!!
!! nomenclature:
!! r_com, r_urb, r_bio, r_mar = reaction rates (molec/cc/sec)
!! rk_com,rk_urb,rk_bio,rk_mar= rate constants in appropriate units
!! s                          = species concentrations (molec/cc)
!! o2                         = oxygen concentration   (molec/cc)
!! cair_mlc!! (used for m)      = air concentration      (molec/cc)
!! h2o                        = water vapor            (molec/cc)
!!
!! author: rahul a. zaveri
!! date  : february 1996
!!
!!-------------------------------------------------------------------------
      subroutine gasrates(s)
      include 'chm1.inc'
      include 'gas1.inc'

      dimension s(nmax)


      call gasrates_het(s)

      goto (1,2,3,4,5,6), iregime

1     call gasrates_com(s)
      return

2     call gasrates_com(s)
      call gasrates_urb(s)
      return

3     call gasrates_com(s)
      call gasrates_urb(s)
      call gasrates_bio(s)
      return

4     call gasrates_com(s)
      call gasrates_mar(s)
      return
 
5     call gasrates_com(s)
      call gasrates_urb(s)
      call gasrates_mar(s)
      return

6     call gasrates_com(s)
      call gasrates_urb(s)
      call gasrates_bio(s)
      call gasrates_mar(s)
      return

      end




!!*********************************************************************
!! subroutine mapgasspecies: maps cnn to and fro stot for the selected 
!!                           gas-phase mechanism.
!!
!! nomenclature:
!! cnn       = full species concentration array.
!! stot      = subset of cnn. species concentration array to be supplied to
!!             lsodes. length of stot depends on the selected mechanism
!! iregime   = selected chemical regime (1-6)
!! imap      = 0 : map cnn to stot
!!           = 1 : map stot to cnn
!! 
!! author: rahul a. zaveri
!! date  : february 1996
!!
!!------------------------------------------------------------------------
      subroutine mapgasspecies(stot,imap)
      include 'chm1.inc'
      include 'gas1.inc'
      dimension stot(nmax)

      goto (1,2,3,4,5,6), iregime


1     call mapgas_com(stot,imap)
      return


2     call mapgas_com(stot,imap)
      call mapgas_urb(stot,imap)
      return


3     call mapgas_com(stot,imap)
      call mapgas_urb(stot,imap)
      call mapgas_bio(stot,imap)
      return


4     call mapgas_com(stot,imap)
      call mapgas_mar(stot,imap)
      return

 
5     call mapgas_com(stot,imap)
      call mapgas_urb(stot,imap)
      call mapgas_mar(stot,imap)
      return


6     call mapgas_com(stot,imap)
      call mapgas_urb(stot,imap)
      call mapgas_bio(stot,imap)
      call mapgas_mar(stot,imap)
      return

      end








!!*********************************************************************
!! subroutine setgasindices: sets up gas-phase species indices for 
!! the selected mechanism.
!!
!! input: iregime    = 1     : com
!!                   = 2     : com + urb
!!                   = 3     : com + urb + bio
!!                   = 4     : com + mar
!!                   = 5     : com + urb + mar
!!                   = 6     : com + urb + bio + mar
!!
!! author: rahul a. zaveri
!! date  : february 1996
!!------------------------------------------------------------------------
      subroutine setgasindices
      include 'chm1.inc'
      include 'gas1.inc'

      ilast = 0

      goto (1,2,3,4,5,6), iregime

1     call setgas_com(ilast)
      return


2     call setgas_com(ilast)
      call setgas_urb(ilast)
      return


3     call setgas_com(ilast)
      call setgas_urb(ilast)
      call setgas_bio(ilast)
      return


4     call setgas_com(ilast)
      call setgas_mar(ilast)
      return


5     call setgas_com(ilast)
      call setgas_urb(ilast)
      call setgas_mar(ilast)
      return


6     call setgas_com(ilast)
      call setgas_urb(ilast)
      call setgas_bio(ilast)
      call setgas_mar(ilast)
      return

      end



!!************************************************************************
!! subroutine selectgasregime: selects an optimum combination of gas-phase
!!                             mechanisms 
!!
!! input : cnn       = full species concentrations array (molec/cc)
!!
!! output: iregime   = 1     : com
!!                   = 2     : com + urb
!!                   = 3     : com + urb + bio
!!                   = 4     : com + mar
!!                   = 5     : com + urb + mar
!!                   = 6     : com + urb + bio + mar    
!!         ngas      = number of gas-phase species in the selected mechanism
!!
!! author: rahul a. zaveri
!! date  : february 1996
!!
!!---------------------------------------------------------------------
      subroutine selectgasregime(ntot)
      include 'chm1.inc'
      include 'gas1.inc'

      cutoff_conc = 5.e+6     ! [molec/cc]


!!---------------------------------------------
!! initialize regime flags to zero...
      m_com = 1 ! 1 (always)
      m_urb = 0	! 0 or 1
      m_bio = 0	! 0 or 2
      m_mar = 0	! 0 or 3


!! decide mechanism flags...
      do k = kpar, kxpar
      if( (cnn(k) .gt. cutoff_conc)  .or.  (emission(k) .gt. 0.0)    ) then 
       m_urb=1
      end if
      enddo

      do k = kisop, kisopo2
      if( (cnn(k) .gt. cutoff_conc)  .or.  (emission(k) .gt. 0.0)    ) then 
       m_bio=2
      end if
      enddo

      do k = kdms, ksulfhox
      if( (cnn(k) .gt. cutoff_conc)  .or.  (emission(k) .gt. 0.0)     ) then 
       m_mar=3
      end if
      enddo

      iregime = m_com + m_urb*((2-m_bio)/2) + m_bio + m_mar



!!-------------------------------------------------------------------------
      goto (1,2,3,4,5,6), iregime

1     ntot = nreg1
      return

2     ntot = nreg2
      return

3     ntot = nreg3
 
      return

4     ntot = nreg4
      return

5     ntot = nreg5
      return

6     ntot = nreg6
      return

      end




!!*************************************************************************
!! subroutine ode_gas
!!
!! purpose: computes p and l.  dy/dt = p-ly.
!!          calls ode_com, ode_urb, ode_bio, ode_mar depending on the
!!          chemical regime (iregime)
!!
!! author: fan feng
!!
!!--------------------------------------------------------------------------

      subroutine ode_gas(ntot,y,t_in)
      include 'chm1.inc'
      include 'gas1.inc'

      dimension y(nmax)

      do i=1,nrxn_com
        r_com(i) = 0.
      enddo

      do i=1,nrxn_urb
        r_urb(i) = 0.
      enddo

      do i=1,nrxn_bio
        r_bio(i) = 0.
      enddo

      do i=1,nrxn_mar
        r_mar(i) = 0.
      enddo

      do i=1,nrxn_het
        r_het(i) = 0.
      enddo


      call gasrates(y)

      do i=1,ngas_max
        p_com(i) = 0.
        p_urb(i) = 0.
        p_bio(i) = 0.
        p_mar(i) = 0.
        p_het(i) = 0.
        total_p(i)= 0.

        l_com(i) = 0.
        l_urb(i) = 0.
        l_bio(i) = 0.
        l_mar(i) = 0.
        l_het(i) = 0.
        total_l(i)= 0.
      enddo
   

      goto (1,2,3,4,5,6), iregime

1     call ode_com
      call ode_het

      do i=1,nreg1

         total_p(i)= real( dble(p_com(i)+p_het(i)) )+emit(i)
         total_l(i)= real( dble(l_com(i)+l_het(i)) ) 



      enddo

      return

!!---------------------------------------------------------
2     call ode_com
      call ode_urb
      call ode_het

      do i=1,nreg2

         total_p(i)= real( dble(p_com(i)+p_urb(i)+p_het(i)) )+emit(i)
         total_l(i)= real( dble(l_com(i)+l_urb(i)+l_het(i)) ) 


      enddo

      return

!!---------------------------------------------------------
3     call ode_com
      call ode_urb
      call ode_bio
      call ode_het

      do i=1,nreg3

         total_p(i)= real( dble(p_com(i)+p_urb(i)+p_bio(i)+p_het(i)) )+emit(i)
         total_l(i)= real( dble(l_com(i)+l_urb(i)+l_bio(i)+l_het(i)) ) 

      enddo

      return

!!---------------------------------------------------------
4     call ode_com
      call ode_mar
      call ode_het

      do i=1,nreg4

         total_p(i)= real( dble(p_com(i)+p_mar(i)+p_het(i)) )+emit(i)
         total_l(i)= real( dble(l_com(i)+l_mar(i)+l_het(i)) ) 

 
      enddo

      return

!!---------------------------------------------------------
5     call ode_com
      call ode_urb
      call ode_mar
      call ode_het

      do i=1,nreg5

         total_p(i)= real( dble(p_com(i)+p_urb(i)+p_mar(i)+p_het(i)) )+emit(i)
         total_l(i)= real( dble(l_com(i)+l_urb(i)+l_mar(i)+l_het(i)) ) 


      enddo

      return

!!---------------------------------------------------------
6     call ode_com
      call ode_urb
      call ode_bio
      call ode_mar
      call ode_het

      do i=1,nreg6


         total_p(i)= real( dble(p_com(i)+p_urb(i)+p_bio(i)+p_mar(i)+p_het(i)) )+emit(i)
         total_l(i)= real( dble(l_com(i)+l_urb(i)+l_bio(i)+l_mar(i)+l_het(i)) ) 


      enddo

      return

      end








!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!modified backward euler (mbe)
!!!---------------      subroutine mbe_solver(odefunc,neq,y,t)        ----
!!! renew dt_sec, y 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine mbe_solver(odefunc,neq,y,t)
!! neq: number of equations

external odefunc
include 'chm1.inc'
include 'gas1.inc'

integer :: j
real :: yout_temp(neq),y(neq),dydt(neq)
real :: t
logical :: flag_change_fast_y





  call odefunc(neq,y,t)


  do j=1,neq

 !!---- p,l,y should be nonnegative ------------------------------

 if (total_p(j)<0 .or. total_l(j)<0) then
 write(*,*)"subroutine mbe_solver(...):  p or l (",j,") at time (second)", t, " <0"
 stop
 end if

 if (y(j)<0) then
  write(*,*)"subroutine mbe_solver(...):  y (",j,") at time (second)", t, "<0"
  stop
 end if

 !!--in fact, with any positive dt_sec, modified backward euler (mbe) can guarantee nonnegativity of y ---------
  yout_temp(j)=(y(j)+total_p(j)*dt_sec)/(1+total_l(j)*dt_sec) !mbe
    
  end do !end of do j=1,neq
 


!!------------------  prepare for stepsize changing --------------------------------------------

 
 flag_change_fast_y=.false.

 do j=1,neq

       dydt(j)=total_p(j)-total_l(j)*y(j) 

       if (dydt(j)>cri_dydt .or. dydt(j)<-cri_dydt) then
       flag_change_fast_y=.true.
       exit 
       end if ! end of if

 end do ! end of j=1,neq

 


!!------------------  change stepsize ---------------------------------------------------------

  if (flag_change_fast_y .eqv. .true.) then  !there exists value that changes fast .
    
      dt_sec=dt_sec/decay_coeff 
     
    
  else
    dt_sec=dt_sec*growth_coeff 
       
  end if


  
  
  if (dt_sec>max_stepsize) then
  dt_sec=max_stepsize 
  end if

  if (dt_sec<min_stepsize) then
  dt_sec=min_stepsize 
  end if



!!------------- output y ---------------------
 do j=1,neq
 y(j)=yout_temp(j)
 end do ! end of j=1,neq

  

return
end subroutine




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!  subroutine odesolver(ntot,stot,t_in)   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



      subroutine odesolver(ntot,stot,t_in)

      include 'chm1.inc'
      include 'gas1.inc'
      dimension stot(nmax)	! local species array

      external ode_gas


      call mbe_solver(ode_gas,ntot,stot,t_in)


      return
      end









