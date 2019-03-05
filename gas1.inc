

!!********************************** stepsize parameter (by Feng Fan)******
real, parameter :: max_stepsize=5    !unit sec     1<=max_stepsize<=60
real, parameter :: min_stepsize=5    !unit sec     1<=min_stepsize<=5  

real, parameter  :: Cri_dydt=7e6  ! unit molec/(cc*sec)

real, parameter :: growth_coeff=1.2 ! growth_coeff must be >=1.
real, parameter :: decay_coeff=1.8 ! decay_coeff must be >=1.


!--------------------------------------------------------------------------

real ::             P_com,L_com, &
                    P_urb,L_urb, &
                    P_bio,L_bio, &
                    P_mar,L_mar, &
                    P_het,L_het, &
                    total_P,total_L

!--------------------------------------------------------------------------

      parameter(nperox = 10)	! total number of alkylperoxy radicals
      parameter(nphoto = 20)	! total number of photolyzing species

      

      parameter(nrxn_het = 28, & !Li Jie has changed 7 to 28
                nrxn_com = 76, & !Feng Fan has changed 74 to 76 for NO3 sinks with vocs
                nrxn_urb = 49, & !Feng Fan has changed 44 to 49 for NO3 sinks with vocs
                nrxn_bio = 20, & !Li Jie has changed 16 to 20
                nrxn_mar = 35)

 

      parameter(nreg1 = ngas_com, &
                nreg2 = ngas_com + ngas_urb, &
                nreg3 = ngas_com + ngas_urb + ngas_bio, &
                nreg4 = ngas_com + ngas_mar, &
                nreg5 = ngas_com + ngas_urb + ngas_mar, &
                nreg6 = ngas_com + ngas_urb + ngas_bio + ngas_mar)

!------------------------------------------------------------------------

      common /regime/ iregime



      common/gasrateconst/rk_com(nrxn_com),rk_urb(nrxn_urb), &
                          rk_bio(nrxn_bio),rk_mar(nrxn_mar), &
                          rk_het(nrxn_het), &
                          rk_param(nperox),rk_photo(nphoto), &
                          Aperox(nperox,nperox),Bperox(nperox,nperox), &
                          uptake_coeff(nrxn_het), hetrxn(nrxn_het)

      common/gasratescom/ r_com(nrxn_com),r_urb(nrxn_urb), &
                          r_bio(nrxn_bio),r_mar(nrxn_mar), &
                          r_het(nrxn_het), &
                          r1_com(nrxn_com),r1_urb(nrxn_urb), &
                          r1_bio(nrxn_bio),r1_mar(nrxn_mar), &
                          r1_het(nrxn_het), &
                          r2_com(nrxn_com),r2_urb(nrxn_urb), &
                          r2_bio(nrxn_bio),r2_mar(nrxn_mar), &
                          r2_het(nrxn_het)


      common/gaspd/ P_com(ngas_max),L_com(ngas_max), &
                    P_urb(ngas_max),L_urb(ngas_max), &
                    P_bio(ngas_max),L_bio(ngas_max), &
                    P_mar(ngas_max),L_mar(ngas_max), &
                    P_het(ngas_max),L_het(ngas_max), &
                    total_P(ngas_max),total_L(ngas_max)


      common/het/ Npcasp(15)


!------------------------------------------------------------------------

      common /localGas/ &
       ih2so4,      ihno3,       ihcl,        inh3,        ino, &
       ino2,        ino3,        in2o5,       ihono,       ihno4, &
       io3,         io1d,        io3p,        ioh,         iho2, &
       ih2o2,       ico,         iso2,        ich4,        ic2h6, &           
       ich3o2,      iethp,       ihcho,       ich3oh,      ianol, &
       ich3ooh,     iethooh,     iald2,       ihcooh,      ircooh, &
       ic2o3,       ipan,        ipar,        iaone,       imgly, &
       ieth,        iolet,       iolei,       itol,        ixyl, &
       icres,       ito2,        icro,        iopen,       ionit, &
       irooh,       iro2,        iano2,       inap,        ixo2, &
       ixpar,       iisop,       iisoprd,     iisopp,      iisopn, &
       iisopo2, &
       idms,        imsa,        idmso,       idmso2,      ich3so2h, &
       ich3sch2oo,  ich3so2,     ich3so3,     ich3so2ch2oo,ich3so2oo, &
       isulfhox, &
       iterp,       isv1,        isv2,        isv3,        isv4, &  
       isv5,        isv6,        idso4,       idno3   ! kterp,ksv1-6,kdso4,kdno3 are added by Li Jie


      common /gasPeroxyRadicals/ &
       jch3o2,      jethp,       jro2,        jc2o3,       jano2, &
       jnap,        jisopp,      jisopn,      jisopo2,     jxo2

      common /photolyzingSpecies/ &
       jphoto_no2,    jphoto_no3,   jphoto_hono,   jphoto_hno3, & 
       jphoto_hno4,   jphoto_n2o5,  jphoto_o3a,    jphoto_o3b, &    
       jphoto_h2o2,   jphoto_hchoa, jphoto_hchob,  jphoto_ch3ooh, &
       jphoto_ethooh, jphoto_ald2,  jphoto_aone,   jphoto_mgly, &
       jphoto_open,   jphoto_rooh,  jphoto_onit,   jphoto_isoprd

 
