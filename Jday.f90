!                                                                       
      subroutine JDAY (iday, imonth, iyear, julday, iseason)            
!-----------------------------------------------------------------------
!                                                                       
!                                                                       
! PURPOSE:  Compute Julian day and season index from date               
!                                                                       
! INPUTS:                                                               
!                                                                       
!     IDAY, IMONTH, IYEAR integer     day, month, year                  
!                                                                       
! OUTPUT:                                                               
!                                                                       
!     JULDAY      integer     Julian day                                
!     ISEASON     integer     season index (1 = spring, 2 = summer,     
!                                           3 = fall,   4 = winter)     
!                                                                       
! CALLS:                                                                
!                                                                       
!     none                                                              
!                                                                       
!-----------------------------------------------------------------------
                                                                        
      implicit none                                                     
                                                                        
      integer iday, imonth, iyear, julday, iseason                      
      integer ndaynorm(13), ndaybis(13)                                 
                                                                        
      data ndaynorm /0,31,59,90,120,151,181,212,243,273,304,334,365/    
      data ndaybis /0,31,60,91,121,152,182,213,244,274,305,335,366/     
                                                                        
                                                                        
      if (mod(iyear,4) .eq. 0) then                                     
        julday = ndaybis(imonth) + iday                                 
        if (julday .lt. 80 .or. julday .ge. 355) then                   
          iseason = 4                                                   
        else if (julday .lt. 172) then                                  
          iseason = 1                                                   
        else if (julday .lt. 264) then                                  
          iseason = 2                                                   
        else                                                            
          iseason = 3                                                   
        endif                                                           
      else                                                              
        julday = ndaynorm(imonth) + iday                                
        if (julday .lt. 81 .or. julday .ge. 356) then                   
          iseason = 4                                                   
        else if (julday .lt. 173) then                                  
          iseason = 1                                                   
        else if (julday .lt. 265) then                                  
          iseason = 2                                                   
        else                                                            
          iseason = 3                                                   
        endif                                                           
      endif                                                             
                                                                        
      return                                                            
      end                                                               
