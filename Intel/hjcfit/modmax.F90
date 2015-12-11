    subroutine modmax(itype,E,F,pmax)
! To calculate Popen(max) 
! itype=1  
!   Pmax = E/(1+E) 
!  where E = Q(imE,jmE)
! itype=2
!   Pmax =  EF/(1+F*EF)

    integer itype
    real*8 pmax,E,F
    
    if(itype.eq.1) then
        pmax=E/(1.d0 + E)
    else
        pmax=E*F/(1.d0 + F + E*F)    
    endif
    
    return
    end        
    