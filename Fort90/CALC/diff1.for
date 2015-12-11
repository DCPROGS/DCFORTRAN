Comparing files qdefine.for and \fort90\calc\old\qdefine.for
****** qdefine.for
c=======end temp for debug
           call MODWIND(charmod,nrow,ncol,iflag,ilast,jlast,0)
           call CQLAST(charmod,ilast,jlast)
****** \fort90\calc\old\qdefine.for
c=======end temp for debug
           call MODWIND(charmod,nrow,ncol,iflag,ilast,jlast)
           call CQLAST(charmod,ilast,jlast)
******

****** qdefine.for
        call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     &  nr1,nc1,iflagc,iconst,nd1,nd2,0)
        do m=1,kstat
****** \fort90\calc\old\qdefine.for
        call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     &  nr1,nc1,iflagc,iconst,nd1,nd2)
        do m=1,kstat
******

****** qdefine.for
           call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     &  nr1,nc1,iflagc,iconst,nd1,nd2,0)
c
****** \fort90\calc\old\qdefine.for
           call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     &  nr1,nc1,iflagc,iconst,nd1,nd2)
c
******

****** qdefine.for
        if(chardef) then
           call MODWIND(charmod,ilast+2,jlast+2,2,ilast,jlast,0)        !display model
        endif
****** \fort90\calc\old\qdefine.for
        if(chardef) then
           call MODWIND(charmod,ilast+2,jlast+2,2,ilast,jlast)  !display model
        endif
******

****** qdefine.for
           call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     &  nr1,nc1,iflagc,iconst,nd1,nd2,0)
           do i=1,npar
****** \fort90\calc\old\qdefine.for
           call INWINDc(cdata,ni0,nj0,nidisp,title,heading,
     &  nr1,nc1,iflagc,iconst,nd1,nd2)
           do i=1,npar
******


