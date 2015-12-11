Comparing files SCBDISP.FOR and scbdisp1.for
****** SCBDISP.FOR
c
c To do displays of pdfs in SCBURST
****** scbdisp1.for
        subroutine SCBDISP(areasav,tausav,ncompsav,idtype,nsav,
     &  docond,dok,dokr,ndim,ndtmax,)
c
c To do displays of pdfs in SCBURST
******

****** SCBDISP.FOR
        real*4 tausav(ndim,ndtmax),areasav(ndim,ndtmax)
        integer*4 ncompsav(ndtmax)
        integer*4 idtype(4,ndtmax)
****** scbdisp1.for
        real*4 tausav(ndim,ndtmax),areasav(ndim,ndtmax)
        integer* ncompsav(ndtmax)
        integer*4 idtype(4,ndtmax)
******

****** SCBDISP.FOR
        logical docond,dok,dokr
c for display
        logical allocated
        ALLOCATABLE::Xval,Yval,Xcal,Ycal
        real Xval(:,:),Yval(:,:)                !for VPLOT
****** scbdisp1.for
        logical docond,dok,dokr
===========
c for display
        ALLOCATABLE Xval,Yval,Xcal,Ycal
        real Xval(:,:),Yval(:,:)                !for VPLOT
******

****** SCBDISP.FOR
        real Xcal(:,:),Ycal(:,:)                !for VPLOT
        ALLOCATABLE::tau,area
        real*4 tau(:),area(:)
c for data
****** scbdisp1.for
        real Xcal(:,:),Ycal(:,:)                !for VPLOT
c for data
******

****** SCBDISP.FOR
        dimension ncal(2),icurvc(2),iline(2)    !for calc curve
        LOGICAL debug,fitted,doframe,plotonly,autplt
        character*40 titlex,titley
****** scbdisp1.for
        dimension ncal(2),icurvc(2),iline(2)    !for calc curve
        LOGICAL debug,fitted,doframe,plotonly,autplt,pspec
        logical varconc,showconc,plotsav
        character*40 titlex,titley
******

****** SCBDISP.FOR
        logical caplock,landscap,ivplot,interp
        character*1 ans
        integer*4 icurvw(1)             !for VPLOT5
****** scbdisp1.for
        logical caplock,landscap,ivplot,interp
        character*1 ans,UC
        integer*4 icurvw(1)             !for VPLOT5
******

****** SCBDISP.FOR
c Use F90 routine to prevent underflow crashes??
c       errflag=.true.
c       call UNDFL(errflag)
c List distribution types that have been saved
****** scbdisp1.for
c Use F90 routine to prevent underflow crashes??
        errflag=.true.
        call UNDFL(errflag)
c List distribution types that have been saved
******

****** SCBDISP.FOR
        fitted=.false.
c       k=kA+kB+kC+kD
c
****** scbdisp1.for
        fitted=.false.
        fstdisp=.true.
        k=kA+kB+kC+kD
c
******

****** SCBDISP.FOR
c=======
50      continue
        ndv1=1          !xval not used
****** scbdisp1.for
c=======
        ndv1=1          !xval not used
******

****** SCBDISP.FOR
        ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd))
        ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc))
        id1=1
****** scbdisp1.for
        ALLOCATE(Xval(ndv1,ndimd),Yval(ndv1,ndimd))
        ALLOCATE(Xcal(ndc1,ndimc),Ycal(ndc1,ndimc)
        id1=1
******

****** SCBDISP.FOR
        print 1,id1
1       format(/,' Plot:',/,
     & ' (1) distribution of number of openings per burst',/,
****** scbdisp1.for
        print 1,id1
1       format(/,' Plot
     & ' (1) distribution of number of openings per burst',/,
******

****** SCBDISP.FOR
                print 2,id2
2               format(/,' Plot: ',/,
     & ' (1) pdf of burst lengths',/,
****** scbdisp1.for
                print 2,id2
2               format(/,' Plot
     & ' (1) pdf of burst lengths',/,
******

****** SCBDISP.FOR
        ncomp=ncompsav(jout)
        if(allocated(tau)) deallocate(tau,area)
        ALLOCATE(tau(ncomp),area(ncomp))
        do i=1,ncomp
****** scbdisp1.for
        ncomp=ncompsav(jout)
        do i=1,ncomp
******

****** SCBDISP.FOR
        do i=1,ncalc
           x=alog10(xmin)+float(i-1)*dxlog      !this is log10 value
           t=10.**x             !log10 taken in Vplot5
****** scbdisp1.for
        do i=1,ncalc
           c=alog10(xmin)+float(i-1)*dxlog      !this is log10 value
           t=10.**x             !log10 taken in Vplot5
******

****** SCBDISP.FOR
        endif
c tau=theta(1,3,5,7,9)
****** scbdisp1.for
        endif
        idisp=1
c tau=theta(1,3,5,7,9)
******

****** SCBDISP.FOR
        DEALLOCATE(Xval,Yval,Xcal,Ycal)
        DEALLOCATE(tau,area)
        call DCASK('Another display',ans,ans)
        if(ans.eq.'Y') goto 50
        return
        end
****** scbdisp1.for
        DEALLOCATE(Xval,Yval,Xcal,Ycal)
        RETURN
        end
******

****** SCBDISP.FOR


        subroutine GETJ(id1,id2,k1,r1,nsav,idtype,docond,jout,ndtmax)
****** scbdisp1.for

        subroutine GETJ(id1,id2,k1,r1,nsav,idtype,docond,jout,ndtmax)
******


