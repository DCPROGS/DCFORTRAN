Comparing files READQ.FOR and d:\fortran\graph\rdvplq.for
****** READQ.FOR
        subroutine READQ(irec,xval,yval,ndimd,ndimc,ncurvd,
     & ndat,isym,ijoin,syms,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ntx,nty,itx,ity,ixlo,ixhi,iylo,iyhi,
     & itit,title,csize,ifont,ilog,iscal,doframe,titlex,titley,ilabel,
     & inumx,inumy,sval,theta,ifitype,ncomp,isdev,weight,y0,yinf,iptype,
****** d:\fortran\graph\rdvplq.for
        subroutine RDVPLQ(istrec,xval,yval,xcal,ycal,ndimd,ndimc,ncurvd,
     & ndat,isym,ijoin,ncurvc,ncal,iline,syms,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ntx,nty,itx,ity,ixlo,ixhi,iylo,iyhi,
     & itit,title1,csize,ifont,ilog,iscal,doframe,titlex,titley,ilabel,
     & inumx,inumy,sval,theta,ifitype,ncomp,isdev,weight,y0,yinf,iptype,
******

****** READQ.FOR
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     & interp,itrace,ntrace,ytsep,ndv1,ndc1,
     & kwi,kwj,icurvw,kmax)
****** d:\fortran\graph\rdvplq.for
     & ncjump,t1c,t2c,nvjump,t1v,t2v,xoff1,iy1v,iy2v,iy1c,iy2c,ivplot,
     & interp,screen,colplotter,
     & itrace,ntrace,ytsep,ndv1,ndc1,
     & kwi,kwj,icurvw,kmax)
******

****** READQ.FOR
c
c This is to read data back from plot queue (plotq.dat) for (re-)fitting
c in CJFIT (also version for CVFIT?).  Based on RDVPLQ (used in autplot)
c Purpose is to read back data for (re-)fitting so xcal, ycal etc not
c returned -just allocated locally here for reading the data
c
c Modif 09/19/97 10:28am for case where several queued traces are being
c averaged.  In this case number of values to be averaged is fixed
c at nsampav -some queued traces may have more points than this (none
c should have fewer!) and xval(), yval() are allocated to this size
c in cjfit.
c Also, avoid overwriting the input values of ndv1,ndimd,ndc1,ndimc
c by reading them here as ndv2 etc (not used).  When this done, the
c value of ndv1 is actual allocated dimension of xval, yval, so
c read their values, xval(i,1), yval(i,1) only if i.le.ndv1
c
c Notes for RDVPLQ
c Modif 04/04/95 11:18am for records written by VLPQ5 (in VPLOT5)
****** d:\fortran\graph\rdvplq.for
c
c Modif 04/04/95 11:18am for records written by VLPQ5 (in VPLOT5)
******

****** READQ.FOR
c  Since all plots now done with vplot5 etc, ifstcol, ndatsav removed.
c 10/26/95 10:57am DATCOP dimension increased to 200000 in RDVPLQ and VPLQ5
****** d:\fortran\graph\rdvplq.for
c  Since all plots now done with vplot5 etc, ifstcol, ndatsav removed.
c
c 10/26/95 10:57am DATCOP dimension increased to 200000 in RDVPLQ and VPLQ5
******

****** READQ.FOR
        real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
        real weight(kwi,kwj)
****** d:\fortran\graph\rdvplq.for
        real*4 XVAL(ndv1,ndimd),YVAL(ndv1,ndimd)
        real*4 XCAL(ndc1,ndimc),YCAL(ndc1,ndimc)
c       dimension XVAL(2048,10),YVAL(2048,10)
c       dimension XCAL(2048,10),YCAL(2048,10)
c=      real weight(100,10)
        real weight(kwi,kwj)
******

****** READQ.FOR
        real theta(kmax),syms(ndimd)
        integer ndat(ndimd),isym(ndimd),ijoin(ndimd)
        ALLOCATABLE::xcal,ycal,ncal,iline
        real*4 XCAL(:,:),YCAL(:,:)
        integer ncal(:),iline(:)
        character*40 titlex,titley
        character*44 title      !plot title (local)
        LOGICAL doframe,mono,mono1,interp,slock,pon
****** d:\fortran\graph\rdvplq.for
        real theta(kmax),syms(ndimd)
        dimension ndat(ndimd),isym(ndimd),ijoin(ndimd)
        integer ncal(ndimc),iline(ndimc)
c arrays needed when more than 2048 points/plot from VPLOTR
c=      integer ifstcol(10),ndatsav(10),ijoinsav(10),isymsav(10)
c       character*1 ans,UC
        character*40 titlex,titley
        character*44 title1     !plot title (local)
        LOGICAL doframe,mono,mono1,interp,slock,pon
******

****** READQ.FOR
        logical caplock,debug
c extra args to hold all details of posh plots
****** d:\fortran\graph\rdvplq.for
        logical caplock,debug
        logical screen,colplotter
c extra args to hold all details of posh plots
******

****** READQ.FOR
c
c Now allocate datcop() below, after k1 read
        ALLOCATE(int2cop(1000),charcop(100))
c
****** d:\fortran\graph\rdvplq.for
c
        ALLOCATE(int2cop(1000),charcop(100))
c Modif 03/26/97 02:58pm for variable size datcop
c Now datcop etc allocate below, after k1 read (=number of values written)
c
******

****** READQ.FOR
        endif
c Local allocation for reading calc curves (not returned)
        ALLOCATE(xcal(ndc1,ndimc),ycal(ndc1,ndimc))
        ALLOCATE(ncal(ndimc),iline(ndimc))
c
c Note: PLOTQ/POSHPLOT now hold values for things are already defined at
****** d:\fortran\graph\rdvplq.for
        endif
c Note: PLOTQ/POSHPLOT now hold values for things are already defined at
******

****** READQ.FOR
c for VPLOTr, both now obsolete)
        krn=irec
        if(iptype.eq.1) then
           read(11,rec=krn) iptype,itit,title,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,
****** d:\fortran\graph\rdvplq.for
c for VPLOTr, both now obsolete)
        krn=istrec
        if(iptype.eq.1) then
           read(11,rec=krn) iptype,itit,title1,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,
******

****** READQ.FOR
        else if(iptype.eq.11.or.iptype.eq.12) then
           read(11,rec=krn) iptype,itit,title,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,
****** d:\fortran\graph\rdvplq.for
        else if(iptype.eq.11.or.iptype.eq.12) then
           read(11,rec=krn) iptype,itit,title1,xmin,xmax,ymin,ymax,
     & xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,csize,ifont,ilog,
******

****** READQ.FOR
        else if(iptype.eq.14) then
          read(11,rec=krn) iptype,ndv2,ndimd2,ndc2,ndimc2,itit,title,
     & xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,
****** d:\fortran\graph\rdvplq.for
        else if(iptype.eq.14) then
          read(11,rec=krn) iptype,ndv1,ndimd,ndc1,ndimc,itit,title1,
     & xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,
******

****** READQ.FOR
        else if(iptype.eq.15) then
           read(11,rec=irec) iptype,ndv2,ndimd2,ndc2,ndimc2,kwi,kwj,kmax,
     &  itit,title,
     &  xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,
****** d:\fortran\graph\rdvplq.for
        else if(iptype.eq.15) then
           read(11,rec=istrec) iptype,ndv1,ndimd,ndc1,ndimc,kwi,kwj,kmax,
     &  itit,title1,
     &  xmin,xmax,ymin,ymax,xcross,ycross,xtic,ytic,ixlo,ixhi,iylo,iyhi,
******

****** READQ.FOR
c
c Can now allocate datcop()
        ALLOCATE(datcop(k1))
****** d:\fortran\graph\rdvplq.for
c
c Modif 03/26/97 02:58pm for variable size datcop -allocate now
c       ALLOCATE(datcop(4*k1))
        ALLOCATE(datcop(k1))
******

****** READQ.FOR
        if(iptype.ge.15.and.iptype.le.25) then
           istr1=irec+ioffset
           ALLOCATE(int4cop(ndimd+ndimc))
****** d:\fortran\graph\rdvplq.for
        if(iptype.ge.15.and.iptype.le.25) then
           istr1=istrec+ioffset
           ALLOCATE(int4cop(ndimd+ndimc))
******

****** READQ.FOR
        if(jcol.eq.1002) then           !colours were queued
           mono=mono1                   !use queued value of mono
        else if(jcol.ne.1002) then      !colours NOT queued -mono as set in INAUT
****** d:\fortran\graph\rdvplq.for
        if(jcol.eq.1002) then           !colours were queued
           if(screen.or.(.not.screen.and.colplotter)) then
                mono=mono1                      !use queued value of mono
           endif
        else if(jcol.ne.1002) then      !colours NOT queued -mono as set in INAUT
******

****** READQ.FOR
c End of int2cop and int4cop
c If any colours not set above by queue, then set to defaults (screen)
c now, so that if text, lines etc added in VPLOT they will have a default colour
****** d:\fortran\graph\rdvplq.for
c End of int2cop and int4cop
c If any colours not sey above by queue, then set to defaults (screen)
c now, so that if text, lines etc added in VPLOT they will have a default colour
******

****** READQ.FOR
           do i=1,n
                k=k+1
                if(i.le.ndv1) then
                   Xval(i,j)=datcop(k)
                endif
           enddo
****** d:\fortran\graph\rdvplq.for
           do i=1,n
             k=k+1
             Xval(i,j)=datcop(k)
           enddo
******

****** READQ.FOR
           do i=1,n
              k=k+1
                if(i.le.ndv1) then
                   Yval(i,j)=datcop(k)
                endif
           enddo
****** d:\fortran\graph\rdvplq.for
           do i=1,n
             k=k+1
             Yval(i,j)=datcop(k)
           enddo
******

****** READQ.FOR
        else if(iptype.eq.11.or.iptype.eq.12) then
c        jc=1
****** d:\fortran\graph\rdvplq.for
        else if(iptype.eq.11.or.iptype.eq.12) then
c        bigplot=.false.
         do 76 j=1,ncurvd
c        ndatsav(j)=ndat(j)     !copy so ndat etc can be rearranged below
c        ijoinsav(j)=ijoin(j)
c        isymsav(j)=isym(j)
c        if(ndat(j).gt.2048) bigplot=.true.
76       continue
c        jc=1
******

****** READQ.FOR
         n=ndat(j)
           do i=1,n
                k=k+1
                if(i.le.ndv1) then
                   Xval(i,j)=datcop(k)
                endif
           enddo
           do i=1,n
                k=k+1
                if(i.le.ndv1) then
                   Yval(i,j)=datcop(k)
                endif
           enddo
c        endif
****** d:\fortran\graph\rdvplq.for
         n=ndat(j)
c        if(bigplot) then
c          ifstcol(j)=jc
cc         NB VPLOTR and VPLOTQ get ndat etc from ndat(jc) not ndat(j)!
c          ndat(jc)=ndatsav(j)
c          ijoin(jc)=ijoinsav(j)
c          isym(jc)=isymsav(j)
c          do 1373 i=1,n
c          k=k+1
c1373      Xval(i,jc)=datcop(k)
c          do 1374 i=1,n
c          k=k+1
c1374      Yval(i,jc)=datcop(k)
c          ngr=1+(n-1)/2048     !# of cols for current data
c          jc=jc+ngr            !col of Yval(i,j) where next data put
c        else
c          ifstcol(j)=j
           do 1371 i=1,n
           k=k+1
1371       Xval(i,j)=datcop(k)
           do 1372 i=1,n
           k=k+1
1372       Yval(i,j)=datcop(k)
c        endif
******

****** READQ.FOR
        if(allocated(int4cop)) DEALLOCATE(int4cop)
        DEALLOCATE(xcal,ycal,ncal,iline)
        RETURN
****** d:\fortran\graph\rdvplq.for
        if(allocated(int4cop)) DEALLOCATE(int4cop)
        RETURN
******


