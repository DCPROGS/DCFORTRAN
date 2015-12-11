Comparing files STABPLOT.FOR and \fort90\hjcfit\stab_hjc.for
****** STABPLOT.FOR
        subroutine STABPLOT(tint,ampl,iprops,nint,nintt,nbad,nbad1,
     & isbad,iebad,namp,ampval,acal,trise,tcfac,iscan,idiskq,
     & index,iopt,mono,stabcut,nfile,iexstrt,iexend,
     & dispmean,ameanval,sdval,ndimm,nthmax,openav,shutav,idtype)
c Version of OPFREQ that produces various types of stability plot
c
****** \fort90\hjcfit\stab_hjc.for
        subroutine STAB_HJC(tint,ampl,iprops,nint,nintt,nbad,nbad1,
     & isbad,iebad,namp,ampval,acal,trise,tcfac,iscan,idiskq,
     & index,iopt,mono,stabcut,jset)

c   ===exclude for now: ,nfile,iexstrt,iexend)
c
c To produce various types of stability plot
c STAB-HJc is version for HJCFIT of stabplot.for
c
******

****** STABPLOT.FOR
        integer isgood(20),iegood(20)
        integer iexstrt(50),iexend(50)
c      character*1 ch,chb
c For display of 'mean of nth op' etc
        real*4 ameanval(0:ndimm),sdval(0:ndimm)
        logical dispmean
c Arrays for Vplot
****** \fort90\hjcfit\stab_hjc.for
        integer isgood(20),iegood(20)
c===    integer iexstrt(50),iexend(50)
c      character*1 ch,chb
c Arrays for Vplot
******

****** STABPLOT.FOR
        integer ndat1(5),icurvd1(5),isym1(5),ijoin1(5)  !for VPLOT2 data
        real symsiz(5),theta(50)
c       real*4 XCAL1(2048,10),YCAL1(2048,10)    !for VPLOT2
****** \fort90\hjcfit\stab_hjc.for
        integer ndat1(5),icurvd1(5),isym1(5),ijoin1(5)  !for VPLOT2 data
        real*4 symsiz(5)
        real*4 theta(50)        !for call of vplot etc (not used)
c       real*4 XCAL1(2048,10),YCAL1(2048,10)    !for VPLOT2
******

****** STABPLOT.FOR
c
        if(dispmean) goto 206
c
778     if(iopt.eq.7) then
****** \fort90\hjcfit\stab_hjc.for
c
778     if(iopt.eq.7) then
******

****** STABPLOT.FOR
        if(pon()) write(7,13) nval,nstep
        if(discprt) write(8,13) nval,nstep
13      format(
****** \fort90\hjcfit\stab_hjc.for
        if(pon()) write(7,13) nval,nstep
c       if(discprt) write(8,13) nval,nstep
13      format(
******

****** STABPLOT.FOR
           print 57,nval
           if(pon()) write(7,57) nval
           if(discprt) write(8,57) nval
57      format(
****** \fort90\hjcfit\stab_hjc.for
           print 57,nval
c          if(discprt) write(8,57) nval
57      format(
******

****** STABPLOT.FOR
           print 58
           if(pon()) write(7,58)
           if(discprt) write(8,58)
58      format(' Bad gaps omitted without restarting group')
****** \fort90\hjcfit\stab_hjc.for
           print 58
c          if(discprt) write(8,58)
58      format(' Bad gaps omitted without restarting group')
******

****** STABPLOT.FOR
         print 20,ifirst(i1),ifirst(i2)
         if(pon()) write(7,20) ifirst(i1),ifirst(i2)
         if(discprt) write(8,20) ifirst(i1),ifirst(i2)
20       format(10x,' Index of first and last transitions= ',2i6)
****** \fort90\hjcfit\stab_hjc.for
         print 20,ifirst(i1),ifirst(i2)
c=       if(discprt) write(8,20) ifirst(i1),ifirst(i2)
20       format(10x,' Index of first and last transitions= ',2i6)
******

****** STABPLOT.FOR
c       if(nbg.gt.0) fmt(4)=star        !asterisk before TG
        print 60,tt,opbar(j),gbar(j),popen(j),freq,nval,nvalo,nvalg
        if(pon()) write(7,60)tt,opbar(j),gbar(j),popen(j),freq,nval,
     & nvalo,nvalg
        if(discprt) write(8,60)tt,opbar(j),gbar(j),popen(j),freq,nval,
     & nvalo,nvalg
60      format(f10.3,2x,f10.4,6x,f10.4,4x,f8.5,f10.4,3i5)
****** \fort90\hjcfit\stab_hjc.for
c       if(nbg.gt.0) fmt(4)=star        !asterisk before TG
c=      print 60,tt,opbar(j),gbar(j),popen(j),freq,nval,nvalo,nvalg
c=      if(discprt) write(8,60)tt,opbar(j),gbar(j),popen(j),freq,nval,
c=     & nvalo,nvalg
60      format(f10.3,2x,f10.4,6x,f10.4,4x,f8.5,f10.4,3i5)
******

****** STABPLOT.FOR
          if(pon()) write(7,15) nops,nsect,nbo,nbg
          if(discprt) write(8,15) nops,nsect,nbo,nbg
15        format(1x,i6,' open periods analysed in ',i4,' groups.',/,
****** \fort90\hjcfit\stab_hjc.for
          if(pon()) write(7,15) nops,nsect,nbo,nbg
c         if(discprt) write(8,15) nops,nsect,nbo,nbg
15        format(1x,i6,' open periods analysed in ',i4,' groups.',/,
******

****** STABPLOT.FOR
          if(pon()) write(7,17) nops,nsect,noptot1,ngtot,nbo,nbg
          if(discprt) write(8,17) nops,nsect,noptot1,ngtot,nbo,nbg
17        format(1x,i6,' open periods analysed in ',i4,' groups.',/,
****** \fort90\hjcfit\stab_hjc.for
          if(pon()) write(7,17) nops,nsect,noptot1,ngtot,nbo,nbg
c         if(discprt) write(8,17) nops,nsect,noptot1,ngtot,nbo,nbg
17        format(1x,i6,' open periods analysed in ',i4,' groups.',/,
******

****** STABPLOT.FOR
          if(pon()) write(7,16) nli,tcrit,nlo,nlg
          if(discprt) write(8,16) nli,tcrit,nlo,nlg
16        format(1x,i4,' open-shut pairs excluded because one of them is'
****** \fort90\hjcfit\stab_hjc.for
          if(pon()) write(7,16) nli,tcrit,nlo,nlg
c         if(discprt) write(8,16) nli,tcrit,nlo,nlg
16        format(1x,i4,' open-shut pairs excluded because one of them is'
******

****** STABPLOT.FOR
c Plot the results?
        print 25
25      format(' Plot these results [Y] ? ')
        read 101,ans
        if(UC(ans).eq.'N') RETURN
c
****** \fort90\hjcfit\stab_hjc.for
c Plot the results?
c       print 25
c25     format(' Plot these results [Y] ? ')
c       read 101,ans
c       if(UC(ans).eq.'N') RETURN
c
******

****** STABPLOT.FOR
        endif
c If nfile>1 then add vertical lines to show boundaries between pooled files
        if(nfile.gt.1) then
           isetcol=1
           ncalc=500            !# points to draw straight line
           if(ymin.gt.1.e-30) then
              aymin=alog10(ymin)
           else
                aymin=-30.
           endif
           dlogy=(alog10(ymax)-aymin)/float(ncalc-1)
           nsav=ncurvc  !# already defined (=1 or 3)
           ncurvc=ncurvc+nfile-1
           j=3  !3 calc curves already defined
           do i=1,nfile-1
                j=j+1           !=4,5,...
                icurvc(i+nsav)=j
                ncal(j)=ncalc
                do k=1,ncalc
                   alogy=aymin+float(k-1)*dlogy
                   ycal(k,j)=10**alogy
                   xcal(k,j)=float(iexend(i))
                enddo
                iline(j)=1      !dotted line
                icol(i+nsav+10)=11      !light blue
           enddo
        endif
c
c
        if(debug()) then
           do 84 i=1,nsect
84         print 85,i,xval(i,1),yval(i,1),xval(i,2),yval(i,2),
     &  xval(i,3),yval(i,3)
85         format(i8,6g11.4)
           pause
        endif
c
****** \fort90\hjcfit\stab_hjc.for
        endif
cc If nfile>1 then add vertical lines to show boundaries between pooled files
c       if(nfile.gt.1) then
c          isetcol=1
c          ncalc=500            !# points to draw straight line
c          if(ymin.gt.1.e-30) then
c             aymin=alog10(ymin)
c          else
c               aymin=-30.
c          endif
c          dlogy=(alog10(ymax)-aymin)/float(ncalc-1)
c          nsav=ncurvc  !# already defined (=1 or 3)
c          ncurvc=ncurvc+nfile-1
c          j=3  !3 calc curves already defined
c          do i=1,nfile-1
c               j=j+1           !=4,5,...
c               icurvc(i+nsav)=j
c               ncal(j)=ncalc
c               do k=1,ncalc
c                  alogy=aymin+float(k-1)*dlogy
c                  ycal(k,j)=10**alogy
c                  xcal(k,j)=float(iexend(i))
c               enddo
c               iline(j)=1      !dotted line
c               icol(i+nsav+10)=11      !light blue
c          enddo
c       endif
cc
cc
c       if(debug()) then
c          do 84 i=1,nsect
c84        print 85,i,xval(i,1),yval(i,1),xval(i,2),yval(i,2),
c     & xval(i,3),yval(i,3)
c85        format(i8,6g11.4)
c          pause
c       endif
c
******

****** STABPLOT.FOR
461     mbad=.true.
        print 46
46      format(
     & ' (1) Make a new stability plot',/,
     & ' (2) Replot this stability plot (eg shut time only)',/,
     & ' (3) Mark BAD regions with cursors for further analysis ',/,
****** \fort90\hjcfit\stab_hjc.for
461     mbad=.true.
        if(.not.stabcut) then   !NB stabcut may be true on entry
           print 46
46         format(
     & ' (1) Make a new stability plot',/,
     & ' (2) Replot this stability plot (e.g. shut time only)',/,
     & ' (3) Mark BAD regions with cursors for further analysis ',/,
******

****** STABPLOT.FOR
     & ' (4) Mark GOOD regions with cursors for further analysis ',/,
     & ' (5) Return to main menu of distributions',/,
     & ' Option number [5] = ')
        i=5
        call INPUTi(i)
        if(i.eq.0) i=5
        if(i.eq.1) goto 47
        if(i.eq.2) goto 252
        if(i.eq.4) mbad=.false.
        if(i.eq.5) goto 999     !return
c NON-MOUSE VERSION (use arrows to move cursor, and hit character to mark
****** \fort90\hjcfit\stab_hjc.for
     & ' (4) Mark GOOD regions with cursors for further analysis ',/,
     & ' (5) Finish stability plots',/,
     & ' Option number [5] = ')
           i=5
           call INPUTi(i)
           if(i.eq.0) i=5
           if(i.eq.1) goto 47
           if(i.eq.2) goto 252
           if(i.eq.4) mbad=.false.
           if(i.eq.5) goto 999  !return
        else if(stabcut) then
           call BELL(1)
           print 463
463        format(' Bad sections already removed',/,
     & ' (must restart program to redefine them)',/,
     & ' (1) Make a new stability plot',/,
     & ' (2) Replot this stability plot (e.g. shut time only)',/,
     & ' (3) Finish stability plots',/,
     & ' Option number [3] = ')
           i=3
           call INPUTi(i)
           if(i.eq.1) goto 47
           if(i.eq.2) goto 252
           if(i.eq.3) goto 999  !return
        endif
c NON-MOUSE VERSION (use arrows to move cursor, and hit character to mark
******

****** STABPLOT.FOR
c
        if(stabcut) then
           call BELL(3)
           print 71
71        format(' Sections already omitted: must restart to change them')
           goto 461
        endif
        stabcut=.true.  !to signal that bit has been cut out
****** \fort90\hjcfit\stab_hjc.for
c
        stabcut=.true.  !to signal that bit has been cut out
******

****** STABPLOT.FOR
        call VIDEOMOD(3)                !utility lib- this makes graph go for good!
c=      call REMBAD(tint,iampl,nint,nbad,isbad,iebad,pon())
****** \fort90\hjcfit\stab_hjc.for
        call VIDEOMOD(3)                !utility lib- this makes graph go for good!
c
        print 71,jset
        if(discprt) write(8,71) jset
71      format(' Bad sections removed for SET ',i3)
c=      call REMBAD(tint,iampl,nint,nbad,isbad,iebad,pon())
******

****** STABPLOT.FOR
           isetcol=0
c        Mark boundaries of pooled files
           if(nfile.gt.1) then
            ymax=0.0
            ymin=1.e30
            do i=1,ndat1(1)
                a=yval1(i,1)
                if(a.gt.ymax) ymax=a
                if(a.lt.ymin) ymin=a
            enddo
            isetcol=1
            ncalc1=20           !20 points to draw straight line
            dy=(ymax-ymin)/float(ncalc1-1)
            nsav=ncurvc1
            ncurvc1=ncurvc1+nfile-1
            j=nsav
            do i=1,nfile-1
                j=j+1           !=4,5,...
                icurvc1(j)=j
                ncal1(j)=ncalc1
                do k=1,ncalc1
                   ycal1(k,j)=ymin+float(k-1)*dy
                   xcal1(k,j)=float(iexend(i))
                enddo
                iline1(j)=1             !dotted line
                icol(j+10)=11   !light blue
            enddo
           endif
c
****** \fort90\hjcfit\stab_hjc.for
           isetcol=0
cc        Mark boundaries of pooled files
c          if(nfile.gt.1) then
c           ymax=0.0
c           ymin=1.e30
c           do i=1,ndat1(1)
c               a=yval1(i,1)
c               if(a.gt.ymax) ymax=a
c               if(a.lt.ymin) ymin=a
c           enddo
c           isetcol=1
c           ncalc1=20           !20 points to draw straight line
c           dy=(ymax-ymin)/float(ncalc1-1)
c           nsav=ncurvc1
c           ncurvc1=ncurvc1+nfile-1
c           j=nsav
c           do i=1,nfile-1
c               j=j+1           !=4,5,...
c               icurvc1(j)=j
c               ncal1(j)=ncalc1
c               do k=1,ncalc1
c                  ycal1(k,j)=ymin+float(k-1)*dy
c                  xcal1(k,j)=float(iexend(i))
c               enddo
c               iline1(j)=1             !dotted line
c               icol(j+10)=11   !light blue
c           enddo
c          endif
c
******

****** STABPLOT.FOR
4621    mbad=.true.
        print 462
462     format(
     & ' (1) Replot with different amplitude lines',/,
****** \fort90\hjcfit\stab_hjc.for
4621    mbad=.true.
        if(.not.stabcut) then   !NB stabcut may be true on entry
           print 462
462        format(
     & ' (1) Replot with different amplitude lines',/,
******

****** STABPLOT.FOR
     & ' (4) Mark GOOD regions with cursors for further analysis ',/,
     & ' (5) Return to main menu of distributions',/,
     & ' Option number [5] = ')
        i=5
        call INPUTi(i)
        if(i.eq.1) goto 508
        if(i.eq.2) goto 500
        if(i.eq.4) mbad=.false.
        if(i.eq.5) goto 999     !return
c NON-MOUSE VERSION (use arrows to move cursor, and hit character to mark
****** \fort90\hjcfit\stab_hjc.for
     & ' (4) Mark GOOD regions with cursors for further analysis ',/,
     & ' (5) Finish stability plots',/,
     & ' Option number [5] = ')
           i=5
           call INPUTi(i)
           if(i.eq.1) goto 508
           if(i.eq.2) goto 500
           if(i.eq.4) mbad=.false.
           if(i.eq.5) goto 999  !return
        else if(stabcut) then
           call BELL(1)
           print 464
464        format(' Bad sections already removed',/,
     & ' (must restart program to redefine them)',/,
     & ' (1) Replot with different amplitude lines',/,
     & ' (2) Make a different amplitude stability plot',/,
     & ' (3) Finish stability plots',/,
     & ' Option number [3] = ')
           i=3
           call INPUTi(i)
           if(i.eq.1) goto 508
           if(i.eq.2) goto 500
           if(i.eq.3) goto 999  !return
        endif
c NON-MOUSE VERSION (use arrows to move cursor, and hit character to mark
******

****** STABPLOT.FOR
c
        if(stabcut) then
           call BELL(3)
           print 71
c71       format(' Sections already omitted: must restart to change them')
           goto 4621
        endif
        stabcut=.true.  !to signal that bit has been cut out
****** \fort90\hjcfit\stab_hjc.for
c
        stabcut=.true.  !to signal that bit has been cut out
******

****** STABPLOT.FOR
c
c SEPARATE SECTION TO DISPLAY 'mean of nth op' etc USING VPLOT5
c idtype=104  Mean length of nth shut time for nth = 0, 1,...,n
c idtype=113  Mean length of nth open period for nth = 0, 1,...,n
c idtype=115  Mean open period adjacent to shut time in spec range
c idtype=116  Mean open period adjacent to open period in spec range
206     continue
        ndv1=nthmax+1
        ndimd=1
        if(allocated(icurvw)) then
****** \fort90\hjcfit\stab_hjc.for
c
999     continue
        if(allocated(xval)) DEALLOCATE(xval,yval,xcal,ycal)
        if(allocated(xval1)) DEALLOCATE(xval1,yval1,xcal1,ycal1)
        if(allocated(icurvw)) then
******

Resync failed.  Files are too different
****** STABPLOT.FOR
        endif
        kwi=nthmax+1
        kwj=1
        ALLOCATE(icurvw(ndimd),weight(kwi,kwj))
        icurvw(1)=1     !show SD
        ndc1=1024
        ndimc=1
        if(allocated(xval1)) DEALLOCATE(xval1,yval1,xcal1,ycal1)
        ALLOCATE(xval1(ndv1,ndimd),yval1(ndv1,ndimd))
        ALLOCATE(xcal1(ndc1,ndimc),ycal1(ndc1,ndimc))
        if(idtype.eq.104.or.idtype.eq.113) then
           if(idtype.eq.104) then
                do i=0,nthmax
                   xval1(i+1,1)=float(i)
                   yval1(i+1,1)=ameanval(i)
                   if(sdval(i).gt.1.e-18) then
                        weight(i+1,1)=1.0/sdval(i)**2
                   else
                        weight(i+1,1)=1.e-37
                   endif
              enddo
                ndat1(1)=nthmax+1
              xmin=0.
                titlex='n (nth shut time)'
                titley='mean shut time'
           else if(idtype.eq.113) then
                do i=1,nthmax
                   xval1(i,1)=float(i)
                   yval1(i,1)=ameanval(i)
                   if(sdval(i).gt.1.e-18) then
                        weight(i,1)=1.0/sdval(i)**2
                   else
                        weight(i,1)=1.e-37
                   endif
              enddo
                ndat1(1)=nthmax
              xmin=0.
              titlex='n (nth open period)'
                titley='mean open period'
           endif
           ncurvd1=1
           icurvd1(1)=1
           ijoin1(1)=0          !do not join
           isym1(1)=-7          !circle
           ncurvc1=1
           icurvc1(1)=1
           ncalc=1024
           ncal1(1)=ncalc
           iline1(1)=2          !dashed line
           xmax=float(nthmax+1)
           dxcalc=(xmax-xmin)/float(ncalc-1)
           do i=1,ncalc
                xcal1(i,1)=xmin+float(i-1)*dxcalc
                if(idtype.eq.104) then
                   ycal1(i,1)=shutav
                else if(idtype.eq.113) then
                   ycal1(i,1)=openav
                endif
           enddo
        endif
        csize=2.5
        ifont=3
        ifitype=0               !no display of fitted parameters
        ilabel=1
        ilog=0
        ntx=5           !if not logt set initial input values
        nty=5
        itx=1
        ity=1
        iscal=1         !scale internally
        xlo=-1          !whole screen
        symsiz(1)=-1.0  !default symbol sizes
        doframe=.true.
        landscap=.true.
        autplt=.false.
        fitted=.true.
        ivplot=.false.
        ncjump=0
        nvjump=0
        iask=-2 !erase on exit from VPLOT; do not ask
        lt2=2
        xw=0.0
        inumy=-1
c       isetcol=0
        isetcol=1
        do i=1,100
           icol(i)=-1
        enddo
        icol(11)=12             !calc line red
        itrace=0
        call VPLOT5(xval1,yval1,ndat1,icurvd1,ncurvd1,ijoin1,symsiz,ndimd,
     & xcal1,ycal1,ncal1,icurvc1,ncurvc1,iline1,ndimc,isym1,ilog,iscal,
     & xmin,xmax,ymin,ymax,xtic,ytic,xcross,ycross,ntx,nty,itx,ity,
     & XLO,XHI,YLO,YHI,y0,yinf,inumx,inumy,ncjump,nvjump,ivplot,
     & titlex,titley,ilabel,doframe,idiskq,autplt,plotonly,itit,title1,
     & cbig,ifont,landscap,fitted,iask,theta,ifitype,ncomp,interp,
     & isetcol,itrace,ndv1,ndc1,weight,kwi,kwj,icurvw,kmax,iver)
c End of display of 'nth shut time' etc
c
999     continue
****** \fort90\hjcfit\stab_hjc.for
        endif
        RETURN
        END

******


