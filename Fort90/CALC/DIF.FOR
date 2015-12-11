Comparing files QWIND2.FOR and qwind3.for
****** QWIND2.FOR
        subroutine QWIND2(QT,ni,nidisp,ncon,ic,ncyc,im,jm,
     & nr1,nc1,ncdep,ix,jx,iflag,titlep,IQ,itry)
c 06/04/90 10:45pm QWIND2 is routine for QGEN2 that displays window
****** qwind3.for
        subroutine QWIND3(QT,ni,nidisp,ncon,ic,ncyc,im,jm,
     & nr1,nc1,ncdep,ix,jx,iflag,titlep,IQ,data,itry)
c QWIND3 is version of QWIND2 that (like the single prec version, QWINDs)
c outputs DATA as single precision 1-D array (=parameters, THETA)
c 06/04/90 10:45pm QWIND2 is routine for QGEN2 that displays window
******

****** QWIND2.FOR
c in declaration of CDATA and in definition of NCN).
c TAB: Move to next number (scrolls when nec)
c INS or SHIFT-TAB: Move to previous number (scrolls when nec)
****** qwind3.for
c in declaration of CDATA and in definition of NCN).
c ENTER or TAB: Move to next number (scrolls when nec)
c INS or SHIFT-TAB: Move to previous number (scrolls when nec)
******

****** QWIND2.FOR
c END: moves down to last nidisp values
c ESC or ENTER: finished.
c When all entered hit ESC/ENTER
c The string of values is converted
****** qwind3.for
c END: moves down to last nidisp values
c ESC: finished.
c When all entered hit ESC.
c The string of values is converted
******

****** QWIND2.FOR
c separate character representation of  each number in CDATA.
c       real data(100,nj)
        real data(100)
****** qwind3.for
c separate character representation of  each number in CDATA.
        real data(100)
******

****** QWIND2.FOR
        integer scrnbuf1(1024),scrnbuf2(1024)
        integer*2 ktype,mr,mc
c       character*3 crow(99)            !for row numbers up to 99
        character*9 crow(99)            !for row labels
****** qwind3.for
        integer scrnbuf1(1024),scrnbuf2(1024)
        character*9 crow(99)            !for row labels
******

****** QWIND2.FOR
        common/gij/njm,ncn,ni1,nj1,nscrol,jcol
      INCLUDE '\lf90\lib\SPINATTI.HDR'          !modif version of SPINATTR.HDR
c      INCLUDE '\hgraph\CONSTS.FOR'     !no Hgraph here!
c
****** qwind3.for
        common/gij/njm,ncn,ni1,nj1,nscrol,jcol
      INCLUDE 'c:\lf90\lib\SPINATTi.HDR'                !modif version of SPINATTR.HDR
c
******

****** QWIND2.FOR
        itry=0  !unless F3 hit
        title=' Enter rates [1/(Ms) for assoc rates]'
        heading='  i,j    Rate const     Value'
****** qwind3.for
        itry=0  !unless F3 hit
        title=' Enter q(i,j) values'
        heading='  i,j    Rate const     Value'
******

****** QWIND2.FOR
c       call CLS(24,0,24,80)    !clear bottom row only (in case model on display)
        call CLRAREA(24,0,24,80)        !clear bottom row only (in case model on display)
        call LOCATE(24,5)
****** qwind3.for
c       call CLS(24,0,24,80)    !clear bottom row only (in case model on display)
        call LOCATE(24,5)
******

****** QWIND2.FOR
        endif
        call DEFWIND(4,5,30,20,72,dblbdr+scrol,ihatt)
c Total number of cols needed=
****** qwind3.for
        endif
        call DEFWIND(4,5,30,20,72,dblbdr+wrap+scrol,ihatt)
c Total number of cols needed=
******

****** QWIND2.FOR
c One row for titles, up to 24 at a time for numbers
c       normatt=bright+dblubkg          !'normal' attribute
        normatt=bright+bluebkg          !'normal' attribute
        call DEFWIND(3,nr1,nc1,nr2,nc2,singlbd+scrol,normatt)
        call FILLB(3,' ',current)
c Draw the window
c       call OPENWIND(3)
        nscrol=0        !initialise number of lines scrolled
****** qwind3.for
c One row for titles, up to 24 at a time for numbers
        normatt=bright+dblubkg          !'normal' attribute
        call DEFWIND(3,nr1,nc1,nr2,nc2,singlbd+wrap+scrol,normatt)
        call FILLB(3,' ')
c Draw the window
        call CLRB(3)
        nscrol=0        !initialise number of lines scrolled
******

****** QWIND2.FOR
        call LOCATEW(3,iw,jw)   !put cursor there
        call GETPOSW(3,mr,mc)   !NB mr,mc must be int*2       why?.........
        mr1=mr                  !integer*4
****** qwind3.for
        call LOCATEW(3,iw,jw)   !put cursor there
        call GETPOSW(3,mr,mc)   !NB mr,mc must be int*2
        mr1=mr                  !integer*4
******

****** QWIND2.FOR
c
        if((ktype.eq.16.and.ival.eq.27).or.
     &  (ktype.eq.4.and.ival.eq.13)) goto 99            !ESC or ENTER to finish
        if(ktype.eq.0.and.ival.eq.59) goto 90   ! F1 key=help
****** qwind3.for
c
        if(ktype.eq.16) goto 99                         !ESC to end
        if(ktype.eq.0.and.ival.eq.59) goto 90   ! F1 key=help
******

****** QWIND2.FOR
c##?       call DISPSCN(scrnbuf1)     !restore orig screen ##
c          call CLS(nr1,nc1,nr2,nc2)
           call CLRAREA(nr1,nc1,nr2,nc2)
           if(iv al.eq.116) nc1=nc1+1   !CTRL-right arrow
           if(ival.eq.115) nc1=nc1-1    !CTRL-left arrow
****** qwind3.for
c##?       call DISPSCN(scrnbuf1)     !restore orig screen ##
           call CLS(nr1,nc1,nr2,nc2)
           if(ival.eq.116) nc1=nc1+1    !CTRL-right arrow
           if(ival.eq.115) nc1=nc1-1    !CTRL-left arrow
******

****** QWIND2.FOR
c ENTER or TAB
        else if((ktype.eq.8.and.ival.eq.9)) then        !TAB key
c     & (ktype.eq.4.and.ival.eq.13)) then               !TAB or ENTER key
c ENTER or TAB key- move to start of next box (next number=next cdata(i,j)
****** qwind3.for
c ENTER or TAB
        else if((ktype.eq.8.and.ival.eq.9).or.
     & (ktype.eq.4.and.ival.eq.13)) then                !TAB or ENTER key
c ENTER or TAB key- move to start of next box (next number=next cdata(i,j)
******

****** QWIND2.FOR
                else if(in.eq.nscrol) then      !scroll down one line
c         NB for some reason the scroll down, with -1 as arg, does not
c               seem to work like scroll up, but just deletes the whole
c               window. Therefore rewrite whole window for now
cIn general i=1+nscrol to i=nidisp+nscrol are in display, so move back 1 row
c i=nscrol to nidisp+nscrol-1; but as nscrol decremented first use former
c                  call SCROLLW(3,-1)   !scroll down one line
c                  call BLDB(3,0,0,blank(1:nc-1),normatt)       !delete row 0
c                  call BLDB(3,0,0,title(1:nc-1),normatt)       !rewrite headings
c                  call BLDB(3,1,0,blank(1:nc-1),normatt)       !delete row 1
c                  call BLDB(3,1,0,heading(1:nc-1),normatt)
                   nscrol=nscrol-1
****** qwind3.for
                else if(in.eq.nscrol) then      !scroll down one line
                   nscrol=nscrol-1
******

****** QWIND2.FOR
           mc1=mc               !int*4
c======
c          call GETIJ2(mr1,mc1,in,jn,k,bad)     !get i,j in CDATA(i,j)
c          if(in.ne.ilast.or.jn.ne.jlast.or.bad) then !in another cell, or no cell
c             call GETRC(ilast,jlast,nidisp,iw,jw,jw2,bad1)
c             if(.not.bad1) then
c                  call BLDB(3,iw,jw,blank(1:ncn),normatt)
c                  call BLDB(3,iw,jw,CDATA(ilast,jlast),normatt)
c             endif
c             ilast=in                  !record posn of last inverse-contrast box
c             jlast=jn
c          else if((in.ne.ilast.or.jn.ne.jlast).and.(.not.bad)) then
c             call GETRC(in,jn,nidisp,iw,jw,jw2,bad1)
c             if(.not.bad1) then
c                  call BLDB(3,iw,jw,blank(1:ncn),inverse)
c                  call BLDB(3,iw,jw,CDATA(in,jn),inverse)
c             endif
c             ilast=in                  !record posn of last inverse-contrast box
c             jlast=jn
c          endif
c===
c          mr1=mr               !int*4
c          mc1=mc               !int*4
c          call GETIJ2(mr1,mc1,in,jn,k,bad)     !get i,j in CDATA(i,j)
c          if(.not.bad.and.(in.ne.ilast.or.jn.ne.jlast)) then     !in another cell
c             call GETRC(ilast,jlast,nidisp,iw,jw,jw2,bad1)
c             if(.not.bad1) then
c                  call BLDB(3,iw,jw,blank(1:ncn),normatt)
c                  call BLDB(3,iw,jw,CDATA(ilast,jlast),normatt)
c                call GETRC(in,jn,nidisp,iw,jw,jw2,bad1)
c                  call BLDB(3,iw,jw,blank(1:ncn),inverse)
c                  call BLDB(3,iw,jw,CDATA(in,jn),inverse)
c                ilast=in                       !record posn of last inverse-contrast box
c                jlast=jn
c             endif
c          endif
cc======
c RIGHT ARROW
c====   else if(ktype.eq.8.and.ival.eq.77) then         !RIGHT arrow key
           if(ival.eq.77) then          !RIGHT arrow key
c          At present right arrow will not cause scroll when end of display
c          reached
c#              call GETPOSW(3,mr,mc)   !NB mr,mc must be int*2
c#              mr1=mr          !int*4
c#              mc1=mc          !int*4
c               if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
                mc=mc+1                 !move cursor right
****** qwind3.for
           mc1=mc               !int*4
           if(ival.eq.77) then          !RIGHT arrow key
                mc=mc+1                 !move cursor right
******

****** QWIND2.FOR
                goto 64 !check if new box entered
c
c       else if(ktype.eq.8.and.ival.eq.75) then         !LEFT arrow key
           else if(ival.eq.75) then             !LEFT arrow key
c         At present left arrow will not cause scroll when start of display
c         reached
c#              call GETPOSW(3,mr,mc)   !NB mr,mc must be int*2
c#              mr1=mr          !int*4
c#              mc1=mc          !int*4
c               if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
                mc=mc-1                 !move cursor left
****** qwind3.for
                goto 64 !check if new box entered
           else if(ival.eq.75) then             !LEFT arrow key
                mc=mc-1                 !move cursor left
******

****** QWIND2.FOR
c DOWN ARROW
c       else if(ktype.eq.8.and.ival.eq.80) then         !DOWN arrow key
           else if(ival.eq.80) then             !DOWN arrow key
c         At present down arrow will not cause scroll when end of display
c         reached
c#              call GETPOSW(3,mr,mc)   !NB mr,mc must be int*2
c#              mr1=mr          !int*4
c#              mc1=mc          !int*4
c               if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
                if(mr.lt.nr-2) mr=mr+1                  !move cursor down
****** qwind3.for
c DOWN ARROW
           else if(ival.eq.80) then             !DOWN arrow key
                if(mr.lt.nr-2) mr=mr+1                  !move cursor down
******

****** QWIND2.FOR
c UP ARROW
c       else if(ktype.eq.8.and.ival.eq.72) then         !UP arrow key
           else if(ival.eq.72) then                     !UP arrow key
c#              call GETPOSW(3,mr,mc)   !NB mr,mc must be int*2
c#              mr1=mr          !int*4
c#              mc1=mc          !int*4
c               if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
                if(mr.gt.2) mr=mr-1
****** qwind3.for
c UP ARROW
           else if(ival.eq.72) then                     !UP arrow key
                if(mr.gt.2) mr=mr-1
******

****** QWIND2.FOR
c BACKSPACE
c       else if(ktype.eq.8.and.ival.eq.8) then          !BACKSPACE key
           else if(ival.eq.8) then                      !BACKSPACE key
c#              call GETPOSW(3,mr,mc)   !NB mr,mc must be int*2
c               if(caplock()) call ERRMES(mr1,mc1,0,0,0,mr,mc)
                mc=mc-1                 !move cursor left
****** qwind3.for
c BACKSPACE
           else if(ival.eq.8) then                      !BACKSPACE key
                mc=mc-1                 !move cursor left
******

****** QWIND2.FOR
15      call CHTOREAL(cdata(i,2),data(i))
c       if(caplock()) then
c          do 6 i=1,ni
c          print 7,i,(cdata(i,j),j=1,nj)
c6         print 71,i,(data(i,j),j=1,nj)
c7         format(i4,2x,5a11)
c71        format(i4,2x,5g13.6)
c          pause
c       endif
c
****** qwind3.for
15      call CHTOREAL(cdata(i,2),data(i))
c
******

****** QWIND2.FOR
        call CAPTSCN(scrnbuf2)
c       call OPENWIND(4)
        call CLRB(4)
        call BLDB(4,0,0,'TAB:          move to next number',current)
        call BLDB(4,1,0,'INS/SHFT-TAB  back to previous number',current)
        call BLDB(4,2,0,'ARROWS:       move round in current window',
     &  current)
        call BLDB(4,3,0,'PAGE-DOWN:    moves down one screenful',current)
        call BLDB(4,4,0,'PAGE-UP:      moves up one screenful',current)
        call BLDB(4,5,0,'HOME:         moves up to start',current)
        call BLDB(4,6,0,'END:          moves down to end',current)
        call BLDB(4,7,0,'BACKSPACE:    erase',current)
        call BLDB(4,8,0,'ESC or ENTER: finished (and leave HELP)',current)
        call BLDB(4,10,0,'F2: move to/from rate constant name column',
     &  current)
        call BLDB(4,11,0,'F3: exit to read rate constants again     ',
     &  current)
        call BLDB(4,12,0,'CTRL-ARROW: moves whole window left/right',
     &  current)
        call BLDB(4,13,0,'* = Conc dep rate; '//char(240)//
     & ' = Micro rev rate',current)
        call ANYKEY
****** qwind3.for
        call CAPTSCN(scrnbuf2)
        call CLRB(4)
        call BLDB(4,0,0,'ENTER or TAB: Move to next number')
        call BLDB(4,1,0,'INS or SHFT-TAB: Back to previous number')
        call BLDB(4,2,0,'ARROWS:    move round in current window')
        call BLDB(4,3,0,'PAGE-DOWN: moves down one screenful')
        call BLDB(4,4,0,'PAGE-UP:   moves up one screenful')
        call BLDB(4,5,0,'HOME:      moves up to start')
        call BLDB(4,6,0,'END:       moves down to end')
        call BLDB(4,7,0,'BACKSPACE:  erase')
        call BLDB(4,8,0,'ESC:       finished (and leave HELP)')
        call BLDB(4,10,0,'F2: move to/from rate constant name column')
        call BLDB(4,11,0,'F3: exit to read rate constants again     ')
        call BLDB(4,12,0,'CTRL-ARROW: moves whole window left/right')
        call BLDB(4,13,0,'* = Conc dep rate; '//char(240)//
     & ' = Micro rev rate')
        call ANYKEY
******


