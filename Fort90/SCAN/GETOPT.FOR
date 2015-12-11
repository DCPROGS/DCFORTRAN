	subroutine GETOPT(ixin1,iyin1,ixup1,iyup1,n1,pchar1,
     & txtwin1,ictxt1,nout1,achar,ibkwin1,inc,incabs,ndisp,
     & ixmes,iymes,ch,iybar1,icbar1,iabar1,ii,iybar2,icbar2,
     & iabar2,iybar3,icbar3,iabar3,ixfill,iyfill,ibk,ibk1,end,
     & stopped,backward,indfst,isdfst,isec,nbuf,sdone,
     & ixcr,iycr,iccr,ixframe,iyframe,ivl,ibdpp,ilabel)
c To separate IV menus in SCAN
c 'window' option (part screen for expand mode) removed for now
c========add 'baseline done' to menu if NOT sdone
	integer*2 txtwin1(ixin1:ixup1,iyin1:iyup1)
	integer*4	iabar1(0:641),iabar2(0:641),iabar3(0:641)
	character*1	achar,ch
	character*1 pchar1(1:n1)
	logical end,backward,stopped,sdone
c
	idest=0	!clear any value left from last call
c
	call MENU_OPTIONS(nout1,achar,pchar1,iret) !convert nout or achar to IRET
	select case(iret)
		case(1) 			!Roll on
			call set_cursor(0,14)
			if(end) then
				idest=30
				goto 99	!RETURN
			endif
			call fillwin(16,624,iyin1,iyup1,ibkwin1)
			call fillwin(320,472,iyin1,iyup1,3)
			call fillwin(16,320,iyin1,iyin1+16,3)
			call fillwin(16,168,iyup1-16,iyup1,15)
			call write_text(txtwin1,ixin1,iyin1,ixup1,iyup1,
     &		ictxt1)
			call set_cursor(0,14)
			inc=incabs		!set increment positive
			backward=.false.
			stopped=.false.

		case(2)		!Increase speed
			if(incabs.lt.(ndisp-16)) then
				incabs=incabs+16
				inc=incabs
				if(backward) inc=-incabs
			endif
c			goto 30
			idest=30
			goto 99	!RETURN

		case(3)		!Baseline
			call set_cursor(ixmes,iymes)
			call det_string('<Enter>=continue; \/=down; /\=up'
     &		,14,ch)
			call move_line(iybar1,icbar1,iabar1,ii)
			call fillwin (ixfill,ixfill+300,iyfill-16,iyfill,ibk1)
c			goto 30
			idest=30
			goto 99	!RETURN

		case(4)		!Next section
			end=.false.
c===========surely COULD be at end??
			isec=isec+1
			call FILLWIN(0,640,90,390,ibk1) !fill data area with colour ibk
c			call fillwin(0,640,90,480,ibk)
			isdfst=1
			indfst=1 + (isec-1)*nbuf
c			goto 333
			idest=333
			goto 99	!RETURN

		case(5)		!Go back
			if(indfst.lt.incabs) then
				idest=25
				goto 99	!RETURN
			endif
			call fillwin(16,624,iyin1,iyup1,ibkwin1)
			call fillwin(320,472,iyin1,iyup1,3)
			call fillwin(16,320,iyin1,iyin1+16,3)
			call fillwin(16,168,iyin1+32,iyin1+48,15)
			call write_text(txtwin1,ixin1,iyin1,ixup1,iyup1,
	&		ictxt1)
			end=.false.
c			goto 654
			inc=-incabs
			backward=.true.
			goto 99	!RETURN

		case(6)		!Decrease speed
			if(incabs.ge.32) then
				incabs=incabs-16
				inc=incabs
				if(backward) inc=-incabs
			endif
			idest=30
			goto 99	!RETURN

		case(7)	!Amplitude line
			call set_cursor(ixmes,iymes)
			call det_string('<Enter>=continue; \/=down; /\=up'
     &		,14,ch)
			call move_line(iybar2,icbar2,iabar2,ii)
			call fillwin (ixfill,ixfill+300,iyfill-16,iyfill,ibk1)
			idest=30
			goto 99	!RETURN

		case(8)		!Stop
			stopped=.true.
			idest=25		!wait for new option
			goto 99	!RETURN

		case(9)		!Expand=screen2
			idest=800		!expand mode now at label 800
			goto 99
c		   	call screen2(ixin2,iyin2,ixup2,iyup2,text2,
c     &    		TXTWIN2,iFWIN2,iCTXT2,n2,
c     &		IDATA,isdfst,ndisp,indfst,nrlast,nsec,
c     &		iybar1,icbar1,iybar2,icbar2,ibk,
c     &		expfac,isec,pch,0,
c     &		nbuf,novlap)
c			call draw(ix,idata,ndisp,jfst,ipen,ipen,ibk)
c			call hline(1,639,iybar1,icbar1)
c			call hline(1,639,iybar2,icbar2)
c			call hline(1,639,iybar3,icbar3)
c			goto 22

		case(10)		!Change ndisplay
			end=.false.
707			continue
			call fillwin (ixfill,ixfill+300,iyfill-16,iyfill,0)
			call set_cursor(ixmes,iymes)
			call det_string('Ndispl = [100 to 2048] :',
	&		icstr,ch)
			read(*,*,err=707) ndisp
			if (ndisp.lt.100.or.ndisp.gt.2048) then
				goto 707
			endif
			call fillwin (ixfill,ixfill+300,iyfill-16,iyfill,ibk1)
			call fillwin(0,640,128,450,ibk1)
			idest=5
			goto 99	!RETURN

		case(11)		!Threshold
			call set_cursor(ixmes,iymes)
			call det_string('<Enter>=continue; \/=down; /\=up'
     &		,14,ch)
			call move_line(iybar3,icbar3,iabar3,ii)
			call fillwin (ixfill,ixfill+300,iyfill-16,iyfill,ibk1)
			idest=30
			goto 99	!RETURN

		case(12)		!Quit
			idest=8
			goto 99	!RETURN

		case(13)          !Move red cursor
			end=.false.
			call set_cursor(ixmes,iymes)
			call det_string('<Enter>=continue; <-=left; ->=right'
     &		,14,ch)
			CALL move_red_cursor(ixcr,iycr,iccr,ixframe,ibk)
			ivl=indfst/ibdpp
			indfst=float(ixcr)*ibdpp
			call FILLWIN(0,640,90,390,ibk1) !fill data area with colour ibk
c			call fillwin(0,640,90,480,ibk)
			call vline(ivl,iyFRAME,iyFRAME+7,0)
			idest=444
			goto 99	!RETURN

		case(14)  		!Other  file

		case(15)       		!Window
		   if(.not.sdone) then
			idest=32
			goto 99
		   endif
c======temp code for SD calc
c 'window' option removed for now
c			call screen2(ixin2,iyin2,ixup2,iyup2,text2,
c     &				TXTWIN2,iFWIN2,iCTXT2,n2,
c     &				IDATA,isdfst,ndisp,indfst,nrlast,nsec,
c     &				iybar1,icbar1,iybar2,cbar2,ibk,
c     &				expfac,isec,pch,ixin2-8,
c     &				nbuf,novlap)
c
c			call draw(ix,idata,ndisp,jfst,ipen,ipen,ibk)
c			do i=0,639
c				call rpixel(i,iybar1,iabar1(i))
c				call rpixel(i,iybar2,iabar2(i))
c				call rpixel(i,iybar3,iabar3(i))
c     			end do
c			call hline(1,639,iybar1,icbar1)
c			call hline(1,639,iybar2,icbar2)
c			call hline(1,639,iybar3,icbar3)
c			idest=22
c			goto 99	!RETURN
c

		case(16)    		!Help
			call first_help(ibk)
			idest=30
			goto 99	!RETURN
	end select
c
99	continue
c Code the idest for simplicity on exit
	if(idest.eq.0) then
	   ilabel=0		!if no 'goto' specified
	else if(idest.eq.5) then
	   ilabel=1
	else if(idest.eq.8) then
	   ilabel=2
	else if(idest.eq.22) then
	   ilabel=3
	else if(idest.eq.25) then
	   ilabel=4
	else if(idest.eq.30) then
	   ilabel=5
	else if(idest.eq.77) then
	   ilabel=6
	else if(idest.eq.333) then
	   ilabel=7
	else if(idest.eq.444) then
	   ilabel=8
	else if(idest.eq.800) then
	   ilabel=9
	else if(idest.eq.32) then
	   ilabel=10
	endif
c
	RETURN
	end

