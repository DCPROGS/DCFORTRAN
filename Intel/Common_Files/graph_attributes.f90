subroutine graph_attributes(main,callid,modplot,lframe,jindex,ind_1,oldrecords,readrec,newfile,&
		graphics1_1,combo1_6,combo1_4,combo1_5,combo1_8,combo1_9,combo1_10,istate,hdisp,izoom,xtitle,ytitle,&
		xmin0,xmax0,ymin0,ymax0,itogglepanel,itext_entry,new_text_entry,&
		iplot,iptype,rescalex,isym1,symsiz1,combo1_11,combo1_12,imodax,iaminax,iamaxax,iatic,&
		imtext3,irxx,iryy,&
		calbarx,calbary,xbeg4,ybeg4,xend4,yend4,&
		ivalx1,ivaly1,ivalx2,ivaly2,ivaltx,ivalty,iax_entry)
		!,&
        !xbeg5,ybeg5,xend5,yend5,d_line,d_hline,d_vline,d_arrow,d_poly,d_text,d_zoom,o_state,&
		!  c_state,s_text,s_line,s_arrow,zoom,link,delete_state,move_state)

!Modif Nov 06 so can replot (especially shut time pdf) showing data
! curves for shut times beyond tcrit.  Normally when tcrit used (burst(j)=true)
! only shut times =< trit are binned, but when rescalex=.true. bin all the
! shut times up to new xmax

!IDRAW(i) = 1 to draw (without box) at the position that has been already
!			defined in rx,ry,rxbox,rybox (no need to define box again)
!	     = 0 to omit text
!	     =-1 to draw with box (imode=1), as for idraw=1 or underline(imode=2)
!	     =-2 when text position not yet defined; there are two sorts
!	     = 2 underline
!	     = 3 italic
!         
!
! IDRAW,ICOL,THICK,ITYPE,IFNT,ANGLE,IJUS,RXBOX,RYBOX,RX,RY,SIZES
!
!    1 = title,				
!	 2 = parameter values
!	 3 = x label, 			
!	 4 = y label
!    5 = z label
!	 6:30 = x numbers  (601:625)		
!    31:55 = y numbers  (701:725)       
!    56:80 = z numbers  (801:825
!    81:100 = extra text (901:920)
!
! IDRAW,ICOL,THICK,ITYPE
!
!    101-150= data/symbols 151-200= calc curves,
!    201-210= arrows       211-220= lines
!    221-230= h lines      231-240= v lines
!        241= cj bar          242= vj bar               243= sd bar
!        244= frame		245= axes
!
!  XBEG,YBEG,XEND,YEND
!      1- 10= arrows       11- 20= lines
!     21- 30= h lines      31- 40= v lines
!         41= cj bar           42= vj bar                43= sd bar
!
!***********************************************

	USE DFLIB
!	USE DFWIN
	use gino_f90
	use menu_f90
	
	use hjcrecords
	
!include '\intel\common_files\cvfit_definitions.f90'
TYPE (RECORD_ATTRIBUTES) oldrecords(25)
CHARACTER*1 CHA
integer intoggle(100)
logical newfile,calbarx,calbary,hdisp
integer :: itogButton,callid
integer iradio_toggle(200),iradiox(200),iradioy(200),isym1(20)
real radiox(200),radioy(200),symsiz1(20)
character*(60) radio_text(200),titlerp
character*200 mtext3
logical logscale		!for modify_axis
logical rescalex
integer graphics1_1(100),combo1_6,combo1_4,combo1_5,combo1_8,combo1_9,combo1_10
integer combo1_11,combo1_12

    CHARACTER*70 textcombo
    CHARACTER*80 MTEXT1,MTEXT2
    CHARACTER*80 NEWTEXT(20),MTEXT(10)
	integer IFNT(100),IJUS(100)
    real SIZEtext(100),ANGLE(100),THICK(250)
	real RX(100),RY(100),RXBOX(4,100),RYBOX(4,100)
	integer ICOL(250),ITYPE(250),IDRAW(250)
	real XBEG(50),YBEG(50),XEND(50),YEND(50),C_THICK(250)
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25),cnumz(25)
	character*10 cexpz(25)
	real xb(10),yb(10),xe(10),ye(10)		!up to 10 arrows
	real xlb(10),ylb(10),xle(10),yle(10)	!up to 10 extra lines
	integer iltype(10)				!line type for ditto
	
	real*4 t1v(10),t2v(10),t1c(10),t2c(10)	!for jump logos and t=0 def.
	real*4 yhline(10)		!record y value
	real*4 xhlb(10),xhle(10)     !start/end of HLINE
	integer ilhtype(10)	!line type for horizontal lines
	
	real*4 xvline(10)		!record x value
	real*4 yvlb(10),yvle(10)     !start/end of VLINE
	integer ilvtype(10)	!line type for vertical lines
	real ameant(100),areat(100)
	logical redrawn,plotcols,mono,ivplot,plot
	logical doframe,landscap,autplt,plotrue
	logical d_line,d_hline,d_vline,d_arrow,d_poly,d_text,d_zoom,o_state,c_state,zoom
	logical s_text,s_line,s_arrow,sepy,newname
	real xint(2048),yint(2048),Y2int(2048)	!for interpolation
	character*40 titlex,titley,titlez
	logical bold,italik,underline,nolabx,ifontrue
	character*75 xtitle,ytitle,ztitle	!output from LAXES
integer :: iwid(10),istat(200),ifstat(200),numset(200),iopen(200),jopen(200),jopen2(200)
logical r_mouse,l_mouse,m_mouse,delete_state,move_state,write_par,show,link
CHARACTER*60 TITLE
	character*200 parval
	character*200 str(100)
	character*64 title1
	character datew*11,timew*8	
logical logx,logy,sqrty,logity,readrec

COMMON/TPOS/IDRAW,ICOL,THICK,C_THICK,ITYPE,IFNT,ANGLE,IJUS,&
     SIZEtext,RXBOX,RYBOX,&
     RX,RY,NARROW,NLINE,NHLINE,NVLINE, XBEG,YBEG,XEND,YEND,&
     NTEXT,NEWTEXT,CNUMX,CNUMY,CNUMZ,CEXPX,CEXPY,CEXPZ,&
     NUMBX,NUMBY,NUMBZ,IHLINREL,IVLINREL

common/logval/logx,logy,sqrty,logity
common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
	xmin,xmax,ymin,ymax


COMMON/JLOGOS/t1c,t2c,t1v,t2v,xoff1,y1v,y2v,y1c,y2c,ncjump,nvjump,ivplot

common/attributes_flag/d_line,d_hline,d_vline,d_arrow,d_poly,d_text,d_zoom,o_state,&
		c_state,s_text,s_line,s_arrow,zoom,link,delete_state,move_state                  
common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue
common/pixpos/ixgrid,iygrid,ixpix,iypix,wid,landscape

logscale=.false.	!default for modify_axis
rescalex=.false.	!until X axis rescaled
idev=0
red=0.
green=0.
blue=0.
2 continue
select case(callid)		   	
	    case(1:79,81:99)! select text
		if(readrec.and.modplot.gt.0) then
		    icallp=0
		    jindex=callid
			if(jindex.eq.7) jindex=31
			if(jindex.eq.8) jindex=56
            if(jindex.eq.9) jindex=81
			icoltempo=oldrecords(modplot)%attributes%icol(jindex)
			!oldrecords(modplot)%attributes%icol(jindex)=12
			if(jindex.lt.6.or.jindex.gt.80) then
			call write_string(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),12,&
			oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
		
			endif
			if(jindex.eq.6) then
			
			do jinde=6,6+oldrecords(modplot)%numbers%numbx
			if(mod(jinde-5,2).eq.0) then
		!	if(mod(i,2).eq.0.and.jindex.eq.6) then
		    nl=len_trim(oldrecords(modplot)%str(jinde))
		    if(nl.gt.3) nolabx=1
			if(numbx.gt.4.and.ipow.ne.-10000) goto 100
            if(numbx.gt.7.and.nolabx.eq.1) goto 100
		    endif
			call write_string(oldrecords(modplot)%str(jinde),oldrecords(modplot)%attributes%rx(jinde),&
			oldrecords(modplot)%attributes%ry(jinde),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),12,&
			oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
100			continue
		    enddo
			callid=391
			goto 2
			else if(jindex.eq.31) then
            do jinde=31,31+oldrecords(modplot)%numbers%numby
			call write_string(oldrecords(modplot)%str(jinde),oldrecords(modplot)%attributes%rx(jinde),&
			oldrecords(modplot)%attributes%ry(jinde),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),12,&
			oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
		    enddo
			callid=392
			goto 2
			endif
		
			ind_1=jindex
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
	!		call string_box(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
	!		oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
	!		oldrecords(modplot)%attributes%ijus(jindex),&
     !		oldrecords(modplot)%attributes%sizetext(jindex),&
	!		oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			endif
			if(callid.le.5) then
				callid=callid+250
				goto 2
			else if(callid.ge.81) then
			    mtext3=oldrecords(modplot)%str(jindex)
				
			!	call gmremovewindow(iradio)
			!	callid=256
			!	goto 2
			endif
		endif

		case(80)	! Select Text
			if(readrec.and.modplot.gt.0) then
			ntog=oldrecords(modplot)%numbers%ntext
			if(ntog.gt.0) then
			iold=1
			do i=1,ntog
			
						radio_text(i)=oldrecords(modplot)%STR(i+80)
			enddo
			itcall=80
			icallp=0
		    call radio_panel(main,iradio,ntog,iradio_toggle,radio_text,3,&
			iradiox,radiox,iradioy,radioy,itcall,titlerp)
			endif
			endif
		case(250)
				iold=0			
				ntext=oldrecords(modplot)%numbers%ntext
				ntext=ntext+1
				oldrecords(modplot)%numbers%ntext=ntext
				jindex=80+ntext
				oldrecords(modplot)%attributes%idraw(jindex)=1
				mtext3=' '
				icallp=250
				callid=256
				goto 2
		
		case(101:149,151:199)
		jindex=callid
		case(201:240)
		jindex=callid
		call gmremovewindow(iradio)	
	
			ax1=oldrecords(modplot)%lines%xbeg(jindex-200)
			ay1=oldrecords(modplot)%lines%ybeg(jindex-200)
			ax2=oldrecords(modplot)%lines%xend(jindex-200)
            ay2=oldrecords(modplot)%lines%yend(jindex-200)
			if(logx) then
			!	ax1=10**ax1
			!	ax2=10**ax2
			endif
			if(logy) then
			!	ay1=10**ay1
			!	ay2=10**ay2
			endif
				call gmDefineKeyselectCallback(13,0)
		call change_axis(main,iax_entry,200,mtext1,ax1,ay1,ax2,ay2,axtic,aytic,&
			ivalx1,ivaly1,ivalx2,ivaly2,ivaltx,ivalty) 
		case(200)	
		call gmremovewindow(iradio)	  
		case(100,150)
		call gmremovewindow(iradio)	
		case(245,263) ! axis frame
			if(readrec.and.modplot.gt.0) then
			jindex=callid
			if(callid.eq.263) then
			jindex=245
		!	if(iplot.eq.2) rescalex=.true.
			endif
			mtext1='Modify axis'
			ax1=oldrecords(modplot)%xmin
			ay1=oldrecords(modplot)%ymin
			ax2=oldrecords(modplot)%xmax
            ay2=oldrecords(modplot)%ymax
			if(logx) then
				ax1=10**ax1
				ax2=10**ax2
			endif
			if(logy) then
				ay1=10**ay1
				ay2=10**ay2
			endif
			if(sqrty) then
			!	ay1=ay1**2
			!	ay2=ay2**2
			endif
			axtic=oldrecords(modplot)%param_axis%xtic
			aytic=oldrecords(modplot)%param_axis%ytic
			ind_1=jindex
			
			if(jindex.eq.245) then 
			call gmDefineKeyselectCallback(13,246)
			call change_axis(main,iax_entry,callid,mtext1,ax1,ay1,ax2,ay2,axtic,aytic,&
			ivalx1,ivaly1,ivalx2,ivaly2,ivaltx,ivalty)
			endif
			endif
			case(244)
	!		call change_axis(main,iax_entry,callid,mtext1,ax1,ay1,ax2,ay2,axtic,aytic,&
	!		ivalx1,ivaly1,ivalx2,ivaly2,ivaltx,ivalty)
			jindex=callid
			imes=gmdisplaymessagebox('Frame',&
			'To change colour,delete,etc,click on the menu bar icons',&
			ginformation,gok)
		case(246) !axis frame
		
			ax1=gmEnqValueSetting(ivalx1)
			ay1=gmEnqValueSetting(ivaly1)
			ax2=gmEnqValueSetting(ivalx2)
			ay2=gmEnqValueSetting(ivaly2)
			axtic=gmEnqValueSetting(ivaltx)
			aytic=gmEnqValueSetting(ivalty)
			xmin1=ax1
			xmax1=ax2
			ymin1=ay1
			ymax1=ay2
			call gmremovewindow(iax_entry)
			call gmDefineKeyselectCallback(13,0)
			if(logx) then
				if(xmin1.le.0 ) xmin1=0.01
				ax1=alog10(xmin1)
				if(xmax1.le.0)  xmax1=0.01
				ax2=alog10(xmax1) 
			endif
			if(logy) then
				if(ymin1.le.0 ) ymin1=0.01
				ay1=alog10(ymin1)
				if(ymax1.le.0)  ymax1=0.01
				ay2=alog10(ymax1) 
			endif
			if(sqrty) then
			!   	if(ymin1.le.0 ) ymin1=0.01
			!	ay1=sqrt(ymin1)
			!	if(ymax1.le.0)  ymax1=0.01
			!	ay2=sqrt(ymax1) 
		    endif
			oldrecords(modplot)%xmin=ax1
			oldrecords(modplot)%ymin=ay1
			oldrecords(modplot)%ymax=ay2
			oldrecords(modplot)%xmax=ax2
			oldrecords(modplot)%param_axis%xtic=axtic
			oldrecords(modplot)%param_axis%ytic=aytic 
		    do i=1,100
					oldrecords(modplot)%attributes%idraw(i)=-2
			enddo
			if(jindex.eq.245.and.iplot.eq.2) then
				rescalex=.true.
            endif
			callid=247	
			goto 1
	   case(248)
	   	call gmDefineKeyselectCallback(13,0)
	   	    sx=0.7*oldrecords(modplot)%dxs                                                     
			sy=0.7*oldrecords(modplot)%dys
			i=jindex-200
			xbi=(oldrecords(modplot)%lines%xbeg(i))
		    xei=(oldrecords(modplot)%lines%xend(i))
		
				ybi=(oldrecords(modplot)%lines%ybeg(i))
				yei=(oldrecords(modplot)%lines%yend(i))
			if(logx) then
				xbi=alog10(oldrecords(modplot)%lines%xbeg(i))
				xei=alog10(oldrecords(modplot)%lines%xend(i))
			endif
			if(logy) then
				ybi=alog10(oldrecords(modplot)%lines%ybeg(i))
				yei=alog10(oldrecords(modplot)%lines%yend(i))
			endif
			if(sqrty) then
				ybi=sqrt(oldrecords(modplot)%lines%ybeg(i))
				yei=sqrt(oldrecords(modplot)%lines%yend(i))
			endif
				call gmremovewindow(iax_entry)
			if(i.le.10) then
			!call gmoveto2d(xbi,ybi)
	       ! call gdrawarrow2d(xei,yei,0)
			call ARROW1(xbi,ybi,xei,yei,10.,sx,sy,oldrecords(modplot)%attributes%itype(i+200),&
     	    oldrecords(modplot)%attributes%thick(200+i),0,idev)
			else
				call glincols(0,idev)
				call movto2(xbi,ybi)
	            call linto2(xei,yei)
			
			endif
			call gflushgraphics()
			ax1=gmEnqValueSetting(ivalx1)
			ay1=gmEnqValueSetting(ivaly1)
			ax2=gmEnqValueSetting(ivalx2)
			ay2=gmEnqValueSetting(ivaly2)
		    oldrecords(modplot)%lines%xbeg(jindex-200)=ax1
			oldrecords(modplot)%lines%ybeg(jindex-200)=ay1
			oldrecords(modplot)%lines%xend(jindex-200)=ax2
            oldrecords(modplot)%lines%yend(jindex-200)=ay2
		
			xbi=oldrecords(modplot)%lines%xbeg(i)
			xei=oldrecords(modplot)%lines%xend(i)
			ybi=oldrecords(modplot)%lines%ybeg(i)
			yei=oldrecords(modplot)%lines%yend(i)
	       
			sx=0.7*oldrecords(modplot)%dxs                                                     
			sy=0.7*oldrecords(modplot)%dys
			if(logx) then
				xbi=alog10(oldrecords(modplot)%lines%xbeg(i))
				xei=alog10(oldrecords(modplot)%lines%xend(i))
			endif
			if(logy) then
				ybi=alog10(oldrecords(modplot)%lines%ybeg(i))
				yei=alog10(oldrecords(modplot)%lines%yend(i))
			endif
			if(sqrty) then
				ybi=sqrt(oldrecords(modplot)%lines%ybeg(i))
				yei=sqrt(oldrecords(modplot)%lines%yend(i))
			endif
			call gmremovewindow(iax_entry)
			if(i.le.10) then
		!	call gmoveto2d(xbi,ybi)
	    !   call gdrawarrow2d(xei,yei,0)
			call ARROW1(xbi,ybi,xei,yei,10.,sx,sy,oldrecords(modplot)%attributes%itype(i+200),&
     	    oldrecords(modplot)%attributes%thick(200+i),oldrecords(modplot)%attributes%icol(200+i),idev)
			else
				call glincols(oldrecords(modplot)%attributes%icol(200+i),idev)
				call movto2(xbi,ybi)
	            call linto2(xei,yei)
			endif
				call gflushgraphics()
		case(251:256,391,392) !write title,label,text,parameters
			 if(modplot.eq.0) goto 1
		     if((readrec.or.newfile).and.oldrecords(modplot)%IPOS.ne.4) then
				iy=1
				if(callid.eq.251) then
				mtext1='Enter/Modify title:'
				title1=	oldrecords(modplot)%STR(1)

!	oldrecords(modplot)%STR(3)=titleX
!	oldrecords(modplot)%STR(4)=titleY
				mtext3=title1
				jindex=1
				else if(callid.eq.252) then
				mtext1='Enter/Modify parameters:'
				parval=	oldrecords(modplot)%STR(2)
				mtext3=parval
				iy=0
				jindex=2
			
				else if(callid.eq.253) then
				mtext1='Enter/Modify X label:'
				xtitle=oldrecords(modplot)%STR(3)
				mtext3=xtitle
				jindex=3
				else if(callid.eq.254) then
					ytitle=oldrecords(modplot)%STR(4)
				mtext3=ytitle
				mtext1='Enter/Modify Y label:'
				jindex=4
				else if(callid.eq.255.and.iptype.eq.40) then
					ztitle=oldrecords(modplot)%STR(5)
					mtext3=ztitle
					mtext1='Enter/Modify Z label:'
					jindex=5
				else if(callid.eq.256) then
				if(iold.eq.1) call gmremovewindow(iradio)
				mtext1='Enter new text:'
			    iold=0
				iy=2
				else if(callid.eq.391) then
				mtext1='Enter/Modify X axis numbers:'
			
				jindex=6
				else if(callid.eq.392) then
				mtext1='Enter/Modify Y axis numbers:'
			
				jindex=31
			
				endif
				continue
				ind_1=jindex
					call gmDefineKeyselectCallback(13,290)
				rxx=oldrecords(modplot)%attributes%rx(jindex)
				ryy=oldrecords(modplot)%attributes%ry(jindex)
				if(logx) then
					rxx=10**rxx
				endif
				if(logy) then
					ryy=10**ryy
				endif
				if(sqrty) then
					if(ryy.gt.0) ryy=ryy**2
				endif

				call add_new_text(main,callid,mtext1,mtext3,iy,itext_entry,new_text_entry,&
				ivalx1,rxx,ivaly1,ryy,jindex)
		
			endif
	   case(257:262)
	  
	   if(readrec.and.oldrecords(modplot)%IPOS.ne.4) then ! draw arrows,etc
		npoint=2
		if(callid.eq.257) then  !arrow
		move_state=.false.
		zoom=.false.
		 d_arrow=.true.
		 d_line=.false.
		 d_vline=.false.
		 d_hline=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 c_state=.false.
		 o_state=.false.
		 link=.false.
		 
		 ind_1=narrow+1
	!	 call widsta(new_arrows(ind_1),1)
	   else if(callid.eq.258) then  ! line
	   	zoom=.false.
		 d_line=.true.
		 d_hline=.false.
		 d_vline=.false.
		 d_arrow=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 c_state=.false.
		 o_state=.false.
		 link=.false.
		 
		 ind_1=nline+11
		 	
		 move_state=.false.
	!	 call widsta(new_lines(ind_1),1)
	   else if(callid.eq.259) then  ! h line
	   	zoom=.false.
		move_state=.false.
		 d_hline=.true.
		 d_line=.false.
		 d_vline=.false.
		 d_arrow=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 s_arrow=.false.
		 c_state=.false.
		 o_state=.false.
		 link=.false.
		 
		 ind_1=nhline+21

	!	 call widsta(new_lines(ind_1+10),1)
	   else if(callid.eq.260) then ! v line
	   	zoom=.false.
		 d_vline=.true.
		 d_line=.false.
		 d_hline=.false.
		 d_arrow=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 s_arrow=.false.
		 c_state=.false.
		 o_state=.false.
		 link=.false.
		 move_state=.false.
		 
		 ind_1=nvline+31
	!	 call widsta(new_lines(ind_1+20),1)
        else if (callid.eq.261) then
		 zoom=.true.
		 d_vline=.false.
		 d_line=.false.
		 d_hline=.false.
		 d_arrow=.false.
		 s_text=.false.
		 s_line=.false.
		 s_arrow=.false.
		 s_arrow=.false.
		 c_state=.false.
		 o_state=.false.
		 move_state=.false.
		 link=.false.
		 ind_1=45
		 jindex=45
		 call gmSetGuiCursor(lframe,Gsmallcross,Gdefault)
	    else if (callid.eq.262) then
			zoom=.false.
			move_state=.false.
			xmin10=oldrecords(modplot)%xmin10
	!		if(izoom.eq.1) then
			oldrecords(modplot)%xmin1=oldrecords(modplot)%xmin10
			oldrecords(modplot)%ymin1=oldrecords(modplot)%ymin10
			oldrecords(modplot)%ymax1=oldrecords(modplot)%ymax10
			oldrecords(modplot)%xmax1=oldrecords(modplot)%xmax10
			xmin=oldrecords(modplot)%xmin10
			ymin=oldrecords(modplot)%ymin10
			xmax=oldrecords(modplot)%xmax10
			ymax=oldrecords(modplot)%ymax10
			if(logx) then
				if(xmin.le.0 ) xmin=0.01
				xmin=alog10(xmin)
				if(xmax.le.0)  xmax=0.01
				xmax=alog10(xmax) 
			endif
			if(logy) then
				if(ymin.le.0 ) ymin=0.01
				ymin=alog10(ymin)
				if(ymax.le.0)  ymax=0.01
				ymax=alog10(ymax) 
			endif
			oldrecords(modplot)%xmin=xmin
			oldrecords(modplot)%ymin=ymin
			oldrecords(modplot)%ymax=ymax
			oldrecords(modplot)%xmax=xmax	
			!	callid=405
				goto 1 
	!		endif
				endif
				endif

			
	
		case(271:276) !select 
		if(readrec.and.modplot.gt.0) then
			ntog=0
			select case(callid)
			
				case(275)
					itcall=100
					jindex=101
					itd=4
					ntog=	oldrecords(modplot)%ncurvd
					do i=1,ntog
						radio_text(i)='Data line '//CHAR(48+I)
					enddo
				case(276)
					itcall=150
					jindex=151
					itd=4
					ntog=	oldrecords(modplot)%ncurvc
					do i=1,ntog
						radio_text(i)='Curve '//CHAR(48+I)
					enddo
				case(271) !(943)
					itcall=200
					itd=3
					jindex=201
					ntog=oldrecords(modplot)%lines%narrow
					do i=1,ntog
						radio_text(i)='Arrow '//CHAR(48+I)
					enddo
				case(272) !(944)
					itcall=210
					jindex=211
					itd=3
					ntog=oldrecords(modplot)%lines%nline
					do i=1,ntog
						radio_text(i)='Line '//CHAR(48+I)
					enddo
				case(273) !(945)
					itcall=220
					jindex=221
					itd=3
					ntog=oldrecords(modplot)%lines%nhline
					do i=1,ntog
						radio_text(i)='Horizontal line '//CHAR(48+I)
					enddo
				case(274) !(946)
					itcall=230
					jindex=231
					itd=3
					ntog=oldrecords(modplot)%lines%nvline
					do i=1,ntog
						radio_text(i)='Vertical line '//CHAR(48+I)
					enddo
			end select
			if(ntog.gt.0) then
		
		!	call toggle_panel(Main,ITOGGLE,itogglepanel,ntog,text_tog,intoggle,itcall,valdat,idat,itogbutton)
		 call radio_panel(main,iradio,ntog,iradio_toggle,radio_text,itd,&
		 iradiox,radiox,iradioy,radioy,itcall,titlerp)
					
			else
			imessy=gmDisplayMessageBox('','No item to be selected',Gexclamation,gok)
		
			endif
			else
			imessy=gmDisplayMessageBox('','No record on screen',Gexclamation,gok)
		
			endif
	
		
			case(290) ! write new text
			call gmDefineKeyselectCallback(13,0)
			if(readrec.and.modplot.gt.0) then
			if(jindex.eq.1.or.jindex.eq.3.or.jindex.eq.4.or.jindex.eq.5.or.jindex.gt.80) then
			call gmEnqTextSetting(new_text_entry,mtext3)
			endif
			rxx=gmEnqValueSetting(ivalx1)
			ryy=gmEnqValueSetting(ivaly1)
			if(jindex.ge.81.and.icallp.eq.250) then
				newtext(ntext)=mtext3
				oldrecords(modplot)%str(ntext+80)=mtext3
			!	d_text=.true.
			endif
			s_text=.false.
			itempcol=oldrecords(modplot)%attributes%icol(jindex)
			!oldrecords(modplot)%attributes%icol(jindex)=0
			call write_string(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),0,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
			!call string_box(oldrecords(modplot)%str(jindex),	oldrecords(modplot)%attributes%rx(jindex),&
			!oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			!oldrecords(modplot)%attributes%ijus(jindex),&
     		!oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			endif
			if(jindex.eq.1) then
				title1=mtext3
				oldrecords(modplot)%str(1)=mtext3
			else if(jindex.eq.2) then
				!parval=mtext3
				!oldrecords(modplot)%str(2)=mtext3
			else if(jindex.eq.3) then
				titlex=mtext3
				oldrecords(modplot)%str(3)=mtext3
			else if(jindex.eq.4) then
				titley=mtext3
				oldrecords(modplot)%str(4)=mtext3	
			endif
			ind_1=jindex 
			if(logx) then
			    if(rxx.le.0.) rxx=0.01 
				rxx=alog10(rxx)
			endif
			if(logy) then 
				if(ryy.le.0.) ryy=0.01 
				ryy=alog10(ryy)
			endif
			if(sqrty) then 
			!	if(ryy.le.0.) ryy=0.01 
			if(ryy.gt.0)	ryy=sqrt(ryy)
			endif
			oldrecords(modplot)%attributes%rx(jindex)=rxx
			oldrecords(modplot)%attributes%ry(jindex)=ryy	
		  	oldrecords(modplot)%attributes%icol(jindex)=itempcol
			call write_string(oldrecords(modplot)%str(jindex),	oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
	!		call string_box(oldrecords(modplot)%str(jindex),	oldrecords(modplot)%attributes%rx(jindex),&
	!		oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
	!		oldrecords(modplot)%attributes%ijus(jindex),&
     !		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),&
	!		imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			endif
		  
		!	index=0
			call gmremovewindow(itext_entry)
		endif
	
		case (301)	! Bold
		if(readrec.and.modplot.gt.0) then
		if(itprev.eq.329) then
			do i=1,7
			istat2=gmenqtoggleswitch(iradio_Toggle(i))
			if(istat2.eq.gon) jindex=i
			enddo
			if(jindex.eq.7) jindex=31
			call gmremovewindow(iradio)
			endif
		  if(jindex.ge.1.and.jindex.le.100) then
		  itempcol=oldrecords(modplot)%attributes%icol(jindex)
		  oldrecords(modplot)%attributes%icol(jindex)=0
		  k=1
		  if(jindex.ge.6.and.jindex.le.30) k=numbx 
		  if(jindex.ge.31.and.jindex.le.55) k=numby
		  do i=1,k
		  	call write_string(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
	!		call string_box(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
	!		oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
	!		oldrecords(modplot)%attributes%ijus(jindex),&
    ! 		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			endif
		  enddo
		  if(oldrecords(modplot)%attributes%thick(jindex).eq.0.) then
			!thick(jindex)=dxs
          else
			oldrecords(modplot)%attributes%thick(jindex)=0.	
          endif
		  oldrecords(modplot)%attributes%icol(jindex)=itempcol	
		  oldrecords(modplot)%attributes%ifnt(jindex)=15
		  ifnt(jindex)=15
		  do i=1,k
		  
		  if(mod(i,2).eq.0.and.jindex.eq.6) then
		    nl=len_trim(oldrecords(modplot)%str(jindex+i-1))
		    if(nl.gt.3) nolabx=1
			if(numbx.gt.4.and.ipow.ne.-10000) goto 101
            if(numbx.gt.7.and.nolabx.eq.1) goto 101
		    endif
					
			call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
101			continue
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
	!		call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
	!		oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
	!		oldrecords(modplot)%attributes%ijus(jindex),&
     !		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			endif
		  enddo
		  endif	
		  endif
        case (302)	! Italic
		if(readrec.and.modplot.gt.0) then
		if(itprev.eq.329) then
			do i=1,7
			istat2=gmenqtoggleswitch(iradio_Toggle(i))
			if(istat2.eq.gon) jindex=i
			enddo
			if(jindex.eq.7) jindex=31
			call gmremovewindow(iradio)
			endif
		  if(jindex.ge.1.and.jindex.le.100) then
		  itempcol=oldrecords(modplot)%attributes%icol(jindex)
		  oldrecords(modplot)%attributes%icol(jindex)=0
		  k=1
		  if(jindex.ge.6.and.jindex.le.30) k=numbx 
		  if(jindex.ge.31.and.jindex.le.55) k=numby
		  do i=1,k
			call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
		  
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
	!		call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
	!		oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
	!		oldrecords(modplot)%attributes%ijus(jindex),&
     !		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			endif
         enddo
	     if(oldrecords(modplot)%attributes%ifnt(jindex).eq.6) then
			oldrecords(modplot)%attributes%ifnt(jindex)=4
		ifnt(jindex)=4
		
          else
			oldrecords(modplot)%attributes%ifnt(jindex)=6
			ifnt(jindex)=6
		
          endif
		  oldrecords(modplot)%attributes%icol(jindex)=itempcol
		  do i=1,k
		  
		
		  if(mod(i,2).eq.0.and.jindex.eq.6) then
		    nl=len_trim(oldrecords(modplot)%str(jindex+i-1))
		    if(nl.gt.3) nolabx=1
			if(numbx.gt.4.and.ipow.ne.-10000) goto 102
            if(numbx.gt.7.and.nolabx.eq.1) goto 102
		  endif
			call write_string(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
102			continue
if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
!			call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
!				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
!     		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			endif		 
		  enddo	
		  
		  call gsetitalicangle(0.0)
		  endif	
		  endif
        case (303)	! Underline
		if(readrec.and.modplot.gt.0) then
		if(itprev.eq.329) then
			do i=1,7
			istat2=gmenqtoggleswitch(iradio_Toggle(i))
			if(istat2.eq.gon) jindex=i
			enddo
			if(jindex.eq.7) jindex=31
			call gmremovewindow(iradio)
			endif
		if(jindex.ge.1.and.jindex.le.100) then
		  if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1)then
		     if(imode.eq.1) then
				imode=2
	         else if(imode.eq.2) then

			    imode=-1
			endif			
		  else
			oldrecords(modplot)%attributes%idraw(jindex)=-1
			imode=2
			idraw(jindex)=-1
          endif
		  k=1
		  if(jindex.ge.6.and.jindex.le.30) k=numbx 
		  if(jindex.ge.31.and.jindex.le.55) k=numby
		  do i=1,k
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
	!		call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
	!			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     !		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
		    endif
		   enddo
		endif
		endif
		case (304,305,306)	! justify
		if(readrec.and.modplot.gt.0) then
		if(itprev.eq.329) then
			do i=1,7
			istat2=gmenqtoggleswitch(iradio_Toggle(i))
			if(istat2.eq.gon) jindex=i
			enddo
			if(jindex.eq.7) jindex=31
			call gmremovewindow(iradio)
			endif
		  k=1
		  if(jindex.ge.6.and.jindex.le.30) k=numbx 
		  if(jindex.ge.31.and.jindex.le.55) k=numby
		  if(jindex.ge.1.and.jindex.le.100) then
		  itempcol=oldrecords(modplot)%attributes%icol(jindex)
		  oldrecords(modplot)%attributes%icol(jindex)=0
		  do i=1,k
		  call write_string(oldrecords(modplot)%str(jindex+i-1),	&
			oldrecords(modplot)%attributes%rx(jindex+i-1),&
		  	oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
		  		 
		  if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
	!		call string_box(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
	!			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
	!			oldrecords(modplot)%attributes%ijus(jindex),&
    ! 		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			endif
		  enddo	  
          if(callid.eq.304) then
			oldrecords(modplot)%attributes%ijus(jindex)=1
			ijus(jindex)=1
          else if(callid.eq.305) then
			oldrecords(modplot)%attributes%ijus(jindex)=0
			ijus(jindex)=0
		  else
			oldrecords(modplot)%attributes%ijus(jindex)=-1
			ijus(jindex)=-1		
          endif
		  
		  oldrecords(modplot)%attributes%icol(jindex)=itempcol
		  do i=1,k
		  if(mod(i,2).eq.0.and.jindex.eq.6) then
		    nl=len_trim(oldrecords(modplot)%str(jindex+i-1))
		    if(nl.gt.3) nolabx=1
			if(numbx.gt.4.and.ipow.ne.-10000) goto 103
            if(numbx.gt.7.and.nolabx.eq.1) goto 103
		  endif
		  if(mod(i,2).eq.0.and.jindex.eq.6) then
				
				endif
			call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
103			continue
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
	!		call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
	!			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     !		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			endif
          enddo
		endif
		endif
		case (307)  ! colour
	
		if (modplot.gt.0.and.lframe.eq.graphics1_1(modplot))then
		if(itprev.eq.329) then
			do i=1,7
			istat2=gmenqtoggleswitch(iradio_Toggle(i))
			if(istat2.eq.gon) jindex=i
			enddo
			if(jindex.eq.7) jindex=31
			call gmremovewindow(iradio)
			endif
			k=1
			if(jindex.ge.6.and.jindex.le.30) k=numbx 
			if(jindex.ge.31.and.jindex.le.55) k=numby
			newcol=-1
			call gmColourControl(RED,GREEN,BLUE)
			call select_colour(red,green,blue,newcol)
			if(newcol.eq.-1) then
			newcol=200+indcol
			call gDefineRGB(newcol,RED,GREEN,BLUE)
			indcol=indcol+1
			endif
			if(jindex.ge.1.and.jindex.le.250) then
				oldrecords(modplot)%attributes%icol(jindex)=newcol
				icol(jindex)=newcol
				call gmActivateGraphicsFrame(graphics1_1(modplot))
				if(jindex.le.100) then
				do i=1,k
				if(mod(i,2).eq.0.and.jindex.eq.6) then
			
		    nl=len_trim(oldrecords(modplot)%str(jindex+i-1))
		    if(nl.gt.3) nolabx=1
			if(numbx.gt.4.and.ipow.ne.-10000) goto 104
            if(numbx.gt.7.and.nolabx.eq.1) goto 104
		   	endif
				call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
				oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
				oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
104				continue
				if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
	!			call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
	!			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
	!			oldrecords(modplot)%attributes%ijus(jindex),&
     !			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
				endif  	
				enddo	
				else 
				callid=407
				goto 1
				endif
			endif
		endif	
	 	    
		case(308)	!delete
		if (modplot.gt.0.and.lframe.eq.graphics1_1(modplot)) then
		if(itprev.eq.329) then
			do i=1,7
			istat2=gmenqtoggleswitch(iradio_Toggle(i))
			if(istat2.eq.gon) jindex=i
			enddo
			if(jindex.eq.7) jindex=31
			call gmremovewindow(iradio)
			endif
			call gmSetGuiCursor(lframe,Gdefault,GDEFAULT)
	
			if(jindex.ge.1.and.jindex.le.100) then	
			oldrecords(modplot)%attributes%icol(jindex)=0
			k=1
		    if(jindex.ge.6.and.jindex.le.30) k=numbx 
		    if(jindex.ge.31.and.jindex.le.55) k=numby
		    do i=1,k
		 	call write_string(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
				oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
	!		call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
	!			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     !		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			endif
			enddo
			else if(jindex.ge.101.and.jindex.le.199) then	!data
				oldrecords(modplot)%attributes%itype(jindex)=-1
		
				callid=407
				goto 1
			endif
			if(jindex.eq.244) then
				icol(244)=0
					oldrecords(modplot)%attributes%icol(jindex)=0
				callid=407
			!	doframe=.false.
				goto 1
			endif
			endif
		
		case(321:329)
					
					jindex=1
					itd=3
					ntog=7
					
					radio_text(1)='Title'
					
					radio_text(2)='Parameters'
					radio_text(3)='X Label'
					radio_text(4)='Y Label'
					radio_text(5)='Z Label'
					radio_text(6)='X Numbers'
					radio_text(7)='Y Numbers'
					radio_text(8)='Z Numbers'
					radio_text(9)='Text'
			itprev=329
			itcall=callid-20
			call radio_panel(main,iradio,ntog,iradio_toggle,radio_text,itd,&
		    iradiox,radiox,iradioy,radioy,itcall,titlerp)
		
		case(331:336)
		  !  jindex=callid-330
		!	call gmremovewindow(iradio)
		!	callid=309
		!	goto 2
		case(309)   ! move
		if(modplot.gt.0) then
		    if(itprev.eq.329) then
			do i=1,7
			istat2=gmenqtoggleswitch(iradio_Toggle(i))
			if(istat2.eq.gon) jindex=i
			enddo
			if(jindex.eq.7) jindex=31
			call gmremovewindow(iradio)
			endif
			s_text=.true.
			d_line=.false.
			d_hline=.false.
			d_vline=.false.
			d_arrow=.false.
		
			s_line=.false.
			s_arrow=.false.
			if(jindex.ge.1.and.jindex.le.100.and.modplot.gt.0) then
			imode=1
			ind_1=jindex
			call gmActivateGraphicsFrame(graphics1_1(modplot))
		!	if(jindex.eq.1.or.jindex.eq.2.or.jindex.eq.3.or.jindex.eq.4.or.jindex.eq.5.or.jindex.gt.80) then
		!	call string_box(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
		!	oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
		!	oldrecords(modplot)%attributes%ijus(jindex),&
     	!	oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),&
		!	imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
		!	oldrecords(modplot)%attributes%idraw(jindex)=-1
		!	else
		!	endif
			else
				imes=gmdisplaymessagebox('','Please select a string first',ginformation,gok)
			endif	
		endif
		case(310)	! box
		if(readrec.and.modplot.gt.0) then
		if(jindex.ge.1.and.jindex.le.100.and.modplot.gt.0) then
			call gmActivateGraphicsFrame(graphics1_1(modplot))
			k=1
		    if(jindex.ge.6.and.jindex.le.30) k=numbx 
		    if(jindex.ge.31.and.jindex.le.55) k=numby
		    if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
				imode=-1
				do i=1,k
	!				call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
	!					oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
	!					oldrecords(modplot)%attributes%ijus(jindex),&
     !				oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
				enddo
			!	oldrecords(modplot)%attributes%idraw(jindex)= 1
			else
			!	oldrecords(modplot)%attributes%idraw(jindex)=-1
				imode=1
				do i=1,k
		!			call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
		!				oldrecords(modplot)%attributes%ry(jindex+i-1),&
		!				oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     	!			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
				enddo
			endif
			
        endif
		endif
		case(311) !rotate
		if(readrec.and.modplot.gt.0) then
		if(jindex.ge.1.and.jindex.le.100.and.modplot.gt.0) then
		  itempcol=oldrecords(modplot)%attributes%icol(jindex)
		  oldrecords(modplot)%attributes%icol(jindex)=0
		  k=1
		  if(jindex.ge.6.and.jindex.le.30) k=numbx 
		  if(jindex.ge.31.and.jindex.le.55) k=numby
		  do i=1,k
		  	 call write_string(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
			 	oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			continue
		    if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
		 ! 	call string_box(oldrecords(modplot)%str(jindex+i-1),	oldrecords(modplot)%attributes%rx(jindex+i-1),&
		!	oldrecords(modplot)%attributes%ry(jindex+i-1),&
		!	oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     	!	oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			endif
		   enddo
			do i=2,5
			ifstat(i)=gmEnqListEntry(Combo1_6,i,textcombo)
			if(ifstat(i).eq.2)  oldrecords(modplot)%attributes%angle(jindex)=float(i-2)*30.
			enddo
          
		  oldrecords(modplot)%attributes%icol(jindex)=itempcol
		  do i=1,k
		  if(mod(i,2).eq.0.and.jindex.eq.6) then
		    nl=len_trim(oldrecords(modplot)%str(jindex+i-1))
		    if(nl.gt.3) nolabx=1
			if(numbx.gt.4.and.ipow.ne.-10000) goto 105
            if(numbx.gt.7.and.nolabx.eq.1) goto 105
		  endif
		    if(mod(i,2).eq.0.and.jindex.eq.6) then
			!		if(nolabx.eq.1.or.ipow.ne.-10000) goto 105
				endif
		
			call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
105			continue
			if(idraw(jindex).eq.-1) then
		  !	call string_box(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
		!	oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     	!	oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			endif
		  enddo		
		  endif
			endif
		  case(312)  !line style
			if(jindex.le.100) goto 1
			if(readrec.and.modplot.gt.0) then
			call gmEnqListStatus(combo1_10, nentry, nselect, ifirst)
			callid=407
			select case(ifirst)
				case(2)
					itip=0
					case(3)
					itip=1
					case(4)
					itip=13
					case(5)
					itip=6
					
					case(6)
					itip=7
					case(7)
					itip=2
					case(8)
					itip=4
					case(9)
					itip=15
					case(10)
					itip=12
				case(11)
					itip=16
			end select
			oldrecords(modplot)%attributes%itype(jindex)=itip
			callid=407
			goto 1
			endif
		  case(313)  ! thickness
		  if(jindex.le.100) goto 1

			if(readrec.and.modplot.gt.0) then
			wid=0.5*oldrecords(modplot)%dys*1.000001
			call gmEnqListStatus(combo1_9, nentry, nselect, ifirst)
			select case(ifirst)
				case(2)
				oldrecords(modplot)%attributes%thick(jindex)=wid
					case(3)
					tip=1.
					oldrecords(modplot)%attributes%thick(jindex)=0.0
					case(4)
					tip=2.
					oldrecords(modplot)%attributes%thick(jindex)=2*wid
			end select
			callid=407
			goto 1
			endif
		  case(319,318) !chaNge font
		  idev=0
		  if(readrec.and.modplot.gt.0) then
			if(jindex.ge.1.and.jindex.le.100.and.modplot.gt.0) then
				k=1
		  if(jindex.ge.6.and.jindex.le.30) k=numbx 
		  if(jindex.ge.31.and.jindex.le.55) k=numby
		  do i=1,k
			call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),0,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
		 
		  	   enddo
			if(callid.eq.319) then
			ifontrue=0		
			do i=2,21
			ifstat(i)=gmEnqListEntry(Combo1_4,i,textcombo)
			if(ifstat(i).eq.2)  oldrecords(modplot)%attributes%ifnt(jindex)=i-1
			enddo
			else
			
			ifstat(2)=gmEnqListEntry(Combo1_8,3,textcombo)
				if(ifstat(2).eq.2)  oldrecords(modplot)%attributes%ifnt(jindex)=100
				ifontrue=100
			ifstat(3)=gmEnqListEntry(Combo1_8,4,textcombo)
				if(ifstat(3).eq.2)  oldrecords(modplot)%attributes%ifnt(jindex)=101
				ifontrue=101
			ifstat(4)=gmEnqListEntry(Combo1_8,2,textcombo)
				if(ifstat(4).eq.2)  oldrecords(modplot)%attributes%ifnt(jindex)=102
				ifontrue=102
			endif
			do i=1,k
			if(mod(i,2).eq.0.and.jindex.eq.6) then
		    nl=len_trim(oldrecords(modplot)%str(jindex+i-1))
		    if(nl.gt.3) nolabx=1
			if(numbx.gt.4.and.ipow.ne.-10000) goto 106
            if(numbx.gt.7.and.nolabx.eq.1) goto 106
		  endif
			 
		
			call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),&
			oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),oldrecords(modplot)%attributes%sizetext(jindex),&
			oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
106			continue
			if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
		 ! 	call string_box(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),oldrecords(modplot)%attributes%ry(jindex+i-1),&
		!	oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     	!	oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
			endif	
			enddo
		    endif
			endif
		case(320) !change size
		if(readrec.and.modplot.gt.0) then
			if(jindex.ge.1.and.jindex.le.100.and.modplot.gt.0) then
				itempcol=oldrecords(modplot)%attributes%icol(jindex)
				oldrecords(modplot)%attributes%icol(jindex)=0
				k=1
				if(jindex.ge.6.and.jindex.le.30) k=numbx 
				if(jindex.ge.31.and.jindex.le.55) k=numby
				do i=1,k
				call write_string(oldrecords(modplot)%str(jindex+i-1),&
				oldrecords(modplot)%attributes%rx(jindex+i-1),oldrecords(modplot)%attributes%ry(jindex+i-1),oldrecords(modplot)%attributes%angle(jindex),&
				oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
				oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
				if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
		 ! 		call string_box(oldrecords(modplot)%str(jindex+i-1),&
		!		oldrecords(modplot)%attributes%rx(jindex+i-1),oldrecords(modplot)%attributes%ry(jindex+i-1),&
		!		oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     	!		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
				endif	
				enddo
				do i=2,5
					ifstat(i)=gmEnqListEntry(Combo1_5,i,textcombo)
					if(ifstat(i).eq.2) sizefac=3+0.5*(i-1) 
				enddo
				if(jindex.le.5) then
				oldrecords(modplot)%attributes%sizetext(jindex)=sizefac
				if(jindex.eq.2) oldrecords(modplot)%attributes%sizetext(jindex)=sizefac-1.
				else
				oldrecords(modplot)%attributes%sizetext(jindex)=sizefac-0.5
				endif
				oldrecords(modplot)%attributes%icol(jindex)=itempcol
				do  i=1,k
					if(mod(i,2).eq.0.and.jindex.eq.6) then
		    nl=len_trim(oldrecords(modplot)%str(jindex+i-1))
		    if(nl.gt.3) nolabx=1
			if(numbx.gt.4.and.ipow.ne.-10000) goto 107
            if(numbx.gt.7.and.nolabx.eq.1) goto 107
		  endif
				 
				call write_string(oldrecords(modplot)%str(jindex+i-1),oldrecords(modplot)%attributes%rx(jindex+i-1),oldrecords(modplot)%attributes%ry(jindex+i-1),&
				oldrecords(modplot)%attributes%angle(jindex),&
				oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
				oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),oldrecords(modplot)%dxs,oldrecords(modplot)%dys)	
107				continue
				if(oldrecords(modplot)%attributes%idraw(jindex).eq.-1) then
		 ! 		call string_box(oldrecords(modplot)%str(jindex+i-1),&
		!		oldrecords(modplot)%attributes%rx(jindex+i-1),oldrecords(modplot)%attributes%ry(jindex+i-1),&
		!		oldrecords(modplot)%attributes%angle(jindex),oldrecords(modplot)%attributes%ijus(jindex),&
     	!		oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),imode,xbox,ybox,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
				endif	
		 		enddo
			endif
			endif
	case(350)
	CHA=GETCHARQQ()
	IF(CHA.EQ.'g') then
		call gSetCharFont(10)	
	endif
	case(353,354,355)
		
		
			call gmDefineKeyselectCallback(13,0)
			
		jindex=callid-350
			ax1=oldrecords(modplot)%xmin
			ay1=oldrecords(modplot)%ymin
			ax2=oldrecords(modplot)%xmax
            ay2=oldrecords(modplot)%ymax
			if(logx) then
				ax1=10**ax1
				ax2=10**ax2
			endif
			if(logy) then
				ay1=10**ay1
				ay2=10**ay2
			endif
			if(sqrty) then
			!	ay1=ay1**2
			!	ay2=ay2**2
			endif
			axtic=oldrecords(modplot)%param_axis%xtic
			aytic=oldrecords(modplot)%param_axis%ytic
		if(jindex.eq.3) then
				mtext1='Enter/Modify X axis:'
				xtitle=oldrecords(modplot)%STR(3)
				mtext3=xtitle
				jindex1=245
				aminax=ax1
				amaxax=ax2
				atic=axtic
				logscale=logx
				icallprev=353
		else if(jindex.eq.4) then
				ytitle=oldrecords(modplot)%STR(4)
				mtext3=ytitle
				mtext1='Enter/Modify Y axis:'
				jindex1=245
				aminax=ay1
				amaxax=ay2
				atic=aytic
				logscale=logy
		else if(jindex.eq.5.and.iptype.eq.40) then
					ztitle=oldrecords(modplot)%STR(5)
					mtext3=ztitle
					mtext1='Enter/Modify Z axis:'
					jindex1=245
			
			
		endif
				continue
				aminax0=aminax
				amaxax0=amaxax
				atic0=atic
				ind_1=jindex
				rxx=oldrecords(modplot)%attributes%rx(jindex)
				ryy=oldrecords(modplot)%attributes%ry(jindex)
				if(logx) then
					rxx=10**rxx
				endif
				if(logy) then
					ryy=10**ryy
				endif
				if(sqrty) then
					if(ryy.gt.0) ryy=ryy**2
				endif
		call gmDefineKeyselectCallback(13,360)	
	call modify_axis(main,imodax,mtext1,mtext3,rxx,ryy,aminax,amaxax,atic,&
		imtext3,irxx,iryy,iaminax,iamaxax,iatic,jindex,logscale)
	case(360)
		call gmDefineKeyselectCallback(13,0)
		aminax=gmEnqValueSetting(iaminax)
		amaxax=gmEnqValueSetting(iamaxax)
		atic=gmEnqValueSetting(iatic)
		if(jindex.eq.3) then	!x axis
		    ax1=aminax
			ax2=amaxax
		   	xmin1=aminax
			xmax1=amaxax
			axtic=atic
			oldrecords(modplot)%param_axis%xtic=axtic
			if(logx) then
				if(xmin1.le.0 ) xmin1=0.01
				ax1=alog10(xmin1)
				if(xmax1.le.0)  xmax1=0.01
				ax2=alog10(xmax1) 
			endif
			oldrecords(modplot)%xmin=ax1
			oldrecords(modplot)%xmax=ax2
		else if (jindex.eq.4) then
			ymin1=aminax
			ymax1=amaxax
			ay1=aminax
			ay2=amaxax
			aytic=atic
			oldrecords(modplot)%param_axis%ytic=aytic 
			if(logy) then
				if(ymin1.le.0 ) ymin1=0.01
				ay1=alog10(ymin1)
				if(ymax1.le.0)  ymax1=0.01
				ay2=alog10(ymax1) 
			endif
			oldrecords(modplot)%ymin=ay1
			oldrecords(modplot)%ymax=ay2
		
		endif

	        call gmEnqTextSetting(imtext3,mtext3)
			
			rxx=gmEnqValueSetting(irxx)
			ryy=gmEnqValueSetting(iryy)
		
		    if(jindex.eq.3) then
				titlex=mtext3
				oldrecords(modplot)%str(3)=mtext3
			else if(jindex.eq.4) then
				titley=mtext3
				oldrecords(modplot)%str(4)=mtext3	
			endif
			ind_1=jindex 
			if(logx) then
			    if(rxx.le.0.) rxx=0.01 
				rxx=alog10(rxx)
			endif
			if(logy) then 
				if(ryy.le.0.) ryy=0.01 
				ryy=alog10(ryy)
			endif
			if(sqrty) then 
			!	if(ryy.le.0.) ryy=0.01 
			if(ryy.gt.0)	ryy=sqrt(ryy)
			endif

			
				
		call gmremovewindow(imodax)
		
		
		
		if(aminax.ne.aminax0.or.amaxax.ne.amaxax0.or.atic.ne.atic0) then
			oldrecords(modplot)%attributes%rx(jindex)=rxx
			oldrecords(modplot)%attributes%ry(jindex)=ryy
		
			do i=1,100
					oldrecords(modplot)%attributes%idraw(i)=-2
			enddo
			if(icallprev.eq.353.and.iplot.eq.2) then
				rescalex=.true.
			!	goto 1
			endif
			callid=247	
			goto 1
		else
			call write_string(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),0,oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
		    oldrecords(modplot)%attributes%rx(jindex)=rxx
			oldrecords(modplot)%attributes%ry(jindex)=ryy
			call write_string(oldrecords(modplot)%str(jindex),oldrecords(modplot)%attributes%rx(jindex),&
			oldrecords(modplot)%attributes%ry(jindex),oldrecords(modplot)%attributes%angle(jindex),&
			oldrecords(modplot)%attributes%ijus(jindex),oldrecords(modplot)%attributes%ifnt(jindex),&
			oldrecords(modplot)%attributes%sizetext(jindex),oldrecords(modplot)%attributes%icol(jindex),&
			oldrecords(modplot)%dxs,oldrecords(modplot)%dys)
		
		
		endif
	case(361)
		if(jindex.ge.101.and.jindex.le.150) then
		if(readrec.and.modplot.gt.0) then
		
			call gmEnqListStatus(combo1_11, nentry, nselect, ifirst)
			select case(ifirst)

				   case(2)
						symz1=symsiz1(jindex-100)
					case(3)
						symz1=0.5*symsiz1(jindex-100)
					case(4)
						symz1=2.*symsiz1(jindex-100)
			end select
			
	
		oldrecords(modplot)%symsiz(jindex-100)=symz1
		
		endif
	callid=407
			goto 1	
		endif
	case(370)
	    if(jindex.ge.101.and.jindex.le.150) then
		if(readrec.and.modplot.gt.0) then
		
			call gmEnqListStatus(combo1_12, nentry, nselect, ifirst)
			select case(ifirst)

				   case(2:11)
						!isym1(jindex-100)=ifirst-2
						isimb=ifirst-2
					case(12:18)
						!isym1(jindex-100)=11-ifirst
					isimb=11-ifirst
			end select
				oldrecords(modplot)%isym(jindex-100)=isimb
		endif
			callid=407
			goto 1	
		endif
	case(400) 
		call widrem(itoggle)	
end select
1 end