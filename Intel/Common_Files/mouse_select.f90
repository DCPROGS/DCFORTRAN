subroutine mouse_select(ind_1,npoint,jtemp,d_line, d_hline, d_vline, d_arrow, &
    d_poly, zoom,d_text, o_state,c_state, s_text,xmov,ymov,xmov0,ymov0,imark,&
	izoom,mm,itemp,inewpos,str,lframe,istate,ilink,link,delete_state,move_state,&
	models,ind_m,imove,ind_s,ind_e,mod_create,dxs,dys)
	
use menu_f90
use gino_f90
use graf_f90
character*11 cnum
character*30 conex,conex1
integer num0(10)

	integer IFNT(100),IJUS(100)
	real RX(100),RY(100),RXBOX(4,100),RYBOX(4,100),sizetext(100),ANGLE(100)
    integer ICOL(250),ITYPE(250),IDRAW(250)
	real XBEG(50),YBEG(50),XEND(50),YEND(50),C_THICK(250),xbox(4),ybox(4),THICK(250)
	integer itemp(100),is0(100),is1(100)
	integer*1 ihlinrel(10)	!=1 if line pos rel to line 1
	integer*1 ivlinrel(10)	!=1 if line pos rel to line 1
	character*10 cnumx(25),cnumy(25),cexpx(25),cexpy(25),cexpz(25)
	character*10 cnumz(25)
	character*80 newtext(20),actext
	character*64   TITLE1
	character*75   xtitle,ytitle,ztitle	!output from LAXES
	character*200  parval
	character*200 str(100)
	character*3 string	
	character*10 statname(100)
	!allocatable jtemp
	integer jtemp(100,100)

	LOGICAL logx,logy,logity,sqrty,indmod
	logical d_line, d_hline, d_vline, d_arrow, d_poly, zoom,d_text, o_state, &
	c_state,s_text,link,delete_state,move_state	
	PARAMETER  N_STATES=200
	TYPE MODEL
		integer irecm
	    INTEGER N
		integer ka
		integer kb
		integer kstat
		integer kstat0
		integer nsub
		integer kcon
		integer ncon
		integer npar
		logical indmod
		logical chardef
		integer ix
		integer iy
		integer model
		character*80 title_model
		real X(N_STATES)
		real Y(N_STATES)
		INTEGER COLOUR(N_STATES)
		LOGICAL STATUS(N_STATES)
		LOGICAL DRAW(N_STATES)
		CHARACTER*3 NAME(N_STATES)
		CHARACTER*15 statNAME(N_STATES)
		character*10 con_states(20)
		character*10 sub_states(10,20)
		character*10 open_states(10,20)
		character*10 start_states(10,20)
		integer	num_states(10,20)
		integer link(n_states,n_states)
		integer inter_link(n_states)
		character*40 name_link(n_states)
		character*20 ligname(10)
		integer nlig
		integer nbound(n_states,10)
		integer nchan
		real vref
		real*8 dgamma(n_states)
		integer nwidth
		integer nheight
		character*2 charmod(25,40)
		integer ilast
		integer jlast
		integer ic(2,200)
		integer index(n_states)
	END TYPE MODEL

	TYPE (MODEL) models(25)
logical bold,italik,underline
COMMON/TPOS/IDRAW,ICOL,THICK,C_THICK,ITYPE,IFNT,ANGLE,IJUS,&
     SIZEtext,RXBOX,RYBOX,&
     RX,RY,NARROW,NLINE,NHLINE,NVLINE, XBEG,YBEG,XEND,YEND,& 
     NTEXT,NEWTEXT,CNUMX,CNUMY,CNUMZ,CEXPX,CEXPY,CEXPZ,&
     NUMBX,NUMBY,NUMBZ,IHLINREL,IVLINREL
common/limit/wxmin,wxmax,wymin,wymax,xmin1,xmax1,ymin1,ymax1,&
	xmin,xmax,ymin,ymax
common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue
common/logval/logx,logy,sqrty,logity

call gDefineRGB(50,1.,0.80,0.8)	!pink
call gDefineRGB(51,0.8,1.0,0.8)	!green
call gDefineRGB(48,0.,0.,0.)
!allocate (jtemp(100,100))
	ired=50
	igreen=51
alpha=10.
sx=0.7*dxs                                                     
sy=0.7*dys
dxs0=dxs
dys0=dys

if(mod_create.eq.1) then
ibk=1
else
ibk=2
endif

    imodel=ind_1
    imod=imodel
call linwid(0.)
if(d_line.or.d_hline.or.d_vline.or.d_arrow.or.d_poly.or.zoom.or.o_state.or.c_state) then
	imark=imark+1

	if(imark.eq.1) then
		xtrue=xmov
		ytrue=ymov
		call movto2(xtrue,ytrue)
		if(o_state.or.c_state) then

			imodel=ind_1
		    x=float(ifixr(xtrue))
			y=float(ifixr(ytrue))
			istate=models(imodel)%n
			istate=istate+1
			models(imodel)%n=istate
			models(imodel)%kstat=istate
			dxs=0.6
			dys=0.8
			indmod=models(imodel)%indmod
			do i=1,models(imodel)%n
				is0(i)=models(imodel)%index(i)
			enddo
			if(o_state) then
				ka=models(imodel)%ka
				do i=istate,ka+2,-1
					CALL LINCOL(igreen)
					models(imodel)%colour(i)=igreen
					models(imodel)%name(i)=char(48+i)
					if(i.eq.10) 	models(imodel)%name(i)='10'
					if(i.gt.10) then
					call intconv(i,cnum)
				    models(imodel)%name(i)=cnum(1:2)
					
					endif
					is1(i)=i+1

					models(imodel)%X(i)=models(imodel)%X(i-1)
					models(imodel)%Y(i)=models(imodel)%Y(i-1)
					models(imodel)%name_link(i)=models(imodel)%name_link(i-1)
					models(imodel)%inter_link(i)=models(imodel)%inter_link(i-1)
					models(imodel)%statname(i)=models(imodel)%statname(i-1)
					models(imodel)%dgamma(i)=models(imodel)%dgamma(i-1)
					call jSYMBOL(models(imodel)%x(i),models(imodel)%y(i),-3,1.2,1.2,13,idev)
					call write_string(models(imodel)%name(i),models(imodel)%x(i)-0.1,&
					models(imodel)%y(i)+0.4,0.,0,1,0.35,48,dxs,dys)
					do l=1,10
						if(models(imodel)%statname(i)(l:l).eq.char(45)) i1=l
					enddo
				if(indmod) then
				if(i1.ne.0) then
				call write_string(models(imodel)%statname(i)(1:i1-1),models(imodel)%X(i)-0.1,&
				models(imodel)%y(i)-0.2,0.,0,1,0.35,48,dxs,dys)	
				call write_string(models(imodel)%statname(i)(i1+1:i1+4),models(imodel)%X(i)-0.1,&
				models(imodel)%y(i)-0.7,0.,0,1,0.35,48,dxs,dys)
				else
				call write_string(models(imodel)%statname(i),models(imodel)%X(i)-0.1,&
				models(imodel)%y(i)-0.2,0.,0,1,0.35,48,dxs,dys)	
				endif
				else
				nrmod=len_trim(models(imodel)%statname(i))
				call write_string(models(imodel)%statname(i)(1:6),MODELS(imodel)%X(i)-0.1,&
				MODELS(imodel)%y(i)-0.2,0.,0,1,0.35,48,dxs,dys)
				!if(nrmod.gt.6)	
				call write_string(models(imodel)%statname(i)(7:12),MODELS(imodel)%X(i)-0.1,&
				MODELS(imodel)%y(i)-0.7,0.,0,1,0.35,48,dxs,dys)
			
				endif
					do 	l=1,models(imodel)%nlig
						models(imodel)%NBOUND(i,l)=models(imodel)%nbound(i-1,l)
						
					enddo
					enddo
					ka=ka+1
				CALL lincol(ired)
				string='O'
				models(imodel)%colour(ka)=ired
				models(imodel)%name(ka)=CHAR(48+ka)
				if(i.eq.10) 	models(imodel)%name(ka)='10'
					if(i.gt.10) then
					call intconv(i,cnum)
				    models(imodel)%name(ka)=cnum(1:2)
					
					endif	
				models(imodel)%ka=ka
				models(imodel)%dgamma(ka)=50.
				models(imodel)%statname(ka)=' '
				CALL lincol(ired)
				call jSYMBOL(x,y,-3,1.2,1.2,13,idev)
				models(imodel)%colour(ka)=ired
				call write_string(models(imodel)%name(ka),x-0.1,y+0.4,0.,0,1,0.35,48,dxs,dys)
				
				models(imodel)%X(ka)=X
				models(imodel)%Y(ka)=Y
				do i=1,ka-1
					is1(i)=i
					
				enddo
				is1(ka)=ka+1
				models(imodel)%name_link(ka)=' '
				models(imodel)%inter_link(ka)=0
				do 	lk=1,models(imodel)%nlig
					models(imodel)%NBOUND(ka,lk)=0
						
				enddo
				is1(istate)=ka 
				do i=1,models(imodel)%n
					   
				do j=1,models(imodel)%n
				jtemp(i,j)=	models(imodel)%link(i,j)
				
				
				enddo
				enddo
				do i=1,models(imodel)%n
				
				do j=1,models(imodel)%n
					models(imodel)%link(i,j)=0
				enddo
				enddo

				do i=1,models(imodel)%n
					
				do j=1,models(imodel)%n
				
					models(imodel)%link(is1(i),is1(j))=jtemp(i,j)
				
					
				enddo
				enddo
				do i=1,models(imodel)%n
				call intconv(i,cnum)
				models(imodel)%name_link(i)=' '    	
				do j=1,models(imodel)%n
				if(j.ne.i.and.models(imodel)%link(i,j).eq.1) then
				call intconv(j,cnum)
				conex=models(imodel)%name_link(i)
				nc=len_trim(conex)
				models(imodel)%name_link(i)=conex(1:nc)//cnum(1:3)//';'
				endif
				enddo
				enddo
			
			else if(c_state) then
				CALL lincol(igreen)
				string='C'
				models(imodel)%colour(ISTATE)=igreen
				models(imodel)%name(ISTATE)=CHAR(48+Istate)
				if(istate.eq.10) 	models(imodel)%name(istate)='10'
				if(istate.gt.10) then
					call intconv(istate,cnum)
				    models(imodel)%name(istate)=cnum(1:2)
				endif
				call jSYMBOL(x,y,-3,1.2,1.2,13,idev)
				
				call write_string(models(imodel)%name(ISTATE),x-0.1,y+0.4,0.,0,1,0.35,48,dxs,dys)
			
				models(imodel)%X(ISTATE)=X
				models(imodel)%Y(ISTATE)=Y
				is1(istate)=istate
				do i=1,models(imodel)%n-1
					is1(i)=is0(i)
				enddo
				
			endif
			
			
			
			
			dxs=dxs0
			dys=dys0
		
			imark=0
		
			models(imodel)%n=istate
			do i=1,models(imodel)%n
				models(imodel)%index(i)=i   
			enddo
			
		else
			CALL lincol(50)
	   		call jSYMBOL(xtrue,ytrue,9,sx,sy,13,idev) 
		    !symbol if isym>0
			xmov=xtrue
			ymov=ytrue
			xmov0=xtrue
			ymov0=ytrue
			if(logx) xtrue=10**(xmov)
			if(logy) ytrue=10**(ymov)
			if(sqrty) ytrue=ymov**2
			if(.not.d_poly) then 
				xbeg(ind_1)=xtrue
				ybeg(ind_1)=ytrue
			endif
			xtruea=xtrue
			ytruea=ytrue
	
			if(zoom) then
				izoom=0
				call gmSetGuiCursor(lframe,Gdefault,Grubberbox)
            else
				call gmSetGuiCursor(lframe,Gdefault,Grubberband)
			endif 
			goto 99
		endif		
	else if(imark.gt.1) then
	   
		xtrue=xmov
		
	    ytrue=ymov
		if(d_hline) yTRUE=ymov0
	    if(d_vline) xTRUE=xmov0
		call movto2(xtrue,ytrue)
	   	call jSYMBOL(xtrue,ytrue,9,sx,sy,13,idev)     !symbol if isym>0
	    CALL LINCOL(icol(ind_1))
	 
		if(zoom) then
		    call iframe(xmov0,xtrue,ymov0,ytrue)
		else if (d_arrow) then
		thick(200+ind_1)=0.
	!	call gmoveto2d(xmov0,ymov0)
	!call gdrawarrow2d(xtrue,ytrue,1)
		   	call ARROW1(xmov0,ymov0,xtrue,ytrue,0.4,sx,sy,itype(ind_1+200),&
     	    thick(200+ind_1),icol(200+ind_1),idev)
			narrow=narrow+1
			
	    else
		!	call movto2(xmov,ymov)
		    call linto2(xmov0,ymov0)
			if(d_line) nline=nline+1
			if(d_vline) nvline=nvline+1
			if(d_hline) nhline=nhline+1
		endif
		if(d_poly) then
	        xtrue=xmov
	        ytrue=ymov
		    if(imark.eq.npoint) then
		       call movto2(xmov0,ymov0)
		       call linto2(xmov,ymov)
		       d_poly=.false.
		       imark=0
		    endif
		else
			if(logx) xtrue=10**(xmov)
			if(logy) ytrue=10**(ymov)
			if(sqrty) ytrue=ymov**2
			if(.not.d_poly) then 
				xend(ind_1)=xtrue
				yend(ind_1)=ytrue
			endif
		   
		    imark=0
		    if(d_hline.or.d_vline.or.d_line.or.d_arrow) then
		       d_line=.false.
		       d_hline=.false.
		       d_vline=.false.
		       d_arrow=.false.
		    else if(zoom) then
			 xminsav=xmin1
			 xmaxsav=xmax1
			 yminsav=ymin1
			 ymaxsav=ymax1
			 xmin1=xbeg(ind_1)
			 xmax1=xend(ind_1)
			 ymin1=ybeg(ind_1)
			 ymax1=yend(ind_1)
			 izoom=1
	         zoom=.false.
			 if(xmax1.lt.xmin1) then
				t=xmax1
				xmax1=xmin1
				xmin1=t
			 endif
			 if(ymax1.lt.ymin1) then
				t=ymax1
				ymax1=ymin1
				ymin1=t
			 endif
			 xmin=xmin1
			 ymin=ymin1
			 xmax=xmax1
			 ymax=ymax1
			 if(logx) then
				if(xmin.lt.0) xmin=0.0001
				xmin=alog10(xmin)
				xmax=alog10(xmax)
			 endif
			 if(logy) then
				if(ymin.lt.0) ymin=0.0001
				ymin=alog10(ymin)
				ymax=alog10(ymax)
			 endif
		    endif
			call gmSetGuiCursor(lframe,Gdefault,GDEFAULT)
		   
		endif
		goto 99
	 
	endif
else if(d_text) then
	imark=0
	xt=xmov
	yt=ymov
	xtrue=xt
	ytrue=yt
	if(logx) xtrue=10**(xt)
	if(logy) ytrue=10**(yt)
	if(sqrty) ytrue=yt**2
	rx(ntext+80)=xtrue
	ry(ntext+80)=ytrue
	
	call write_string(newtext(ntext),xt,yt,angle(ntext+80),ijus(ntext+80),&
	ifnt(ntext+80),sizetext(ntext+80),icol(ntext+80),dxs,dys)
	idraw(ntext+80)=1
	d_text=.false.
	ntext=ntext+1
else if(s_text) then
	if(inewpos.eq.0) then
		   inewpos=1
		   call gmSetGuiCursor(lframe,Gsmallcross,Grubberband)
	else if(inewpos.eq.1) then
		inewpos=0
		call gmSetGuiCursor(lframe,GDEFAULT,GDEFAULT)
		  	   !	call string_box(str(ind_1),rx(ind_1),ry(ind_1),angle(ind_1),&
		!		ijus(ind_1),sizetext(ind_1),icol(ind_1),-1,xbox,ybox,dxs,dys)
		if(ind_1.eq.6.or.ind_1.eq.31) then
			if(ind_1.eq.6) then
				num=numbx
			else
				num=numby
			endif
			do k=1,num
				call write_string(str(ind_1+k-1),rx(ind_1+k-1),ry(ind_1+k-1),angle(ind_1),&
				ijus(ind_1),ifnt(ind_1),sizetext(ind_1),0,dxs,dys)
			enddo
			dx=rx(ind_1)
			dy=ry(ind_1)
			do k=1,num
		
			if(ind_1.eq.6) then
					rx(ind_1+k-1)=xmov-dx+rx(ind_1+k-1)
					ry(ind_1+k-1)=ymov
			else
					rx(ind_1+k-1)=xmov
					ry(ind_1+k-1)=ymov-dy+ry(ind_1+k-1)
			endif
			enddo
		    do k=1,num
				call write_string(str(ind_1+k-1),rx(ind_1+k-1),ry(ind_1+k-1),angle(ind_1),&
				ijus(ind_1),ifnt(ind_1),sizetext(ind_1),12,dxs,dys)
			enddo
		else
				call write_string(str(ind_1),rx(ind_1),ry(ind_1),angle(ind_1),&
				ijus(ind_1),ifnt(ind_1),sizetext(ind_1),0,dxs,dys)
				rx(ind_1)=xmov
				ry(ind_1)=ymov
				call write_string(str(ind_1),rx(ind_1),ry(ind_1),angle(ind_1),&
				ijus(ind_1),ifnt(ind_1),sizetext(ind_1),12,dxs,dys)
		endif
    endif
	goto 11
	if(mm.gt.0.and.inewpos.eq.0) then                ! show box(es) if inside
		   inewpos=1
		   call gmSetGuiCursor(lframe,Gsmallcross,Grubberband)
		   do k=1,mm
			call string_box(str(itemp(k)),rx(itemp(k)),ry(itemp(k)),angle(itemp(k)),&
			ijus(itemp(k)),sizetext(itemp(k)),12,1,xbox,ybox,dxs,dys)
           enddo	      
    else if(inewpos.eq.1) then
		   do k=1,mm
				call string_box(str(itemp(k)),rx(itemp(k)),ry(itemp(k)),angle(itemp(k)),&
				ijus(itemp(k)),sizetext(itemp(k)),icol(itemp(k)),-1,xbox,ybox,dxs,dys)
				call write_string(str(itemp(k)),rx(itemp(k)),ry(itemp(k)),angle(itemp(k)),&
				ijus(itemp(k)),ifnt(itemp(K)),sizetext(itemp(k)),0,dxs,dys)
				rx(itemp(k))=xmov
				ry(itemp(k))=ymov
				call write_string(str(itemp(k)),rx(itemp(k)),ry(itemp(k)),angle(itemp(k)),&
				ijus(itemp(k)),ifnt(itemp(K)),sizetext(itemp(k)),icol(itemp(k)),dxs,dys)
		   enddo
		   inewpos=0
		   call gmSetGuiCursor(lframe,GDEFAULT,GDEFAULT)
    endif
11  continue
else if(link) then
	imodel=ind_1
	istate=models(imodel)%n
	indmod=models(imodel)%indmod
    if(ilink.eq.0.and.istate.gt.1) then
	
		do i=1,istate
			if(xmov.lt.models(imodel)%X(I)+1.0.and.xmov.gt.models(imodel)%X(I)-1.0) then
			if(ymov.lt.models(imodel)%y(I)+1.0.and.ymov.gt.models(imodel)%y(I)-1.0) then
				xmov0=xmov
				ymov0=ymov
				ind_s=i
				call gmSetGuiCursor(lframe,Gdefault,Grubberband)
				ilink=1
				exit
            endif
			endif
		enddo
	else
		
		dxs=0.6
		dys=0.8
		do i=1,istate
		if(i.ne.ind_s) then
			if(xmov.lt.models(imodel)%X(I)+1.0.and.xmov.gt.models(imodel)%X(I)-1.0) then
			if(ymov.lt.models(imodel)%y(I)+1.0.and.ymov.gt.models(imodel)%y(I)-1.0) then
				ind_e=i
				
				call lincol(46)
				call gmoveto2d(models(imodel)%X(ind_s),models(imodel)%y(ind_s))
				call gdrawlineto2d(models(imodel)%X(i),models(imodel)%y(i))
				a=(models(imodel)%X(ind_s)-models(imodel)%X(i))/&
				(models(imodel)%y(ind_s)-models(imodel)%y(i))
				if(a.gt.0.4.and.a.lt.1.75) then
				call gmoveto2d(MODELS(imodel)%X(ind_s)-0.1,MODELS(imodel)%y(ind_s)+0.1)
				call gdrawlineto2d(MODELS(imodel)%X(i)-0.1,MODELS(imodel)%y(i)+0.1)
				else
				call gmoveto2d(MODELS(imodel)%X(ind_s)-0.1,MODELS(imodel)%y(ind_s)-0.1)
				call gdrawlineto2d(MODELS(imodel)%X(i)-0.1,MODELS(imodel)%y(i)-0.1)
				endif
				call lincol(models(imodel)%colour(i))
				call jSYMBOL(models(imodel)%X(i),models(imodel)%y(i),-3,1.2,1.2,13,idev)
				call write_string(models(imodel)%name(i),models(imodel)%X(i)-0.1,&
				models(imodel)%y(i)+0.4,0.,0,1,0.35,48,dxs,dys)
				statname(i)=models(imodel)%statname(i)
				i1=0			
				do l=1,10
			!	if(statname(i)(l:l).eq.'*') statname(i)(l:l)='x'
				if(statname(i)(l:l).eq.char(45)) i1=l
			    enddo
				if(indmod) then
				if(i1.ne.0) then
					call write_string(models(imodel)%statname(i)(1:i1-1),models(imodel)%X(i)-0.1,&
					models(imodel)%y(i)-0.2,0.,0,1,0.35,48,dxs,dys)	
					call write_string(models(imodel)%statname(i)(i1+1:i1+4),models(imodel)%X(i)-0.1,&
					models(imodel)%y(i)-0.7,0.,0,1,0.35,48,dxs,dys)
				else
					call write_string(models(imodel)%statname(i),models(imodel)%X(i)-0.1,&
					models(imodel)%y(i)-0.2,0.,0,1,0.35,48,dxs,dys)	
				endif
				else
				call write_string(models(imodel)%statname(i)(1:6),MODELS(imodel)%X(i)-0.1,&
			    MODELS(imodel)%y(i)-0.2,0.,0,1,0.35,48,dxs,dys)
			    call write_string(models(imodel)%statname(i)(7:12),MODELS(imodel)%X(i)-0.1,&
			    MODELS(imodel)%y(i)-0.7,0.,0,1,0.35,48,dxs,dys)
			    endif
				
				call gmSetGuiCursor(lframe,Gdefault,Gdefault)
				ilink=0
				models(imodel)%link(ind_e,ind_s)=1
				models(imodel)%link(ind_s,ind_e)=models(imodel)%link(ind_e,ind_s)
				models(imodel)%inter_link(i)=models(imodel)%inter_link(i)+1
				models(imodel)%inter_link(ind_s)=models(imodel)%inter_link(ind_s)+1
				do lk=1,30
				conex(lk:lk)=' '
				enddo
				call intconv(i,cnum)
				conex=models(imodel)%name_link(ind_s)
				nc=len_trim(conex)
				nnr=ichar(conex(1:1))
				if(conex(1:1).eq.''.or.nnr.eq.0) then
				models(imodel)%name_link(ind_s)=cnum(1:3)//';'
				else
				models(imodel)%name_link(ind_s)=conex(1:nc)//cnum(1:3)//';'
				endif
				do lk=1,30
				conex1(lk:lk)=' '
				enddo
				conex1=models(imodel)%name_link(i)
				call intconv(ind_s,cnum)
				nc=len_trim(conex1)
				models(imodel)%name_link(i)=conex1(1:nc)//cnum(1:3)//';'
				exit 
            endif
			endif
		endif
		enddo
		i1=0
		call lincol(models(imodel)%colour(ind_s))
		call jSYMBOL(models(imodel)%X(ind_s),models(imodel)%y(ind_s),-3,1.2,1.2,13,idev)
		call write_string(models(imodel)%name(ind_S),models(imodel)%X(ind_s)-0.1,&
		models(imodel)%y(ind_s)+0.4,0.,0,1,0.35,48,dxs,dys)
		statname(ind_s)=models(imodel)%statname(ind_s)
				do l=1,10
			!	if(statname(ind_s)(l:l).eq.'*') statname(ind_s)(l:l)='x'
					if(statname(ind_s)(l:l).eq.char(45)) i1=l
			    enddo
				if(indmod) then
				if(i1.ne.0) then
	call write_string(models(imodel)%statname(ind_s)(1:i1-1),models(imodel)%X(ind_s)-0.1,&
			models(imodel)%y(ind_s)-0.2,0.,0,1,0.35,48,dxs,dys)	
	call write_string(models(imodel)%statname(ind_s)(i1+1:i1+4),models(imodel)%X(ind_s)-0.1,&
			models(imodel)%y(ind_s)-0.7,0.,0,1,0.35,48,dxs,dys)
	else
	call write_string(models(imodel)%statname(ind_s),models(imodel)%X(ind_s)-0.1,&
			models(imodel)%y(ind_s)-0.2,0.,0,1,0.35,48,dxs,dys)	
	endif
	else
		call write_string(models(imodel)%statname(ind_s)(1:6),MODELS(imodel)%X(ind_s)-0.1,&
			MODELS(imodel)%y(ind_s)-0.2,0.,0,1,0.35,48,dxs,dys)
			call write_string(models(imodel)%statname(ind_s)(7:12),MODELS(imodel)%X(ind_s)-0.1,&
			MODELS(imodel)%y(ind_s)-0.7,0.,0,1,0.35,48,dxs,dys)
	endif	
	endif
ELSE IF(DELETE_STATE) THEN
		imodel=ind_1
		istate=models(imodel)%n
		indmod=models(imodel)%indmod
		dxs=0.6
		dys=0.8
		if (imod.le.300) then
			istatsel=gmDisplayMessageBox('',&
					'Delete not available for old models',&
					Gexclamation,gok)
			goto 99
		endif
		CALL lincol(ibk)
		do ind_i=1,istate
		if(xmov.lt.models(imodel)%X(Ind_i)+1.0.and.xmov.gt.models(imodel)%X(Ind_i)-1.0) then
		if (ymov.lt.models(imodel)%y(ind_i)+1.0.and.ymov.gt.models(imodel)%y(Ind_i)-1.0) then
		call jSYMBOL(models(imodel)%X(ind_i),models(imodel)%y(ind_i),-3,1.2,1.2,13,idev)
		ind_d=ind_i
		do k=1,istate
			if(k.ne.ind_i.and.models(imodel)%link(k,ind_i).eq.1) then
			jtemp(k,ind_i)=0
			jtemp(ind_i,k)=0
				models(imodel)%link(k,ind_i)=0
				models(imodel)%link(ind_i,k)=0
			!	models(imodel)%inter_link(k)=models(imodel)%inter_link(k)-1
			!	models(imodel)%inter_link(ind_i)=models(imodel)%inter_link(ind_i)-1
				CALL lincol(ibk)
				call gmoveto2d(models(imodel)%X(ind_i),models(imodel)%y(ind_i))
				call gdrawlineto2d(models(imodel)%X(k),models(imodel)%y(k))
				a=(models(imodel)%X(ind_i)-models(imodel)%X(k))/&
				(models(imodel)%y(ind_i)-models(imodel)%y(k))
				if(a.gt.0.4.and.a.lt.1.75) then
				call gmoveto2d(MODELS(imodel)%X(ind_i)-0.1,MODELS(imodel)%y(ind_i)+0.1)
				call gdrawlineto2d(MODELS(imodel)%X(k)-0.1,MODELS(imodel)%y(k)+0.1)
				else
				call gmoveto2d(MODELS(imodel)%X(ind_i)-0.1,MODELS(imodel)%y(ind_i)-0.1)
				call gdrawlineto2d(MODELS(imodel)%X(k)-0.1,MODELS(imodel)%y(k)-0.1)
			
				endif
				
			endif
		enddo
		do i=1,models(imodel)%n
			models(imodel)%inter_link(i)=0
			do j=1,models(imodel)%n
				jtemp(i,j)=	models(imodel)%link(i,j)
				jtemp(j,i)=jtemp(i,j)
				
			enddo
		enddo
		do i=1,models(imodel)%n
			do j=1,models(imodel)%n
			models(imodel)%link(i,j)=0
				models(imodel)%link(j,i)=0
				if(jtemp(i,j).eq.1) then
				models(imodel)%inter_link(i)=models(imodel)%inter_link(i)+1
				endif
			enddo
		enddo
		do i=1,models(imodel)%n-1
			do j=i,models(imodel)%n
			if(i.lt.ind_d) then
				if(j.lt.ind_d) then
					models(imodel)%link(i,j)=jtemp(i,j)
					!	models(imodel)%link(j,i)=models(imodel)%link(i,j)
				else
					models(imodel)%link(i,j)=jtemp(i,j+1)
				endif
			else
				models(imodel)%link(i,j)=jtemp(i+1,j+1)
			!	models(imodel)%link(j,i)=models(imodel)%link(i,j)
			endif
		enddo
		enddo
		do i=1,models(imodel)%n-1
			do j=i,models(imodel)%n
			models(imodel)%link(j,i)=models(imodel)%link(i,j)
		enddo
		enddo
		do i=1,models(imodel)%n-1
			!call intconv(i,cnum)
			models(imodel)%name_link(i)=' '    	
			do j=1,models(imodel)%n-1
				if(models(imodel)%link(i,j).eq.1) then
				call intconv(j,cnum)
				conex=models(imodel)%name_link(i)
				nc=len_trim(conex)
				models(imodel)%name_link(i)=conex(1:nc)//cnum(1:3)//';'

				endif
			enddo
		enddo
		if(ind_i.le.models(imodel)%ka) then
		
			models(imodel)%ka=models(imodel)%ka-1
			ka=models(imodel)%ka-1
		endif
		do k=1,ind_i-1
			is1(k)=k
		enddo
		do k=1,models(imodel)%n-1
			if(k.ge.ind_i) then
			is1(k)=(k+1)
			models(imodel)%colour(k)=models(imodel)%colour(k+1)
			models(imodel)%inter_link(k)=models(imodel)%inter_link(k+1)
			models(imodel)%X(k)=models(imodel)%X(k+1)
			models(imodel)%Y(k)=models(imodel)%Y(k+1)
			!models(imodel)%name_link(k)=models(imodel)%name_link(k+1)
			!models(imodel)%inter_link(k)=models(imodel)%inter_link(k+1)
			models(imodel)%statname(k)=models(imodel)%statname(k+1)
			call intconv(k,cnum)
			models(imodel)%name(k)=cnum
			models(imodel)%dgamma(k)=models(imodel)%dgamma(k+1)
			do 	l=1,models(imodel)%nlig
						models(imodel)%NBOUND(k,l)=models(imodel)%nbound(k+1,l)
						
					enddo
			endif
			call lincol(models(imodel)%colour(k))
			call jSYMBOL(models(imodel)%X(k),models(imodel)%y(k),-3,1.2,1.2,13,idev)
			call write_string(models(imodel)%name(k),models(imodel)%X(k)-0.1,&
			models(imodel)%y(k)+0.4,0.,0,1,0.35,48,dxs,dys)
			statname(k)=models(imodel)%statname(k)
			
			do l=1,10
				if(statname(k)(l:l).eq.char(45)) i1=l
			enddo
			if(indmod) then
				if(i1.ne.0) then
				call write_string(models(imodel)%statname(k)(1:i1-1),models(imodel)%X(k)-0.1,&
				models(imodel)%y(k)-0.2,0.,0,1,0.35,48,dxs,dys)	
				call write_string(models(imodel)%statname(k)(i1+1:i1+4),models(imodel)%X(k)-0.1,&
				models(imodel)%y(k)-0.7,0.,0,1,0.35,48,dxs,dys)
				else
				call write_string(models(imodel)%statname(k),models(imodel)%X(k)-0.1,&
				models(imodel)%y(k)-0.2,0.,0,1,0.35,48,dxs,dys)	
				endif
			else
				call write_string(models(imodel)%statname(k)(1:6),MODELS(imodel)%X(k)-0.1,&
				MODELS(imodel)%y(k)-0.2,0.,0,1,0.35,48,dxs,dys)
				call write_string(models(imodel)%statname(k)(7:12),MODELS(imodel)%X(k)-0.1,&
				MODELS(imodel)%y(k)-0.7,0.,0,1,0.35,48,dxs,dys)
			endif
			
		enddo
		
	
	    
			
			
			do i=1,models(imodel)%n
				models(imodel)%index(i)=i   
			enddo	
			
		models(imodel)%n=models(imodel)%n-1
		goto 99
		endif
		endif
		do k=1,istate
			iflagd=0
			call point_intersection(models(imodel)%X(ind_i),models(imodel)%y(ind_i),&
			models(imodel)%X(k),models(imodel)%y(k),xmov,ymov,1.0,0.2,iflagd)
			if (models(imodel)%link(k,ind_i).eq.1.and.iflagd.eq.1) then
				models(imodel)%link(k,ind_i)=0
				models(imodel)%link(ind_i,k)=0
				models(imodel)%inter_link(k)=models(imodel)%inter_link(k)-1
				models(imodel)%inter_link(ind_i)=models(imodel)%inter_link(ind_i)-1
			
				call chatonos(models(imodel)%name_link(k),num0,nk)
				models(imodel)%name_link(k)=' '
				do lk=1,nk-1
				if(num0(lk).ne.ind_i) then
					call intconv(num0(lk),cnum)
					kc=len_trim(cnum)
					jk=len_trim(	models(imodel)%name_link(k))
					models(imodel)%name_link(k)=models(imodel)%name_link(k)(1:jk)//&
					cnum(1:kc)//';'
				endif
				enddo
				!jk=len_trim(	models(imodel)%name_link(k))
				!models(imodel)%name_link(k)=models(imodel)%name_link(k)(1:jk)//';'
				
				call chatonos(models(imodel)%name_link(ind_i),num0,nk)
				models(imodel)%name_link(ind_i)=' '
				do lk=1,nk-1
				if(num0(lk).ne.k) then
					call intconv(num0(lk),cnum)
					kc=len_trim(cnum)
					jk=len_trim(	models(imodel)%name_link(ind_i))
					models(imodel)%name_link(ind_i)=models(imodel)%name_link(ind_i)(1:jk)//&
					cnum(1:kc)//';'
				endif
				enddo
			!	jk=len_trim(	models(imodel)%name_link(ind_i))
			!	models(imodel)%name_link(ind_i)=models(imodel)%name_link(ind_i)(1:jk)//';'
			
				CALL lincol(ibk)
				call gmoveto2d(MODELS(imodel)%X(ind_i),MODELS(imodel)%y(ind_i))
				call gdrawlineto2d(MODELS(imodel)%X(k),MODELS(imodel)%y(k))
				
				a=(models(imodel)%X(ind_i)-models(imodel)%X(k))/&
				(models(imodel)%y(ind_i)-models(imodel)%y(k))
				if(a.gt.0.4.and.a.lt.1.75) then
				call gmoveto2d(MODELS(imodel)%X(ind_i)-0.1,MODELS(imodel)%y(ind_i)+0.1)
				call gdrawlineto2d(MODELS(imodel)%X(k)-0.1,MODELS(imodel)%y(k)+0.1)
				else
				call gmoveto2d(MODELS(imodel)%X(ind_i)-0.1,MODELS(imodel)%y(ind_i)-0.1)
				call gdrawlineto2d(MODELS(imodel)%X(k)-0.1,MODELS(imodel)%y(k)-0.1)
			
				endif
				CALL LINCOL(models(imodel).colour(k))
				call jSYMBOL(models(imodel)%X(k),models(imodel)%y(k),-3,1.2,1.2,13,idev)
				call write_string(models(imodel)%name(k),models(imodel)%X(k)-0.1,&
				models(imodel)%y(k)+0.4,0.,0,1,0.35,48,dxs,dys)
				statname(k)=models(imodel)%statname(k)
				do l=1,10
			!	if(statname(k)(l:l).eq.'*') statname(k)(l:l)='x'
				if(statname(k)(l:l).eq.char(45)) i1=l
			    enddo
				if(indmod) then
				if(i1.ne.0) then
	call write_string(models(imodel)%statname(k)(1:i1-1),models(imodel)%X(k)-0.1,&
			models(imodel)%y(k)-0.2,0.,0,1,0.35,48,dxs,dys)	
	call write_string(models(imodel)%statname(k)(i1+1:i1+4),models(imodel)%X(k)-0.1,&
			models(imodel)%y(k)-0.7,0.,0,1,0.35,48,dxs,dys)
	else
	call write_string(models(imodel)%statname(k),models(imodel)%X(k)-0.1,&
			models(imodel)%y(k)-0.2,0.,0,1,0.35,48,dxs,dys)	
	endif
				else
				call write_string(models(imodel)%statname(k)(1:6),MODELS(imodel)%X(k)-0.1,&
			MODELS(imodel)%y(k)-0.2,0.,0,1,0.35,48,dxs,dys)
			call write_string(models(imodel)%statname(k)(7:12),MODELS(imodel)%X(k)-0.1,&
			MODELS(imodel)%y(k)-0.7,0.,0,1,0.35,48,dxs,dys)
				endif
				CALL LINCOL(models(imodel).colour(ind_i))
				call jSYMBOL(models(imodel)%X(ind_i),models(imodel)%y(ind_i),-3,1.2,1.2,13,idev)
				call write_string(models(imodel)%name(ind_i),models(imodel)%X(ind_i)-0.1,&
				models(imodel)%y(ind_i)+0.4,0.,0,1,0.35,48,dxs,dys)
				statname(ind_i)=models(imodel)%statname(ind_i)
				do l=1,10
			!	if(statname(ind_i)(l:l).eq.'*') statname(ind_i)(l:l)='x'
				if(statname(ind_i)(l:l).eq.char(45)) i1=l
			    enddo
				if(indmod) then
				if(i1.ne.0) then
	call write_string(models(imodel)%statname(ind_i)(1:i1-1),models(imodel)%X(ind_i)-0.1,&
			models(imodel)%y(ind_i)-0.2,0.,0,1,0.35,48,dxs,dys)	
	call write_string(models(imodel)%statname(ind_i)(i1+1:i1+4),models(imodel)%X(ind_i)-0.1,&
			models(imodel)%y(ind_i)-0.7,0.,0,1,0.35,48,dxs,dys)
	else
	call write_string(models(imodel)%statname(ind_i),models(imodel)%X(ind_i)-0.1,&
			models(imodel)%y(ind_i)-0.2,0.,0,1,0.35,48,dxs,dys)	
	endif
				else
				call write_string(models(imodel)%statname(ind_i)(1:6),MODELS(imodel)%X(ind_i)-0.1,&
			MODELS(imodel)%y(ind_i)-0.2,0.,0,1,0.35,48,dxs,dys)
				call write_string(models(imodel)%statname(ind_i)(7:12),MODELS(imodel)%X(ind_i)-0.1,&
			MODELS(imodel)%y(ind_i)-0.7,0.,0,1,0.35,48,dxs,dys)
		
				endif
	!	models(imodel)%n=models(imodel)%n-1
				exit
			endif
		enddo
		enddo
		
ELSE IF(move_STATE) THEN
	imodel=ind_1
	istate=models(imodel)%n
	indmod=models(imodel)%indmod
	if(imove.eq.0) then
		do i=1,istate
		if(xmov.lt.models(imodel)%X(I)+1.0.and.xmov.gt.models(imodel)%X(I)-1.0) then
		if(ymov.lt.models(imodel)%y(I)+1.0.and.ymov.gt.models(imodel)%y(I)-1.0) then
			xmov0=models(imodel)%X(I)
			ymov0=models(imodel)%y(I)
			imove=1
			ind_m=i
			call gmSetGuiCursor(lframe,Gdefault,Grubberband)
			exit
		endif
		endif
		enddo
	else
		x=float(ifixr(xmov))
		y=float(ifixr(ymov))
		do k=1,istate
		if(k.ne.ind_m) then
			if(xmov.lt.models(imodel)%X(k)+1.0.and.xmov.gt.models(imodel)%X(k)-1.0) then
			if(ymov.lt.models(imodel)%y(k)+1.0.and.ymov.gt.models(imodel)%y(k)-1.0) then
			imove=0
		!	xmov0=xmov
		!	ymov0=ymov
			call gmSetGuiCursor(lframe,Gdefault,Gdefault)
			goto 99
			endif
			endif
		endif
		enddo
		CALL lincol(ibk)
		call jSYMBOL(models(imodel)%X(ind_m),models(imodel)%y(ind_m),-3,1.2,1.2,13,idev)
		do k=1,istate
			if(k.ne.ind_m.and.models(imodel)%link(k,ind_m).eq.1) then
				call gmoveto2d(models(imodel)%X(ind_m),models(imodel)%y(ind_m))
				call gdrawlineto2d(models(imodel)%X(k),models(imodel)%y(k))
				a=(models(imodel)%X(ind_m)-models(imodel)%X(k))/&
				(models(imodel)%y(ind_m)-models(imodel)%y(k))
				if(a.gt.0.4.and.a.lt.1.75) then
				call gmoveto2d(MODELS(imodel)%X(ind_m)-0.1,MODELS(imodel)%y(ind_m)+0.1)
				call gdrawlineto2d(MODELS(imodel)%X(k)-0.1,MODELS(imodel)%y(k)+0.1)
				else
				call gmoveto2d(models(imodel)%X(ind_m)-0.1,models(imodel)%y(ind_m)-0.1)
				call gdrawlineto2d(models(imodel)%X(k)-0.1,models(imodel)%y(k)-0.1)
				endif
			endif
		enddo
		
		models(imodel)%X(ind_m)=x
		models(imodel)%y(ind_m)=y
		call lincol(46)
		dxs=0.6
		dys=0.8
		do k=1,istate
			if(k.ne.ind_m.and.models(imodel)%link(k,ind_m).eq.1) then
			call lincol(46)
				call gmoveto2d(models(imodel)%X(ind_m),models(imodel)%y(ind_m))
				call gdrawlineto2d(models(imodel)%X(k),models(imodel)%y(k))
				a=(models(imodel)%X(ind_m)-models(imodel)%X(k))/&
				(models(imodel)%y(ind_m)-models(imodel)%y(k))
				if(a.gt.0.4.and.a.lt.1.75) then
				call gmoveto2d(MODELS(imodel)%X(ind_m)-0.1,MODELS(imodel)%y(ind_m)+0.1)
				call gdrawlineto2d(MODELS(imodel)%X(k)-0.1,MODELS(imodel)%y(k)+0.1)
				else
				call gmoveto2d(models(imodel)%X(ind_m)-0.1,models(imodel)%y(ind_m)-0.1)
				call gdrawlineto2d(models(imodel)%X(k)-0.1,models(imodel)%y(k)-0.1)
				
				endif
				CALL LINCOL(models(imodel).colour(k))
				call jSYMBOL(models(imodel)%X(k),models(imodel)%y(k),-3,1.2,1.2,13,idev)
				call write_string(models(imodel)%name(k),models(imodel)%X(k)-0.1,&
				models(imodel)%y(k)+0.4,0.,0,1,0.35,48,dxs,dys)
				statname(k)=models(imodel)%statname(k)
				do l=1,10
			!	if(statname(k)(l:l).eq.'*') statname(k)(l:l)='x'
				if(statname(k)(l:l).eq.char(45)) i1=l
			    enddo
				if(indmod) then
				if(i1.ne.0) then
				call write_string(models(imodel)%statname(k)(1:i1-1),models(imodel)%X(k)-0.1,&
				models(imodel)%y(k)-0.2,0.,0,1,0.35,48,dxs,dys)	
				call write_string(models(imodel)%statname(k)(i1+1:i1+4),models(imodel)%X(k)-0.1,&
				models(imodel)%y(k)-0.7,0.,0,1,0.35,48,dxs,dys)
				else
				call write_string(models(imodel)%statname(k),models(imodel)%X(k)-0.1,&
				models(imodel)%y(k)-0.2,0.,0,1,0.35,48,dxs,dys)	
				endif
				else
				call write_string(models(imodel)%statname(k)(1:6),MODELS(imodel)%X(k)-0.1,&
				MODELS(imodel)%y(k)-0.2,0.,0,1,0.35,48,dxs,dys)
				call write_string(models(imodel)%statname(k)(7:12),MODELS(imodel)%X(k)-0.1,&
				MODELS(imodel)%y(k)-0.7,0.,0,1,0.35,48,dxs,dys)
				endif
			endif
		enddo
		CALL LINCOL(models(imodel).colour(ind_m))
		call jSYMBOL(x,y,-3,1.2,1.2,13,idev)
		call write_string(models(imodel)%name(ind_m),x-0.1,y+0.4,0.,0,1,0.35,48,dxs,dys)
		statname(ind_m)=models(imodel)%statname(ind_m)
				do l=1,10
			!	if(statname(ind_m)(l:l).eq.'*') statname(ind_m)(l:l)='x'
				if(statname(ind_m)(l:l).eq.char(45)) i1=l
			    enddo
				if(indmod) then
				if(i1.ne.0) then
	call write_string(models(imodel)%statname(ind_m)(1:i1-1),models(imodel)%X(ind_m)-0.1,&
			models(imodel)%y(ind_m)-0.2,0.,0,1,0.35,48,dxs,dys)	
	call write_string(models(imodel)%statname(ind_m)(i1+1:i1+4),models(imodel)%X(ind_m)-0.1,&
			models(imodel)%y(ind_m)-0.7,0.,0,1,0.35,48,dxs,dys)
	else
	call write_string(models(imodel)%statname(ind_m),models(imodel)%X(ind_m)-0.1,&
			models(imodel)%y(ind_m)-0.2,0.,0,1,0.35,48,dxs,dys)	
	endif
				else
		call write_string(models(imodel)%statname(ind_m)(1:6),MODELS(imodel)%X(ind_m)-0.1,&
			MODELS(imodel)%y(ind_m)-0.2,0.,0,1,0.35,48,dxs,dys)
		
		call write_string(models(imodel)%statname(ind_m)(7:12),MODELS(imodel)%X(ind_m)-0.1,&
			MODELS(imodel)%y(ind_m)-0.7,0.,0,1,0.35,48,dxs,dys)
		endif
		imove=0
		call gmSetGuiCursor(lframe,Gdefault,GDEFAULT)
		xmov0=models(imodel)%X(Ind_m)
		ymov0=models(imodel)%y(Ind_m)
		
	endif
endif
99 call gFlushGraphics()
!deallocate (jtemp)
dxs=dxs0
dys=dys0  
    
end	