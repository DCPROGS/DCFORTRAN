!     Last change:  D    15 Jan 104    2:21 pm
subroutine draw_model(jgraph,imodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
		models,plot,ipos,mod_create,initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
	use menu_f90	
	
	character*10 statname(100)
	integer :: Graph1_2(100)
    integer :: GraphMainPanel1_2(100)
    integer :: Graphics1_2(100)
	logical plot,indmod
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
		CHARACTER*15 STATNAME(N_STATES)
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

	TYPE (MODEL) MODELS(25)
	common/text_attr/bold,italik,underline,ipow,idev,nolabx,ifontrue
common/switch/iswicon,igraphText(100),ibutton1,icongmod
	mtype=1
	dxs0=dxs
	dys0=dys
	dxs=0.6
	dys=0.8	
	indmod=models(jgraph)%indmod
	if(plot) then
	call linwid(1.)
	 if(ipos.le.1) then
		CALL PAPENQ(XPAP,YPAP,IPAPTY)
		CALL VPTSWI(2)
     endif
		xbeg=0.05*xpap
		ybeg=0.05*ypap
		xend=0.95*xpap
		yend=0.90*ypap
	!	xbeg=xpap
	!	ybeg=ypap
	!	xend=xpap
	!	yend=ypap
		wxmax=float(models(jgraph)%nwidth)-2.
		wymax=float(models(jgraph)%nheight)
		select case(ipos)
		case(1)
			xbeg=0.05*xpap
			ybeg=0.52*ypap
			xend=0.48*xpap
			yend=0.95*ypap
		case(2)
			xbeg=0.52*xpap
			ybeg=0.52*ypap
			xend=0.95*xpap
			yend=0.95*ypap
		case(3)
			xbeg=0.05*xpap
			ybeg=0.05*ypap
			xend=0.48*xpap
			yend=0.48*ypap
		case(4)
			xbeg=0.52*xpap
			ybeg=0.05*ypap
			xend=0.95*xpap
			yend=0.48*ypap
	 end select
		CALL SETVP2(0.,wxmax,0.,wymax,xbeg,xend,ybeg,yend)
		x=wxmax/2.-1.
		y=wymax-1.
		nl=len_trim(models(jgraph)%title_model)
		call write_string(models(jgraph)%title_model(1:nl),x,y,0.,0,5,0.5,1,dxs,dys)
	else
	call graph2(models(jgraph)%ix,models(jgraph)%iy,jgraph,imodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
	models(jgraph)%nwidth,models(jgraph)%nheight,mtype,models(jgraph)%title_model,mod_create,&
	initialpage,ini_MainPanel,ini_panel1,ini_panel1_1,ini_Text1_1)
	endif
	isym=-3
	icolw=48
	if(idev.gt.2) then
	!	isym=3
		icolw=0
	endif
	
	
		do ind_m=1,models(jgraph)%n

			do k=1,models(jgraph)%n
			if(k.ne.ind_m.and.models(jgraph)%link(k,ind_m).eq.1) then
				call glincols(46,idev)
				call gmoveto2d(models(jgraph)%X(ind_m),models(jgraph)%y(ind_m))
				call gdrawlineto2d(models(jgraph)%X(k),models(jgraph)%y(k))
				diff=models(jgraph)%y(ind_m)-models(jgraph)%y(k)
				if(diff.ne.0.0) then 
				a=(models(jgraph)%X(ind_m)-models(jgraph)%X(k))/&
				(models(jgraph)%y(ind_m)-models(jgraph)%y(k))
				else
				    a=2.
				endif
				if(a.gt.0.4.and.a.lt.1.75) then
				call gmoveto2d(models(jgraph)%X(ind_m)-0.1,models(jgraph)%y(ind_m)+0.1)
				call gdrawlineto2d(models(jgraph)%X(k)-0.1,models(jgraph)%y(k)+0.1)
				else
				call gmoveto2d(models(jgraph)%X(ind_m)-0.1,models(jgraph)%y(ind_m)-0.1)
				call gdrawlineto2d(models(jgraph)%X(k)-0.1,models(jgraph)%y(k)-0.1)
				endif
				
            endif
        enddo
		enddo
		do ind_m=1,models(jgraph)%n
			
			if(idev.eq.0) then
				CALL gLINCOLs(models(jgraph).colour(ind_m),idev)
				call jSYMBOL(models(jgraph)%X(ind_m),models(jgraph)%y(ind_m),isym,1.2,1.2,13,idev)
			else
				if(models(jgraph).colour(ind_m).eq.50) call glincols(12,idev) !red
				if(models(jgraph).colour(ind_m).eq.51) call glincols(10,idev)  ! gren
				call jSYMBOL(models(jgraph)%X(ind_m),models(jgraph)%y(ind_m),-3,1.2,1.2,13,idev)
			endif
			call write_string(models(jgraph)%name(ind_m)(1:3),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)+0.4,0.,0,1,0.35,icolw,dxs,dys)
			statname(ind_m)=models(jgraph)%statname(ind_m)
			do l=1,10
			!	if(statname(ind_m)(l:l).eq.'*') statname(ind_m)(l:l)='x'
				if(statname(ind_m)(l:l).eq.char(45)) i1=l
			enddo
			if(models(jgraph)%indmod) then
			if(i1.ne.0) then
			call write_string(models(jgraph)%statname(ind_m)(1:i1-1),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.2,0.,0,1,0.35,icolw,dxs,dys)
			call write_string(models(jgraph)%statname(ind_m)(i1+1:i1+4),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.7,0.,0,1,0.35,icolw,dxs,dys)
			else
			call write_string(models(jgraph)%statname(ind_m),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.2,0.,0,1,0.35,icolw,dxs,dys)
			endif
			else
			call write_string(models(jgraph)%statname(ind_m)(1:6),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.2,0.,0,1,0.35,icolw,dxs,dys)
			call write_string(models(jgraph)%statname(ind_m)(7:12),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.7,0.,0,1,0.35,icolw,dxs,dys)

			endif
		enddo
		!	call gFlushGraphics()
		call linwid(0.)
end
