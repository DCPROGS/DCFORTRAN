!     Last change:  D    15 Jan 104    2:21 pm
subroutine draw_indmodel(jgraph,imodel,main,graph1_2,GraphMainPanel1_2,graphics1_2,&
		models,plot,ipos)
	use menu_f90	
	
	character*10 statname(100)
	logical plot,indmod
	PARAMETER  N_STATES=100
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
		INTEGER model
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
		character*20 name_link(n_states)
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
	END TYPE MODEL

	TYPE (MODEL) MODELS(25)

	dxs0=dxs
	dys0=dys
	dxs=0.6
	dys=0.8	
	indmod=models(jgraph)%indmod
	nx=kstat0*4+4
	ny=nsub*4+4
	if(plot) then
	call linwid(1.)
	 if(ipos.le.1) then
		CALL PAPENQ(XPAP,YPAP,IPAPTY)
		CALL VPTSWI(2)
     endif
		xbeg=0.1*xpap
		ybeg=0.1*ypap
		xend=0.92*xpap
		yend=0.92*ypap
		wxmax=nx
		wymax=ny
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
		nl=nblank1(models(jgraph)%title_model)
		call write_string(models(jgraph)%title_model(1:nl),x,y,0.,0,5,0.5,1,dxs,dys,idev)
	else
		
	endif
	CALL gLINCOLs(14,idev)

	do i=1,nsub
		do j=1,kstat0
			if(models(jgraph)%start_states(i,j).eq.'y'.and.models(jgraph)%start_states(i,j+1).eq.'y') then
			x(i,j)=nx-4*j
			x(i,j+1)=nx-4(j+1)
			y(i,j)=ny-4*(2*i-1)
			y(i,j+1)=ny-4*(2*i-1)	
			call gmoveto2d(x(i,j),y(i,j+1))
			call gdrawlinetx(i,j),y(i,j+1))	
			else (models(jgraph)%start_states(i,j).eq.'n'.and.models(jgraph)%start_states(i,j+1).eq.'n') then
			x(i,j)=nx-4*j
			x(i,j+1)=nx-4(j+1)
			y(i,j)=ny-4*(2*i)
			y(i,j+1)=ny-4*(2*i)
			call gmoveto2d(x(i,j),y(i,j+1))
			call gdrawlinetx(i,j),y(i,j+1))	
				
			endif
			
		enddo
	enddo
	
	do ind_m=1,models(jgraph)%n

			do k=1,models(jgraph)%n
			if(k.ne.ind_m.and.models(jgraph)%link(k,ind_m).eq.1) then
				call lincol(46)
				call gmoveto2d(models(jgraph)%X(ind_m),models(jgraph)%y(ind_m))
				call gdrawlineto2d(models(jgraph)%X(k),models(jgraph)%y(k))
				a=(models(jgraph)%X(ind_m)-models(jgraph)%X(k))/&
				(models(jgraph)%y(ind_m)-models(jgraph)%y(k))
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
			CALL gLINCOLs(models(jgraph).colour(ind_m),idev)
			call jSYMBOL(models(jgraph)%X(ind_m),models(jgraph)%y(ind_m),-3,1.2,1.2,13,idev)
			call write_string(models(jgraph)%name(ind_m)(1:3),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)+0.4,0.,0,5,0.35,14,dxs,dys,idev)
			statname(ind_m)=models(jgraph)%statname(ind_m)
			do l=1,10
				if(statname(ind_m)(l:l).eq.'*') statname(ind_m)(l:l)='x'
				if(statname(ind_m)(l:l).eq.char(45)) i1=l
			enddo
			if(indmod) then
			if(i1.ne.0) then
	call write_string(models(jgraph)%statname(ind_m)(1:i1-1),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.2,0.,0,1,0.45,14,dxs,dys,idev)
	call write_string(models(jgraph)%statname(ind_m)(i1+1:i1+4),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.7,0.,0,1,0.45,14,dxs,dys,idev)
	else
	call write_string(models(jgraph)%statname(ind_m),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.2,0.,0,1,0.45,14,dxs,dys,idev)
	endif
			else
			call write_string(statname(ind_m),models(jgraph)%X(ind_m)-0.1,&
			models(jgraph)%y(ind_m)-0.5,0.,0,1,0.45,14,dxs,dys,idev)
			endif
		enddo
		!	call gFlushGraphics()
		call linwid(0.)
end
