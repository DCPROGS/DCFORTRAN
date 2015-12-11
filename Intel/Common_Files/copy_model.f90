!     Last change:  D    15 Jan 104    2:24 pm
subroutine copy_model(models,iold,inew,jgraph,jnew,ratcons,indk,indkn)

	use menu_f90
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
		character*15 statname(n_states)
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


		TYPE rate_constant 
		integer irec
		integer imod
		character*74 title
		character*10 titlep(200)
		real*8 value(200)
		character*15 qij(200)
		integer iconc(200)
		character*20 ligant(200)
		integer nsetq
		integer ieq(200)
		integer jeq(200)
		integer ifq(200)
		integer jfq(200)
		real efacq(200)
		real*8 qt(100,100)
		character*20 micro(200)
		integer ncyc
		integer nsc(50)
			integer im(50,50)
		integer jm(50,50)
	END TYPE rate_constant
	
	type(rate_constant) ratcons(500)

	models(jnew)%n=models(jgraph)%n
	models(jnew)%ka=models(jgraph)%ka
	models(jnew)%kb=models(jgraph)%kb
	models(jnew)%indmod=models(jgraph)%indmod
	models(jnew)%ncon=models(jgraph)%ncon
	models(jnew)%kcon=models(jgraph)%kcon
	models(jnew)%nsub=models(jgraph)%nsub
	models(jnew)%kstat0=models(jgraph)%kstat0
	models(jnew)%kstat=models(jgraph)%kstat
	models(jnew)%nlig=models(jgraph)%nlig
	models(jnew)%npar=models(jgraph)%npar
	models(jnew)%chardef=models(jgraph)%chardef
	models(jnew)%title_model=models(jgraph)%title_model
	models(jnew)%nchan=models(jgraph)%nchan
	models(jnew)%vref=models(jgraph)%vref
	models(jnew)%nwidth=models(jgraph)%nwidth+6
	models(jnew)%nheight=models(jgraph)%nheight+4
	models(jnew)%ilast=models(jgraph)%ilast
	models(jnew)%jlast=models(jgraph)%jlast
	do k=1,models(jnew)%nlig
		models(jnew)%ligname(k)=(models(jgraph)%ligname(k))
	enddo
	do k=1,models(jnew)%n
		models(jnew)%dgamma(k)=(models(jgraph)%dgamma(k))

		models(jnew)%statname(k)=models(jgraph)%statname(k)
		models(jnew)%x(k)=models(jgraph)%x(k)
	models(jnew)%y(k)	=models(jgraph)%y(k)
		models(jnew)%colour(k)=models(jgraph)%colour(k)
		models(jnew)%name(k)=models(jgraph)%name(k)
		models(jnew)%draw(k)=models(jgraph)%draw(k)
		models(jnew)%inter_link(k)=models(jgraph)%inter_link(k)
		models(jnew)%name_link(k)=models(jgraph)%name_link(k)
		models(jnew)%index(k)=models(jgraph)%index(k)
		do j=1,models(jnew)%n
			models(jnew)%link(k,j)=models(jgraph)%link(k,j)
		enddo
		do j=1,10
			models(jnew)%nbound(k,j)=models(jgraph)%nbound(k,j)
		enddo
	enddo
	if(models(jnew)%indmod) then
		do j=1,models(jnew)%kcon
			models(jnew)%con_states(j)=models(jgraph)%con_states(j)
		enddo
		do i=1,models(jnew)%nsub
	
		do j=1,models(jnew)%kstat0
			models(jnew)%sub_states(i,j)=models(jgraph)%sub_states(i,j)
		enddo
		enddo
	
	endif
   
	models(jnew)%ix=models(jgraph)%ix+2
	models(jnew)%iy=models(jgraph)%iy+2
    if(models(jnew)%ix.le.3)  models(jnew)%ix=4 
	
	ratcons(indkn)%ncyc=ratcons(indk)%ncyc
	do k=1,models(jnew)%npar
		ratcons(indkn)%qij(k)=ratcons(indk)%qij(k)
		!ratcons(indkn)%ligant(k)=ratcons(indk)%ligant(k)	
		ratcons(indkn)%value(k)=ratcons(indk)%value(k)
		ratcons(indkn)%titlep(k)=ratcons(indk)%titlep(k)
		do i=1,2
		models(jnew)%ic(i,k)=	models(jgraph)%ic(i,k)
		enddo
		if(ratcons(indkn)%ncyc.gt.0) then
				
			!	ratcons(indkn)%micro(k)='MR'	!dagger sign '=q(1,2)' indicates micro rev route
				
		endif
	enddo
!	goto 99
	if(ratcons(indkn)%ncyc.gt.0) then
	do l=1,ratcons(indkn)%ncyc
	    ratcons(indkn)%nsc(l)=ratcons(indk)%nsc(l)
		do m=1,ratcons(indkn)%nsc(l)
		ratcons(indkn)%im(l,m)=ratcons(indk)%im(l,m)
		ratcons(indkn)%jm(l,m)=ratcons(indk)%jm(l,m)
		enddo
	enddo
	endif
		
99 end
