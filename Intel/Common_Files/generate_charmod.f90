subroutine generate_charmod(jgraph,models,ilast,jlast,new)

	character*2 charmod(25,40)
	integer imatrix(25,40),ix(50),iy(50)
	
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

	
	jlast=(models(jgraph)%nwidth-4)/2
	ilast=(models(jgraph)%nheight-4)/2
	
	do jn=1,jlast
		do in=1,ilast
			charmod(in,jn)='  '
		enddo
	enddo
	do k=1,models(jgraph)%n
		jn=ifixr(models(jgraph)%X(k)/2.)
		in=ifixr(0.5*(models(jgraph)%nheight-models(jgraph)%y(k)))
		ix(k)=jn
		iy(k)=in
		if(models(jgraph)%colour(k).eq.12) then
			charmod(in,jn)(1:1)='O'
		else
			charmod(in,jn)(1:1)='C'
		endif
		charmod(in,jn)(2:2)=models(jgraph)%name(k)
		if(k.gt.9) 	charmod(in,jn)=models(jgraph)%name(k)
		imatrix(in,jn)=IVAL(charmod,in,jn)
	enddo
	if(new.eq.1) then
	do i=1,models(jgraph)%n
		do j=i+1,models(jgraph)%n
		if(models(jgraph)%link(i,j).eq.1) then
			if(ix(i).lt.ix(j)) then
				if(iy(i).lt.iy(j)) then
				
				charmod(iy(i)+1,ix(i)+1)='\'
				else if (iy(i).gt.iy(j)) then
				
				charmod(iy(i)-1,ix(i)+1)='/'
				else
					charmod(iy(i),ix(i)+1)='-'
				endif
			else if(ix(i).gt.ix(j)) then
				if(iy(i).lt.iy(j)) then
				
				charmod(iy(i)+1,ix(i)-1)='/'
				else if (iy(i).gt.iy(j)) then
				
					charmod(iy(i)-1,ix(i)-1)='\'
				else
					charmod(iy(i),ix(i)-1)='-'
				endif
			else
				if(iy(i).lt.iy(j)) charmod(iy(i)+1,ix(i))='|'
				if(iy(i).gt.iy(j)) charmod(iy(i)-1,ix(i))='|'
			endif
		endif
		enddo
	enddo
	else
	do i=1,models(jgraph)%n
		do j=i+1,models(jgraph)%n
		if(models(jgraph)%link(i,j).eq.1) then
			
			if(ix(i).gt.ix(j)) then
				if(iy(i).lt.iy(j)) then
				
					charmod(iy(i)+1,ix(i)-1)='/'
				else if (iy(i).gt.iy(j)) then
				
					charmod(iy(i)-1,ix(i)-1)='\'
				else
					charmod(iy(i),ix(i)-1)='ÄÄ'
				endif
			else if(ix(i).lt.ix(j)) then
				if(iy(i).lt.iy(j)) then
				
					charmod(iy(i)+1,ix(i)+1)='\'
				else if (iy(i).gt.iy(j)) then
				
					charmod(iy(i)-1,ix(i)+1)='/'
				else
					charmod(iy(i),ix(i)+1)='ÄÄ'
				endif
			else
				if(iy(i).lt.iy(j)) charmod(iy(i)+1,ix(i))=' ³'
				if(iy(i).gt.iy(j)) charmod(iy(i)-1,ix(i))=' ³'
			endif
		endif
		enddo
	enddo
	endif
	do in=1,ilast
	do jn=1,jlast
	imatrix(in,jn)=IVAL(charmod,in,jn)
	models(jgraph)%charmod(in,jn)=charmod(in,jn)
	enddo
	enddo
end
	