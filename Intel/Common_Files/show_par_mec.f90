subroutine show_par_mec(main,Data_list,ixd,iyd,PalArray6,button6,imodel,models,gfilem)
use menu_f90

integer :: Main
integer :: Data_list
integer :: DataMainPanel
integer :: PalArray6(50,50)
integer :: Static6
integer :: Button6(4)
real :: xdata(100,1,4,25)
character*60 stitle
character*70 mtitle
character*11 cnumb
character*40 gfilem
logical :: newfile=.false.
type(GARRAYCELL) arrayattribs
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
		integer jgraph
		character*80 title_model
		REAL X(N_STATES)
		REAL Y(N_STATES)
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

jset=1
njset=1
iplot=imodel
nset=1
niobs=25
nset=1
stitle=models(imodel)%title_model
nrows=models(imodel)%n
nplot=100
ncols=4 ! attention 3rd dimension for xdata!!!
mtitle(1:5)='Model'

call values_list(Main,Data_list,ixd,iyd,PalArray6,button6,jset,iplot,ncols,nrows,xdata,stitle,&
		niobs,njset,nplot,mtitle,static6,nset,gfilem,newfile)

end