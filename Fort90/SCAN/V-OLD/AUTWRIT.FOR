	subroutine AUTWRIT(kt,tint,iampl,iprops,ntmax,nfits,timsav,ifits,
     & ibaselin,nfmax,nbuf,novlap,srate,finter,errfac,derivfac,
     & tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
     & nwrit,nwsav,newpar,opendown,invert,confac,
     & usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     & expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     & nsetup,filtfile,npfilt,ffilt,
     & sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,ioff1,
     & disptran,dispderiv,dispguess,ampfac,tmin,
     & tsfac,tlfac,sdone,dfinter,tlast,goback,shutsav,
     & shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     & infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     & itrig,itrigvga,itriglev,inc,incabs,
     & indfst,isdfst,isec,ndisp,ndisp1,
     & iscanver,tcfac,facjump,
     & ifreq1,ifreq2,ifreq3,imin,imax,ifmin,ifmax,
     & cjump,ijump,njdim,jumps,kjumps,tzerod,intzero,tsample,
     & ktjump,njump,njfit,nnull,ktlast)
c
c For automatic backup of tint,iampl etc to disc, + temp files for restart,
c in SCAN.
c Point amp histos need to be kept only between elements ifmin,ifmax (all
c the rest are zeroes)
c
c Following to be recalc following restart (along with filt1() etc)
c     & filt1,nfilt1,dtf1,timamax,
c     & tf01,tf1,tf50,tf99,tf999,npfilt,
c
	real*8 dfinter,tlast
	logical invert,opendown,newpar,sdone
	logical disp,pdpdata,usepots
	logical disptran,dispderiv,dispguess
	logical shut,shutprev,shutsav,goback,backward
	logical discprt
c	logical pon,slock,debug,caplock
c declarations from CONSAM
	character ADCFIL*30,cdate*11,cdate1*11,ndev*2
	character adctime*8
c for point amp histos
	integer*4 ifreq1(imin:imax),ifreq2(imin:imax),ifreq3(imin:imax)
	integer*4 ifmin(4),ifmax(4)
c Declarations for temporary data files
	real*8 timsav(nfmax)
	integer*2 ifits(nfmax),ibaselin(nfmax)
	real*4 tint(ntmax)
	integer*2 iampl(0:ntmax)
	integer*1 iprops(ntmax)
	character filtfile*20
c For cjumps
	real*8 tzerod,tsample
	integer*2 jumps(njdim),kjumps(njdim)
	logical cjump
	integer*4 ipatch
c
	common/miscdat/ipatch,Emem	!for DSKWRIT/READJUMP/AUTWRIT/READSCAN
c
	common/rstrtblk/nchan,pdpdata,adcfil,cdate1
	common/dp/discprt

c
c NB CJUMP/CJFIT/READ14/GETPATCH/READJUMP all use IPATCH=INTEGER*2
C BUT SCAN/EKDIST USE IPATCH=INTEGER*4 -NOW CONVERTED IN READJUMP
C=	ipatch=int4(jpatch)
c
c Find largest and smallest non-zero entries in ifreq()
	call GETIFMAX(imin,imax,ifreq1,ifreq2,ifreq3,ifmin,ifmax)
c
c Data for SCANTEMP.DAT
c (1) First write individual values needed for restart (=== bytes) starting
c	at 1st byte
c  (2) Start writing arrays at byte #=ioffset
c
c Judged by doing first write only, values up to tsample take 510 bytes!
c	ioffset=512
	ioffset=768
	OPEN(unit=13,file='SCANTEMP.DAT',status='UNKNOWN',
     &    access='TRANSPARENT')
	 write(unit=13,rec=1) iscanver,kt,nfits,ntmax,nfmax,ioffset,
     & nbuf,novlap,srate,finter,
     & tsect,ioff,ndat,nsec,nrlast,avtot,navamp,avamp,rms,
     & nwrit,nwsav,newpar,opendown,invert,
     & usepots,disp,smult,scrit,vary,ntrig,navtest,dgain,iboff,
     & expfac,bdisp,ibflag,iautosub,xtrig,ndev,cdate,adctime,
     & nsetup,filtfile,ffilt,npfilt,
     & sfac1,sfac2,sfac3,nscale,calfac,calfac1,calfac2,iyoff,
     & ioff1,disptran,dispderiv,dispguess,ampfac,tmin,
     & tsfac,tlfac,sdone,dfinter,tlast,
     & shut,shutprev,backward,prevlevel,t0sav,y0sav,vard,nshutfit,
     & infit,infirst,ixfprev,idiskq,ifirst,base,basevga,ibasevga,
     & itrig,itrigvga,itriglev,inc,incabs,
     & indfst,isdfst,isec,ndisp,ndisp1,
     & pdpdata,adcfil,cdate1,nchan,
     & iscanver,tcfac,facjump,shutsav,goback,
     & imin,imax,errfac,derivfac,confac,
     & cjump,ijump,njdim,tzerod,intzero,tsample,ktjump,njfit,
     & njump,nnull,ipatch,Emem,ktlast
c	goto 99	!==========check offset!!
	write(unit=13,rec=ioffset)
     &  (tint(i),i=1,kt),(iampl(i),i=1,kt),(iprops(i),i=1,kt),
     &  (timsav(i),i=1,nfits),(ifits(i),i=1,nfits),
     &  (ibaselin(i),i=1,nfits),
     &  ifmin,ifmax,
     &  (ifreq1(i),i=ifmin(1),ifmax(1)),
     &  (ifreq2(i),i=ifmin(2),ifmax(2)),
     &  (ifreq3(i),i=ifmin(3),ifmax(3)),
     &  (jumps(i),i=1,ijump),(kjumps(i),i=1,ijump)
c
c99	continue
	CLOSE(unit=13)
c
	RETURN
	end


