	subroutine SETEC50(readini,imod0,dcmod,imodsav,nlig,
     &	conc,titlep,IQ)
c
c Subroutine for HJCFIT to set constraints -called from main prog when
c ordering method used for MR but called from GETQD1 when spanning tree used
c
c Modif 09/01/04 09:25pm, for case where 2 constraints are set, and nlig>1,
c to ask for conc of all the other ligands at which the specified ec50 was
c determined. These 'other conc' are kept in two new arrays, eg conc_ec1
c has conc of all lignands (except #1) at which ec50 for ligand 1 was determined
c  These arrays added to common/ec4/ (needed only if nlig>1)
c
	real*4 conc_ec1(10),conc_ec2(10)
c
 	logical readini
	logical dcmodel,dcmod
	real*4 conc(10,10)	!to hold concentration ith ligand, jth set
	character*10 titlep(200)
	integer*4 IQ(100,100)
	real*4 xs
	logical fixec50,prtec50
	real*8 ec501,xqlo,xqhi
	common/ec/fixec50,nmod,ec501,i50,j50,m50,prtec50,xqlo,xqhi,dcmodel
c	!nmod=imod0 (already in commom/model)
	common/ec2/monot,nlvar,vhold,vref,cfacd,dgamma !for ec50_hjc, qset_hjc
	character*20 ligname(10)
	common/ec3/ligname		!for qset_hjc
	COMMON/LIG/nligsav,IL(100)	!for hjclik, checkqd, qset_hjc
	real*8 ec502,xqlo2,xqhi2
	common/ec4/nfixec50,nlvar2,ec502,i502,j502,m502,
     &     xqlo2,xqhi2,conc_ec1,conc_ec2	 !for 2nd fixed ec50
	real*8 penfunc,penfac
	common/pen/penalty,penfunc,penfac 	!for hjclik, qset_hjc
	character*1 ans
	common/cpar/ncdep,IX,JX,x	!for hjclik, checkqd, qset_hjc, hjcdisp
	COMMON/EBLK/NEQ,IE(200),JE(200),IF(200),JF(200),EFAC(200) !for hjclik, checkqd, qset_hjc
	integer NSC(50),IM(50,100),JM(50,100),IX(100),JX(100)
	COMMON/MPAR/NCYC,NSC,IM,JM		!for hjclik, checkqd, qset_hjc
	logical obeymr(50),automr(50)
	common/mr/obeymr,automr
	integer*4 isetmr(50)
	common/mr1/isetmr
	logical discprt
	common/dp/discprt
c
25	continue
	if(ncdep.gt.0) then
         print 138
         if(discprt) write(8,138)
	   call BELL(1)
138	format(
     &' ------------------------------------------------------------')
	   ans='N'
	   if(readini.and.fixec50.and.imod0.eq.imodsav) ans='Y'
	   call DCASK(' Specify EC50 (no block) as constraint',ans,ans)
	   if(ans.eq.'N') then
		fixec50=.false.
		nfixec50=0		!number fixed by ec50 (0 or 1 or 2)
	   else
		fixec50=.true.
c NB at present, modec50 works only for nlig=1
		if(dcmodel.and.nlig.eq.1) then
		   ans='N'
		   if(dcmod) ans='Y'
		   print 19,nmod,ans
19		   format(
     &  ' Check: is this model ',i3,' in DC''s QMECHS file [',a1,'] ? ')
		   call INPUTa(ans)
		   if(ans.eq.'N') dcmodel=.false.	!don't use modec50
		endif
		if(nlig.eq.1) then
		   nlvar=1
		   nfixec50=1		!number fixed by ec50 (0 or 1 or 2)
		else if(nlig.gt.1) then
		   ans='N'
		   if(nfixec50.eq.2) ans='Y'	!not in .ini at present
		   call DCASK('Constrain EC50 for TWO agonists',ans,ans)
		   if(ans.eq.'N') then
			nfixec50=1		!number fixed by ec50 (0 or 1 or 2)
		   else
			nfixec50=2		!number fixed by ec50 (0 or 1 or 2)
		   endif
		endif
c
		do ifix=1,nfixec50
c		   do j=1,nlig
c			print 141,j,ligname(j),conc(j,1)*1.e6
cc			if(discprt) write(8,141) j,ligname(j),
cc     &			conc(j,1)*1.e6
c141			format('  (conc of ligand #',i2,',  ',a10,' = ',
c     &			g13.6,' micromolar)')
c		   enddo
		   if(ifix.eq.1) then
			n=nlvar
			if(n.eq.0) n=1
			xs=sngl(ec501*1.d6)		!micromolar default
		   else
			n=nlvar2
			if(n.eq.0) then  	!pick a value different from nlvar
			   do j=1,nlig
				if(j.ne.nlvar) then
				   n=j
				   goto 2712
				endif
			   enddo
			endif
2712			xs=sngl(ec502*1.d6)		!micromolar default
		   endif
271		   continue
c
		   n=ifix
		   print 292,n,ligname(n),xs
292		   format(' Ligand #',i2,
     &  ': EC50 for ',a10,' (without block) (micromolar) [',f9.4,'] = ')
		   call INPUTr(xs)
		   if(ifix.eq.1) then
			nlvar=n
			ec501=dble(xs*1.e-6)	!molar
			if(nlig.gt.1) then	!set conc of other ligands for ec50
			   do i=1,nlig
				if(i.ne.nlvar) then
				   x=1.e6*conc_ec1(i)
				   print 30,ligname(i)(1:10),x
30				   format(
     & '  Conc of ',a10,' at which EC50 was determined (micromolar) [',
     & f9.4,'] = ')
				   call INPUTr(x)
				   conc_ec1(i)=x*1.e-6	!molar
				endif
			   enddo
			endif
		   else if(ifix.eq.2) then
			nlvar2=n
			ec502=dble(xs*1.e-6)	!molar
			if(nlig.gt.1) then	!set conc of other ligands for ec50
			   do i=1,nlig
				if(i.ne.nlvar2) then
				   x=1.e6*conc_ec2(i)
				   print 30,ligname(i)(1:10),x
c30				   format(
c     & 'Conc of ',a10,' at which EC50 was determined (micromolar) [',
c     & f9.4,'] = ')
				   call INPUTr(x)
				   conc_ec2(i)=x*1.e-6	!molar
				endif
			   enddo
			endif
		   endif
c
		   print 291,n,ligname(n),xs
		   if(discprt) write(8,291) n,ligname(n),xs
291		   format(
     & ' EC50 for ligand # ',i2,'(',a10,')  constrained to be ',g13.6,
     & ' micromolar',/,'  at concentrations of other ligands: ')
		   do j=1,nlig
			if(j.ne.n) then
			   if(ifix.eq.1) then
				x=conc_ec1(j)*1.e6
			   else if(ifix.eq.2) then
				x=conc_ec2(j)*1.e6
			   endif
			   print 144,j,ligname(j),x
			   if(discprt)write(8,144) j,ligname(j),x
144			   format('  (conc of ligand #',i2,',  ',a10,' = ',
     &			 g13.6,' micromolar)')
			endif
		   enddo
c
c Now get i,j for the q(i,j) to be fixed by ec50
		   if(ifix.eq.1) then
			i=i50
			j=j50
		   else
		      i=i502
			j=j502
		   endif
		   print 21,i,j
21		   format(
     & ' Use EC50 to constrain rate constant from state i to state j = '
     & ,/,'   specify: i, j [',i3,',',i3,'] = ')
		   call INPUT2i(i,j)
c	Check this is not already fixed via mr, or constrained
		   if(ncyc.gt.0) then
			do m1=1,ncyc
			   m=isetmr(m1)	!actual cycle number
			   if(obeymr(m)) then
				if(i.eq.im(m,1).and.j.eq.jm(m,1)) then
				   call BELL(1)
				   print 221,i,j,m
221				   format(
     & ' Element ',i2,',',i2,' is fixed by microscopic reversibility',/,
     & ' (cycle #',i3,'). Do not constrain it with EC50! Start again.')
				   goto 25
				endif
			   endif
			enddo
		   endif
		   if(neq.gt.0) then		!check constraints
			do n=1,neq
			   if(i.eq.ie(n).and.j.eq.je(n)) then
				call BELL(1)
				print 222,i,j
222				format(
     & ' Element ',i2,',',i2,' is already constrained.',/,
     & ' Do not constrain it with EC50!  Start again.')
				goto 25
			   endif
			enddo
		   endif
		   if(ifix.eq.1) then
			i50=i
			j50=j
		   else
			i502=i
			j502=j
		   endif
c         Set limits to constrained rate constant to prevent silly values
		   if(ifix.eq.1) then
			x1=sngl(xqlo)
			x2=sngl(xqhi)
		   else
			x1=sngl(xqlo2)
			x2=sngl(xqhi2)
		   endif
		   print 226,i,j,x1,x2
226		   format(
     & ' Lower and upper limits for q(',i2,',',i2,') [',
     &  g11.4,',',g11.4,'] = ')
		   call INPUT2r(x1,x2)
		   if(ifix.eq.1) then
			xqlo=dble(x1)
			xqhi=dble(x2)
			m50=IQ(i50,j50)		!OK so define param #
			m5=m50            	!for print
			xs=1.e6*sngl(ec501)      !for print
		   else
			xqlo2=dble(x1)
			xqhi2=dble(x2)
			m502=IQ(i502,j502)		!OK so define param #
			m5=m502			!for print
			xs=1.e6*sngl(ec502)      !for print
		   endif
		   print 22,m5,i,j,titlep(m5),xs
22		   format(' rate ',
     & i3,3x,' q(',i2,',',i2,') =',1x,a10,' is constrained to give',/,
     & ' an EC50 = ',g16.6,' micromolar: O.K. [Y] ? ')
		   ans='Y'
		   call INPUTa(ans)
		   if(ans.eq.'N') goto 25
		enddo		!ifix=1,nfixec50
c
		penfac=10.0d0
		x1=sngl(penfac)
		print 227,x1
227		format(' Factor for penalty function [',f7.1,'] = ')
		call INPUTr(x1)
		penfac=dble(x1)
c
	   endif	!end of fixec50=true
	endif		!end of ncdep.gt.0
c
	RETURN
	end


