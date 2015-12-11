Comparing files HJCFIT.FOR and c:\fort90\hjcfit\old\hjcfit.for
****** HJCFIT.FOR
        real*8 ec502,xqlo2,xqhi2
        real*4 conc_ec1(10),conc_ec2(10),concsav(10)    !conc of other ligands for ec50
        real*4 conc_ec(10,10)           !for copy of conc()
        common/ec4/nfixec50,nlvar2,ec502,i502,j502,m502,
     &     xqlo2,xqhi2,conc_ec1,conc_ec2         !for 2nd fixed ec50
c  common/ec/ is in checkqd, qset_hjc
****** c:\fort90\hjcfit\old\hjcfit.for
        real*8 ec502,xqlo2,xqhi2
        common/ec4/nfixec50,nlvar2,ec502,i502,j502,m502,
     &     xqlo2,xqhi2   !for 2nd fixed ec50
c  common/ec/ is in checkqd, qset_hjc
******

****** HJCFIT.FOR
     &    nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     &    useprim,(isetmr(i),i=1,ncyc),ratemax,nmr,nlig,
     &    (conc_ec1(i),i=1,nlig),
     &    (conc_ec2(i),i=1,nlig)
c
****** c:\fort90\hjcfit\old\hjcfit.for
     &    nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     &    useprim,(isetmr(i),i=1,ncyc),ratemax,nmr
c
******

****** HJCFIT.FOR
     & '  EC50 for ',a10,' = ',g16.6,' micromolar,',/,
     & ' using penalty factor = ',g13.6,' if needed')
                if(nlig.gt.1) then
                   if(discprt) write(8,261)
261                format(
     &  '    Concentration of other ligands for EC50 determination')
                   do i=1,nlig
                        if(i.ne.n) then
                           if(ifix.eq.1) then
                                x=1.e6*conc_ec1(i)
                           else if(ifix.eq.2) then
                                x=1.e6*conc_ec2(i)
                           endif
                           if(discprt) write(8,325) ligname(i)(1:10),x
325                        format(
     &'    Conc of ',a10,' at which EC50 was determined (micromolar) = '
     & ,f9.4)
                        endif
                   enddo
                endif
          enddo         !ifix=1,nfixec50
****** c:\fort90\hjcfit\old\hjcfit.for
     & '  EC50 for ',a10,' = ',g16.6,' micromolar,',/,
     & ' using penalty factor = ',g13.6,' if needed',/)
          enddo         !ifix=1,nfixec50
******

****** HJCFIT.FOR
           npad=npad-4          !for nmr
           npad=npad-4          !for nlig
           npad=npad-2*4*nlig   !for conc_ec1,conc_ec2
           if(npad.lt.0) then
****** c:\fort90\hjcfit\old\hjcfit.for
           npad=npad-4          !for nmr
           if(npad.lt.0) then
******

****** HJCFIT.FOR
     & nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     & useprim,(isetmr(i),i=1,ncyc),ratemax,nmr,nlig,
     & (conc_ec1(i),i=1,nlig),
     & (conc_ec2(i),i=1,nlig),
     & inipad
****** c:\fort90\hjcfit\old\hjcfit.for
     & nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     & useprim,(isetmr(i),i=1,ncyc),ratemax,nmr,
     & inipad
******

****** HJCFIT.FOR
     &   kAm,kFm,km)
c========leave prtec50=true====temp=======
           prtec50=.false.
****** c:\fort90\hjcfit\old\hjcfit.for
     &   kAm,kFm,km)
           prtec50=.false.
******

****** HJCFIT.FOR
     &' micromolar')
                if(nlig.gt.1) then
                   if(discprt) write(8,261)
c261               format(
c     & '    Concentration of other ligands for EC50 determination')
                   do i=1,nlig
                        if(i.ne.n) then
                           if(ifix.eq.1) then
                                x=1.e6*conc_ec1(i)
                           else if(ifix.eq.2) then
                                x=1.e6*conc_ec2(i)
                           endif
                           if(discprt) write(8,325) ligname(i)(1:10),x
c325                       format(
c     & '  Conc of ',a10,' at which EC50 was determined (micromolar) [',
c     & f9.4,'] = ')
                        endif
                   enddo
                endif
           enddo                !ifix=1,nfixec50
****** c:\fort90\hjcfit\old\hjcfit.for
     &' micromolar')
           enddo                !ifix=1,nfixec50
******

****** HJCFIT.FOR
c
c Copy conc(i,j) to conc_ec(i,j) so former stays unchanged as conc used
c for set #j
        do i=1,10
           do j=1,10
                conc_ec(i,j)=conc(i,j)
           enddo
        enddo
        do ifix=1,nfixec50
           do i=1,nlig
                concsav(i)=conc_ec(i,1)
           enddo
           if(ifix.eq.1) then
                nlv=nlvar
                do i=1,nlig
                   conc_ec(i,1)=conc_ec1(i)     !conc of other ligands at which ec50 determined
                enddo
           else if(ifix.eq.2) then
                nlv=nlvar2
                do i=1,nlig
                   conc_ec(i,1)=conc_ec2(i)     !conc of other ligands at which ec50 determined
                enddo
****** c:\fort90\hjcfit\old\hjcfit.for
c
        do ifix=1,nfixec50
           if(ifix.eq.1) then
            nlv=nlvar
           else if(ifix.eq.2) then
            nlv=nlvar2
           endif
           print 142,nlv,ligname(nlv)
         if(discprt) write(8,142) nlv,ligname(nlv)
c142       format(/,' EC50 calculations for final parameter values',/,
c     &  ' Equilibrium conc-response curve for ligand # ',i2,' = ',a10)
c Conc of other ligands is as in set 1 at the moment -could make loop for nset
           if(nlig.gt.1) then
                do j=1,nlig
                   if(j.ne.nlv) then
                        print 143,j,ligname(j),conc(j,1)*1.e6
                        if(discprt) write(8,143) j,ligname(j),conc(j,1)*1.e6
c143                    format('  (conc of ligand #',i2,',  ',a10,' = ',g13.6,
c     &                 ' micromolar)')
                   endif
                enddo
******

****** HJCFIT.FOR
           endif
           print 142,nlv,ligname(nlv)
         if(discprt) write(8,142) nlv,ligname(nlv)
c142       format(/,' EC50 calculations for final parameter values',/,
c     &  ' Equilibrium conc-response curve for ligand # ',i2,' = ',a10)
cc Conc of other ligands is as in set 1 at the moment -could make loop for nset
c Conc of other ligands for determination of ec50 now in conc_ec1, conc_ec2
           if(nlig.gt.1) then
                do j=1,nlig
                   if(j.ne.nlv) then
                        print 143,j,ligname(j),conc_ec(j,1)*1.e6
                        if(discprt) write(8,143)j,ligname(j),conc_ec(j,1)*1.e6
c143                    format('  (conc of ligand #',i2,',  ',a10,' = ',g13.6,
c     &                 ' micromolar)')
                   endif
                enddo
           endif
           kdim=100
****** c:\fort90\hjcfit\old\hjcfit.for
           endif
           kdim=100
******

****** HJCFIT.FOR
                call EC50_HJC(EC50out,curinfs,curmax,concmax,cur0,pop0,
     &    pmax,nlv,QT,conc_ec,k,kdim,nerr)
           else
****** c:\fort90\hjcfit\old\hjcfit.for
                call EC50_HJC(EC50out,curinfs,curmax,concmax,cur0,pop0,
     &    pmax,nlv,QT,conc,k,kdim,nerr)
           else
******

****** HJCFIT.FOR
                call EC50_HJC(EC50out,curinfs,curmax,concmax,cur0,pop0,
     &       pmax,nlv,QT,conc_ec,k,kdim,nerr)
           endif
****** c:\fort90\hjcfit\old\hjcfit.for
                call EC50_HJC(EC50out,curinfs,curmax,concmax,cur0,pop0,
     &       pmax,nlv,QT,conc,k,kdim,nerr)
           endif
******

****** HJCFIT.FOR
           if(ifix.eq.2) ec50out2=ec50out       !for printing below
           do i=1,nlig
              conc_ec(i,1)=concsav(i)
           enddo
        enddo   !do ifix=1,nfixec50
c
****** c:\fort90\hjcfit\old\hjcfit.for
           if(ifix.eq.2) ec50out2=ec50out       !for printing below
        enddo   !do ifix=1,nfixec50
cc      else
c===fix nlvar
c          call EC50_HJC(EC50out,curinf,curmax,concmax,cur0,pop0,
c     &  pmax,nlvar,QT,conc,k,kdim,nerr)
c          if(nerr.ne.0) then
c               call BELL(1)
c               print 590,nerr
c               if(discprt) write(8,590) nerr
cc590           format('  Error in bisection for EC50: nerr = ',i2)
c          endif
cc      endif
c       if(monot) then
c         print 581,curinf,ec50out*1.e6
c         if(discprt) write(8,581) curinf,ec50out*1.e6
c581       format(/,
c     & ' Equilibrium response-concentration curve is monotonic',/,
c     & ' Maximum response (pA) = ',g11.4,/,
c     & '    EC50: conc for 50% of this equilib. current (muM) = ',
c     &   g11.4,/)
c       else
c         print 582,curmax,concmax*1.e6,ec50out*1.e6,curinf
c         if(discprt) write(8,582)curmax,concmax*1.e6,ec50out*1.e6,curinf
c582       format(/,
c     &    ' Equilibrium response-concentration curve has maximum.',/,
c     &    '   Max equilib response = ',g12.5,' pA at ',g12.5,' muM',/,
c     &    '   EC50 = conc for 50% of this max. (muM) (left of max) = ',
c     &     g12.5,/,
c     &    '   Response at conc -> infinity = ',g12.5,' pA',/)
c       endif
cc and same for peak d/r curve
c       call PEAKEC50(EC50,curinf,monot,curmax,concmax,QT,
c     &         p0,env,gamma,plotcur,debug(),k,kqd)
c       if(monot) then
c         print 593,curinf,ec50*1.e6
c         if(discprt) write(8,593) curinf,ec50*1.e6
c593       format(/,
c     & ' Peak response-concentration curve is monotonic',/,
c     & ' Maximum response (pA) = ',g11.4,/,
c     & '    Conc for 50% of this maximum current = ',g11.4,' (muM)',/)
c       else
c         print 594,curmax,concmax*1.e6,ec50*1.e6,curinf
c         if(discprt) write(8,594)curmax,concmax*1.e6,ec50*1.e6,curinf
c594       format(/,
c     &    ' Peak response-concentration curve has maximum.',/,
c     &    '   Max peak response = ',g12.5,' pA at ',g12.5,' muM',/,
c     &    '   Conc for 50% of this max. current (left of max) = ',
c     &     g12.5,' muM',/,
c     &    '   Response at conc -> infinity = ',g12.5,' pA',/)
c       endif
c
******

****** HJCFIT.FOR
           npad=npad-4          !for nmr
           npad=npad-4          !for nlig
           npad=npad-2*4*nlig   !for conc_ec1,conc_ec2
           if(npad.lt.0) then
****** c:\fort90\hjcfit\old\hjcfit.for
           npad=npad-4          !for nmr
           if(npad.lt.0) then
******

****** HJCFIT.FOR
     & nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     & useprim,(isetmr(i),i=1,ncyc),ratemax,nmr,nlig,
     & (conc_ec1(i),i=1,nlig),
     & (conc_ec2(i),i=1,nlig),
     & inipad
****** c:\fort90\hjcfit\old\hjcfit.for
     & nfixec50,ec502,i502,j502,m502,xqlo2,xqhi2,
     & useprim,(isetmr(i),i=1,ncyc),ratemax,nmr,
     & inipad
******


