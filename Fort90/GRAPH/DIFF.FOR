Comparing files getcons.for and F:\fort90\graph\getcons.for
****** getcons.for
c
        logical good
c
        common/dp/discprt
****** F:\fort90\graph\getcons.for
c
        common/dp/discprt
******

****** getcons.for
              if(axfile) then
                   call READAXON(adcfil,title,cdate1,adctim1,ilen,inchan,
     &            calfac,pAV,srate,ffilt,iver,ioff,good)
****** F:\fort90\graph\getcons.for
              if(axfile) then
                   call READAXON(adcfil,title,cdate1,adctime1,ilen,inchan,
     &            calfac,pAV,srate,ffilt,iver,ioff,good)
******


