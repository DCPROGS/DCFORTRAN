	subroutine butonoff(ix_valve,iy_valve,
     &    iv_length,iv_width,iv_heigth,iv_color,
     &    ixc,iyc,irc,icb,ic_on,ic_seq,ibox_color,ic_color,ivalve,
     &    ix_valvep,iy_valvep,ixcp,iycp,ivalvep,ixw,iyw,nv)
c==============================================================================
	character	cnum*11

	    call fillcircle(ixcp,iycp,irc,icb,ic_seq)
	    call draw_valve(ix_valvep,iy_valvep,iv_length,iv_heigth,
     &    iv_width,
     &    iv_color,ibox_color,icc)
	    call intconv(ivalvep,cnum)
	    call wrstring18(ix_valvep+iv_length/2-8,
     &    iy_valvep+iv_heigth-16,
     &    cnum,ic_color,iv_color)


	if(ivalve.eq.nv) goto 99
	    call intconv(ivalve,cnum)
	    call wrstring18(ixw,iyw,cnum,ic_on,
     &    ic_color)
	    ixw=ixw+24

	    call fillwin(ix_valve,iy_valve,ix_valve+iv_length+
     &    int(iv_width/2),iy_valve+iv_heigth+int(iv_width/2),
     &    iv_color)
	    call drawframe(ix_valve+int(iv_width/2),iy_valve+
     &    int(iv_width/2)
     &    ,ix_valve+iv_length+int(iv_width/2),
     &    iy_valve+iv_heigth+int(iv_width/2),
     &    ibox_color)
	    call fillcircle(ixc,iyc,irc,icb,ic_on)
	    call wrstring18(ix_valve+iv_length/2+8,
     &    iy_valve+iv_heigth,
     &    cnum,ic_color,iv_color)


99	end
