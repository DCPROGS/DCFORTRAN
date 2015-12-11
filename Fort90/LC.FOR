	character function LC(ch)
c Converts argument to lower case
	character*1 ch
c
      if((ch.ge.'A').and.(ch.le.'Z')) ch = char(ichar(ch)+32)
	LC=ch
	return
	end

