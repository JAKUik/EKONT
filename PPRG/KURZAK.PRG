********************************
**	Prohl¡§en¡ kurzovn¡ho l¡stku
*PROCEDURE kurzak
PARA _dd
	quiting=.F.
	IF type('m.path_kurzy')='U'
		quiting=.T.
		m.path_kurzy=''
		ENDIF
	IF type('m._dd')='D'
		dtkurz=m._dd
	  ELSE
		dtkurz=gomonth(date(),-1)
		ENDIF
	PRIV _l
	_l=36
	DEFINE WINDOW w_ask ;
		FROM INT((SROW()-3)/2),INT((SCOL()-(m._l-1))/2) ;
		TO INT((SROW()-3)/2)+2,INT((SCOL()-(m._l-1))/2)+m._l ;
		NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
	ACTIVATE WINDOW w_ask SAME
	@ 0,1 SAY "Datum kurzovn¡ho l¡stku:"
	@ 0,26 GET m.dtkurz SIZE 1,8 COLOR SCHEME 4
	READ MODAL
	RELEASE WINDOW w_ask
	IF lastkey()=27
		RETURN
		ENDIF
	aa=rtrim(m.path_kurzy)+"HK"+right(dtos(m.dtkurz),6)+".DOC"
	IF file(m.aa)
		WAIT WIND NOWA m.aa
		=viewfile(m.aa)
	  ELSE
		WAIT WIND NOWA "Nenalezen soubor kurzovn¡ho l¡stku: "+upper(m.aa)
		ENDIF
IF m.quiting
	QUIT
  ELSE
	RETURN
	ENDIF
