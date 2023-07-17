*******************************
**	Nastaven¡ tiskov‚ho vìstupu
*PROC tisk_out
PARA edt,nzp
	PUSH KEY CLEAR
	DO WHILE !set_chprn(edt)
		IF ask(-1,-1,"A","Souhlas¡ç s pýesmØrov n¡m tisku")
			EXIT
			ENDIF
		ENDDO
	POP KEY
RETURN

**	Nastaven¡ ý¡d¡c¡ch sekvenc¡ tisk rny
PROCEDURE set_chprn
PARA edt
	IF type('len_pg')='U'
		PUBLIC len_pg, margin
		ENDIF
	PUBLIC cnon, cnoff, ecnon, ecnoff, pica, elit, nlq,;
		draft, dblon, dbloff, resetprn, prekodtisk
	prekodtisk="ASCII"
	aa='@M '
	io='LPT1'+space(18)
	IF !empty(m.lpt1) AND m.lpt1<>'-'
		a='LPT1 - '+m.lpt1
		aa=aa+a+','
		IF m.imp_output='LPT1'
			io=a
			ENDIF
		ENDIF
	IF !empty(m.lpt2) AND m.lpt2<>'-'
		a='LPT2 - '+m.lpt2
		aa=aa+a+','
		IF m.imp_output='LPT2'
			io=a
			ENDIF
		ENDIF
	IF !empty(m.lpt3) AND m.lpt3<>'-'
		a='LPT3 - '+m.lpt3
		aa=aa+a+','
		IF m.imp_output='LPT3'
			io=a
			ENDIF
		ENDIF
	a='EXPORT pro windows'
	aa=aa+a
	IF m.imp_output='EXPO'
		io=a
		ENDIF

	IF len(aa)<4
		WAIT WIND "Nen¡ nastavena § dn  definice tisk rny !"
		WAIT WIND "Tisk je pýesmØrov n do souboru 'TISK.BAK'"
		SET PRINTER TO tisk.bak ADDI
		RETURN .T.
		ENDIF
	io=left(m.io+space(22),22)

*--------------------------------------
* Dotaz na jednu polo§ku
	IF m.edt
		DEFINE WINDOW w_tisk ;
			FROM INT((SROW()-3)/2),INT((SCOL()-44)/2) ;
			TO INT((SROW()-3)/2)+2,INT((SCOL()-44)/2)+43 ;
			NOFLOAT NOCLOSE SHADOW COLOR SCHEME 5
		ACTIVATE WINDOW w_tisk SAME
		@ 0,1 SAY "Tiskov‚ zaý¡zen¡:"
		@ 0,19 GET m.io SIZE 1,22 COLOR SCHEME 4 PICT '&aa'
		READ MODAL
		RELEASE WINDOW w_tisk
		ENDIF
*--------------------------------------
	imp_output=left(m.io,4)
	PRIV tptisk
	tptisk=m.&imp_output
	_sel=sele()
	SELE 0
	USE setprint
	LOCA ALL FOR rtrim(tptisk)+'<' = rtrim(setprint.naz)+'<'

	IF !found()
		WAIT WIND "Nenalezena definice tisk rny: "+tptisk
		WAIT WIND "Tisk je pýesmØrov n do souboru 'Tisk.Bak'"
		SET PRINTER TO tisk.bak ADDI
		USE
		SELE (_sel)
		RETURN IIF(m.edt,.F.,.T.)
		ENDIF

	len_pg=setprint.len_pg
	margin=setprint.margin
	prekodtisk=setprint.prekodtisk
	IF !nzp
*		PUBLIC cnon, cnoff, ecnon, ecnoff, pica, elit, nlq,;
			draft, dblon, dbloff, resetprn
		cnon  =  ""
		cnoff =  ""  
		ecnon =	 ""
		ecnoff = ""
		pica =	 ""
		elit =   ""
		nlq  =   ""
		draft =	 ""
		dblon  = ""
		dbloff =  ""
		resetprn = ""
	  ELSE
		resetprn=bintoc(setprint.resetprn)
		cnon=bintoc(setprint.cnon)
		cnoff=bintoc(setprint.cnoff)
		ecnon=bintoc(setprint.ecnon)
		ecnoff=bintoc(setprint.ecnoff)
		pica=bintoc(setprint.pica)
		elit=bintoc(setprint.elit)
		nlq=bintoc(setprint.nlq)
		draft=bintoc(setprint.draft)
		dblon=bintoc(setprint.dblon)
		dbloff=bintoc(setprint.dbloff)
		_PSCODE=m.resetprn
*		_PECODE=m.resetprn
		ENDIF

	IF m.imp_output!="EXPO"
		SET PRINTER TO &imp_output
	  ELSE
*		WAIT WIND NOWA "Tisk je pýesmØrov n do souboru 'Export.Txt'"
		SET PRINTER TO &ggf.Export.Txt
		ENDIF
	USE
	SELE (_sel)
RETURN .T.

**	Rozebere ýetØzec ud vaj¡c¡ k¢dy jednotlivìch znak…
PROC bintoc
PARA _tx
	PRIV _rt
	_rt=''
	_tx=alltrim(_tx)+','
	a=at(',',_tx)
	DO WHILE a>1
		_rt=_rt+chr(val(left(_tx,a-1)))
		_tx=substr(_tx,a+1)
		a=at(',',_tx)
		ENDDO
RETURN _rt
