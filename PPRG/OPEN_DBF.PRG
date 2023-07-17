********************************************
****			VERZE 1.10				****
********************************************
****	Vçeobecn‚ otev¡r n¡ datab ze	****
**	1 - Cesta k b zi i s '\'
**	2 - N zev datab ze bez cesty
**	3 - N zev rozçiýuj¡c¡ funkce
**		¬.funkce -  1. Struktura datab ze
**					2. Indexovac¡ pý¡kazy
**					3. Podm¡nka urŸuj¡c¡ modifikaci struktury
**					4. Funkce spuçtØn  pýi modifikaci struktury
**		Jm‚no vytv ýen‚ datab ze k funkci 1
**	4 - Index dle kter‚ho se m  nastavit tý¡dØn¡
**	5 - .T. = Exclusive
**	6 - .T. = Reindex
**	7 - Alias datab ze

* PROCEDURE open_dbf
PARA _pt, _nb, _fnc, _ord, _exc, _rei, _ali

*WAIT WIND "Otevírám: "+m._nb

PRIV _flg, _nerr
	* Pý¡prava makra pro otev¡r n¡ datab ze s Alias
	IF !empty(m._ali)
		_alim=" ALIAS "+m._ali
	  ELSE
		_ali=m._nb
		_alim=" ALIAS "+m._nb
		ENDIF
	SELE 0
	IF !file(m._pt+m._nb+".dbf")
		DO &_fnc WITH 1, m._pt+m._nb
		ENDIF
	_nerr=0
	ON ERROR _nerr=error()
	IF m._exc
		USE (m._pt+m._nb) EXCLUSIVE &_alim
	  ELSE
		USE (m._pt+m._nb) &_alim
		ENDIF
	ON ERROR &_onerr

	IF m._nerr=108 OR m._nerr=1705
		WAIT WIND "Nelze vìluŸnØ otevý¡t datab ze. Ji§ jsou pou§¡v ny jinìm u§ivatelem."
		RETURN .F.
		ENDIF

	IF m._nerr=1707
		_nerr=0
		ON ERROR _nerr=error()
		IF m._exc
			USE (m._pt+m._nb) EXCLUSIVE &_alim
		  ELSE
			USE (m._pt+m._nb) &_alim
			ENDIF
		ENDIF
	ON ERROR &_onerr

	IF m._nerr=108 OR m._nerr=1705
		WAIT WIND "Nelze vìluŸnØ otevý¡t datab ze. Ji§ jsou pou§¡v ny jinìm u§ivatelem."
		RETURN .F.
		ENDIF
	
	IF m._nerr>0
		WAIT WIND "Pýi otev¡r n¡ datab z¡ vznikla chyba Ÿ.: "+strn(m._nerr)
		fl_open=.F.
		RETURN TO MASTER
		ENDIF
	_nerr=0

	IF &_fnc(3)
*    	WAIT WIND NOWA "Prov d¡m aktualizace struktury datab ze: "+m._nb
		SELE 0
		PRIV aa
		aa="_"+left(sys(3),7)
		DO &_fnc WITH 1, m.aa
		DO app_data WITH m._ali, m._pt+m._nb, m.aa
		IF m._exc
			ON ERROR USE (m._pt+m._nb) EXCLUSIVE &_alim
			USE (m._pt+m._nb) EXCLUSIVE &_alim
		  ELSE
			ON ERROR USE (m._pt+m._nb) &_alim
			USE (m._pt+m._nb) &_alim
			ENDIF
		ON ERROR &_onerr
		DO &_fnc WITH 4
		ENDIF
	IF !file(m._pt+m._nb+".cdx") OR m._rei
		IF m._exc
			ON ERROR _nerr=error()
			DELE TAG ALL
			ON ERROR &_onerr
			ENDIF
		SET TALK ON
		DO &_fnc WITH 2
		SET TALK OFF
		ENDIF
	SET ORDER TO &_ord
RETURN

**	Zkop¡rov n¡ obsahu star‚ datab ze do datab ze nov‚
**	vŸetnØ smazanìch z znam….
**	_sele - Alias zdrojov‚ datab ze,  _file - Cesta k zdroj. datab zi
PROCEDURE app_data
PARA _sele, _file, _prcdata
	PRIV _del
	SELE (m._sele)
	SET DELE OFF
	SCAN ALL
		SCATTER MEMVAR MEMO
		INSERT INTO (m._prcdata) FROM MEMVAR
		IF dele()
			SELE (m._prcdata)
			DELE
			SELE (m._sele)
			ENDIF
		ENDSCAN
	USE
	IF file(m._file+".BAK")
		DELE FILE (m._file+".BAK")
		ENDIF
	RENAME (m._file+".DBF") TO (m._file+".BAK")
	IF file(m._file+".CDX")
		DELE FILE (m._file+".CDX")
		ENDIF
	IF file(m._file+".FPT")
		DELE FILE (m._file+".FPT")
		ENDIF
	SELE (m._prcdata)
	USE
	RENAME (m._prcdata+".DBF") TO (m._file+".DBF")
	IF file(m._prcdata+".FPT")
		RENAME (m._prcdata+".FPT") TO (m._file+".FPT")
		ENDIF
	SELE 0
	SET DELE ON
RETURN
