
***
* Parametry : _act "O" - naŸten¡ memo pol¡ z aktualn¡ datab ze
*			  _act "E" - editovani memo pole - pýi zmØnØ vrac¡ .T.
*			  _act "C" - zaps n¡ zmØn do p…vodn¡ datab ze (pokud se
*						 nebudou zmØny ukl dat (jaky to ma smysl ?)
*			             mus¡ se zavý¡t doŸasn  dbf (viz. _cursor)		
*			  _cursor  - jm‚no pro doŸasnou datab zi. Stejn‚ jm‚no je
*						 týeba uv dØt jako alias u zobrazovan‚ho memo pole
* 			  _obje    - jm‚no objektu k editaci
*  			   m_o      - .T. zak §e editovat MEMO
*			   mm_app    - .T. vlo§en¡ pr zdn‚ho MEMO
*			  mess		- zpr va na spodn¡ ý dek (ý dek ze 'set mess';centrovanØ) 	
*						  za znakem '|' vlo§it Ÿ¡slo barevn‚ho ch‚matu
* !! BDF urŸen  k editaci mus¡ bìt pýi parametru 'O' aktu ln¡ !!


PARAMETERS _act,_cursor,_obje,m_o,mm_app,mess	
PRIV _ve,_ord,_se,_ali

_ve  = RECNO()
_ord = ORDER()
_se  = SELE()
_ali = ALIAS(_se)
IF type("mess")="L"
	mess=""
 	barva=10
  ELSE
  	x=rat("|",mess)
  	barva=right(m.mess,len(m.mess)-x)
  	barva=val(m.barva)
  	mess = left(mess,x-1)
	ENDIF	



RETURN MMED_&_act.()


PROCEDURE MMED_O

*********
* Zjiçœuje memo pole ze struktury aktu ln¡ dbf
* Strukturu zjiçtØnìch memo pol¡ ukl d  do matice '_curs'

	=afields(hcurs)
	x = 1
	FOR I = 1 TO alen(hcurs,1)
		IF hcurs(I,2) = "M"
			DIME _curs(x,2)
			_curs(X,1) = hcurs(I,1)
			_curs(X,2) = hcurs(I,2)				
			x=x+1
			ENDIF
		ENDFOR

	
	CREATE CURSOR &_cursor FROM ARRAY _curs
	APPEND BLANK
	FOR I = 1 TO ALEN(_curs,1)
		nove = _ali+"."+_curs(I,1)
		IF ! mm_app
			REPLACE (_curs(I,1)) WITH &nove
	    	ENDIF
	    ENDFOR
SELECT (_se)
RETURN



PROCEDURE MMED_E
	IF set("disp")  = "VGA50"
	 	DEFINE WIND w_memo FROM 5,2 TO 40,75 TITLE '' DOUBLE SHADOW COLOR SCHEME 13	
	  ELSE
	  	DEFINE WIND w_memo FROM 5,2 TO 20,75 TITLE '' DOUBLE SHADOW COLOR SCHEME 13
		ENDIF
	
	c_obj = _cursor+"."+_obje
	a_obj = _ali+"."+_obje
	IF type("m_o") = 'L'.AND. m_o = .T.
 		ACTIVATE SCREEN
 		@ SET("mess"),(scol()-len(m.mess))/2 say m.mess color scheme m.barva
 		MODIFY MEMO &c_obj WIND w_memo NOEDIT
		wokno=wontop()
		ACTIVATE SCREEN
		@ SET("mess"),0 say repl(" ",scol())
		ACTIVATE WINDOW (wokno)
	  ELSE
 		ACTIVATE SCREEN
 		@ SET("mess"),(scol()-len(m.mess))/2 say m.mess color scheme m.barva
		MODIFY MEMO &c_obj WIND w_memo
		wokno=wontop()
 		ACTIVATE SCREEN
		@ SET("mess"),0 say repl(" ",scol())
		ACTIVATE WINDOW (wokno)
		ENDIF
	RELEASE WIND w_memo
	
	
	_exa = SET("EXACT")
	SET EXACT ON
	IF len(c_obj) > 250
		RETURN porov()
		ENDIF
	IF &c_obj == &a_obj
		RETURN .F.
	  ELSE
		RETURN .T.
		ENDIF
	SET EXACT &_exa
	RELEASE WIND w_memo
RETURN 


PROCEDURE MMED_C

	SELE  (_cursor)
	=AFIELDS(hcurs)
	FOR i = 1 TO ALEN(hcurs,1)
		a_obj = _ali+"."+hcurs(i,1)
		c_obj = _cursor+"."+hcurs(i,1)
		REPLACE &a_obj WITH &c_obj
	   	ENDFOR
	USE
	SELECT (_se)
RETURN


PROCEDURE porov
	SET EXACT ON
	FOR i = 1 to len(c_obj)
		_s = substr(c_obj,i,1)
		_t = substr(a_obj,i,1)
		IF ! _s==_t
			RETURN .F.
			ENDIF
		RETURN .T.
	ENDFOR
RETURN


