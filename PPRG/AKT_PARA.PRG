	PARAMETERS _x_zdr,_x_cil,_x_den
	CLOSE DATA
	RELEASE ALL EXCEPT _x_*
	SET ESCA ON
	DO setup
	SET LIBR TO ontimer ADDI
	PUSH MENU _msysmenu
	SYSM=SET('sysmenu')
	SET TYPEAHEAD TO 0
	SET SYSMENU TO
	SET EXCL OFF
	SET ODOM TO 100
	SET AUTOSAVE ON
	SET CLOCK TO 0,0
	SET CENTURY OFF
	SET HELP OFF
	SET PROC TO akt_dbf
	IF SYS(5)='K:'
		m.ladeni=.T.
		ENDIF
	_onerr=IIF(m.ladeni,"",m._onerr)
	ON ERROR &_onerr
	SELE 0
	USE setekont
	_pp=AFIELDS(p_setdbf)
	FOR i=1 TO _pp
		A=p_setdbf(i,1)
		&a=setekont.&a
		IF TYPE(A)='C'
			&a=RTRIM(&a)
			ENDIF
		ENDFOR
	USE
	RELE p_setdbf
	DO setekont
		**	Screen SAVER
	 	=on_night(90,' ๙*',.F.,.T.,1,0)
	
**********************************
****	D…leงit glob lnก promุnn
	
	PUBLIC esc, APP, ok, tskf, fl_open,refscr,fl_modi,gl_data,;
			username,dir_s(5),dir_t(5),err,fil_s,fil_t
	ok=1
	esc=.F.
	app=.F.
	fl_open=.T.
	refscr = .F.
	toexit = .F.
	fl_modi = .F.
	gl_data = ""
	err=0
	fil_s = ""
	fil_t = ""
	
	
	_x_den = val(_x_den)
		
	_onerr=IIF(m.ladeni,"",m._onerr)
	gglm=10
	username=net("whoami")
	IF EMPTY(m.username)
		username=getenv("USERNAME")
		ENDIF
	IF EMPTY(m.username)
		username=SPACE(10)
		ENDIF
	

	
	_x_cil = alltrim(upper(_x_cil))
	_x_cil = iif(right(_x_cil,1) = "\",_x_cil,_x_cil+"\")
	_x_zdr = alltrim(upper(_x_zdr))
	_x_zdr = iif(right(_x_zdr,1) = "\",_x_zdr,_x_zdr+"\")

	
*	IF file(_x_zdr+"c_day.mem")
*		RESTORE FROM (_x_zdr+"c_day.mem") ADDITIVE
*	  ELSE
*	  ppden = 0
*	ENDIF
*
*ษอออออออออออออออออออออออออออออออออป
*บ Kopie zdroj - "aktual"          บ
*บ Kopie DATA  - "nove" (poet dnก)บ							
*ศอออออออออออออออออออออออออออออออออผ

	IF  !(akt_dbf (1,m._x_zdr,m._x_cil+"aktual",0) AND ;
			 akt_dbf(1,m._x_cil,m._x_cil+"nove",m._x_den))	
		WAIT WIND "Adres ý pro aktualizaci nelze vytvoýit, program bude ukonen!"	
		ENDIF

*ษอออออออออออออออออออออออป
*บ  Vlastnก aktualizace  บ
*ศอออออออออออออออออออออออผ



		DO akt_dbf WITH 3,m._x_cil+"aktual",m._x_cil+"nove"		&& aktualizace
		DO akt_dbf WITH 3,m._x_cil+"nove",m._x_cil+"aktual"
	    =akt_dbf(1,m._x_cil+"nove",m._x_zdr,0)                 && aktualni zpet na medium
		
* zde zaradit PACK hlavni databaze do ktere se v nasledujicim kroku 
* aktalizuje puvodne vybranych 14 dni		
		DO akt_dbf WITH 3,m._x_cil+"nove",m._x_cil+"" 			       && aktualni do velke dbf
		DO akt_dbf WITH 5,m._x_cil+"aktual",m._x_cil+"nove"
	
	
	
	RELEASE LIBRARY ontimer
	SET PRINT TO
	SET CLOCK TO 0,SCOLS()-8
	SET PROCEDURE TO
	REST MACROS
	SET FUNC 4 TO "MODI COMM alt_t;"
	SET FUNC 10 TO "quit;"
	=zaver('i','i')
	ON ERROR
	SET HELP ON
	SET TALK ON
	ON KEY LABEL alt+C
	POP MENU _msysmenu
	SET SYSMENU &sysm
	SET SYSMENU ON
	SET ESCA ON
	QUIT
