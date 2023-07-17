**	_ukaz=.T.	- Mo§nost opravovat i I¬O a DI¬
PARA _ukaz
IF type('_ukaz') = "C"
	IF upper(alltrim(_ukaz)) = ".T."
		_ukaz = .T.
	  else
	    _ukaz = .F.
	  	ENDIF
    ENDIF
CLOSE ALL
*RELEASE ALL EXCEPT cesta*
SET ESCA ON
DO setup
SET LIBR TO ontimer ADDI
PUSH MENU _msysmenu
SYSM=SET('sysmenu')
SET SYSMENU TO
SET EXCL OFF
SET ODOM TO 100
SET AUTOSAVE ON
SET CLOCK TO 0,0
SET CENTURY ON
SET HELP OFF
IF SYS(5)='J:'
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
=on_night(90,' ù*',.F.,.T.,1,0)

**********************************
****	D…le§it‚ glob ln¡ promØnn‚
PRIV username
gglm=10
gl_usernam=net("whoami")
IF EMPTY(m.gl_usernam)
	gl_usernam=SPACE(10)
	ENDIF

******************
**	PromØnn‚
PUBLIC esc, APP, ok, tskf, fl_open
ok=1
esc=.F.
APP=.F.
fl_open=.T.
*	Vytvoýen¡ n hodn‚ho jm‚na tiskov‚ho souboru
tskf=SYS(3)
tskf=CHR(ASC(m.tskf)+17)+SUBSTR(m.tskf,2)+".prt"

**	ALT+F9 - Nastaven¡ tiskov‚ho vìstupu
DO tisk_out WITH .F.

ON KEY LABEL alt+f9 DO tisk_out WITH .T.

**	Definice pro vçechny LIST_C
kalk_vysl='0'
PUBLIC on_list_c, on_list_next
on_list_next=5
DIME on_list_c(m.on_list_next)
on_list_c( 1)='ALT+F9:		DO tisk_out WITH .T.'
*	'	## ALT+F9-Nastaven¡ tiskov‚ho vìstupu'
IF EMPTY(m.gl_usernam)
	on_list_c( 2)='Ctrl+Tab: DO kalkul'
	ON KEY LABEL ctrl+TAB DO kalkul
ELSE
	on_list_c( 2)="Ctrl+Tab: DO kalkul WITH left(m.gl_usernam,8)+'.cal',.F."
	ON KEY LABEL ctrl+TAB DO kalkul WITH LEFT(m.gl_usernam,8)+'.cal', .F.
	ENDIF
*	'	## CTRL+Tab-KalkulaŸka'
on_list_c( 3)='#SET=K-W-'
on_list_c( 4)='#P=preftisk.lcp'

DO tel_sez WITH m._ukaz

RELEASE LIBRARY ontimer
SET PRINT TO
SET CLOCK TO 0,SCOLS()-8
SET PROCEDURE TO
REST MACROS
SET FUNC 4 TO "MODI COMM alt_t;"
SET FUNC 10 TO "quit;"
*=zaver('i','i')
ON ERROR
SET HELP ON
SET TALK ON
ON KEY LABEL alt+C
POP MENU _msysmenu
POP KEY
SET SYSMENU &sysm
SET SYSMENU ON
SET ESCA ON
IF !m.ladeni
	QUIT
	ENDIF
RETURN

******************************************************************
******************************************************************
**	_ukaz	- .T. = Mo§nost z sahu do dat
PROCEDURE tel_sez
PARA _ukaz

	PRIV cesta, _sel
	cesta=""
	=ini("telefony.eni")

	_ukaz = !m._ukaz
	_whe = fullpath("telefony.fxp")
	PUSH KEY CLEAR
	SET COMP ON
	IF EMPTY(m.cesta)
		WAIT WIND "Nebylo zad no jm‚no seznamu!"
		RETURN
		ENDIF

	IF  "." $ m.cesta
		m.cesta = left(m.cesta,len(m.cesta)-4)
		ENDIF

	IF !FILE(m.cesta+".DBF")
		WAIT WIND "Datab ze telefonn¡ho seznamu nenalezena." TIME 2
		POP KEY
		RETURN
		ENDIF
	SET COMP OFF
	_sel=SELE()
	SELE 0
	USE (m.cesta) AGAIN
	SET ORDER TO naz

	IF type("m.gl_usernam") != "C" 
		gl_usernam=net("whoami")
		ENDIF

**	Okno pro MEMO
	DEFINE WIND w_memo FROM 5,2 TO 20,75 DOUBLE SHADOW COLOR SCHEME 13
	DO list_c WITH -1,-1,15,0,;
		"Telefonn¡ seznam ",;
		[:left(naz,20)+'³ '+left(jme,19)+' ³ '+tel  ],;
		"//   N zev firmy      ³   Jm‚no osoby       ³    Tel.                    ",;
		[:left(naz,20)+'³ '+left(jme,19)+' ³ '+fax  ],;
		"//   N zev firmy      ³   Jm‚no osoby       ³    Fax                     ",;
		[:left(jme,19)+' ³ '+uli+' ³ '+mes+'³'+psc  ],;
		"//  Jm‚no osoby       ³ Ulice                     ³    MØsto       ³ PsŸ         ",;
		"\   F1-Help     ENTER-Oprava     INS-Vlo§it     F7-Tý¡dit podle ... ",;
		'{}',;
		'CR#:	DO tel_edit WITH .F.	## ENTER-Prohl¡§en¡/oprava £daje',;
		'INS#:	DO tel_edit WITH .T.    ## INSERT-Novì z znam',;
		'F5#:	DO sez_obal_a			## F5-Tisk ob lky - adres t',;
		'F6#:	DO sez_obal_o			## F6-Tisk ob lky - odes¡latel',;
		'F2#:	DO tel_zobr 		    ## F2-Zobrazen¡ vyýazenìch klient…',;
		'ALT+T:'
	*n zev:naz,jme,tel,fax,uli,mes

	USE
	SELE (_sel)
	IF file("h:\station\"+getenv("NSTATION")+"\start-fp.fxp")	
		DO ("h:\station\"+getenv("NSTATION")+"\start-fp")	
		ENDIF
	POP KEY
RETURN

PROCEDURE tel_edit
PARA APP
	_obr = left(_whe,rat("\",_whe))+"seznam.spr"
	* _ukaz = .T.
	DO &_obr
	refscr=.T.
	* _ukaz = .F.
RETURN


**	Zobrazen¡ i vyýazenìch klient…
PROCEDURE tel_zobr
	IF EMPTY(SET('filter'))
		__fltr="del"
	  ELSE
		__fltr=""
		ENDIF
	SET FILT TO &__fltr
	refscr=.T.
RETURN

**	Tisk ob lek
PROCEDURE sez_obal_a
	IF ask(-1,-1,"A","Tisk na malou ob lku?")
		sp=SPACE(24)
	  ELSE
		IF m.esc
			RETURN
			ENDIF
		sp=SPACE(43)
		ENDIF
	SET CONS OFF
	tskf=SYS(3)+'.prt'
	SET ALTE TO &tskf
	SET ALTE ON
	?m.nlq+REPL(CHR(10),9)
	?sp+'        '+naz
	?
	?sp+'        '+jme
	?
	?sp+'        '+uli
	?
	?sp+LEFT(psc,3)+' '+RIGHT(psc,2)+'  '+mes+m.draft+CHR(12)
	SET ALTE TO
	SET HEAD OFF
	DO tisk WITH 0
	SET CONS ON
RETURN

PROCEDURE sez_obal_o
	sp=SPACE(4)
	SET CONS OFF
	tskf=SYS(3)+'.prt'
	SET ALTE TO &tskf
	SET ALTE ON
	?m.nlq+sp+naz
	?sp+jme
	?sp+uli
	?sp+mes
	?sp+LEFT(psc,3)+' '+RIGHT(psc,2)+m.draft+CHR(12)
	SET ALTE TO
	SET HEAD OFF
	DO tisk WITH 0
	SET CONS ON
RETURN
