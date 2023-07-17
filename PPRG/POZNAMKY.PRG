*******************
**	Poznamkovy blok
**	DB_BLOK	= Cesta i s jm‚nem datab ze
**	TITLE	= Nadpis ok‚nka seznamu
PARAM db_blok, title

IF empty(db_blok)
	RETURN
	ENDIF
IF empty(title)
	title=" Pozn mkovì blok "
	ENDIF

PRIV err,_se,_cent,ssm
ssm=set('sysm')
_se=sele()
SELE 0

db_blok=alltrim(m.db_blok)

IF !blok_open()
	SELECT (m._se)
	RETURN
	ENDIF
	
PUSH KEY CLEAR

_cent=set('century')
SET CENTURY ON

PRIVATE on_list_c

lst=.T.

*IF m.lst

SET ORDER TO nazev
GO TOP
DO list_c WITH 1,1,20,9,;
	[nazev+' '+iif(empty(datum),blank(dtoc(datum)),dtoc(datum))+' '+alarm+iif(empty(text),' ','û')],;
	m.title,;
	"\ F1-N povØda   ENTER-Pozn mka  INS-Nov   F4-Oprava   F7-Tý¡dit...",;
	[|11],;
	[||1PF=iif("*" $ nazev,"gr+/rb",0)],;
	'CR#:  DO blok_txt			## ENTER-Prohl¡§en¡/oprava pozn mky',;
	'INS:  DO blok_edt WITH .T.	## INS-Nov  polo§ka do bloku',;
	'DEL#: DO blok_del			## DEL-Vymaz n¡ polo§ky z bloku',;
	'F4#:  DO blok_edt WITH .F.	## F4-Oprava £daj… polo§ky v bloku',;
	'F8#:  DO blok_tisk			## F8-Tisk pozn mky',;
	'Ctrl+Enter:	toexit=.T.	## Ctrl+Enter-Konec'

*	ENDIF

*		'F5#:  DO blok_ala ## F5-Upozornit'
*	IF m.typ='ALARM'
*		WAIT WIND moment NOWAIT
*		REPLACE ALL alarm WITH '' FOR alarm=chr(255)
*		ENDIF

USE
SELECT (m._se)
SET CENTURY &_cent
POP KEY
RETURN

PROCEDURE blok_tisk
	PRIV _memow, _cons, _nz, tskf
	_menow=set('MEMOWIDTH')
	_cons=set('CONSOLE')
	tskf=db_blok+".PRT"
	SET MEMO TO 69
	SET CONS OFF
	SET PRINT TO &tskf
	SET PRINT ON
	_nz=alltrim(nazev)
	?space(10+(30-len(_nz)/2))+csup(_nz)
	?
	?space(9),text
	?chr(12)
	SET PRINT OFF
	SET PRINT TO
	DO TISK WITH 6,.T.
	SET CONS &_cons
	SET MEMO TO _menow
RETURN

******************
PROCEDURE blok_edt
PARAM App
PRIVATE i,w,y,x,v,s

w='i'
FOR i=1 TO fcount()
	w=m.w+','+field(m.i)
	ENDFOR
PRIVATE &w

IF m.App
	SCATTER MEMVAR BLANK
  ELSE
	SCATTER MEMVAR
	ENDIF

v=3
s=52
y=iif(m.App, int((srow()-m.v)/2), min(m.my+1,srows()-m.v))
x=int((scol()-m.s)/2)

DEFINE WINDOW blok_nazev FROM m.y,m.x TO m.y+m.v-1, m.x+m.s-1 ;
	FLOAT NOCLOSE SHADOW DOUBLE COLOR SCHEME 5

ACTIVATE WINDOW blok_nazev NOSHOW
@ 0,2 SAY "N zev:"
@ 0,9 GET m.nazev SIZE 1,20
@ 0,31 SAY "Datum:"
@ 0,38 GET m.datum SIZE 1,10 ;
	VALID iif(inlist(lastkey(),13,27) OR (readkey()%256)!=15, .T., 0)
ACTIVATE WINDOW blok_nazev
READ MODAL VALID inlist(lastkey(),13,27)
RELEASE WINDOW blok_nazev

IF lastkey()=13
*	IF empty(datum)
*		alarm=''
*		ENDIF
	IF m.App
		INSERT INTO blok FROM MEMVAR
	  ELSE
		GATHER MEMVAR
		ENDIF
	ENDIF
RETURN


******************
PROCEDURE blok_del
	IF ask(m.my+1,m.mx+10,.T.,'Vymazat polo§ku "'+alltrim(nazev)+'" ?')
		DELETE
		ENDIF
	RETURN


******************
PROCEDURE blok_txt
	=edtmemo("O","MEMO3")
	DO def_menu
	IF  edtmemo("E","MEMO3","text")
		REPL datum WITH date()	
		ENDIF
	=edtmemo("C","MEMO3")
	 POP MENU _msysmenu
	 SET SYSMENU &ssm
	 RETURN


******************
*      P…vodn¡ funkce pro editaci
*
*	PRIVATE v,s
*	v=18
*	s=70
*
*  DEFINE WINDOW edmem ;
*	FROM INT((SROW()-m.v-2)/2),INT((SCOL()-m.s-2)/2) ;
*	TO INT((SROW()-m.v-2)/2)+m.v+1,INT((SCOL()-m.s-2)/2)+m.s+1 ;
*	TITLE alltrim(nazev) ;
*	FOOTER "CTRL+ENTER - Ulo§it          ESC - Pýeruçit" ;
*	FLOAT NOCLOSE SHADOW GROW SYSTEM COLOR SCHEME 18
* ACTIVATE WINDOW edmem
* @ 0,0 EDIT text SIZE m.v,m.s
* PUSH KEY
* ON KEY LABEL TAB KEYBOARD "    "
* DO def_menu
* READ 
*
* IF lastkey()!=27
*	REPL datum WITH date()
*	ENDIF
* POP MENU _msysmenu
* SET SYSMENU OFF
* POP KEY
* RELEASE WINDOW edmem
* RETURN

*******************************
**  Otevreni databaze bloku  **
*******************************
PROCEDURE blok_open
	PRIV aa
	aa=substr(m.db_blok,rat('\',m.db_blok)+1)
	IF used(aa)
		RETURN .T.
		ENDIF
	IF !file(m.db_blok+'.dbf')
		ERASE (m.db_blok+'.cdx')
		CREATE DBF (m.db_blok) (datum D, alarm C(1), nazev C(20), text M)
		USE
		ENDIF
	SELECT 0
	ON ERROR USE (m.db_blok) ALIAS blok
	USE (m.db_blok) ALIAS blok
	ON ERROR &_onerr
	IF !file(m.db_blok+'.cdx')
		INDEX ON datum TAG datum
		INDEX ON cs(nazev) TAG nazev
		ENDIF

RETURN .T.

******************
PROCEDURE def_menu
PUSH MENU _msysmenu
SET SYSMENU TO
SET SYSMENU ON

DEFINE PAD _msm_blank	OF _msysmenu PROMPT space(20) SKIP	COLOR SCHEME 3
DEFINE PAD _msm_edit	OF _msysmenu PROMPT " O\<pravy "	COLOR SCHEME 3	KEY ALT+P, ""
DEFINE PAD _msm_windo	OF _msysmenu PROMPT " Ok\<no "		COLOR SCHEME 3	KEY ALT+N, ""
ON PAD _msm_edit	OF _msysmenu ACTIVATE POPUP _medit
ON PAD _msm_windo	OF _msysmenu ACTIVATE POPUP _mokno

DEFINE POPUP _medit MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR _med_undo	OF _medit PROMPT "\<Undo"					KEY CTRL+U, "^U"
DEFINE BAR _med_redo	OF _medit PROMPT "\<Redo"					KEY CTRL+R, "^R"
DEFINE BAR _med_sp100	OF _medit PROMPT "\-"
DEFINE BAR _med_cut		OF _medit PROMPT "Cu\<t"					KEY CTRL+X, "^X"
DEFINE BAR _med_copy	OF _medit PROMPT "\<Copy"					KEY CTRL+C, "^C"
DEFINE BAR _med_paste	OF _medit PROMPT "\<Paste"					KEY CTRL+V, "^V"
DEFINE BAR _med_sp200	OF _medit PROMPT "\-"
DEFINE BAR _med_slcta	OF _medit PROMPT "Select \<All"				KEY CTRL+A, "^A"
DEFINE BAR _mst_captr	OF _medit PROMPT "Capture"
DEFINE BAR _med_sp300	OF _medit PROMPT "\-"
DEFINE BAR _med_find	OF _medit PROMPT "\<Find..."				KEY CTRL+F, "^F"
DEFINE BAR _med_finda	OF _medit PROMPT "Find A\<gain"				KEY CTRL+G, "^G"
DEFINE BAR _med_repl	OF _medit PROMPT "R\<eplace And Find Again"	KEY CTRL+E, "^E"
DEFINE BAR _med_repla	OF _medit PROMPT "Replace All"

DEFINE POPUP _mokno MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR _mwi_move	OF _mokno PROMPT "\<Posuv"		KEY CTRL+F7, "^F7"
DEFINE BAR _mwi_size	OF _mokno PROMPT "\<Velikost"	KEY CTRL+F8, "^F8"
DEFINE BAR _mwi_zoom	OF _mokno PROMPT "\<ZvØtçit "	KEY CTRL+F10, "^F10"
DEFINE BAR _mwi_min		OF _mokno PROMPT "Z\<mençit "	KEY CTRL+F9, "^F9"

RETURN

*********************************************************************************
***		Zatim k nicemu
IF m.typ='ALARM'
	SET ORDER TO
	title=" !! UPOZORN·NÖ !! "
	SET FILTER TO user=&usr AND !empty(alarm) AND !empty(datum) AND datum<=date()
	GO TOP
	lst=!eof()
	IF m.lst
		LOCATE ALL FOR alarm=chr(19)
		IF found()
			CLEAR TYPEAHEAD
			DO WHILE inkey()=0
				WAIT WIND m.title NOWAIT
				=beep(rand()*800,200)
				WAIT CLEAR
				=beep(rand()*800,200)
			ENDDO
			SET ORDER TO TAG datum
		ELSE
			SET ORDER TO TAG datum
			LOCATE ALL FOR datum=date()
			IF !found()
				GO BOTTOM
			ENDIF
		ENDIF
		CLEAR TYPEAHEAD
		WAIT WIND m.title NOWAIT
	ENDIF
ELSE
	WAIT CLEAR
ENDIF

******************
PROCEDURE blok_ala
PRIVATE str
IF empty(datum)
	REPLACE alarm WITH ''
ELSE
	str='!'+chr(19)+iif(m.typ='ALARM',chr(255),' ')+'!'
	REPLACE alarm WITH subst(m.str, at(alarm,m.str)+1, 1)
	DO CASE
		CASE alarm='!'
			WAIT WIND "! = Upozornit" NOWAIT
		CASE alarm=chr(19)
			WAIT WIND chr(19)+" = Upozornit + zvukovì sign l" NOWAIT
		OTHER
			WAIT WIND "Neupozoråovat" NOWAIT
	ENDCASE
ENDIF
RETURN
