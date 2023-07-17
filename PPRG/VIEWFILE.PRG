*************************
**  Zobrazeni souboru  **
*************************
*PROCEDURE viewfile
PARAM soub,lmrg,edt
PRIVATE wzoom, sysm

IF empty(m.lmrg)
	lmrg=0
	ENDIF
wzoom=.T.
DEFINE WIND __viewfile FROM 0,-1 TO srows()-1,scols() IN SCREEN ;
	TITLE "Prohl¡§en¡ souboru  "+upper(soub) ;
	FOOTER ",PgUp,PgDn,^HOME,^END,^"+chr(27)+",^"+chr(26)+",HOME - Posuv          ^F10 - Velikost" ;
	SYSTEM CLOSE FLOAT NOGROW NOZOOM SHADOW COLOR SCHEME 18			&&	18
DEFINE WIND __view_in FROM -2,-1-m.lmrg TO wrows('__viewfile'),250-m.lmrg ;
	IN WINDOW __viewfile SYSTEM FLOAT GROW CLOSE COLOR SCHEME 18	&&	8

PUSH KEY
PUSH MENU _msysmenu
sysm=set('sysmenu')
SET SYSMENU TO
SET SYSMENU ON
DEFINE PAD _msm_edit OF _msysmenu PROMPT "\<Vyhled n¡" COLOR SCHEME 3 ;
	KEY ALT+V, ""
ON PAD _msm_edit OF _msysmenu ACTIVATE POPUP _medit

DEFINE POPUP _medit MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR _med_find  OF _medit PROMPT "\<Hledej..."		KEY CTRL+F, "^F"
DEFINE BAR _med_finda OF _medit PROMPT "Hledej \<Dalç¡"		KEY CTRL+G, "^G"
DEFINE BAR _med_goto  OF _medit PROMPT "BØ§ \<na ý dek..."
* DEFINE BAR _med_sp300 OF _medit PROMPT "\-"
* DEFINE BAR _med_slcta OF _medit PROMPT "OznaŸ \<vçe"		KEY CTRL+A, "^A"
* DEFINE BAR _med_copy  OF _medit PROMPT "K\<op¡ruj"		KEY CTRL+C, "^C"

ON KEY LABEL F1 *
ON KEY LABEL Ctrl+F10 DO zoomvfil
ON KEY LABEL Ctrl+Leftarrow  DO shiftwnd WITH 2
ON KEY LABEL Ctrl+Rightarrow DO shiftwnd WITH -2
ON KEY LABEL Home DO homewnd
ACTIV WIND __viewfile
IF !m.edt
	MODIFY FILE (soub) NOEDIT WINDOW __view_in IN WIND __viewfile
  ELSE
	MODIFY FILE (soub) WINDOW __view_in IN WIND __viewfile
	ENDIF
POP MENU _msysmenu
SET SYSMENU &sysm
POP KEY
RELEASE WIND __viewfile, __view_in
RETURN


PROCEDURE zoomvfil
PUSH KEY CLEAR
IF !m.wzoom
	ZOOM WINDOW __viewfile NORM FROM 0,-1 TO srows()-1,scols()
ELSE
	ZOOM WINDOW __viewfile NORM FROM 1,2 TO srows()-3,scols()-4
ENDIF
ZOOM WINDOW (wontop()) NORM FROM -1,-1 TO wrows('__viewfile'),250
m.wzoom=!m.wzoom
POP KEY
RETURN

PROCEDURE shiftwnd
PARAM kam
PUSH KEY CLEAR
MOVE WIND (wontop()) BY 0,kam
POP KEY
RETURN

PROCEDURE homewnd
PUSH KEY CLEAR
MOVE WIND (wontop()) TO -1,-1
POP KEY
KEYBOARD '{HOME}' PLAIN
RETURN
