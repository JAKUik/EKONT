**********************
****  Kalkulacka  ****
**********************
PROCEDURE kalkul
PARAM fname, allpriv
PUSH KEY CLEAR
ON KEY LABEL F1 *
IF (param()>1 AND !m.allpriv) OR (type([m.Ladeni])='L' AND m.Ladeni)
	PRIVATE exp, vysl, x, y, z, t, dph, dph2, n, savfile, oldpoi, oldsep, oldsel, c_rem
	PRIVATE kalk_exp, kalk_x, kalk_y, kalk_vx, kalk_vy, kalk_vz, kalk_vt, kalk_dph, kalk_dph2
	PRIVATE _talk
	oldsel=select()
ELSE
	PRIVATE ALL
	oldsel=select()
	SELECT 0
ENDIF

_talk=set('TALK')
SET TALK OFF

savfile='KALK.MEM'
IF param()>0 AND type([m.fname])='C' AND !empty(m.fname)
	savfile=m.fname
	IF right(m.savfile,1)$':\'
		savfile=m.savfile+'KALK.MEM'
	ENDIF
	IF rat('.',m.savfile)<=max(rat('\',m.savfile),rat(':',m.savfile))
		savfile=m.savfile+'.MEM'
	ENDIF
ENDIF


oldpoi=set('point')
oldsep=set('separ')
SET POINT TO
SET SEPAR TO

kalk_exp=space(100)
kalk_vysl=''
kalk_x=2
kalk_y=2
kalk_dph=22
kalk_dph2=5
kalk_colv='GR+/N'
STORE 0 TO kalk_vx, kalk_vy, kalk_vz, kalk_vt
IF file(m.savfile)
	RESTORE FROM (m.savfile) ADDI
ENDIF
x=m.kalk_vx
y=m.kalk_vy
z=m.kalk_vz
t=m.kalk_vt
dph=m.kalk_dph
dph2=m.kalk_dph2
exp=m.kalk_exp
vysl=m.kalk_vysl

n=iif(type('m.t')='N', m.t, 0)

c_rem=scheme(5,10)
DO CASE
CASE m.c_rem=='W/W'
	c_rem='N/W'
CASE m.c_rem=='N/N'
	c_rem='W/N'
ENDCASE

DEFINE WINDOW kalk FROM max(m.kalk_y,0),max(m.kalk_x,0);
	TO max(m.kalk_y,0)+13, max(m.kalk_x,0)+45 ;
	TITLE " KalkulaŸka  V 1.1 " ;
	FLOAT NOCLOSE SHADOW COLOR SCHEME 5
ACTIVATE WINDOW kalk NOSHOW
IF (wlrow()+wrows()+2)>srows()
	MOVE WINDOW kalk BY srows()-(wlrow()+wrows()+2), 0
ENDIF
IF (wlcol()+wcols()+2)>scols()
	MOVE WINDOW kalk BY 0, scols()-(wlcol()+wcols()+2)
ENDIF
@ 1,2 GET m.exp  SIZE 1,40 && VALID kalk_val(m.exp, .T.)
@ 4,14 SAY '(CTRL-Ulo§ ALT-Uka§  X Y Z )' COLOR (m.c_rem)
@ 5,0 TO 5,43
=kalk_sh()

oldcur=set('cursor')
SET CURSOR ON

ACTIVATE WINDOW kalk

ON KEY LABEL F1 *
ON KEY LABEL CTRL+X  DO kalk_sav WITH 'm.x', (m.t)
ON KEY LABEL CTRL+Y  DO kalk_sav WITH 'm.y', (m.t)
ON KEY LABEL CTRL+Z  DO kalk_sav WITH 'm.z', (m.t)
ON KEY LABEL ALT+X  DO kalk_shm WITH 'm.x'
ON KEY LABEL ALT+Y  DO kalk_shm WITH 'm.y'
ON KEY LABEL ALT+Z  DO kalk_shm WITH 'm.z'
ON KEY LABEL CTRL+F7 DO kalk_mov
ON KEY LABEL F4 DO kalk_dph
ON KEY LABEL TAB DO kalk_xdph
ON KEY LABEL PgUp DO kalk_hid WITH -1
ON KEY LABEL PgDn DO kalk_hid WITH 1
WAIT CLEAR
READ MODAL SHOW kalk_sh() VALID kalk_key()
ON KEY

SET CURSOR &oldcur

kalk_y=wlrow('kalk')
kalk_x=wlcol('kalk')
kalk_exp=m.exp
kalk_vysl=m.vysl
kalk_vx=m.x
kalk_vy=m.y
kalk_vz=m.z
kalk_vt=m.t
kalk_dph=m.dph
kalk_dph2=m.dph2
ERASE (m.savfile)
SAVE TO (m.savfile) ALL LIKE kalk_*
RELEASE WINDOW kalk
SET POINT TO m.oldpoi
SET SEPAR TO m.oldsep
SELECT (m.oldsel)
SET TALK &_talk
POP KEY
RETURN


FUNCTION kalk_key
PRIVATE k
k=lastkey()
DO CASE
	CASE m.k=27
		RETURN .T.
	CASE m.k=13
		=kalk_val(m.exp, .T.)
	OTHER
		CLEAR TYPEAHEAD
		KEYBOARD '{HOME}'
ENDCASE
RETURN .F.



FUNCTION kalk_val
PARAM expr, ev
PRIVATE tp, oldcent
kalk_colv=iif(m.ev, 'GR+/N', 'R+/N')
n=0
IF empty(m.expr)
	t=0
	vysl=''
ELSE
	tp=type(m.expr)
	DO CASE
		CASE m.tp=='C'
			t=eval(m.expr)
			vysl='"'+m.t+'"'
			vysl=iif(len(m.vysl)>100, left(m.vysl,100), m.vysl)
		CASE m.tp=='L'
			t=eval(m.expr)
			vysl=iif(m.t, '.T.', '.F.')
		CASE m.tp=='D'
			t=eval(m.expr)
			m.oldcent=set('CENTURY')
			SET CENTURY ON
			vysl='{'+dtoc(m.t)+'}'
			SET CENTURY &oldcent
		CASE m.tp=='N'
			t=eval(m.expr)
			n=m.t
			vysl=str(m.t,40,12)
			IF !('E'$m.vysl) AND '.'$m.vysl
				DO WHILE right(m.vysl, 1)=='0'
					m.vysl=left(m.vysl,len(m.vysl)-1)
				ENDDO
				IF right(m.vysl, 1)=='.'
					m.vysl=left(m.vysl,len(m.vysl)-1)
				ENDIF
			ENDIF
			m.vysl=alltrim(m.vysl)
		OTHER
			t=0
			vysl="*** ERROR ***"
	ENDCASE
ENDIF
*SHOW GETS OFF
=kalk_sh()
IF !m.ev
	DO CASE
		CASE m.expr=='m.x'
			@ 4,35 SAY ''
		CASE m.expr=='m.y'
			@ 4,37 SAY ''
		CASE m.expr=='m.z'
			@ 4,39 SAY ''
	ENDCASE
ENDIF
RETURN .T.


FUNCTION kalk_sav
PARAM var, value
PUSH KEY CLEAR
&var=value
=kalk_sh()
=beep(100,60)
=beep(200,60)
=beep(300,60)
POP KEY
RETURN


FUNCTION kalk_shm
PARAM var
PUSH KEY CLEAR
PRIVATE orig
orig=m.vysl
=kalk_val(m.var, .F.)
ON KEY LABEL CTRL+X  DO kalk_sav WITH 'm.x', (m.t)
ON KEY LABEL CTRL+Y  DO kalk_sav WITH 'm.y', (m.t)
ON KEY LABEL CTRL+Z  DO kalk_sav WITH 'm.z', (m.t)
ON KEY LABEL ALT+X  DO kalk_val WITH 'm.x', .F.
ON KEY LABEL ALT+Y  DO kalk_val WITH 'm.y', .F.
ON KEY LABEL ALT+Z  DO kalk_val WITH 'm.z', .F.
READ
ON KEY
=kalk_val(m.orig, .T.)
POP KEY
RETURN


FUNCTION kalk_sh
PRIVATE d1, d2
ACTIVATE WINDOW kalk SAME
@ 3,2	SAY m.vysl SIZE 1,40 COLOR (m.kalk_colv)
@ 4,35	SAY 'X' COLOR (iif(empty(m.x), m.c_rem, scheme(5,7)))
@ $,$+1	SAY 'Y' COLOR (iif(empty(m.y), m.c_rem, scheme(5,7)))
@ $,$+1	SAY 'Z' COLOR (iif(empty(m.z), m.c_rem, scheme(5,7)))
n=round(m.n,2)
d1=ceiling(m.n*(m.dph/100)*10)/10
d2=ceiling(m.n/(100+m.dph)*m.dph*10)/10
@ 6,4	SAY "TAB,F4 "+chr(26) COLOR (m.c_rem)
@ $,$+1	SAY "DPH:"
@ 6,18	SAY m.dph PICTURE "9999.99" COLOR W+/N
@ $,$	SAY "%" COLOR W+/N
@ 6,30	SAY "^F7 "+chr(26)+" Posuv" COLOR (m.c_rem)
@  8,2	SAY "Bez"
@  9,2	SAY "DPH"
@ 10,2	SAY "Celkem"
@  8,9	SAY m.n			PICTURE "999999999999.99" COLOR (m.kalk_colv)
@  9,9	SAY m.d1		PICTURE "999999999999.99" COLOR BG+/N
@ 10,9	SAY m.n+m.d1	PICTURE "999999999999.99" COLOR BG+/N
@  8,27	SAY m.n-m.d2	PICTURE "999999999999.99" COLOR BG+/N
@  9,27	SAY m.d2		PICTURE "999999999999.99" COLOR BG+/N
@ 10,27	SAY m.n			PICTURE "999999999999.99" COLOR (m.kalk_colv)
RETURN .T.


FUNCTION kalk_mov
PUSH KEY CLEAR
ON KEY LABEL F1 *
PRIVATE x,y,k
DEFINE WINDOW kalk_mov FROM 2,58 TO 2,76 SHADOW NONE COLOR SCHEME 7
ACTIVATE WINDOW kalk_mov
@ 0,1 SAY "Pýesun kalkulaŸky"

y=wlrow('kalk')
x=wlcol('kalk')
DO WHILE .T.
	k=inkey(0, 'H')
	DO CASE
		CASE inlist(k, 13, 10, 32)		&& OK
			EXIT
		CASE k=27						&& ESC
			MOVE WIND kalk TO m.y,m.x
			EXIT
		CASE k=5						&& UP
			IF wlrow('kalk')>0
				MOVE WIND kalk BY -1,0
			ENDIF
		CASE k=24						&& DOWN
			IF wlrow('kalk')+wrows('kalk')+2<srows()
				MOVE WIND kalk BY 1,0
			ENDIF
		CASE k=19						&& LEFT
			IF wlcol('kalk')>0
				MOVE WIND kalk BY 0,-1
			ENDIF
		CASE k=4						&& RIGHT
			IF wlcol('kalk')+wcols('kalk')+2<scols()
				MOVE WIND kalk BY 0,1
			ENDIF
		CASE k=1						&& HOME
			MOVE WIND kalk TO 0,0
		CASE k=6						&& END
			MOVE WIND kalk TO srows()-(wrows('kalk')+2),0
		CASE k=18						&& PGUP
			MOVE WIND kalk TO 0,scols()-(wcols('kalk')+2)
		CASE k=3						&& PGDN
			MOVE WIND kalk TO srows()-(wrows('kalk')+2),scols()-(wcols('kalk')+2)
	ENDCASE
ENDDO
RELEASE WINDOW kalk_mov
POP KEY
RETURN


FUNCTION kalk_dph
PUSH KEY CLEAR
ON KEY LABEL F1 *
PRIVATE col
col=repli(',',5)+'*'+scheme(5,6)
ACTIVATE WINDOW kalk
@ 6,18 GET m.dph PICTURE "@K 9999.99" COLOR (m.col)
READ MODAL VALID inlist(lastkey(),13,27)
SHOW GETS OFF
POP KEY
RETURN


FUNCTION kalk_xdph
PUSH KEY CLEAR
ON KEY LABEL F1 *
PRIVATE a
a=m.dph
dph=m.dph2
dph2=m.a
SHOW GETS OFF
POP KEY
RETURN


FUNCTION kalk_hid
PARAM kam
PUSH KEY CLEAR
ACTIVATE WINDOW kalk
PRIVATE y, s
y=wlrow()
DO WHILE betw(wlrow(), -8, srows()-4)
	MOVE WINDOW kalk BY m.kam,0
ENDDO
=inkey(0,'HM')
s=sign(m.y-wlrow())
DO WHILE wlrow()!=m.y
	MOVE WINDOW kalk BY m.s,0
ENDDO
ON KEY LABEL F1 *
POP KEY
RETURN
