******************************
****  ASK(y,x,impl,text)  ****
******************************
*PROCEDURE ask
PARAM y, x, imp, txt
PRIVATE compstat, len, ws, t, q, k, ask_y, ask_n
PRIVATE tx, bell

IF type([m.Esc])=='U'
	PUBLIC Esc
	Esc=.F.
ENDIF

compstat = set('COMPATIBLE')
SET COMPATIBLE FOXPLUS

IF left(m.txt,1)=='\'
	bell=.T.
	tx=subs(m.txt, 2)
ELSE
	bell=.F.
	tx=m.txt
ENDIF

len=len(m.tx)
ws=m.len+18
x=iif (m.x<0, int((scol()-m.ws)/2), min(m.x, scol()-m.ws))
y=iif (m.y<0, int((srow()-3)/2),    min(m.y, srow()-3))

t=type ([m.imp])
DO CASE
CASE m.t=='L'
	imp=iif(m.imp, 1, 2)
CASE m.t=='N'
	imp=iif(m.imp=1, 1, 2)
CASE m.t=='C'
	q=iif(len(m.imp)>0, upper(left(m.imp,1)), 'N')
	imp=iif(m.q$'AYT1', 1, 2)
OTHER
	imp=2
ENDCASE


DEFINE WINDOW ask_wind FROM m.y,m.x TO m.y+2,m.x+m.ws-1 ;
		FLOAT NOCLOSE SHADOW COLOR SCHEME 5
ACTIVATE WIND ask_wind NOSHOW
ask_y=0
ask_n=0
CLEAR PROMPT
@ 0,2 SAY m.tx
IF m.imp=1
	@ 0,m.len+4  GET m.ask_y PICTURE "@*T \<Ano"
	@ 0,m.len+10 GET m.ask_n PICTURE "@*T \<Ne"
ELSE
	@ 0,m.len+10 GET m.ask_n PICTURE "@*T \<Ne"
	@ 0,m.len+4  GET m.ask_y PICTURE "@*T \<Ano"
ENDIF

SHOW WIND ask_wind
IF m.bell
	?? chr(7)
ENDIF
READ CYCLE MODAL VALID (readkey()%256)!=5
k=readkey()%256
Esc=(m.k=12)
RELEASE WIND ask_wind

IF m.compstat = 'ON'
	SET COMPATIBLE ON
ENDIF

RETURN (m.ask_y=1)
