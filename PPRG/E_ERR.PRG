*******************************************
**	Parametry:
**	error()	- ¬¡slo chyby
**	msg		- Text chyby
**	prg		- Procedura v n¡§ chyba vznikla
**	file	- Soubor
**	line	- ü dek chyby
**	comm	- Pý¡kaz
**	kdo		- U§ivatel jemu§ chyba vznikla
**	m.sq	- Seqvence

PARAMETERS p2,p3,p4,p5,p6,p7,p8,p9

	PRIV _sel
	_sel=sele()
	er_add = dtos(date())+sys(3)
	er_user = p8

	SELE 0
	CREATE CURSOR cu1 (prgn C(8), usern C(8))

	PRIV er_data
	er_data=""

    IF !ini("e_err.eni")
		USE
		SELE (_sel)
		RETURN .F.
		ENDIF
		
    IF empty("er_data") OR !file(m.er_data+"NUL")
		WAIT WIND "Neexistuje adres ý pro pýenos chyby do poçty."
		USE
		SELE (_sel)
		RETURN .F.
		ENDIF

	IF !file(m.er_data+"a_mess.dbf")
		CREATE DBF (m.er_data+"a_mess") ;
			(addr C(8),send C(8),d_send D,term D, tit C (75), mes M, ;
				m_user C(10), m_date D, m_time C(8), m_add C(16))
	  ELSE
	  	IF !used("a_mess")
		  	SELE 0
		  	USE (m.er_data+"a_mess")
			ENDIF
		ENDIF
	SELE a_mess
	INSERT INTO a_mess (tit, send) VALUES (m.p3, "ERROR")
	REPLACE d_send WITH date(), term WITH date()+1, m_add WITH er_add;
			m_date WITH date(), m_time WITH time(), m_user WITH er_user
	REPLACE	mes WITH "Chyba No - "+str(m.p2)+chr(13)+chr(10) ADDITIVE	
	REPLACE	mes WITH "Chyba    - "+m.p3+chr(13)+chr(10) ADDITIVE	
	REPLACE	mes WITH "Program  - "+m.p4+chr(13)+chr(10) ADDITIVE
	REPLACE	mes WITH "Soubor   - "+m.p5+chr(13)+chr(10) ADDITIVE
	REPLACE	mes WITH "ü dek    - "+str(m.p6)+chr(13)+chr(10) ADDITIVE
	REPLACE	mes WITH "Pý¡kaz   - "+m.p7+chr(13)+chr(10) ADDITIVE
	REPLACE	mes WITH "Chybista - "+m.p8+chr(13)+chr(10) ADDITIVE
	REPLACE	mes WITH "Sekvence - "+m.p9+chr(13)+chr(10) ADDITIVE
	USE

	IF ! file(m.er_data+"a_user.dbf")
		CREATE DBF (m.er_data+"a_user") ;
			 (n_use C(8), zpr C(16), read_m D, out_m D, cteno L, m_user C(10), m_date D, m_time C(8), m_add C(16))
	  ELSE
		IF ! used("a_user")
			SELE 0
			USE  (m.er_data+"a_user")
			ENDIF
		ENDIF

	PRIV a, b, f
	a=at(',',p9+",")
	b=upper(left(p9,a-1))
	f=.F.

	SELE cu1
	SCAN ALL FOR cu1.usern!="JINE"
		IF alltrim(prgn)=b
			INSERT INTO a_user (zpr, cteno, n_use, m_user, m_date, m_time, m_add) ;
				VALUES (er_add, .F., cu1.usern, er_user, date(), time(), dtos(date())+sys(3))
			DO d_diru WITH usern
			f=.T.
			ENDIF
		ENDSCAN
	IF !f
		SCAN ALL FOR cu1.usern="JINE"
			IF alltrim(prgn)=b
				INSERT INTO a_user (zpr, cteno, n_use, m_user, m_date, m_time, m_add) ;
					VALUES (er_add, .F., cu1.usern, er_user, date(), time(), dtos(date())+sys(3))
				DO d_diru WITH usern
				ENDIF
			ENDSCAN
		ENDIF
	USE
	USE IN a_user
	SELE (_sel)
RETURN .T.

PROCEDURE readuser
PARA s1
	PRIV a
	a=at(',',s1)
	INSERT INTO cu1 VALUES (left(s1,a-1),substr(s1,a+1))
RETURN

PROCEDURE d_diru
PARAMETERS usak
	dire = getenv("USERDIR")+"\TODAY.DAT"
	DELE FILE (dire)
RETURN
