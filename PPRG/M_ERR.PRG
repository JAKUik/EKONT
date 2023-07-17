PARAMETERS p0,p1,p2,p3,p4,p5,p6,p7,p8,p9

IF ! p0
	RETURN
	ENDIF


er_add = dtos(date())+sys(3)
er_user = alltrim(upper(net("whoami")))



IF ! file ("h:\mekont\data\a_mess.dbf")
	CREATE DBF h:\mekont\data\a_mess;
		(addr C(8),send C(8),d_send D,term D, tit C (75), mes M ,m_user C(10), m_date D, ;
		 m_time C(8), m_add C(16))
  ELSE
  	IF ! used("a_mess")
	  	SELE 0
	  	USE h:\mekont\data\a_mess 		 
		ENDIF
	ENDIF

IF ! file("h:\mekont\data\a_user.dbf")
	CREATE DBF h:\mekont\data\a_user;
		 (n_use C(8), zpr C(16), read_m D, out_m D, cteno L, m_user C(10), m_date D, m_time C(8), m_add C(16))
  ELSE
	IF ! used("a_user")
		SELE 0
		USE  h:\mekont\data\a_user
		ENDIF
	ENDIF
	SELE a_mess
	INSERT INTO a_mess (tit) VALUES (m.p3)
	REPLACE send WITH er_user
	REPLACE d_send WITH date(), term WITH date()+1, m_add WITH er_add;
			m_date WITH {01/01/80}, m_time WITH time(), m_user WITH er_user
	REPLACE	mes WITH "ERROR No - "+str(m.p2) ADDITIVE	
	=k_rad()
	REPLACE	mes WITH "ERROR - "+m.p3 ADDITIVE	
	=k_rad()
	REPLACE	mes WITH "PRG - "+m.p4 ADDITIVE
	=k_rad()
	REPLACE	mes WITH "FILE - "+m.p5 ADDITIVE
	=k_rad()
	REPLACE	mes WITH "LINE - "+str(m.p6) ADDITIVE
	=k_rad()
	REPLACE	mes WITH "COMM - "+m.p7 ADDITIVE
	=k_rad()
	REPLACE	mes WITH "KDO - "+m.p8 ADDITIVE
	=k_rad()
	REPLACE	mes WITH "SQ  - "+m.p9 ADDITIVE
	=k_rad()

INSERT INTO a_user (zpr) VALUES (er_add)
SELE a_user
REPLACE cteno WITH .F., n_use WITH p1
REPLACE m_user WITH er_user,m_date WITH date(), m_time WITH time(), m_add WITH dtos(date())+sys(3)
DO d_diru WITH p1

RETURN

FUNCTION k_rad
	REPLACE mes WITH chr(13) additive
	REPLACE mes WITH chr(10) additive
	RETURN


PROCEDURE d_diru
	PARAMETERS usak
	dire = getenv("USERDIR")+"\TODAY.DAT"
	dele file (dire)

