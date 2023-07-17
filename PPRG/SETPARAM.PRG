PROCEDURE parstan
	SELE 0
	USE setekont
	GO TOP
	SCATTER MEMVAR
	DO list_c WITH -1,-1,15,0,;
		[nstation+' ³ '+floppy+' ³ '+color_type+' ³ '+str(set132_on,3)+ ;
			' ³  '+str(set132_off,3)+' ³'+imp_output+'³ '+ ;
			left(lpt1,6)+' ³ '+left(lpt2,6)+' ³ '+left(lpt3,6)],;
		" Parametry programu ",;
		"// N zev stanice  ³Flp³Typ³132on³132off³ LPT ³ LPT1   ³ LPT2   ³ LPT3",;
		"\ F1-Help   ENTER-Oprava   INS-Pýid n¡ stanice   F7-Tý¡dit podle ...",;
			'CR#:		DO sete_opr WITH .F.	## ENTER-Oprava stanice ',;
			'INS:		DO sete_opr WITH .T.	## INS-Pýid n¡ stanice',;
			'ALT+D#:	DO sete_del				## ALT+D-Smaz n¡ stanice'
	USE
RETURN

PROCEDURE parprint
	SELE 0
	USE setprint
	DO list_c WITH -1,-1,10,0,;
		[naz+' ³ '+str(len_pg,3)+' ³ '+str(margin,5)],;
		" Parametry tisk ren ",;
		"// N zev tisk rny ³ü dk…³ L.okraj",;
		"\ F1-Help   ENTER-Oprava   INS-Pýid n¡ tisk rny   F7-Tý¡dit podle ...",;
			'CR#:		DO setp_opr WITH .F.	## ENTER-Oprava tisk rny ',;
			'INS:		DO setp_opr WITH .T.	## INS-Pýid n¡ tisk rny ',;
			'ALT+D#:	DO setp_del				## ALT+D-Smaz n¡ tisk rny '
	USE
RETURN

PROCEDURE setp_opr
PARA app
	IF m.app
		INSERT INTO setprint VALUES ('EPSON',62,15,'0','15','18','15,27,77',;
			'18,27,80','27,80','27,77','27,120,49','27,120,48','14','20')
		ENDIF
	DO setprint.spr
	IF m.esc AND m.app
		DELE
		RETURN
		ENDIF
RETURN

PROCEDURE setp_del
	IF ask(-1,-1,'N',"Opravdu smazat definici tisk rny "+naz)
		DELE
		ENDIF
RETURN

PROCEDURE sete_opr
PARA app
	IF m.app
		INSERT INTO setekont (nstation) VALUES (getenv('nstation'))
		ENDIF
	DO setekont.spr
	IF m.esc AND m.app
		DELE
		RETURN
		ENDIF
RETURN

PROCEDURE sete_del
	IF ask(-1,-1,'N',"Opravdu smazat definici stanice "+nstation)
		DELE
		ENDIF
RETURN

