PARAMETERS num,par_1,par_2,par_3
PUBLIC dir_s(3,5),err
err = 0


	DO CASE 
		CASE num = 1
			RETURN cop_dir(par_1,par_2,par_3)
		CASE num = 2
			DO cop_one WITH par_1,par_2,par_3			
		CASE num = 3
			DO act_all WITH par_1,par_2
		CASE num = 4
			DO act_one WITH par_1,par_2
		CASE num = 5
			DO uklid WITH par_1,par_2	
		ENDCASE

RETURN


					
					*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
					*º AktualizaŸni funkce  º
					*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼




PROCEDURE cop_dir
	PARAMETERS s_dir,t_dir,co_da
	DO ma_ti WITH s_dir
	IF ":"$t_dir AND len(t_dir)<4
		t_dir = left(t_dir,1)+":\"
	  ELSE	
		IF !_mkdir(t_dir)
			RETURN(.F.)
			ENDIF
		ENDIF
	t_dir = alltrim(upper(t_dir))
	t_dir = iif(right(t_dir,1) = "\",t_dir,t_dir+"\")
	s_dir = alltrim(upper(s_dir))
	s_dir = iif(right(s_dir,1) = "\",s_dir,s_dir+"\")
	FOR i = 1 to (alen(dir_s)/5)
		DO cop_one  WITH s_dir+dir_s(i,1),t_dir+dir_s(i,1),co_da
		ENDFOR
RETURN (.T.)


PROCEDURE act_all
	PARAMETERS s_dir,t_dir
	PRIV s_dir,t_dir
	DO ma_ti WITH s_dir
	s_dir = op_so(s_dir)	
	t_dir = op_ta(t_dir)
	FOR i = 1 to (alen(dir_s)/5)
		DO akt_one WITH s_dir+dir_s(i,1),t_dir+dir_s(i,1)
		ENDFOR
RETURN		



PROCEDURE akt_one
	PARAMETERS s_file,t_file	
	DO o_so_ta WITH s_file,t_file
	SET DELE OFF
	SELE 1
	SET RELA TO m_add INTO akt
	SCAN ALL
		IF eof("akt")
			SCAT MEMVAR MEMO
			INSERT INTO akt FROM MEMVAR
		  ELSE
			IF dtos(zdr.m_date)+zdr.m_time > dtos(akt.m_date)+akt.m_time
				SCAT MEMVAR MEMO
				SELE akt
				GATH MEMVAR MEMO
				SELE zdr
				ENDIF
			ENDIF
		IF dele()
			SELE akt
			DELE
			SELE zdr
			ENDIF
		ENDSCAN
		CLOSE DATA
		DELE FILE &fil_s
		DELE FILE &fil_t
RETURN		



PROCEDURE cop_one
	PARAMETERS s_db,t_db,c_day
	=working()
	ON ERROR USE (s_db)
	USE (s_db)
	ON ERROR &_onerr
	IF c_day = 0
		f_dat = {01.01.80}
	  ELSE
	  	f_dat = date()-c_day
	  	ENDIF
		SET DELE OFF
		COPY TO &t_db FOR m_date>f_dat
	=endwork()	
RETURN 




PROCEDURE ma_ti				&& naplni matici dir_s z cesty s_dir(param-sou_d)
	PARAMETERS sou_d
	ma_dir = sou_d+"*.dbf"
	=adir(dir_s,ma_dir)
RETURN



FUNCTION op_so
	PARAMETERS s_dir
	s_dir = alltrim(upper(s_dir))
	s_dir = iif(right(s_dir,1) = "\",s_dir,s_dir+"\")
*	s_dir = gl_data+s_dir
RETURN s_dir


FUNCTION op_ta
	PARAMETERS t_dir
	t_dir = alltrim(upper(t_dir))
	t_dir = iif(right(t_dir,1) = "\",t_dir,t_dir+"\")
*	t_dir = gl_data+t_dir
RETURN t_dir




PROCEDURE o_so_ta
	PARAMETERS s_file,t_file	
	CLOSE DATA
	SELE 1
	ON ERROR err=error()
	USE (s_file) EXCL ALIAS zdr
	ON ERROR &_onerr
	IF m.err!=0
		IF m.err=3 OR m.err=108
			WAIT WIND "Datab ze jsou pou§¡v ny jeçtØ nØkìm jinìm. Nelze prov‚st aktalizaci."
		  ELSE
			WAIT WIND "Vznikla chyba Ÿ."+strn(m.err)+" pýi kop¡rov n¡ datab z¡. Aktual. neprobØhla."
			ENDIF
		ENDIF			
	
	fil_s = sys(3)+".IDX"
	INDEX ON m_add TO &fil_s
	ON ERROR err=error()
	SELE 2
	USE (t_file) EXCL ALIAS akt
	ON ERROR &_onerr
	IF m.err!=0
		IF m.err=3 OR m.err=108
			WAIT WIND "Datab ze jsou pou§¡v ny jeçtØ nØkìm jinìm. Nelze prov‚st aktalizaci."
		  ELSE
			WAIT WIND "Vznikla chyba Ÿ."+strn(m.err)+" pýi kop¡rov n¡ datab z¡. Aktual. neprobØhla."
			ENDIF
		ENDIF			
	ON ERROR &_onerr
	IF m.err!=0
		IF m.err=3 OR m.err=108
			WAIT WIND "Datab ze jsou pou§¡v ny jeçtØ nØkìm jinìm. Nelze prov‚st aktalizaci."
		  ELSE
			WAIT WIND "Vznikla chyba Ÿ."+strn(m.err)+" pýi kop¡rov n¡ datab z¡. Aktual. neprobØhla."
			ENDIF
		ENDIF
	fil_t = sys(3)+".IDX"
	INDEX ON m_add TO &fil_t
	ON ERROR &_onerr
RETURN	


PROCEDURE uklid
	PARAMETERS dir_a,dir_b
	FOR i = 1 to (alen(dir_s)/5)
		fil_ek1 = dir_a+"\"+dir_s(i,1)		
		fil_ek2 = dir_b+"\"+dir_s(i,1)		
		fifi = left(fil_ek1,rat(".",fil_ek1))+"fpt"
		fifa = left(fil_ek2,rat(".",fil_ek2))+"fpt"
		dele file (fil_ek1)
		dele file (fil_ek2)		
		dele file (fifi)
		dele file (fifa)		
		ENDFOR
		= _rmdir (dir_a)
		= _rmdir (dir_b)
*	=adir(inid,"*.idx")
*	FOR i = 1 to alen(inid)/5
*		fuf  = inid(i,1)
*		DELE FILE &fuf
*		ENDFOR
*
