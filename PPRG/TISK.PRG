**	Procedura na vyti�t�n� souboru s konverz� ��d�c�ch znak� tisk�rny
**	na ESC sekvence
**	vf_lmrg	- O kolik posunou p�ed zobrazen�m sestavu sloupc�
**	edt		- .T. povoluje editaci zobrazen�ho souboru
* PROCEDURE tisk
PARA vf_lmrg, edt
	IF m.set132_on != -1
		=vmod(m.set132_on)
		ON ERROR err=1405
		RUN cesky
		ON ERROR &_onerr
		ENDIF
	=viewfile(m.tskf, m.vf_lmrg, m.edt)
	IF m.set132_off != -1
		=vmod(m.set132_off)
		ENDIF
	IF ask(-1,-1,"N","Vytisknout sestavu")
		* nacist esc sekvence
		DO TISK_OUT WITH .F.,.T.

*<BEGIN_CODETABLE>
	* KA    Kamenicky->ASCII
	*kod1_ascii=[��������������������������������������������]
	*kod2_ascii=[C�ed�DTceELIll�AEzZo�OuUy��SLYRtaiounNUOsrrR]

	* KL    Kamenicky->LATIN2
	*kod1_lat2 =[��������������������������������������������]
	*kod2_lat2 =[���Ԅқ�ط�֒�����������왚����������������]

	* KAR   Kamenicky->ASCII + konverze ramecku
	*kod1_asc_r=[�������������������������������������������������������������������������������������������]
	*kod2_asc_r=[C�ed�DTceELIll�AEzZo�OuUy��SLYRtaiounNUOsrrR�������||||++||++++++--|-+||++==|=|=-=-++++||++]

	* KLR   Kamenicky->LATIN2 + konverze ramecku
	*kod1_lat2k=[�������������������������������������������������������������������������������������������]
	*kod2_lat2k=[���Ԅқ�ط�֒�����������왚���������������謭�����||||++||++++++--|-+||++==|=|=-=-++++||++]

	* KLB   Kamenicky->LATIN2 + korekce ramecku
	*kod1_lat2r=[�������������������������������������������������������������������������������������������]
	*kod2_lat2r=[���Ԅқ�ط�֒�����������왚���������������謭����������������ٿ�����ų���������������ں���]

	* KLJ   Kamenicky->LATIN2 + jednoduche ramecky
	*kod1_lat2j=[�������������������������������������������������������������������������������������������]
	*kod2_lat2j=[���Ԅқ�ط�֒�����������왚���������������謭����������������ٿ���������������������������]

	PRIV kodz, kodna
		DO CASE
		  CASE prekodtisk="ASCII"
*			kodz =[��������������������������������������������]
			kodz= [���Ԅқ�ط�֒�����������왚����������������]
			kodna=[C�ed�DTceELIll�AEzZo�OuUy��SLYRtaiounNUOsrrR]
		  CASE prekodtisk="KAM"
*			kodz =[��������������������������������������������]
			kodz= [���Ԅқ�ط�֒�����������왚����������������]
			kodna=[��������������������������������������������]
		  CASE prekodtisk="LAT2"
*			kodz =[��������������������������������������������]
*			kodna=[���Ԅқ�ط�֒�����������왚����������������]
			kodz= [���Ԅқ�ط�֒�����������왚����������������]
			kodna=[���Ԅқ�ط�֒�����������왚����������������]
		  CASE prekodtisk="WIN"
*			kodz= [�������������������������������������������������������������������������������������������]
			kodz= [���Ԅқ�ط�֒�����������왚����������������]
*			kodna=[�����ύ����徾��ɞ��������܊��؝�������Ԛ����������||||++||++++++--|-+||++==|=|=-=-++++||++]
			kodna=[�����ύ����徾��ɞ��������܊��؝�������Ԛ���]
		  OTHERWISE
			kodz =""
			kodna=""
		  ENDCASE
*<END_CODETABLE>

		SET CONS OFF
		_pscode=m.resetprn
* konverze + tisk
*		SET PRINT TO (m.imp_output)
	    SET PRINT ON
		x = FOPEN(m.tskf,0)
		DO WHILE !FEOF(x)
			q = FREAD(x,128)
			q = STRTRAN(q,"",cnon)
			q = STRTRAN(q,"",cnoff)
			q = STRTRAN(q,"",ecnon)
			q = STRTRAN(q,"",ecnoff)
			q = STRTRAN(q,"",pica)
			q = STRTRAN(q,"",elit)
			q = STRTRAN(q,"",nlq)
			q = STRTRAN(q,"",draft)
			q = STRTRAN(q,"",dblon)
			q = STRTRAN(q,"",dbloff)
			q = STRTRAN(q,"",resetprn)
		 	q = STRTRAN(q,chr(26),"")
		 	IF !empty(m.kodz)
				q = CHRTRAN(q,m.kodz,m.kodna)
				ENDIF
		 	?? q
			ENDDO
		=FCLOSE(x)
		SET PRINT OFF
		SET PRINT TO
		SET CONS ON
		* nacist opet nahradni znaky
		DO TISK_OUT WITH .F.
	  ELSE
		m.esc=.F.
		ENDIF
RETURN
