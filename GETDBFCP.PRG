**********************************************
****  Zjist¡ k¢dovou str nku DBF souboru  ****
**********************************************
PARAMETER fname
*
* fname = Jm‚no souboru vŸetnØ cesty a pý¡pony
* cpage = { 852 | 895 | 1250 | 0 }
*
* Vrac¡: 
*  - k¢dovou str nku
*  - 0 - ¦ dn  CP nen¡ nastavena
*  - -1 - Soubor nelze otevý¡t
*  - -3 - Soubor nen¡ FoxPro tabulka

PRIVATE cpage
cpage=-1

IF _windows
	PRIVATE oldsel, onerr, err
	oldsel=select()
	onerr=on('error')
	SELECT 0
	ON ERROR err=error()
	err=0
	USE (m.fname) AGAIN SHARED NOUPDATE
	ON ERROR &onerr
	IF used()
		cpage=cpdbf()
		USE
	ELSE
		cpage=iif(m.err=15, -3, -1)
	ENDIF
	SELECT (m.oldsel)
ELSE
	PRIVATE fh, buf

	fh = fopen(m.fname)
	IF m.fh > 0
		* Kontrola, zda je soubor FoxPro tabulka
		buf=fread(m.fh, 32)
		IF (substr(m.buf,1,1) = chr(139) or substr(m.buf,1,1) = chr(203);
				or substr(m.buf,31,1) != chr(0) or substr(m.buf,32,1) != chr(0))
			=fclose(m.fh)
			RETURN -3	&& Soubor nen¡ FoxPro tabulka
		ELSE
			* NaŸten¡ identifikace k¢dov‚ str nky na pozici 29
			cpage=asc(substr(m.buf,1+29,1))
			=fclose(m.fh)
		ENDIF
	ELSE
		RETURN -1	&& Soubor nelze otevý¡t
	ENDIF

	DO CASE
	CASE m.cpage=1
		cpage=437
	CASE m.cpage=2
		cpage=850
	CASE m.cpage=3
		cpage=1252
	CASE m.cpage=4
		cpage=10000
	CASE m.cpage=100
		cpage=852
	CASE m.cpage=101
		cpage=866
	CASE m.cpage=102
		cpage=865
	CASE m.cpage=103
		cpage=861
	CASE m.cpage=104
		cpage=895
	CASE m.cpage=105
		cpage=620
	CASE m.cpage=106
		cpage=737
	CASE m.cpage=107
		cpage=857
	CASE m.cpage=108
		cpage=863
	CASE m.cpage=150
		cpage=10007
	CASE m.cpage=151
		cpage=10029
	CASE m.cpage=152
		cpage=10006
	CASE m.cpage=200
		cpage=1250
	CASE m.cpage=201
		cpage=1251
	CASE m.cpage=203
		cpage=1253
	CASE m.cpage=202
		cpage=1254
	CASE m.cpage=204
		cpage=1257
	ENDCASE
ENDIF

RETURN m.cpage
