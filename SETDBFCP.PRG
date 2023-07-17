***********************************************
****  Nastav¡ k¢dovou str nku DBF souboru  ****
***********************************************
PARAMETER fname, cpage
*
* fname = Jm‚no souboru vŸetnØ cesty a pý¡pony
* cpage = { 852 | 895 | 1250 | 0 }
*
* Vrac¡:
*  0  - OK
*  -1 - Soubor nelze otevý¡t
*  -2 - Chybn  k¢dov  str nka
*  -3 - Soubor nen¡ FoxPro tabulka

PRIVATE fnam
fnam=alltr(m.fname)+iif(rat('.', m.fname)<=rat('\', m.fname), '.DBF', '')

PRIVATE fh, buf, outbyte
DO CASE
CASE empty(m.cpage)
	outbyte=0
CASE m.cpage=852
	outbyte=100
CASE m.cpage=895
	outbyte=104
CASE m.cpage=1250
	outbyte=200
OTHERWISE
	RETURN -2	&& Chybn  k¢dov  str nka
ENDCASE

fh = fopen(m.fnam,2)
IF m.fh > 0
	* Kontrola, zda je soubor FoxPro tabulka
	buf=fread(m.fh, 32)
	IF (substr(m.buf,1,1) = chr(139) or substr(m.buf,1,1) = chr(203);
			or substr(m.buf,31,1) != chr(0) or substr(m.buf,32,1) != chr(0))
		=fclose(m.fh)
		RETURN -3	&& Soubor nen¡ FoxPro tabulka
	ELSE
		* Z pis identifikace k¢dov‚ str nky na pozici 29
		IF substr(m.buf,1+29,1)!=chr(m.outbyte)
			=fseek(m.fh,29)
			=fwrite(m.fh, chr(m.outbyte))
		ENDIF
		=fclose(m.fh)
	ENDIF
ELSE
	RETURN -1	&& Soubor nelze otevý¡t
ENDIF

RETURN 0	&& OK



* Ostatn¡ k¢dy:
*
*	C.Page  Byte
*	------- ----
*	437		1
*	850		2
*	1252	3
*	10000	4
*	852		100
*	866		101
*	865		102
*	861		103
*	895		104
*	620		105
*	737		106
*	857		107
*	863		108
*	10007	150
*	10029	151
*	10006	152
*	1250	200
*	1251	201
*	1253	203
*	1254	202
*	1257	204
*	0		0
