**********************************************
**	NaŸten¡ a zpracov n¡ INI souboru  V1.1  **
**********************************************
**	_file	- cel‚ jm‚no INI souboru
**	_obl	- oblast ke zpracov n¡
**
**	Pý¡kazy:
**		PATH	- Nastav¡ PATH, pýema§e p…vodn¡ nastaven¡.
**		ADDPATH	- Pýid  cestu k st vaj¡c¡mu PATH.
**		DATA	- Nastav¡ zadanou cestu do promØnn‚ GL_DATA, kterou vytvoý¡.
**		; *		- Koment ý, tyto ý dky budou ignorov ny.
**		[PROC]	- N sleduj¡c¡ ý dky (mimo komentovan‚) bude pýed vat 
**				  proceduýe PROC.
**		[]		- UkonŸen¡ vol n¡ procedury.
**		{obl.}	- Provede pouze pý¡kazy v oblasti dle pýedan‚ho parametru.
**		{}		- UkonŸen¡ oblasti.
**		ostatn¡	- Vçechny ostatn¡ ý dky budou zpracov ny jako macro (&ý dek).
**				  Pokud chceme prov dØt z pis do promØnnìch, mus¡ bìti ji§
**				  dý¡ve definov ny jako PUBLIC
**
****************************************************************************
PARA _file, _obl

SET TALK OFF

IF type("_file")!='C'
	WAIT WIND "Nen¡ zad n inicializaŸn¡ soubor."
	RETURN .F.
	ENDIF

IF !file(m._file)
	WAIT WIND "Neexistuje inicializaŸn¡ soubor "+m._file+"."
	RETURN .F.
	ENDIF

IF type("m._obl")!='C'
	_obl=""
	ENDIF
_obl=upper(m._obl)

PRIV f, s, p, a, b, r, x, rt, proc
rt=.T.
f=fopen(m._file)
IF f=0
	WAIT WIND "Nelze otevý¡t inicializaŸn¡ soubor "+m._file+"."
	RETURN .F.
	ENDIF

proc=""
__obl=""

r=0
DO WHILE !feof(f)
	r=r+1
	s=alltrim(strt(fgets(f),chr(9),' '))
	a=at('=',s)
	ON ERROR DO errini

	DO CASE
	  CASE empty(s) OR left(s,1)='*' OR left(s,1)=';'
	  CASE left(s,1)='{'
		b=at('}',s)
		IF b=0
			DO errini
			ENDIF
		__obl=upper(substr(s,2,b-2))
	  CASE !m._obl==m.__obl
*	  	WAIT WIND "Vypusteno: "+str(r)
	  CASE left(s,1)='['
		b=at(']',s)
		IF b=0
			DO errini
			ENDIF
		proc=substr(s,2,b-2)
	  CASE !empty(m.proc)
		=&proc.(s)
	  CASE a>0 AND alltrim(upper(left(s,a-1)))="PATH"
		x=substr(s,a+1)
		SET PATH TO &x
	  CASE a>0 AND alltrim(upper(left(s,a-1)))="ADDPATH"
		x=substr(s,a+1)
		IF rat(';',x)!=len(x)
			x=x+';'
			ENDIF
		SET PATH TO &x+set("PATH")
	  CASE a>0 AND alltrim(upper(left(s,a-1)))="DATA"
		PUBLIC gl_data
		x=substr(s,a+1)
		gl_data=x
	  OTHER
		&s
		ENDCASE
	ENDDO

IF type("_onerr")="C"
	ON ERROR &_onerr
  ELSE
	ON ERROR
	ENDIF

IF !fclose(f)
	WAIT WIND "Nepodaýilo se uzavý¡t inicializaŸn¡ soubor."
	RETURN .F.
	ENDIF
RETURN m.rt

*******************************************
**	Hl çen¡ chyby pýi prov dØn¡ INI souboru
PROCEDURE errini
	WAIT WIND "Chyba na ý dku: "+alltrim(str(r))+"   "+left(s,50)
* Z pis do ERR souboru k INI file
	rt=.F.
RETURN
