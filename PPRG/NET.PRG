
PARAMETER jm_fun,pr1,pr2,pr3
	PRIVAT ret
	SET LIBR TO blnet ADDITIVE
	jm_fun = alltrim(upper(jm_fun))
	DO CASE
		CASE jm_fun = "FULLNAME"
			ret = n_fullname(m.pr1)	
		CASE jm_fun ="WHOAMI"
			ret = n_whoami()
		OTHER
			WAIT WIND "Bad function or function name!"
			ret = .F.
		ENDCASE	
	RELE LIBR blnet
RETURN m.ret



Function n_fullname
PARAMETER cUser
IF PARAMETERS()=0
   cUser = N_WhoAmI()
ENDIF
RETURN N_PropVal(cUser,1,"IDENTIFICATION",.T.)

function N_WhoAmI
PRIVATE nId,cUser
nId    = N_BindLevel("U")
cUser  = ""
if !empty(nId)
   cUser = N_ObjectName(nId)
endif
RETURN cUser

function N_BindLevel
PARAMETERS cWhich
PRIVATE cBuffer,cRequest
PRIVATE xAnswer,y
IF PARAMETERS() = 0
   cWhich = "U"
ENDIF

cBuffer  = SPACE(5)
cRequest = CHR(70)
xAnswer  = -1
IF Netware(227,cRequest,@cBuffer) == 0
   DO CASE
   CASE cWhich = "R"
      y       = ASC(cBuffer)%16
      xAnswer = SUBSTR("ALOSN",y+1,1)
   CASE cWhich = "W"
      y       = INT(ASC(cBuffer)/16)
      xAnswer = SUBSTR("ALOSN",y+1,1)
   OTHERWISE
      xAnswer = Long2Fox(SUBSTR(cBuffer,2,4))
   ENDCASE
ENDIF
RETURN xAnswer

function N_ObjectName
parameter nId
PRIVATE cBuffer,cRequest
PRIVATE cName

cBuffer  = SPACE(54)
cRequest = CHR(54)+Fox2Long(nId)
cName    = ""
IF Netware(227,cRequest,@cBuffer) == 0
   cName = CleanStr( SUBSTR(cBuffer,7) )
ENDIF
RETURN cName



function N_PropVal
PARAMETERS cObject,nType,cProperty,lClean,aList
PRIVATE cBuffer,xReturn,nSegment,nPos,cName,nObjtype
PRIVATE cId,cRequest,cList,isList
PRIVATE x,y,z

cBuffer  = SPACE(130)
xReturn  = ""
nSegment = 1
isList   = .F.

cRequest = CHR(61)+;
           CHR(0)+CHR(nType)+;
           Lstring(cObject,48)+;
           CHR(nSegment)+;
           Lstring(UPPER(cProperty),16)

IF type("lClean") <> "L"
   lClean = .T.
ENDIF

DO WHILE Netware(227, cRequest, @cBuffer) == 0
   IF ASC(SUBSTR(cBuffer,130,1)) >0
      IF nSegment == 1
         cList  = ""
         isList = .T.
         xReturn = 0
      ENDIF
      nPos = 1

      DO WHILE nPos < 128
        cId = Long2Fox(SUBSTR(cBuffer,nPos,4))  && Cteni identifikacniho cisla
        IF cId == 0                             && Zkouska konce segmentu
           exit
        ENDIF
        cName   = N_ObjectName(cId)    && Vyhledani nazvu a typu
        cList   = cList + cName+";"    && a vlozeni do navratoveho pole
        xReturn = xReturn +1
        nPos    = nPos + 4             && potom ukaze na dalsi identifikacni cislo
      ENDDO
   ELSE
      IF nSegment == 1
         xReturn = ""
      ENDIF
      xReturn = xReturn + left(cBuffer, 128)
   ENDIF
   nSegment = nSegment +1
   cRequest = STUFF(cRequest,53,1,CHR(nSegment))
ENDDO

DO CASE
CASE isList
   IF xReturn > 0
      DIMENSION aList[xReturn]
      z  = 1
      FOR x = 1 TO xReturn
         y = AT(";",cList,x)
         aList[x] = SUBSTR(cList,z,y-z)
         z = y+1
      NEXT
   ENDIF
CASE lClean and type("xReturn")="C"
   xReturn = CleanStr(xReturn)
ENDCASE

RETURN (xReturn)
function Fox2Long
parameter  nValue
return chr( nValue/65536)         +;
       chr( (nValue%65536)/ 4096) +;
       chr( (nValue%4096)/ 256)   +;
       chr( nValue % 256 )
