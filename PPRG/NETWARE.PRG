***********************************************************
* Program:  Netware.prg                                   *
*  Autori:  Joseph D. Booth a Greg Lief                   *
*    Ucel:  Rozhrani mezi NetWare a FoxPro                *
***********************************************************
function Netware
parameters nService,cRequest,cReply
dimension aRegs[10]
private nReturn

nReturn = -1

aRegs[ 1] = nService * 256
aRegs[ 8] = chr(len(cRequest))+chr(0) + cRequest
aRegs[ 5] = .T.
aRegs[ 9] = chr(len(cReply))+chr(0) + cReply
aRegs[ 6] = .F.
if fp_int86( 33, @aRegs )
   nReturn = aRegs[1] % 256
   if nReturn < 0
      nReturn = nReturn +256
   endif
endif
cReply = substr(aRegs[9],3)
return nReturn
*********************************************************