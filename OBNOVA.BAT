@ECHO OFF
CLS
IF "%2" == "" GOTO EXIT
REM
REM     Parametry:      %1 - OznaŸen¡ disketov‚ mechaniky
REM                     %2 - N zev adres ýe obnovovan‚ firmy
REM                     %3 - Extenze adres ýe obnovovan‚ firmy s teŸkou
REM
IF NOT EXIST %1\%2%3\%2.ARJ GOTO CHYBA
ECHO.
ECHO Deactivuji indexov‚ a star‚ soubory
DEL DATA\%2%3\*.CDX >NUL
DEL DATA\%2%3\*.BAK >NUL
ECHO.
ECHO Ukl d m star‚ datab ze pod extenzi   BAK
REN DATA\%2%3\*.DBF *.BAK
IF EXIST DATA\%2%3\*.FPT REN DATA\%2%3\*.FPT DATA\%2%3\*.baf
REM REN DATA\%2%3\POZNAMKY.TEX *.BAK >NUL
ECHO.
ECHO Obnovuji soubory firmy  -  %2%3
ECHO.
CD Data
ARJ x %1\%2%3\%2.arj %2%3\*.* -i1 -y -va
CD ..
IF NOT EXIST DATA\%2%3\DENIK.DBF REN DATA\%2%3\*.BAK *.DBF
IF NOT EXIST DATA\%2%3\*.FPT REN DATA\%2%3\*.baf DATA\%2%3\*.FPT
ECHO.
ECHO Vìçe uveden‚ soubory byly obnoveny.
ECHO.
PAUSE
GOTO EXIT
:CHYBA
ECHO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ECHO !!!  Nemohu naj¡t soubory k obnovØ  !!!
ECHO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
PAUSE
:EXIT
