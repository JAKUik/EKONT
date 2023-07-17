@ECHO OFF
CLS
ECHO.
ECHO Uschoval jsi pýedchoz¡ verzi programu ?   A vçichni se odhl sili ?
ECHO.
PAUSE

C:\WINDOWS\system32\xcopy.exe *.BAT D:\ekont\*.* /s /y
C:\WINDOWS\system32\xcopy.exe *.FR* D:\ekont\*.* /s /y
C:\WINDOWS\system32\xcopy.exe *.FXP D:\ekont\*.* /s /y
C:\WINDOWS\system32\xcopy.exe *.SCX D:\ekont\*.* /s /y
C:\WINDOWS\system32\xcopy.exe *.SPX D:\ekont\*.* /s /y
C:\WINDOWS\system32\xcopy.exe *.SCT D:\ekont\*.* /s /y
C:\WINDOWS\system32\xcopy.exe *.SPR D:\ekont\*.* /s /y
C:\WINDOWS\system32\xcopy.exe *.BGR D:\ekont\*.* /s /y
C:\WINDOWS\system32\xcopy.exe *.COM D:\ekont\*.* /s /y
C:\WINDOWS\system32\xcopy.exe Novinky.TEX D:\ekont\*.* /s /y
C:\WINDOWS\system32\xcopy.exe DATA\v?r_*.dbf D:\ekont\DATA\*.* /y
C:\WINDOWS\system32\xcopy.exe DATA\N_*.dbf D:\ekont\DATA\*.* /y
C:\WINDOWS\system32\xcopy.exe *.CLR D:\ekont\*.* /s /y
C:\WINDOWS\system32\xcopy.exe *.PLB D:\ekont\*.* /s /y
C:\WINDOWS\system32\xcopy.exe *.LC? D:\ekont\*.* /s /y

REM C:\WINDOWS\system32\xcopy.exe *.PRG D:\ekont\*.* /s /y
