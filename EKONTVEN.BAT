@ECHO OFF
CLS
MKDIR C:\Ekontven

m-x copy ZALOHA.BAT C:\ekontven\*.*
m-x copy OBNOVA.BAT C:\ekontven\*.*
m-x copy *.FR* C:\ekontven\*.* /s
m-x copy *.FXP C:\ekontven\*.* /s
m-x copy *.PRG C:\ekontven\*.* /s
m-x copy *.PLB C:\ekontven\*.* /s
m-x copy *.SCX C:\ekontven\*.* /s
m-x copy *.SPX C:\ekontven\*.* /s
m-x copy *.SCT C:\ekontven\*.* /s
m-x copy *.SPR C:\ekontven\*.* /s
m-x copy *.BGR C:\ekontven\*.* /s
m-x copy *.COM C:\ekontven\*.* /s
m-x copy *.EXE C:\ekontven\*.* /s
m-x copy *.TEX C:\ekontven\*.* 
m-x copy Helpik.* C:\ekontven\*.* /s
m-x copy DATA\v?r_*.dbf C:\ekontven\DATA\*.*
m-x copy DATA\N_*.dbf C:\ekontven\DATA\*.*
m-x copy DATA\c*.dbf C:\ekontven\DATA\*.*
m-x copy DATA\Set*.dbf C:\ekontven\DATA\*.*
m-x copy HTML\*.* C:\ekontven\HTML\*.*
m-x copy *.CLR C:\ekontven\*.* /s
m-x copy *.LC? C:\ekontven\*.* /s
m-x copy *.ENI C:\ekontven\*.* /s
m-x copy *.PLB C:\ekontven\*.* /s

ECHO.
ECHO.
ECHO !!! Pøelož všechny moduly !!!
ECHO.
PAUSE
