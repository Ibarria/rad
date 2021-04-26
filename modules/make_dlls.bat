@echo Making the Basic DLL
@cl -nologo  /Zi /Od /LDd Basic.cpp /Def Basic.def /link kernel32.lib user32.lib /entry:DllMain
@cl -nologo -c start_win.c 

@REM /nodefaultlib

@REM @echo Making the GL DLL
