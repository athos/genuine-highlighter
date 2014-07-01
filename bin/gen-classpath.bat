@echo off

for /f "usebackq tokens=*" %%p in (`lein classpath`) do set CLPATH=%%p;leiningen
echo %CLPATH% > .lein-classpath
