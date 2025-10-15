@echo off
REM Check if filename argument is provided
if "%1"=="" (
  echo Usage: tata filename.tts
  exit /b 1
)

REM Run SWI-Prolog with the provided filename
swipl -f src\finalExecute.pl -g run -g halt -- %1