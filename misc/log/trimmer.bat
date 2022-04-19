@echo off

cd /d "%~dp0"

if not exist LogTrimmer.class (
    javac LogTrimmer.java
)

java LogTrimmer
