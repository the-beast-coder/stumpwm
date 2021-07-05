#!/bin/sh 
options="Shutdown\nRestart\nLog Out\nSuspend\nHibernate"
echo -e "$options" | dmenu -i
