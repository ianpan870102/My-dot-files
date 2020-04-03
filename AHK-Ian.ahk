#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
; #Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

#SingleInstance force

Capslock::Esc

!n::
Send, {down}
return

!p::
Send, {up}
return

!b::
Send, {left}
return

!f::
Send, {right}
return

!a::
Send, {home}
return

!e::
Send, {end}
return

!t::
Send, +{left}
Send, ^x
Send, {right}
Send, ^v
return

!k::
Send, +{end}
Send, ^x
return
