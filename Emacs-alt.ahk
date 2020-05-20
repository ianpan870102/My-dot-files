#NoEnv  ; Recommended for performance and compatibility with future AutoHotkey releases.
#Warn  ; Enable warnings to assist with detecting common errors.
SendMode Input  ; Recommended for new scripts due to its superior speed and reliability.
SetWorkingDir %A_ScriptDir%  ; Ensures a consistent starting directory.

; #SingleInstance force

Capslock::Esc

!b::
Send, {left}
return

!f::
Send, {right}
return

!n::
Send, {down}
return

!p::
Send, {up}
return

!a::
Send, {home}
return

!e::
Send, {end}
return

!d::
Send, {delete}
return

!t::
Send, +{left}
Send, ^x
Send, {right}
Send, ^v
return

!k::
Send, +{end}
Send, {delete}
return

!x::
Send, ^x
return

!c::
Send, ^c
return

!g::
Send, ^g
return


!h::
Send, ^h
return

!s::
Send, ^s
return

!w::
Send, ^w
return

!o::
Send, ^o
return

;; Below is the same mod except for AltGr too

<^>!b::
Send, {left}
return

<^>!f::
Send, {right}
return

<^>!n::
Send, {down}
return

<^>!p::
Send, {up}
return

<^>!a::
Send, {home}
return

<^>!e::
Send, {end}
return

<^>!d::
Send, {delete}
return

<^>!t::
Send, +{left}
Send, ^x
Send, {right}
Send, ^v
return

<^>!k::
Send, +{end}
Send, {delete}
return

<^>!x::
Send, ^x
return

<^>!c::
Send, ^c
return

<^>!g::
Send, ^g
return

<^>!h::
Send, ^h
return

<^>!s::
Send, ^s
return

<^>!w::
Send, ^w
return

<^>!o::
Send, ^o
return
