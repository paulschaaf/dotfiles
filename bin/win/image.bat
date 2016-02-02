@echo off

goto %2

:linux
	call %0 %1 boot
	call %0 %1 swap
	call %0 %1 root
	call %0 %1 home
	goto end

:win2k
	call %0 %1 win2kos
	call %0 %1 apps
	goto end

rem **** Partition info

:dos622
	call ld_or_sv %1 %2 1:1
	goto end

:win2kos
	call ld_or_sv %1 %2 1:2
	goto end

:games
	call ld_or_sv %1 %2 1:3
	goto end

:boot
	call ld_or_sv %1 %2 1:4
	goto end

:swap
	call ld_or_sv %1 %2 1:6
	goto end

:share
	call ld_or_sv %1 %2 2:1
	goto end

:apps
	call ld_or_sv %1 %2 2:2
	goto end

:data
   call ld_or_sv %1 %2 2:3
	goto end

:root
	call ld_or_sv %1 %2 2:4
	goto end

:home
	call ld_or_sv %1 %2 2:5
	goto end

:end
echo 
