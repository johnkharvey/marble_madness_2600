# Makefile for Steam Tunnel Bob

#==================
# Global Variables
#==================
DASM_VERSION=2.20.12
STELLA_VERSION=6.0.2
MARBLE_VERSION=2020_01_11

#=========
# Aliases
#=========
DASM=bin/dasm/dasm
WGET=/usr/local/bin/wget
TAR=/usr/bin/tar
STELLA=/Applications/Stella.app/Contents/MacOS/Stella

#===============
# Build Targets
#===============
.PHONY:	all prechecks run clean megaclean

all:	bin/marble_v${MARBLE_VERSION}_ntsc.bin bin/marble_v${MARBLE_VERSION}_pal.bin

prechecks:	${DASM}

${WGET}:
	brew install wget

${DASM}:	${WGET} ${TAR}
	mkdir -p bin/dasm
	wget -O bin/dasm/dasm.tar.gz https://github.com/dasm-assembler/dasm/releases/download/${DASM_VERSION}/dasm-${DASM_VERSION}-osx-x64.tar.gz
	tar xvf bin/dasm/dasm.tar.gz --directory bin/dasm/
        # Do a touch so it doesn't keep appearing out-of-date
	find bin/dasm/ -type f -exec touch {} +
	rm -f bin/dasm/dasm.tar.gz

bin/marble_v${MARBLE_VERSION}_ntsc.bin:	marble.asm	prechecks
	${DASM} marble.asm -f3 -obin/marble_v${MARBLE_VERSION}_ntsc.bin

bin/marble_v${MARBLE_VERSION}_pal.bin:	marble.asm prechecks
	perl -pi -e "s:NTSC = 1:NTSC = 0:g" marble.asm
	${DASM} marble.asm -f3 -obin/marble_v${MARBLE_VERSION}_pal.bin
	perl -pi -e "s:NTSC = 0:NTSC = 1:g" marble.asm

#===============
# Run Targets
#===============
${STELLA}:	${WGET}
	mkdir -p tmp/
	wget -O tmp/stella.dmg https://github.com/stella-emu/stella/releases/download/${STELLA_VERSION}/Stella-${STELLA_VERSION}-macos.dmg
	cd tmp && sudo hdiutil attach stella.dmg
	cp -r /Volumes/Stella/Stella.app /Applications/
	sleep 1
	sudo hdiutil detach /Volumes/Stella
	rm -rf tmp

run:	${STELLA}
	${STELLA} bin/marble_v${MARBLE_VERSION}_ntsc.bin

#===============
# Clean Targets
#===============
clean:
	rm -f bin/marble_v${MARBLE_VERSION}_ntsc.bin
	rm -f bin/marble_v${MARBLE_VERSION}_pal.bin
	rm -rf tmp

megaclean:	clean
	brew uninstall wget
	rm -rf bin/dasm
	rm -rf /Applications/Stella.app
