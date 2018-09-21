all: d devbot_list devbot

CFLAGS = -Wall -Wextra
HFLAGS = -Wall -Wno-type-defaults -Wno-missing-signatures -Wno-name-shadowing

devbot: bot.hs Apocrypha/Client.hs Apocrypha/Network.hs Devbot/Core.hs
	ghc --make ${HFLAGS} bot.hs -o $@

devbot_list: json.hpp devbot.cpp list.hs Apocrypha/Network.hs
ifneq ("$(shell which ghc)","")
	ghc --make ${HFLAGS} list.hs -o $@
else
	g++ -std=c++11 ${CFLAGS} devbot.cpp -o $@
endif

d: d.c d.hs Apocrypha/Network.hs
ifneq ("$(shell which ghc)","")
	ghc --make ${HFLAGS} d.hs -o $@
else
	gcc -std=c99 ${CFLAGS} d.c -o $@
endif

json.hpp:
	wget --quiet https://github.com/nlohmann/json/releases/download/v3.1.2/json.hpp

clean:
	-rm -f *.o d devbot_list *.hi
	@echo Done
