all: d devbot_list devbot devbot_status

HS = "$(shell which ghc)"

CFLAGS  = -Wall -Wextra
HFLAGS  = -Wall -Wno-type-defaults -Wno-missing-signatures -Wno-name-shadowing
HFLAGS += -dynamic -lpthread

devbot: Devbot/bot.hs Apocrypha/Client.hs Apocrypha/Network.hs Devbot/Core.hs
ifneq (${HS}, "")
	ghc --make ${HFLAGS} Devbot/bot.hs -o $@
else
	touch devbot
endif

devbot_list: json.hpp devbot.cpp Devbot/list.hs Apocrypha/Network.hs
ifneq (${HS}, "")
	ghc --make ${HFLAGS} Devbot/list.hs -o $@
else
	g++ -std=c++11 ${CFLAGS} devbot.cpp -o $@
endif

devbot_status: Devbot/status.hs status.sh
ifneq (${HS}, "")
	ghc --make ${HFLAGS} Devbot/status.hs -o $@
else
	cp status.sh $@
endif


d: d.c Apocrypha/d.hs Apocrypha/Network.hs
ifneq (${HS}, "")
	ghc --make ${HFLAGS} Apocrypha/d.hs -o $@
else
	gcc -std=c99 ${CFLAGS} d.c -o $@
endif

json.hpp:
	wget --quiet https://github.com/nlohmann/json/releases/download/v3.1.2/json.hpp

clean: tidy
	-rm -f d devbot devbot_list devbot_status
	@echo Done

tidy:
	find . -name '*.hi' -exec rm '{}' ';'
	find . -name '*.o'  -exec rm '{}' ';'
