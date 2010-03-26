#######################################################################
# Generic make script for compiling erlang code                       #
# The environment variable $ERLHOME has to be set to where erlang/OTP #
# is installed                                                        #
# Compiles the code into a ebin dir. relative to the source dir.      #
####################################################################### 
#Compiles the code into a ebin dir. relative to the source dir. 

VERSION := 0.0.1
EUNIT	:= /usr/lib/erlang/lib/eunit
YAWS	:= /usr/lib/yaws
XMERL	:= /usr/lib/erlang/lib/xmerl
STDLIB	:= /usr/lib/erlang/lib/stdlib
ERLC 	:= erlc
GEN 	:= beam

EFLAGS := -pa ebin +debug_info

INCLUDE := -Iinclude -I$(EUNIT) -I$(YAWS)/include -I$(XMERL) -I$(STDLIB)
EBIN 	:= ebin
SRC     := $(wildcard src/*.erl)
HEADERS := $(wildcard $(INCLUDE)/*.hrl)	
CODE  	:= $(patsubst src/%.erl, ebin/%.beam, $(SRC))
DOTSRC  := $(wildcard src/*.app.src)
DOTAPP  := $(patsubst src/%.app.src, ebin/%.app, $(DOTSRC))

.PHONY: clean all

$(EBIN)/%.beam: src/%.erl
	$(ERLC) $(INCLUDE) -pa $(EUNIT)/ebin -W -b beam -o $(EBIN) $(EFLAGS) $(WAIT) $<

all: $(CODE) 

clean:
	rm -f $(EBIN)/*.beam
	rm -f ./*.script
	rm -f ./*.boot
	rm -f ./*.dump

boot:
	erl -pa ./ebin/ -noshell -run systools make_script yeref-$(VERSION) -run init stop

package: all
	cp yeref.rel yeref-$(VERSION).rel
	mkdir yeref-$(VERSION)
	cp -rf src yeref-$(VERSION)
	cp -rf ebin yeref-$(VERSION)
	cp -rf include src yeref-$(VERSION)
	cp -rf priv src yeref-$(VERSION)
	cp -rf Makefile yeref-$(VERSION)
	cp -rf yeref-$(VERSION).rel yeref-$(VERSION)
	cp -rf ktuo*.tar.gz yeref-$(VERSION)
	cp -rf rotating_logger*.tar.gz yeref-$(VERSION)
	cp -rf emongo*.tar.gz yeref-$(VERSION)
	tar czf yeref-$(VERSION).tar.gz yeref-$(VERSION)
	mv yeref-$(VERSION).tar.gz ../
	rm -rf yeref-$(VERSION)
	rm yeref-$(VERSION).rel
