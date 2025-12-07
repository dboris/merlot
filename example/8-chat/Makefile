# Chat Room Example
#
# Demonstrates multi-module project with cross-module calls:
# - message.ml: Message types and formatting
# - user.ml: User actor (depends on message)
# - room.ml: Room actor (depends on message, user)
# - chat_server.ml: Main entry (depends on all)

# Run compiler from project root so it finds stdlib
MERLOT_ROOT = $(shell cd ../.. && pwd)
MERLOT = $(MERLOT_ROOT)/_build/default/bin/main.exe
STDLIB = $(MERLOT_ROOT)/stdlib
PRIV = $(MERLOT_ROOT)/priv
THIS_DIR = example/8-chat

.PHONY: all clean run

all: Merlot.Message.beam Merlot.User.beam Merlot.Room.beam Merlot.Chat_server.beam merlot_actor.beam
	@echo "Built 4 modules + runtime support"

# Compile each module from project root (so stdlib is found)
Merlot.Message.beam: message.ml
	cd $(MERLOT_ROOT) && $(MERLOT) $(THIS_DIR)/$<

Merlot.User.beam: user.ml Merlot.Message.beam
	cd $(MERLOT_ROOT) && $(MERLOT) -I $(THIS_DIR) $(THIS_DIR)/$<

Merlot.Room.beam: room.ml Merlot.Message.beam Merlot.User.beam
	cd $(MERLOT_ROOT) && $(MERLOT) -I $(THIS_DIR) $(THIS_DIR)/$<

Merlot.Chat_server.beam: chat_server.ml Merlot.Message.beam Merlot.User.beam Merlot.Room.beam
	cd $(MERLOT_ROOT) && $(MERLOT) -I $(THIS_DIR) $(THIS_DIR)/$<

# Runtime support module
merlot_actor.beam: $(PRIV)/merlot_actor.erl
	erlc $<

run: all
	erl -noshell -pa . -pa $(STDLIB) \
		-eval "'Merlot.Chat_server':main(ok), halt()."

clean:
	rm -f *.core *.beam *.cmi *.cmt Merlot.*.core Merlot.*.beam erl_crash.dump
