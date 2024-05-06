ERLC = erlc
EBIN_DIR = ebin
SRC_DIR = src

SOURCES = $(wildcard $(SRC_DIR)/*.erl)

BEAMS = $(patsubst $(SRC_DIR)/%.erl,$(EBIN_DIR)/%.beam,$(SOURCES))

all: $(BEAMS)

$(EBIN_DIR)/%.beam: $(SRC_DIR)/%.erl
	$(ERLC) -o $(EBIN_DIR) $<

clean:
	rm -f $(EBIN_DIR)/*.beam

run:
	erl -pa $(EBIN_DIR) -eval 'main:run().'