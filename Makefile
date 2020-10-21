.phony: all, clean

SOURCE_DIR=src
ASSETS_DIR=docs
JS_DIR=${ASSETS_DIR}/js
ALL_ELM_FILES=$(shell find ${SOURCE_DIR} -name *.elm)

all: ${JS_DIR}/Game.min.js ${JS_DIR}/Game.debug.js ${ASSETS_DIR}/debug.html  
	@echo "finished building frontend application"

${JS_DIR}/%.min.js: ${JS_DIR}/%.js
	uglifyjs $< --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output $@

${JS_DIR}/%.debug.js: ${SOURCE_DIR}/%.elm ${ALL_ELM_FILES}
	elm make $< --debug --output=$@

${JS_DIR}/%.js: ${SOURCE_DIR}/%.elm ${ALL_ELM_FILES}
	elm make $< --optimize --output=$@

${ASSETS_DIR}/debug.html: ${ASSETS_DIR}/index.html
	sed 's/Game.min.js/Game.debug.js/' $< > $@

clean:
	rm -f ${JS_DIR}/Game.min.js ${JS_DIR}/Game.debug.js