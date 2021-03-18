SHELL           = /bin/sh

INCFLAG = -I
INCPATH += $(INCFLAG).
FCFLAGS += -g

SRCPATH = ../../src
HASHPATH = $(SRCPATH)

HASHOBJS = ccpp_hashable.o ccpp_hash_table.o

# Make sure we have a log file
ifeq ($(LOGFILE),)
LOGFILE := ccpp_test.log
endif

# TARGETS

ccpp_hashable.o: $(HASHPATH)/ccpp_hashable.F90
	@echo "${FC} -c ${FCFLAGS} ${INCPATH} $^" 2>&1 >> $(LOGFILE)
	@${FC} -c ${FCFLAGS} ${INCPATH} $^ 2>&1 >> $(LOGFILE)

ccpp_hash_table.o: $(HASHPATH)/ccpp_hash_table.F90
	@echo "${FC} -c ${FCFLAGS} ${INCPATH} $^" 2>&1 >> $(LOGFILE)
	@${FC} -c ${FCFLAGS} ${INCPATH} $^ 2>&1 >> $(LOGFILE)

test_hash_table: test_hash.F90 $(HASHOBJS)
	@echo "${FC} ${FCFLAGS} ${INCPATH} -o $@ $^" 2>&1 >> $(LOGFILE)
	@${FC} ${FCFLAGS} ${INCPATH} -o $@ $^ 2>&1 >> $(LOGFILE)

test: test_hash_table
	@echo "Run Hash Table Tests"
	@./test_hash_table

# CLEAN
clean:
	@rm -f *.o *.mod ccpp_test.log
	@rm -f test_hash_table
