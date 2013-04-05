



one:  two three
	$(eval PIPE := $(shell hydra-head -p))
	echo "Task one start." >> $(PIPE)
	sleep 1 
	echo "Task one finished." >> $(PIPE)

two:
	$(eval PIPE := $(shell hydra-head -p))
	echo "Task two start!" >> $(PIPE)
	sleep 2
	echo "Task two finished!" >> $(PIPE)


three:
	$(eval PIPE := $(shell hydra-head -p))
	echo "Task three start!" >> $(PIPE)
	sleep 3
	echo "Task three finished!" >> $(PIPE)

