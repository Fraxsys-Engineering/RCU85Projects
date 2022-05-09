## Source and output name, change to match the assembly filename
NAME := foo

## Memory region Definition
# - Config1 : 256 bytes RAM @ 0x0000
MEMSTART := 0000
MEMLEN   := 0100

AS := asm85

all: $(NAME)

clean:
	rm -rf *.lst *.hex

cleanall:
	rm -rf *.lst *.hex *.bin

$(NAME): $(NAME).hex

%.hex: %.asm
	$(AS) -b $(MEMSTART):$(MEMLEN) $<

