This is a NES emulator w/ 6502 (technically 2A03) CPU emulator.

I followed this guide here:  
[https://bugzmanov.github.io/nes_ebook](https://bugzmanov.github.io/nes_ebook)

I tried to implement most of the opcodes myself from scratch, but referenced the source implementation for things that we confusing (indirect mode JMP, and the ADC overflow flag are confusing).S