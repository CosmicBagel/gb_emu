# gb_emu

## Usage:
Running a release:
```
gb_emu.exe a_cool_rom.gb
```
Or use GUI to select and load rom.

Keybindings:
- esc -> toggle ui 
- wasd -> direction pad
- j,k -> b,a
- u,i -> select, start
- shift -> turbo

Yes, supports gamepad (plus triggers for turbo)

---
## Development:

Install rust (latest stable): https://www.rust-lang.org/tools/install  

Get test rom from https://github.com/retrio/gb-test-roms/raw/master/cpu_instrs/cpu_instrs.gb

Building:

```shell
cargo build
```

Running:  
```shell
cargo run cpu_instrs.gb
```

