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

---
## Linux notes:

- Requires vulkan support ~~(will not work in WSL2)~~
  - For WSL2 install `vulkan-tools` before building
- Requires dev libraries `libglib2.0-dev libgtk-3-dev libudev-dev`
  - eg `apt install libglib2.0-dev libgtk-3-dev libudev-dev` on Ubuntu 22.04 LTS
- Your milage may vary depending on distro `¯\_(ツ)_/¯`

