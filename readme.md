Install rust (latest stable): https://www.rust-lang.org/tools/install  

Get test rom from https://github.com/retrio/gb-test-roms/raw/master/cpu_instrs/cpu_instrs.gb

Building:

```shell
cargo build
```

Running:  
(Will look for `cpu_instrs.gb` in execution path)
```shell
cargo run
```

Keybindings:
- esc -> toggle ui
- wasd -> direction pad
- j,k -> b,a
- u,i -> select, start