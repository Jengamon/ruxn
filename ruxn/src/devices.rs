use crate::{Uxn, Stack, Device};

fn inspect(wst: &Stack) {
    println!("\n");
    for y in 0..0x08 {
        for x in 0..0x08 {
            let p = y * 0x08 + x;
            eprintln!("{}", if p == wst.ptr.get() {
                format!("[{:02x}]", wst.data.get()[p as usize])
            } else {
                format!(" {:02x} ", wst.data.get()[p as usize])
            });
        }
        eprintln!();
    }
}

pub fn system_talk(uxn: &mut Uxn, dev: &mut Device, b0: u8, w: u8) {
    if w == 0 {
        match b0 {
            0x2 => dev.data[0x2] = uxn.wst.ptr.get(),
            0x3 => dev.data[0x3] = uxn.rst.ptr.get(),
            _ => {}
        }
    } else {
        match b0 {
            0x2 => uxn.wst.ptr.set(dev.data[0x2]),
            0x3 => uxn.rst.ptr.set(dev.data[0x3]),
            0xe => inspect(&uxn.wst),
            0xf => uxn.ram.ptr = 0x0000,
            _ => {}
        }
    }
}