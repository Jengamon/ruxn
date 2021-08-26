use std::rc::Rc;
use std::cell::{Cell, RefCell};
use thiserror::Error;

mod ops;

const MODE_RETURN: u8 = 0x40;
const MODE_KEEP: u8 = 0x80;
const ERRORS: [&'static str; 3] = ["underflow", "overflow", "division by zero"];

type MemoryBlock = Rc<Cell<[u8; 65536]>>;

#[derive(Debug, Clone, Copy)]
pub enum KeepMode {
    Keep,
    NoKeep,
}

#[derive(Debug, Clone, Copy)]
pub struct Stack {
    /// The current location in the stack
    ptr: u8,
    /// The last allocated position in the stack
    kptr: u8,
    /// An error code
    error: u8,
    /// Stack data
    data: [u8; 256],
}

#[derive(Debug, Clone)]
pub struct Memory {
    /// Current pointed location
    ptr: u16,
    /// Memory data
    data: MemoryBlock,
}

#[derive(Clone)]
pub struct Device {
    // We aren't using a Uxn* pointer, cuz
    // I don't want to unneccarily ref-count the main device,
    // so this design rn requires talk to accept a &mut Uxn instead.
    addr: u8,
    data: [u8; 16],
    mem: MemoryBlock,
    talk: Option<Rc<RefCell<dyn FnMut(&mut Uxn, &mut Device, u8, u8)>>>,
}

impl std::fmt::Debug for Device {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Device")
            .field("addr", &self.addr)
            .field("data", &self.data)
            .field("mem", &self.mem)
            .finish_non_exhaustive()
    }
}

pub struct Uxn {
    wst: Stack,
    rst: Stack,

    ram: Memory,

    dev: [Device; 16],
    ops: Vec<fn(&mut Uxn, &mut Stack, &mut Stack, KeepMode)>,
}

impl std::fmt::Debug for Uxn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Device")
            .field("wst", &self.wst)
            .field("rst", &self.rst)
            .field("ram", &self.ram)
            .field("dev", &self.dev)
            .finish_non_exhaustive()
    }
}

impl Stack {
    fn new() -> Stack {
        Stack {
            ptr: 0,
            kptr: 0,
            error:0,
            data: [0; 256]
        }
    }

    pub fn push8(&mut self, a: u8) {
        if self.ptr == 0xff {
            self.error = 2;
        } else {
            self.data[self.ptr as usize] = a;
            self.ptr += 1;
        }
        
    }

    pub fn pop8(&mut self, keep: KeepMode) -> u8 {
        match keep {
            KeepMode::Keep => {
                if self.kptr == 0 {
                    self.error = 1;
                    0
                } else {
                    self.kptr -= 1;
                    self.data[self.kptr as usize]
                }
            },
            KeepMode::NoKeep => {
                if self.ptr == 0 {
                    self.error = 1;
                    0
                } else {
                    self.ptr -= 1;
                    self.data[self.ptr as usize]
                }
            }
        }
    }

    pub fn push16(&mut self, a: u16) {
        self.push8((a >> 8) as u8);
        self.push8((a & 0xff) as u8);
    }

    pub fn pop16(&mut self, keep: KeepMode) -> u16 {
        let a = self.pop8(keep) as u16;
        let b = self.pop8(keep) as u16;
        a + (b << 8)
    }
}

impl Memory {
    fn new() -> Memory {
        Memory {
            ptr: 0,
            data: Rc::new(Cell::new([0u8; 65536]))
        }
    }

    pub fn mempoke8(&mut self, a: u16, b: u8) {
        let mut data = self.data.get();
        data[a as usize] = b;
        self.data.set(data);
    }

    pub fn mempeek8(&self, a: u16) -> u8 {
        self.data.get()[a as usize]
    }

    pub fn mempoke16(&mut self, a: u16, b: u16) {
        self.mempoke8(a, (b >> 8) as u8);
        self.mempoke8(a + 1, (b & 0xff) as u8);
    }

    pub fn mempeek16(&self, a_: u16) -> u16 {
        let a = self.mempeek8(a_) as u16;
        let b = self.mempeek8(a_ + 1) as u16;
        (a << 8) + b
    }
}

#[derive(Error, Debug)]
pub enum DeviceError {
    #[error("No device exists at address {0}")]
    NoDevice(u8),
}

impl Device {
    fn empty(id: u8, mem: MemoryBlock) -> Device {
        Device {
            addr: id * 0x10,
            data: [0; 16],
            mem,
            talk: None
        }
    }

    pub fn install<S, F>(&mut self, _name: S, talk: F) -> &mut Self 
        where
            S: AsRef<str>,
            F: 'static + FnMut(&mut Uxn, &mut Device, u8, u8)
    {
        self.talk = Some(Rc::new(RefCell::new(talk)));
        self
    }

    pub fn deinstall(&mut self) -> &mut Self {
        self.talk = None;
        // Clean out the data from the device
        self.data = [0; 16];
        self
    }

    pub fn devpoke8(&mut self, uxn: &mut Uxn, a: u8, b: u8) -> Result<(), DeviceError>{
        let mut talk = self.talk.take();
        let res = if let Some(ref mut talk) = talk {
            self.data[(a & 0xf) as usize] = b;
            talk.borrow_mut()(uxn, self, a & 0xf, 1);
            Ok(())
        } else {
            Err(DeviceError::NoDevice(self.addr))
        };
        self.talk = talk;
        res
    }

    pub fn devpeek8(&mut self, uxn: &mut Uxn, a: u8) -> Result<u8, DeviceError>{
        let mut talk = self.talk.take();
        let res = if let Some(ref mut talk) = talk {
            talk.borrow_mut()(uxn, self, a & 0xf, 0);
            Ok(self.data[(a & 0xf) as usize])
        } else {
            Err(DeviceError::NoDevice(self.addr))
        };
        self.talk = talk;
        res
    }

    pub fn devpoke16(&mut self, uxn: &mut Uxn, a: u8, b: u16) -> Result<(), DeviceError> {
        self.devpoke8(uxn, a, (b >> 8) as u8)?;
        self.devpoke8(uxn, a + 1, (b & 0xff) as u8)?;
        Ok(())
    }

    pub fn devpeek16(&mut self, uxn: &mut Uxn, a_: u8) -> Result<u16, DeviceError> {
        let a = self.devpeek8(uxn, a_)? as u16;
        let b = self.devpeek8(uxn, a_ + 1u8)? as u16;
        Ok((a << 8) + b)
    }
}

#[derive(Error, Debug)]
pub enum UxnError {
    #[error("Halted: {0} {1}#{2:04x}, at 0x{3:04x}")]
    Halted(&'static str, &'static str, isize, u16),
}

impl Uxn {
    pub fn new() -> Uxn {
        let ram = Memory::new();
        let dev = [
            Device::empty(0, ram.data.clone()),
            Device::empty(1, ram.data.clone()),
            Device::empty(2, ram.data.clone()),
            Device::empty(3, ram.data.clone()),
            Device::empty(4, ram.data.clone()),
            Device::empty(5, ram.data.clone()),
            Device::empty(6, ram.data.clone()),
            Device::empty(7, ram.data.clone()),
            Device::empty(8, ram.data.clone()),
            Device::empty(9, ram.data.clone()),
            Device::empty(10, ram.data.clone()),
            Device::empty(11, ram.data.clone()),
            Device::empty(12, ram.data.clone()),
            Device::empty(13, ram.data.clone()),
            Device::empty(14, ram.data.clone()),
            Device::empty(15, ram.data.clone()),
        ];
        Uxn {
            wst: Stack::new(),
            rst: Stack::new(),
            ram,
            dev,
            ops: vec![
                ops::op_lit,
                ops::op_inc,
                ops::op_pop,
                ops::op_dup,
                ops::op_nip,
                ops::op_swp,
                ops::op_ovr,
                ops::op_rot,

                ops::op_equ,
                ops::op_neq,
                ops::op_gth,
                ops::op_lth,
                ops::op_jmp,
                ops::op_jnz,
                ops::op_jsr,
                ops::op_sth,

                ops::op_pek,
                ops::op_pok,
                ops::op_ldr,
                ops::op_str,
                ops::op_lda,
                ops::op_sta,
                ops::op_dei,
                ops::op_deo,

                ops::op_add,
                ops::op_sub,
                ops::op_mul,
                ops::op_div,
                ops::op_and,
                ops::op_ora,
                ops::op_eor,
                ops::op_sft,

                ops::op_lit16,
                ops::op_inc16,
                ops::op_pop16,
                ops::op_dup16,
                ops::op_nip16,
                ops::op_swp16,
                ops::op_ovr16,
                ops::op_rot16,

                ops::op_equ16,
                ops::op_neq16,
                ops::op_gth16,
                ops::op_lth16,
                ops::op_jmp16,
                ops::op_jnz16,
                ops::op_jsr16,
                ops::op_sth16,

                ops::op_pek16,
                ops::op_pok16,
                ops::op_ldr16,
                ops::op_str16,
                ops::op_lda16,
                ops::op_sta16,
                ops::op_dei16,
                ops::op_deo16,
            ]
        }
    }

    pub fn eval(&mut self, vec: u16) -> Result<isize, UxnError> {
        eprintln!("=== MEMORY ===\n{:02x?}", self.ram.data.get());
        let mut instr;
        // vec == 0 means that the code is pointing to 0x00 which is illegal ig
        // self.dev[0].data[0xf] is accessing the last piece of data from the first device
        // IDK why...
        if vec == 0 || self.dev[0].data[0xf] != 0 {
            return Ok(0)
        }
        self.ram.ptr = vec;
        if self.wst.ptr > 0xf8 {
            self.wst.ptr = 0xf8;
        }
        let mut ram = self.ram.data.get();
        while ram[self.ram.ptr as usize] != 0 {
            instr = ram[self.ram.ptr as usize];
            self.ram.ptr += 1;
            /* Return Mode */
            let (ref mut src, ref mut dst) = {
                if (instr & MODE_RETURN) != 0 {
                    (self.rst, self.wst)
                } else {
                    (self.wst, self.rst)
                }
            };
            /* Keep Mode */
            let keep = if (instr & MODE_KEEP) != 0 {
                src.kptr = src.ptr;
                KeepMode::Keep
            } else {
                KeepMode::NoKeep
            };
            let OP_NAMES = [
                "lit","inc","pop","dup","nip","swp","ovr","rot",
                "equ","neq","gth","lth","jmp","jnz","jsr","sth", 
                "pek","pok","ldr","str","lda","sta","dei","deo", 
                "add","sub","mul","div","and","ora","eor","sft", 
            ];

            fn find_flags(instr: u8) -> String {
                let mut flags = "".to_string();
                if (instr & 0x20) != 0 {
                    flags += "2";
                }
                if (instr & MODE_KEEP) != 0 {
                    flags += "k";
                }
                if(instr & MODE_RETURN) != 0 {
                    flags += "r";
                }
                flags
            }

            eprintln!("{:02x} {}{} {:02x?} {:02x?}", instr, OP_NAMES[(instr&0x1f) as usize], find_flags(instr),&src.data[..src.ptr as usize], &dst.data[..dst.ptr as usize]);
            (self.ops[(instr & 0x3f) as usize])(self, src, dst, keep);
            ram = self.ram.data.get();
            if self.wst.error != 0 {
                return Err(UxnError::Halted("Working-stack", ERRORS[(self.wst.error - 1) as usize], instr.into(), self.ram.ptr));
            }
            if self.rst.error != 0 {
                return Err(UxnError::Halted("Return-stack", ERRORS[(self.rst.error - 1) as usize], instr.into(), self.ram.ptr));
            }

            // Put them stacks back
            if (instr & MODE_RETURN) != 0 {
                self.rst = *src;
                self.wst = *dst;
            } else {
                self.wst = *src;
                self.rst = *dst;
            }
        }
        Ok(1)
    }
}

use std::path::Path;
const PAGE_PROGRAM: u16 = 0x0100;

fn load<P: AsRef<Path>>(uxn: &mut Uxn, path: P) -> std::io::Result<()> {
    use std::io::Read;
    let mut file = std::fs::File::open(path)?;
    let mut data = [0; 65536 - PAGE_PROGRAM as usize];
    file.read(&mut data)?;
    let mut ram = uxn.ram.data.get();
    for (i, datp) in data.iter().enumerate() {
        ram[i + PAGE_PROGRAM as usize] = *datp;
    }
    uxn.ram.data.set(ram);
    Ok(())
}

fn run(uxn: &mut Uxn) -> Result<(), UxnError> {
    uxn.eval(PAGE_PROGRAM)?;
    // TODO FIgure out what the hell this code means
    //while((uxn.dev[0].dat[0xf] != 0) && )
    Ok(())
}

fn main() -> anyhow::Result<()> {
    // Do setup
    // TODO Take an argument to a uxn input file (an assembled script)
    let mut uxn = Uxn::new();
    load(&mut uxn, "./left.rom")?;
    uxn.dev[0].install("system", |uxn, dev, b0, w| {
        if w == 0 {
            match b0 {
                0x2 => {
                    dev.data[0x2] = uxn.wst.ptr;
                    dev.data[0x3] = uxn.rst.ptr;
                }
            }
        } else {

        }
    });
    run(&mut uxn)?;
    Ok(())
}
