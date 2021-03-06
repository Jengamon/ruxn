use crate::KeepMode;

use super::{Stack, Uxn};

/* Stack */
pub fn op_lit(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, _keep: KeepMode) {
    let dat = uxn.ram.mempeek8(uxn.ram.ptr);
    uxn.ram.ptr += 1;
    src.push8(dat);
}

pub fn op_inc(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.push8(a.wrapping_add(1));
}

pub fn op_pop(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    src.pop8(keep);
}

pub fn op_dup(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.push8(a);
    src.push8(a);
}

pub fn op_nip(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.pop8(keep);
    src.push8(a);
}

pub fn op_swp(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(a);
    src.push8(b);
}

pub fn op_ovr(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b); src.push8(a); src.push8(b);
}

pub fn op_rot(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    let c = src.pop8(keep);
    src.push8(b); src.push8(a); src.push8(c);
}
/* Logic */
pub fn op_equ(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8((b == a) as u8);
}

pub fn op_neq(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8((b != a) as u8);
}

pub fn op_gth(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8((b > a) as u8);
}

pub fn op_lth(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8((b < a) as u8);
}

pub fn op_jmp(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    uxn.ram.ptr = (uxn.ram.ptr as i16 + a as i8 as i16) as u16;
}

pub fn op_jnz(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    if src.pop8(keep) != 0 {
        uxn.ram.ptr = (uxn.ram.ptr as i16 + a as i8 as i16) as u16;
    }
}

pub fn op_jsr(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    dst.push16(uxn.ram.ptr);
    uxn.ram.ptr = (uxn.ram.ptr as i16 + a as i8 as i16) as u16;
}

pub fn op_sth(_uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    dst.push8(a);
}
/* Memory */
pub fn op_pek(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.push8(uxn.ram.mempeek8(a as u16));
}

pub fn op_pok(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    uxn.ram.mempoke8(a as u16, b);
}

pub fn op_ldr(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.push8(uxn.ram.mempeek8(uxn.ram.ptr + a as u16));
}

pub fn op_str(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    uxn.ram.mempoke8(uxn.ram.ptr + a as u16, b);
}

pub fn op_lda(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    src.push8(uxn.ram.mempeek8(a));
}

pub fn op_sta(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop8(keep);
    uxn.ram.mempoke8(a, b);
}

pub fn op_dei(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let mut dev = uxn.dev[(a >> 4) as usize].clone();
    // Uses unwrap, can crash here
    match dev.devpeek8(uxn, a) {
        Ok(n) => src.push8(n),
        Err(e) => eprintln!("Device Error: {}", e)
    };
    uxn.dev[(a >> 4) as usize] = dev;
}

pub fn op_deo(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    let mut dev = uxn.dev[(a >> 4) as usize].clone();
    match dev.devpoke8(uxn, a, b) {
        Ok(()) => (),
        Err(e) => eprintln!("Device Error: {}", e)
    };
    uxn.dev[(a >> 4) as usize] = dev;
}
/* Arithmetic */
pub fn op_add(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b.wrapping_add(a));
}

pub fn op_sub(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b.wrapping_sub(a));
}

pub fn op_mul(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b.wrapping_mul(a));
}

pub fn op_div(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let mut a = src.pop8(keep);
    let b = src.pop8(keep);
    if a == 0 {
        src.error = 3;
        a = 1;
    }
    src.push8(b.wrapping_div(a));
}

pub fn op_and(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b & a);
}

pub fn op_ora(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b | a);
}

pub fn op_eor(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b ^ a);
}

pub fn op_sft(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b >> (a & 0x07) << ((a & 0x70) >> 4));
}

/* Stack 16bit */
pub fn op_lit16(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, _keep: KeepMode) {
    let dat = uxn.ram.mempeek16(uxn.ram.ptr);
    uxn.ram.ptr += 2;
    src.push16(dat);
}

pub fn op_inc16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    src.push16(a.wrapping_add(1));
}

pub fn op_pop16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    src.pop16(keep);
}

pub fn op_dup16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    src.push16(a);
    src.push16(a);
}

pub fn op_nip16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    src.pop16(keep);
    src.push16(a);
}

pub fn op_swp16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push16(a);
    src.push16(b);
}

pub fn op_ovr16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push16(b); src.push16(a); src.push16(b);
}

pub fn op_rot16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    let c = src.pop16(keep);
    src.push16(b); src.push16(a); src.push16(c);
}
/* Logic 16bit */
pub fn op_equ16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push8((b == a) as u8);
}

pub fn op_neq16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push8((b != a) as u8);
}

pub fn op_gth16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push8((b > a) as u8);
}

pub fn op_lth16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push8((b < a) as u8);
}

pub fn op_jmp16(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    uxn.ram.ptr = a;
}

pub fn op_jnz16(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    if src.pop8(keep) != 0 {
        uxn.ram.ptr = a;
    }
}

pub fn op_jsr16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    dst.push16(uxn.ram.ptr);
    uxn.ram.ptr = src.pop16(keep);
}

pub fn op_sth16(_uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    dst.push16(a);
}

/* Memory 16bit */
pub fn op_pek16(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.push16(uxn.ram.mempeek16(a as u16));
}

pub fn op_pok16(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop16(keep);
    uxn.ram.mempoke16(a as u16, b);
}

pub fn op_ldr16(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.push16(uxn.ram.mempeek16(uxn.ram.ptr + a as u16));
}

pub fn op_str16(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop16(keep);
    uxn.ram.mempoke16(uxn.ram.ptr + a as u16, b);
}

pub fn op_lda16(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    src.push16(uxn.ram.mempeek16(a));
}

pub fn op_sta16(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    uxn.ram.mempoke16(a, b);
}

pub fn op_dei16(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let mut dev = uxn.dev[(a >> 4) as usize].clone();
    match dev.devpeek16(uxn, a) {
        Ok(n) => src.push16(n),
        Err(e) => eprintln!("Device Error: {}", e)
    };
    uxn.dev[(a >> 4) as usize] = dev;
}

pub fn op_deo16(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop16(keep);
    let mut dev = uxn.dev[(a >> 4) as usize].clone();
    match dev.devpoke16(uxn, a, b) {
        Ok(()) => (),
        Err(e) => eprintln!("Device Error: {}", e)
    };
    uxn.dev[(a >> 4) as usize] = dev;
}

/* Arithmetic 16bit */
pub fn op_add16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push16(b.wrapping_add(a));
}

pub fn op_sub16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push16(b.wrapping_sub(a));
}

pub fn op_mul16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push16(b.wrapping_mul(a));
}

pub fn op_div16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let mut a = src.pop16(keep);
    let b = src.pop16(keep);
    if a == 0 {
        src.error = 3;
        a = 1;
    }
    src.push16(b.wrapping_div(a));
}

pub fn op_and16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push16(b & a);
}

pub fn op_ora16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push16(b | a);
}

pub fn op_eor16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push16(b ^ a);
}

pub fn op_sft16(_uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop16(keep);
    src.push16(b >> (a & 0x07) << ((a & 0x70) >> 4));
}