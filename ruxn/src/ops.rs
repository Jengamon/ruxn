use crate::KeepMode;

use super::{Stack, Memory, Uxn};

pub fn op_nop(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    
}

/* Stack */
pub fn op_lit(uxn: &mut Uxn, src: &mut Stack, _dst: &mut Stack, _keep: KeepMode) {
    let dat = uxn.ram.mempeek8(uxn.ram.ptr);
    uxn.ram.ptr += 1;
    src.push8(dat);
}

pub fn op_inc(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.push8(a + 1);
}

pub fn op_pop(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    src.pop8(keep);
}

pub fn op_dup(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.push8(a);
    src.push8(a);
}

pub fn op_nip(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.pop8(keep);
    src.push8(a);
}

pub fn op_swp(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(a);
    src.push8(b);
}

pub fn op_ovr(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b); src.push8(a); src.push8(b);
}

pub fn op_rot(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    let c = src.pop8(keep);
    src.push8(b); src.push8(a); src.push8(c);
}
/* Logic */
pub fn op_equ(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8((b == a) as u8);
}

pub fn op_neq(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8((b != a) as u8);
}

pub fn op_gth(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8((b > a) as u8);
}

pub fn op_lth(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8((b < a) as u8);
}

pub fn op_jmp(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    uxn.ram.ptr += a as u16;
}

pub fn op_jnz(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    if src.pop8(keep) == 0 {
        uxn.ram.ptr += a as u16;
    }
}

pub fn op_jsr(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    dst.push16(uxn.ram.ptr);
    uxn.ram.ptr += a as u16;
}

pub fn op_sth(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    dst.push8(a);
}
/* Memory */
pub fn op_pek(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.push8(uxn.ram.mempeek8(a as u16));
}

pub fn op_pok(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    uxn.ram.mempoke8(a as u16, b);
}

pub fn op_ldr(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.push8(uxn.ram.mempeek8(uxn.ram.ptr + a as u16));
}

pub fn op_str(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    uxn.ram.mempoke8(uxn.ram.ptr + a as u16, b);
}

pub fn op_lda(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    src.push8(uxn.ram.mempeek8(a));
}

pub fn op_sta(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop8(keep);
    uxn.ram.mempoke8(a, b);
}

pub fn op_dei(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let mut dev = uxn.dev[(a >> 4) as usize].clone();
    // Uses unwrap, can crash here
    src.push8(dev.devpeek8(uxn, a).unwrap());
    uxn.dev[(a >> 4) as usize] = dev;
}

pub fn op_deo(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    let mut dev = uxn.dev[(a >> 4) as usize].clone();
    // Uses unwrap, can crash here
    dev.devpoke8(uxn, a, b).unwrap();
    uxn.dev[(a >> 4) as usize] = dev;
}
/* Arithmetic */
pub fn op_add(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b + a);
}

pub fn op_sub(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b - a);
}

pub fn op_mul(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b * a);
}

pub fn op_div(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let mut a = src.pop8(keep);
    let b = src.pop8(keep);
    if(a == 0) {
        src.error = 3;
        a = 1;
    }
    src.push8(b / a);
}

pub fn op_and(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b & a);
}

pub fn op_ora(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b | a);
}

pub fn op_eor(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop8(keep);
    src.push8(b ^ a);
}

pub fn op_sft(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
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

pub fn op_inc16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    src.push16(a + 1);
}

pub fn op_pop16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    src.pop16(keep);
}

pub fn op_dup16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    src.push16(a);
    src.push16(a);
}

pub fn op_nip16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    src.pop16(keep);
    src.push16(a);
}

pub fn op_swp16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push16(a);
    src.push16(b);
}

pub fn op_ovr16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push16(b); src.push16(a); src.push16(b);
}

pub fn op_rot16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    let c = src.pop16(keep);
    src.push16(b); src.push16(a); src.push16(c);
}
/* Logic 16bit */
pub fn op_equ16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push8((b == a) as u8);
}

pub fn op_neq16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push8((b != a) as u8);
}

pub fn op_gth16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push8((b > a) as u8);
}

pub fn op_lth16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    src.push8((b < a) as u8);
}

pub fn op_jmp16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    uxn.ram.ptr = a;
}

pub fn op_jnz16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    if src.pop8(keep) == 0 {
        uxn.ram.ptr = a;
    }
}

pub fn op_jsr16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    dst.push16(uxn.ram.ptr);
    uxn.ram.ptr = src.pop16(keep);
}

pub fn op_sth16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    dst.push16(a);
}

/* Memory 16bit */
pub fn op_pek16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.push16(uxn.ram.mempeek16(a as u16));
}

pub fn op_pok16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop16(keep);
    uxn.ram.mempoke16(a as u16, b);
}

pub fn op_ldr16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    src.push16(uxn.ram.mempeek16(uxn.ram.ptr + a as u16));
}

pub fn op_str16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop16(keep);
    uxn.ram.mempoke16(uxn.ram.ptr + a as u16, b);
}

pub fn op_lda16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    src.push16(uxn.ram.mempeek16(a));
}

pub fn op_sta16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop16(keep);
    let b = src.pop16(keep);
    uxn.ram.mempoke16(a, b);
}

pub fn op_dei16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let mut dev = uxn.dev[(a >> 4) as usize].clone();
    // Uses unwrap, can crash here
    src.push16(dev.devpeek16(uxn, a).unwrap());
    uxn.dev[(a >> 4) as usize] = dev;
}

pub fn op_deo16(uxn: &mut Uxn, src: &mut Stack, dst: &mut Stack, keep: KeepMode) {
    let a = src.pop8(keep);
    let b = src.pop16(keep);
    let mut dev = uxn.dev[(a >> 4) as usize].clone();
    // Uses unwrap, can crash here
    dev.devpoke16(uxn, a, b).unwrap();
    uxn.dev[(a >> 4) as usize] = dev;
}
/* Arithmetic 16bit */