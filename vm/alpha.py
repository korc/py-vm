#!/usr/bin/python
# coding=utf8

import vm
import sys
import re
import subprocess
import struct
import traceback
from vm import str2int_le

def create_opcodes_map(opcodes):
    ret={}
    for name,mode,b,desc in opcodes:
        b=map(lambda x: int(x,16), b.split("."))
        inf=ret.setdefault(b[0],{})
        p_mode=inf.setdefault("mode",mode)
        if p_mode!=mode:
            raise RuntimeError("Op mode conflict",b)
        if len(b)>1:
            inf=inf.setdefault("func",{}).setdefault(b[1],{})
        inf["name"]=name
        inf["description"]=desc
    return ret

opcodes_map=create_opcodes_map(map(lambda x: x.split(None, 3),"""ADDF F-P 15.080 Add F_floating
ADDG F-P 15.0A0 Add G_floating
ADDL Opr 10.00 Add longword
ADDQ Opr 10.20 Add quadword
ADDS F-P 16.080 Add S_floating
ADDT F-P 16.0A0 Add T_floating
AMASK Opr 11.61 Architecture mask
AND Opr 11.00 Logical product
BEQ Bra 39 Branch if =  zero
BGE Bra 3E Branch if ³ zero
BGT Bra 3F Branch if > zero
BIC Opr 11.08 Bit clear
BIS Opr 11.20 Logical sum
BLBC Bra 38 Branch if low bit clear
BLBS Bra 3C Branch if low bit set
BLE Bra 3B Branch if £ zero
BLT Bra 3A Branch if < zero
BNE Bra 3D Branch if ¹ zero
BR Bra 30 Unconditional branch
BSR Mbr 34 Branch to subroutine
CALL_PAL Pcd 00 Trap to PALcode
CMOVEQ Opr 11.24 CMOVE if =  zero
CMOVGE Opr 11.46 CMOVE if ³ zero
CMOVGT Opr 11.66 CMOVE if > zero
CMOVLBC Opr 11.16 CMOVE if low bit clear
CMOVLBS Opr 11.14 CMOVE if low bit set
CMOVLE Opr 11.64 CMOVE if £ zero
CMOVLT Opr 11.44 CMOVE if < zero
CMOVNE Opr 11.26 CMOVE if ¹ zero
CMPBGE Opr 10.0F Compare byte
CMPEQ Opr 10.2D Compare signed quadword equal
CMPGEQ F-P 15.0A5 Compare G_floating equal
CMPGLE F-P 15.0A7 Compare G_floating less than or equal
CMPGLT F-P 15.0A6 Compare G_floating less than
CMPLE Opr 10.6D Compare signed quadword less than or equal
CMPLT Opr 10.4D Compare signed quadword less than
CMPTEQ F-P 16.0A5 Compare T_floating equal
CMPTLE F-P 16.0A7 Compare T_floating less than or equal
CMPTLT F-P 16.0A6 Compare T_floating less than
CMPTUN F-P 16.0A4 Compare T_floating unordered
CMPULE Opr 10.3D Compare unsigned quadword less than or equal
CMPULT Opr 10.1D Compare unsigned quadword less than
CPYS F-P 17.020 Copy sign
CPYSE F-P 17.022 Copy sign and exponent
CPYSN F-P 17.021 Copy sign negate
CTLZ Opr 1C.32 Count leading zero
CTPOP Opr 1C.30 Count population
CTTZ Opr 1C.33 Count trailing zero
CVTDG F-P 15.09E Convert D_floating to G_floating
CVTGD F-P 15.0AD Convert G_floating to D_floating
CVTGF F-P 15.0AC Convert G_floating to F_floating
CVTGQ F-P 15.0AF Convert G_floating to quadword
CVTLQ F-P 17.010 Convert longword to quadword
CVTQF F-P 15.0BC Convert quadword to F_floating
CVTQG F-P 15.0BE Convert quadword to G_floating
CVTQL F-P 17.030 Convert quadword to longword
CVTQS F-P 16.0BC Convert quadword to S_floating
CVTQT F-P 16.0BE Convert quadword to T_floating
CVTST F-P 16.2AC Convert S_floating to T_floating
CVTTQ F-P 16.0AF Convert T_floating to quadword
CVTTS F-P 16.0AC Convert T_floating to S_floating
DIVF F-P 15.083 Divide F_floating
DIVG F-P 15.0A3 Divide G_floating
DIVS F-P 16.083 Divide S_floating
DIVT F-P 16.0A3 Divide T_floating
ECB Mfc 18.E800 Evict cache block
EQV Opr 11.48 Logical equivalence
EXCB Mfc 18.0400 Exception barrier
EXTBL Opr 12.06 Extract byte low
EXTLH Opr 12.6A Extract longword high
EXTLL Opr 12.26 Extract longword low
EXTQH Opr 12.7A Extract quadword high
EXTQL Opr 12.36 Extract quadword low
EXTWH Opr 12.5A Extract word high
EXTWL Opr 12.16 Extract word low
FBEQ Bra 31 Floating branch if =  zero
FBGE Bra 36 Floating branch if ³ zero
FBGT Bra 37 Floating branch if > zero
FBLE Bra 33 Floating branch if £ zero
FBLT Bra 32 Floating branch if < zero
FBNE Bra 35 Floating branch if ¹ zero
FCMOVEQ F-P 17.02A FCMOVE if = zero
FCMOVGE F-P 17.02D FCMOVE if ³ zero
FCMOVGT F-P 17.02F FCMOVE if > zero
FCMOVLE F-P 17.02E FCMOVE if £ zero
FCMOVLT F-P 17.02C FCMOVE if < zero
FCMOVNE F-P 17.02B FCMOVE if ¹ zero
FETCH Mfc 18.8000 Prefetch data
FETCH_M Mfc 18.A000 Prefetch data, modify intent
IMPLVER Opr 11.6C Implementation version
INSBL Opr 12.0B Insert byte low
INSLH Opr 12.67 Insert longword high
INSLL Opr 12.2B Insert longword low
INSQH Opr 12.77 Insert quadword high
INSQL Opr 12.3B Insert quadword low
INSWH Opr 12.57 Insert word high
INSWL Opr 12.1B Insert word low
ITOFF F-P 14.014 Integer to floating move, F_floating
ITOFS F-P 14.004 Integer to floating move, S_floating
ITOFT F-P 14.024 Integer to floating move, T_floating
JMP Mbr 1A.0 Jump
JSR Mbr 1A.1 Jump to subroutine
JSR_COROUTINE Mbr 1A.3 Jump to subroutine return
LDA Mem 08 Load address
LDAH Mem 09 Load address high
LDBU Mem 0A Load zero-extended byte
LDWU Mem 0C Load zero-extended word
LDF Mem 20 Load F_floating
LDG Mem 21 Load G_floating
LDL Mem 28 Load sign-extended longword
LDL_L Mem 2A Load sign-extended longword locked
LDQ Mem 29 Load quadword
LDQ_L Mem 2B Load quadword locked
LDQ_U Mem 0B Load unaligned quadword
LDS Mem 22 Load S_floating
LDT Mem 23 Load T_floating
MAXSB8 Opr 1C.3E Vector signed byte maximum
MAXSW4 Opr 1C.3F Vector signed word maximum
MAXUB8 Opr 1C.3C Vector unsigned byte maximum
MAXUW4 Opr 1C.3D Vector unsigned word maximum
MB Mfc 18.4000 Memory barrier
MF_FPCR F-P 17.025 Move from FPCR
MINSB8 Opr 1C.38 Vector signed byte minimum
MINSW4 Opr 1C.39 Vector signed word minimum
MINUB8 Opr 1C.3A Vector unsigned byte minimum
MINUW4 Opr 1C.3B Vector unsigned word minimum
MSKBL Opr 12.02 Mask byte low
MSKLH Opr 12.62 Mask longword high
MSKLL Opr 12.22 Mask longword low
MSKQH Opr 12.72 Mask quadword high
MSKQL Opr 12.32 Mask quadword low
MSKWH Opr 12.52 Mask word high
MSKWL Opr 12.12 Mask word low
MT_FPCR F-P 17.024 Move to FPCR
MULF F-P 15.082 Multiply F_floating
MULG F-P 15.0A2 Multiply G_floating
MULL Opr 13.00 Multiply longword
MULQ Opr 13.20 Multiply quadword
MULS F-P 16.082 Multiply S_floating
MULT F-P 16.0A2 Multiply T_floating
ORNOT Opr 11.28 Logical sum with complement
PERR Opr 1C.31 Pixel error
PKLB Opr 1C.37 Pack longwords to bytes
PKWB Opr 1C.36 Pack words to bytes
RC Mfc 18.E000 Read and clear
RET Mbr 1A.2 Return from subroutine
RPCC Mfc 18.C000 Read process cycle counter
RS Mfc 18.F000 Read and set
S4ADDL Opr 10.02 Scaled add longword by 4
S4ADDQ Opr 10.22 Scaled add quadword by 4
S4SUBL Opr 10.0B Scaled subtract longword by 4
S4SUBQ Opr 10.2B Scaled subtract quadword by 4
S8ADDL Opr 10.12 Scaled add longword by 8
S8ADDQ Opr 10.32 Scaled add quadword by 8
S8SUBL Opr 10.1B Scaled subtract longword by 8
S8SUBQ Opr 10.3B Scaled subtract quadword by 8
SEXTB Opr 1C.00 Sign extend byte
SEXTW Opr 1C.01 Sign extend word
SLL Opr 12.39 Shift left logical
SQRTF F-P 14.08A Square root F_floating
SQRTG F-P 14.0AA Square root G_floating
SQRTS F-P 14.08B Square root S_floating
SQRTT F-P 14.0AB Square root T_floating
SRA Opr 12.3C Shift right arithmetic
SRL Opr 12.34 Shift right logical
STB Mem 0E Store byte
STF Mem 24 Store F_floating
STG Mem 25 Store G_floating
STS Mem 26 Store S_floating
STL Mem 2C Store longword
STL_C Mem 2E Store longword conditional
STQ Mem 2D Store quadword
STQ_C Mem 2F Store quadword conditional
STQ_U Mem 0F Store unaligned quadword
STT Mem 27 Store T_floating
STW Mem 0D Store word
SUBF F-P 15.081 Subtract F_floating
SUBG F-P 15.0A1 Subtract G_floating
SUBL Opr 10.09 Subtract longword
SUBQ Opr 10.29 Subtract quadword
SUBS F-P 16.081 Subtract S_floating
SUBT F-P 16.0A1 Subtract T_floating
TRAPB Mfc 18.0000 Trap barrier
UMULH Opr 13.30 Unsigned multiply quadword high
UNPKBL Opr 1C.35 Unpack bytes to longwords
UNPKBW Opr 1C.34 Unpack bytes to words
WH64 Mfc 18.F800 Write hint — 64 bytes
WMB Mfc 18.4400 Write memory barrier
XOR Opr 11.40 Logical difference
ZAP Opr 12.30 Zero bytes
ZAPNOT Opr 12.31 Zero bytes not""".split("\n")))

class cached_property(object):
    fset=None
    def __init__(self, fget):
        self.fget=fget
        self.__name__=fget.__name__
        self.__module__=fget.__module__
    def __get__(self, instance, owner):
        if instance is None: return self
        try: return instance._property_cache[self.__name__]
        except AttributeError: instance._property_cache={}
        except KeyError: pass
        instance._property_cache[self.__name__]=v=self.fget(instance)
        return v
    def __set__(self, instance, value):
        if self.fset is not None:
            value=self.fset(instance, value)
        try: instance._property_cache[self.__name__]=value
        except AttributeError: instance._property_cache={self.__name__:value}
    def __delete__(self, instance):
        try: del instance._property_cache[self.__name__]
        except (KeyError,AttributeError):
            raise AttributeError("Attribute not set", self.__name__)

class FieldInt(long):
    def __getitem__(self, key):
        if isinstance(key, (long,int)): return self&(1<<key)
        v=self>>(key.start or 0)
        if key.stop is None: return v
        else: return v&((1<<(key.stop-(key.start or 0)))-1)

def s_ext(v, src, dst):
    if v&(1<<src-1):
        return v|(((1<<(dst-src))-1)<<src)
    else: return v

class AlphaInstruction(vm.Instruction):
    @property
    def opcode(self): return self[26:]
    @property
    def ra(self): return self[21:26]
    @property
    def rb(self): return self[16:21]
    @property
    def rc(self): return self[:5]
    @property
    def mem_disp(self): return self[:16]
    @property
    def branch_disp(self): return self[:21]
    @property
    def is_lit(self): return self[12]
    @property
    def lit(self): return self[13:21]
    @property
    def mem_func(self): return self[5:16]
    @property
    def oper_func(self): return self[5:12]
    @property
    def fp_func(self): return self[5:16]
    @property
    def pal_func(self): return self[0:26]
    @cached_property
    def data(self):
        return self.vm.mem[self.vm.pc:self.vm.pc+4]
    @cached_property
    def value(self):
        return FieldInt(struct.unpack("<I", self.data)[0])
    def __getitem__(self, key): return self.value[key]
    @cached_property
    def op_mode(self):
        return opcodes_map[self.opcode]["mode"]
    @cached_property
    def name(self):
        return self.exec_params[0]
    @cached_property
    def exec_params(self):
        e=opcodes_map[self.opcode]
        self.next_pc=self.pc+4
        if self.op_mode=="Mem":
            return e["name"],dict(ra=self.ra,rb=self.rb, mem_disp=self.mem_disp)
        elif self.op_mode=="Opr":
            params=dict(ra=self.ra, rc=self.rc)
            if self.is_lit: params["lit"]=self.lit
            else: params["rb"]=self.rb
            return e["func"][self.oper_func]["name"],params
        elif self.op_mode=="Mfc":
            return e["func"][self.mem_func]["name"],dict(ra=self.ra, rb=self.rb)
        elif self.op_mode=="Mbr":
            if "func" in e:
                return e["func"][self[14:16]]["name"],dict(ra=self.ra, hint=self[:13])
            else:
                return e["name"],dict(branch_disp=self.branch_disp, ra=self.ra)
        elif self.op_mode=="Bra":
            return e["name"],dict(branch_disp=self.branch_disp, ra=self.ra)
        else:
            raise ValueError("Invalid opcode?",self.data)
    def execute(self):
        name,params=self.exec_params
        x=params.copy()
        x1=[]
        if "ra" in x: x1.append(self.vm.regs.reg_name(x.pop("ra"))[0])
        if "mem_disp" in x and "rb" in x:
            x1.append("0x%x(%s)"%(x.pop("mem_disp"),self.vm.regs.reg_name(x.pop("rb"))[0]))
        elif "rb" in x and "rc" in x:
            x1.append(self.vm.regs.reg_name(x.pop("rb"))[0])
            x1.append(self.vm.regs.reg_name(x.pop("rc"))[0])
        elif "lit" in x and "rc" in x:
            x1.append("0x%x"%x.pop("lit"))
            x1.append(self.vm.regs.reg_name(x.pop("rc"))[0])
        print "Exec {s.pc:#08x}: {s.name:} {p:} {x:}".format(s=self, p=",".join(x1), x=x if x else "")
        #print "Executing:","{pc:#x}:  {d:s}  {v:032b}".format(pc=self.pc,d=" ".join(map(lambda x: x.encode("hex"),self.data)), v=self.value),name,params
        getattr(self,"op_%s"%name)(**params)
    max_int=((1<<64)-1)
    def do_branch(self, branch_disp):
        self.next_pc=self.max_int&(self.next_pc+(s_ext(branch_disp<<2,23,64)))
    def op_LDA(self, ra, rb, mem_disp):
        self.vm.regs[ra]=self.max_int&(self.vm.regs[rb]+s_ext(mem_disp,16,64))
    def op_STQ(self, ra, rb, mem_disp):
        va=self.vm.regs[rb]+s_ext(mem_disp,16,64)
        self.vm.mem[self.max_int&va:self.max_int&(va+8)]=self.vm.regs[ra]
    def op_LDQ(self, ra, rb, mem_disp):
        va=self.vm.regs[rb]+s_ext(mem_disp,16,64)
        self.vm.regs[ra]=self.vm.mem[va&self.max_int:self.max_int&(va+8)]
    def op_BIS(self, ra, rc, lit=None, rb=None):
        if lit is None: lit=self.vm.regs[rb]
        self.vm.regs[rc]=self.vm.regs[ra]|lit
    def op_TRAPB(self, **kwargs): return
    def op_JSR(self, ra, hint):
        print "Entering eval loop, press enter to continue."
        while True:
            prompt="JSR [0x%x, $%d=0x%x]> "%(hint, ra, self.vm.regs[ra])
            d=raw_input(prompt)
            if d=="": break
            try: exec d
            except Exception,e:
                print "EXCEPTION:",e
                traceback.print_exc()
    def op_LDL(self, ra, rb, mem_disp):
        va=self.vm.regs[rb]+s_ext(mem_disp,16,64)
        self.vm.regs[ra]=s_ext(str2int_le(self.vm.mem[self.max_int&va:self.max_int&(va+4)]),32,64)
    def op_BSR(self, ra, branch_disp):
        self.vm.regs[ra]=self.next_pc
        return self.do_branch(branch_disp)
    def op_LDQ_U(self, ra, rb, mem_disp):
        va=(self.vm.regs[rb]+s_ext(mem_disp,16,64))&((self.max_int)&~7)
        self.vm.regs[ra]=self.vm.mem[va:self.max_int&(va+8)]
    def op_XOR(self, ra, rc, lit=None, rb=None):
        if lit is None: lit=self.vm.regs[rb]
        self.vm.regs[rc]=self.vm.regs[ra]^lit
    def op_EXTBL(self, ra, rc, lit=None, rb=None):
        if lit is None: lit=self.vm.regs[rb]
        v=self.vm.regs[ra]>>((lit&7)*8)
        self.vm.regs[rc]=v&0xff
    def op_MSKBL(self, ra, rc, lit=None, rb=None):
        if lit is None: lit=self.vm.regs[rb]
        bm=(self.max_int)&(~(0xff<<((lit&7)*8)))
        self.vm.regs[rc]=self.vm.regs[ra]&bm
    def op_INSBL(self, ra, rc, lit=None, rb=None):
        if lit is None: lit=self.vm.regs[rb]
        self.vm.regs[rc]=(self.vm.regs[ra]&0xff)<<((lit&7)*8)
    def op_STQ_U(self, ra, rb, mem_disp):
        va=(self.vm.regs[rb]+s_ext(mem_disp,16,64))&(~7&self.max_int)
        self.vm.mem[va:self.max_int&(va+8)]=self.vm.regs[ra]
    def op_ZAPNOT(self, ra, rc, lit=None, rb=None):
        if lit is None: lit=self.vm.regs[rb]
        mask=sum(map(lambda x: ((0xff)<<(x<<3)) if lit&(1<<x) else 0,range(8)))
        self.vm.regs[rc]=self.vm.regs[ra]&mask
    def op_STL(self, ra, rb, mem_disp):
        va=self.max_int&(self.vm.regs[rb]+s_ext(mem_disp,16,64))
        self.vm.mem[va:self.max_int&(va+4)]=((1<<32)-1)&self.vm.regs[ra]
    def op_BLT(self, ra, branch_disp):
        if self.vm.regs[ra]&(1<<63):
            return self.do_branch(branch_disp)
    def op_INSWL(self, ra, rc, lit=None, rb=None):
        if lit is None: lit=self.vm.regs[rb]
        self.vm.regs[rc]=(self.vm.regs[ra]&0xffff)<<((lit&7)*8)
    def op_BNE(self, ra, branch_disp):
        if self.vm.regs[ra]!=0:
            return self.do_branch(branch_disp)

class ObjDumpDisasm(object):
    disasm_re=r=re.compile(r'^ *(?P<addr>[0-9a-f]+):\t(?P<hex>[0-9a0-f ]+)\t(?P<op>\S+)(?:\t(?P<params>\S+))?(?: \(File Offset: 0x(?P<foff>[0-9a-f]+)\))?$',re.M)
    def __init__(self, fname=None):
        self.data={}
        self.op_nums={}
        if fname is not None:
            self.parse_objdump(open(fname).read())
    @classmethod
    def from_bin(cls, fname):
        cmd=["objdump","-F","-M","reg-names=raw","-b","binary","-m","alpha","-D",fname]
        data=subprocess.Popen(cmd,stdout=subprocess.PIPE).stdout.read()
        ret=cls()
        ret.parse_objdump(data)
        return ret
    def anal(self):
        d=set()
        for m in self.data.itervalues():
            d.add((" ".join(map(lambda x: "{0:08b}".format(ord(x)),reversed(m["hex"].replace(" ","").decode("hex")))),m["hex"],"[%(op_num)x] %(op)s\t%(params)s"%m,str(self.op_nums[m["op_num"]])))
        for z in sorted(d, key=lambda x: x[0]):
            print "\t".join(z)
    def __getitem__(self, key):
        return self.data[key]
    def param2signature(self, val):
        if "(" in val: return "p"
        elif val.startswith("0x"):
            int(val,16)
            return "i"
        elif val.isdigit(): return "i"
        elif val.startswith("$"): return "r"
        getattr(AlphaRegisters, val)
        return "r"
    def parse_objdump(self, data):
        st=0
        for match in self.disasm_re.finditer(data):
            m=match.groupdict()
            skip_data=data[st:match.start()]
            if skip_data.strip() and not ("skipping" in skip_data or "\t.long " in skip_data):
                print "Skipped data:",`skip_data`
            st=match.end()
            m["op_int"]=op_int=struct.unpack("<I",m["hex"].replace(" ","").decode("hex"))[0]
            m["op_num"]=op_num=op_int>>26
            tmpl="".join(map(self.param2signature, m["params"].split(",") if m["params"] else []))
            if op_num in self.op_nums:
                inf=self.op_nums[op_num]
                if inf is not None:
                    if not (inf["tmpl"]==tmpl and inf["name"]==m["op"]):
                        print "Detected parameters template change:",hex(op_num), m["op"],tmpl, inf
                        self.op_nums[op_num]=None
            else: self.op_nums[op_num]={"tmpl":tmpl, "name":m["op"]}
            
            self.data[int(m["addr"],16)]=m

class AlphaRegisters(vm.Registers):
    v0=vm.RegAlias(0)
    t0,t1,t2,t3,t4,t5,t6,t7=map(vm.RegAlias,range(1,9))
    s0,s1,s2,s3,s4,s5,s6=map(vm.RegAlias,range(9,16))
    fp=s6
    a0,a1,a2,a3,a4,a5=map(vm.RegAlias,range(16,22))
    t8,t9,t10,t11=map(vm.RegAlias,range(22,26))
    ra,pv,at,gp,sp,zero=map(vm.RegAlias,range(26,32))
    t12=pv
    f0,f1,f2,f3,f4,f5,f6,f7,f8,f9=map(lambda x: vm.RegAlias(x,"fpoint"),range(10))
    f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20=map(lambda x: vm.RegAlias(x,"fpoint"),range(10,21))
    f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,f31=map(lambda x: vm.RegAlias(x,"fpoint"),range(21,32))
    def reg_size(self, reg): return 64
    def __init__(self):
        self.values=[None]*32
        self.fpoint=[None]*32
    def __getitem__(self, key):
        if key==31: return 0
        return super(AlphaRegisters, self).__getitem__(key)

class AlphaVM(vm.VM):
    def_org=0x10000
    def_stack=0x20000
    def_sp=def_stack+0x800
    pc=None
    inst_cls=AlphaInstruction
    reg_cls=AlphaRegisters
    def initialize_vm(self):
        vm.VM.initialize_vm(self)
        print "Extra memory segment:", self.mem.map_virtual(0x40000, 0x1000, "ra_start")


def try_pass(d):
    m.regs.gp=0x40500
    m.mem[m.regs.gp+8:]=open("net.bin").read()
    m.mem[m.regs.gp+0x28:]=d
    m.pc=0x10814
    while m.pc!=0x10964: m.step()
    return m.mem[m.regs.gp+8:m.regs.gp+18]

if __name__=='__main__':
    import user #@UnusedImport
    m=AlphaVM()
    m.initialize_vm()
    fname=sys.argv[1]
    m.code=m.mem.map_file(fname, org=m.def_org)
    m.pc=m.code.offset+0x600+0xb0
    d=open("net.bin").read()
    m.regs.ra=0x40000
    m.regs.pv=m.pc
    
    #m.mem[m.stack.offset+0x100:]=d
    #ddbg=ObjDumpDisasm.from_bin(fname)