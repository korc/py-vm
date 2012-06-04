#!/usr/bin/python

import mmap
import os
from ctypes import create_string_buffer
import traceback

class VMError(Exception): pass
class VMMemoryError(Exception): pass
class AddressNotMapped(VMMemoryError): pass
class NotWritableMemory(VMMemoryError): pass
class OutOfBoundsError(VMMemoryError): pass
class UnInitializedRegister(VMError): pass

def int2str_le(val, bcount):
    return "".join([chr((val>>x)&0xff) for x in range(0,bcount*8,8)])

def str2int_le(val):
    return sum(map(lambda x: ord(x[1])<<(8*x[0]),enumerate(val)))

class MemorySegment(object):
    seg_type=None
    name=None
    offset=None
    size=None
    access_history=None
    def __init__(self, **attrs):
        for k,v in attrs.iteritems(): setattr(self, k, v)
    def __delete__(self):
        if self.seg_type=="mmap":
            self.data.close()
    def __repr__(self): return "<%s.%s %s %r [%s:%s] at 0x%x>"%(
            self.__class__.__module__, self.__class__.__name__, self.seg_type,
            self.name,
            "(ukn)" if self.offset is None else hex(self.offset),
            "(ukn)" if self.size is None or self.offset is None else hex(self.offset+self.size),
            id(self))
    def calc_slice(self, key):
        if self.offset and isinstance(key, slice):
            return slice(key.start-self.offset, key.stop-self.offset, key.step)
        return key
    def __getitem__(self, key):
        nkey=self.calc_slice(key)
        ret=self.data[nkey]
        if not ret and key.stop: raise OutOfBoundsError("No more memory in this segment")
        k=(hex(key.start) if key.start is not None else "", hex(key.stop) if key.stop is not None else "")
        print "mem read: {s.name}[{k[0]}:{k[0]}] -> {v!r}".format(s=self, k=k, v=ret)
        if self.access_history:
            l=len(ret)
            st=nkey.start or 0
            m=(self.access_history>>(st))&((1<<l)-1)
            if m:
                print "{:#x}:".format(st+self.offset)," ".join(self.mark_changed(map(lambda x: x.encode("hex"),ret), st))
                new_data=raw_input("Uninitialized memory> ")
                if new_data:
                    ret=new_data.decode("string_escape")
                    print "Returning %r instead"%(ret)
        return ret
    def __setitem__(self, key, value):
        nkey=self.calc_slice(key)
        if self.data[nkey]=="": raise IndexError(key)
        if not self.writable: raise NotWritableMemory(key)
        if isinstance(value, (long,int)):
            value=int2str_le(value, key.stop-key.start)
        self.data[nkey]=value
        if self.access_history:
            l=len(value)
            m=((1<<l)-1)<<(nkey.start or 0)
            self.access_history=~(~self.access_history|m)
        print "memwrite: {s.name}[{st}:{en}] = {v!r}".format(s=self, v=value,
            st=hex(key.start) if key.start is not None else "",
            en=hex(key.stop) if key.stop is not None else "")
    def mark_changed(self, d, offset):
        return [(c if self.access_history&(1<<(offset+idx)) else "\033[32m%s\033[0m"%c) for idx,c in enumerate(d)]
    def hex_dump(self, start=None, size=None):
        if start is None: start=self.offset
        for off in range(start-self.offset, size or self.size, 16):
            d=self.data[off:off+16]
            print "%08x:  %s  %s"%(off+self.offset,
                " ".join(self.mark_changed(map(lambda x: x.encode("hex"),d),off)).ljust(16*3-1),
                "".join(map(lambda x: x if ord(x)>=0x20 and ord(x)<0x7f else ".",d)))
    def __contains__(self, key):
        if isinstance(key, slice):
            en=self.offset+self.size
            if key.stop is not None and key.stop>en+1: return False
            if key.start is not None and key.start<self.offset: return False
            return True
        else: raise RuntimeError("Key have to be slice")

class Memory(object):
    def __init__(self):
        self.segments=[]
    def map_file(self, fname, org=0, size=None, data_offset=0):
        fobj=open(fname,"rb")
        data=mmap.mmap(fobj.fileno(), size or 0, access=mmap.ACCESS_READ, offset=data_offset)
        if size is None: size=data.size()
        segment=MemorySegment(name=fname, offset=org, size=size, seg_type="mmap", writable=False, data=data)
        self.segments.append(segment)
        return segment
    def map_virtual(self, start, size, name=None):
        segment=MemorySegment(name=name or "virt_%x"%start, offset=start, size=size, seg_type="virtual", writable=True)
        segment.access_history=(1<<size)-1
        segment.data=create_string_buffer(size)
        self.segments.append(segment)
        return segment
    def find_segment(self, key):
        for segment in reversed(self.segments):
            if key in segment:
                #print "Found segment:",`segment`
                return segment
        raise AddressNotMapped(key)
    def __getitem__(self, key):
        return self.find_segment(key)[key]
    def __setitem__(self, key, value):
        if key.stop is None and isinstance(value, basestring):
            key=slice(key.start,key.start+len(value))
        self.find_segment(key)[key]=value

class Instruction(object):
    def __init__(self, *args, **attrs):
        if args: attrs["vm"]=args[0]
        for k,v in attrs.iteritems(): setattr(self, k, v)
        if "vm" in attrs: self.pc=self.vm.pc

class RegAlias(object):
    def __init__(self, nr,subset=None):
        self.nr=nr
        self.subset=subset
    def __get__(self, instance, owner):
        if instance is None: return self
        if isinstance(instance, VM): instance=instance.regs
        if self.subset: return getattr(instance,self.subset)[self.nr]
        return instance[self.nr]
    def __set__(self, instance, value):
        if isinstance(instance, VM): instance=instance.regs
        if self.subset: getattr(instance, self.subset)[self.nr]=value
        else: instance[self.nr]=value
    def __delete__(self, instance):
        if self.subset: getattr(instance, self.subset)[self.nr]=None
        else: instance[self.nr]=None
    def __repr__(self):
        return "<RegAlias $%d at %x>"%(self.nr, id(self))

class Registers(object):
    def reg_name(self, nr, subset=None):
        return [k for k in dir(self.__class__)
                if not k.startswith("_") and isinstance(getattr(self.__class__,k,None),RegAlias) and getattr(self.__class__,k).nr==nr and getattr(self.__class__,k).subset==subset]
    def __getitem__(self, key):
        ret=self.values[key]
        if ret is None:
            ret=raw_input("%8x: Uninitialized register $%d [%s] (=0)> "%(self.vm.pc,key,",".join(self.reg_name(key))))
            if ret=="": ret=0
            else:
                try: ret=eval(ret)
                except Exception,e:
                    print "Exception:",e
                    traceback.print_exc()
            self.values[key]=ret
        print "reg read: %s -> %#x"%(",".join(self.reg_name(key)),ret)
        return ret
    def __setitem__(self, key, value):
        if value is None: pass
        elif isinstance(value,basestring):
            value=str2int_le(value[:self.reg_size(key)>>3])
        else: value=value&((1<<self.reg_size(key))-1)
        print "regwrite: %s -> %#x"%(",".join(self.reg_name(key)),value)
        self.values[key]=value

class VM(object):
    pc=RegAlias("pc")
    inst_cls=Instruction
    reg_cls=Registers
    def __init__(self):
        self.regs=self.reg_cls()
        self.regs.vm=self
        self.mem=Memory()
    def __iter__(self):
        while True:
            yield self.step()
    def step(self):
        instr=self.inst_cls(self)
        instr.execute()
        self.pc=instr.next_pc
        return instr
    def initialize_vm(self):
        self.pc=self.def_org
        self.regs.sp=self.def_sp
        self.stack=self.mem.map_virtual(self.def_stack,0x1000, name="stack")