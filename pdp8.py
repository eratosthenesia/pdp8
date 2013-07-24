def car(x):
  return x[0]

def cdr(x):
  return x[1:]

def cons(_car,_cdr):
  return [_car]+forcelist(_cdr)

def str_concat(strs):
  r = ''
  for s in strs:
    r += s + ' '
  return unlast(r)

def unlast(x):
  x = forcelist(x)
  return x[:x.__len__()-1]

def last(x):
  x = forcelist(x)
  return x[x.__len__()-1]

def rot_r(num, amt, size):
  return ((num >> amt) | (num << (size-amt)))&((1<<size)-1)

class Register():
  def __init__(self, nbits, *val):
    self.mask  = (1 << nbits) - 1
    self.nbits = nbits
    if val:
      self.n     = val[0] & self.mask
    else:
      self.n = 0
  def set(self, v, *next):
    self.n = v&self.mask
    if next:
      if car(next):
        car(next).set(v/(1+self.mask),*cdr(next))
  def val(self, *next):
    if next:
      if car(next) != ():
        return self.n+(1+self.mask)*\
               car(next).val(*cdr(next))
    return self.n
  def inc(self, *next):
    self.set(self.n+1)
    if next and not self.n:
      if next[0] != ():
        next[0].inc(*cdr(next))
  def inv(self, *next):
    self.set(self.mask-self.n)
    if next:
      if car(next):
        car(next).invert(*cdr(next))

def init_instances(num, classname, *args):
  retval = [0]*num
  for i in range(0,num):
    retval[i] = classname(*args)
  return retval

class MemoryPDP8():
  def __init__(self, numwords, wordsize):
    self.mem = init_instances(numwords, Register, wordsize)
    self.numwords = numwords
    self.wordsize = wordsize
  def reset(self):
    for each in self.mem:
      each.set(0)
  def valid_addr(self, addr):
    return addr >= 0 and addr < self.numwords
  def iaddr(self, addr):
    if self.valid_addr(addr):
      if addr >= 0x8 and addr < 0x10:
        self.ref(addr).inc()
      return self.get(addr)
  def ref(self, addr):
    if self.valid_addr(addr):
      return self.mem[addr]
  def get(self, addr):
    if self.valid_addr(addr):
      return self.mem[addr].val()
  def set(self, addr, v):
    if self.valid_addr(addr):
      self.ref(addr).set(v)

def xor(a,b):
  return (a or b) and not(a and b)

class Device():
  def __init__(self):
    pass
  def run(pdp8, iot):
    pass

class Terminal(Device):
  def __init__(self):
    pass
  def run(self, pdp8, iot):
    tsf = iot.val() & 01
    tpc = iot.val() & 04  
    if tpc:
      print chr(pdp8.ac.val()%256),
    if tsf:
      pdp8.step()

class Keyboard():
  def __init__(self):
    self.buffer = ''
    self.flag   = 0
  def getnext(self):
    retval = ord(car(self.buffer))
    self.buffer = cdr(self.buffer)
    return retval
  def run(self, pdp8, iot):
    ksf = iot.val() & 01
    kcc = iot.val() & 02
    krs = iot.val() & 04
    if krs:
      if self.buffer == '':
        pdp8.ac.set(0)
      else:
        pdp8.ac.set(self.getnext())
    if kcc:
      pdp8.ac.set(0)
    if ksf:
      if self.buffer == '':
        self.buffer = raw_input(pdp8.name+' > ')
      if self.buffer != '':
        pdp8.step()

class PDP8():
  def __init__(self, numwords, *name):
    if name:
      self.name = car(name)
    else:
      self.name = 'PDP8'
    self.core = MemoryPDP8(numwords, 12)
    self.mq   = Register(12)
    self.sc   = Register(12)
    self.l    = Register(1)
    self.ac   = Register(12)
    self.page = Register(5)
    self.word = Register(7)
    self.addr = Register(12)
    self.opr  = Register(3)
    self.i    = Register(1)
    self.z    = Register(1)
    self.curr = Register(7)
    self.dev  = Register(6)
    self.iot  = Register(3)
    self.bits = Register(8)
    self.halt = True
    self.key_in = Keyboard()
    self.term_out = Terminal()
    self.mn_def = {}
  def reset(self):
    self.core.reset()
    self.setpc(0)
    self.mn_def = {}
  def add_def(self, mn, val):
    self.mn_def[mn] = val
  def swap_ifdef(self, mn):
    r = lookup(self.mn_def,mn)
    if r != -1:
      return myoct(r,4)
    return mn
  def get_dev(self, dev_num):
    if dev_num == 3:
      return self.key_in
    if dev_num == 4:
      return self.term_out
  def run(self):
    self.halt = False
    while self.halt == False:
      self.decode()
      self.compute()
  def runstep(self):
    self.decode()
    self.compute()
  def step(self):
    self.word.inc(self.page)
  def setpc(self,num):
    self.word.set(num,self.page)
  def setcurr(self,num):
    self.core.set(self.pc(),num)
  def getcurr(self):
    return self.core.get(self.pc())
  def pc(self):
    return self.word.val(self.page)
  def decode(self):
    curr_ins = self.core.get(self.pc())
    self.curr_ins = curr_ins
    self.curr.set(curr_ins,\
                  self.z,self.i,self.opr)
    page = Register(5)
    if(not self.z.val()):
      page.set(self.page.val())
    self.addr.set(self.curr.val(page))
    if(self.i.val()):
      self.addr.set(self.core.iaddr(self.addr.val()))
    opr_val = self.opr.val()
    if opr_val == 6:
      self.iot.set(curr_ins,self.dev)
    if opr_val == 7:
      self.bits.set(curr_ins)
    self.step()
  def compute(self):
    opr_val = self.opr.val()
    self.op = self.core.get(self.addr.val())
    if opr_val == 0:
      self._and()
    elif opr_val == 1:
      self._tad()
    elif opr_val == 2:
      self._isz()
    elif opr_val == 3:
      self._dca()
    elif opr_val == 4:
      self._jms()
    elif opr_val == 5:
      self._jmp()
    elif opr_val == 6:
      self._iot()
    elif opr_val == 7:
      self._opr()
  def _and(self):    
    self.ac.set(self.op&self.ac.val())
  def _tad(self):
    self.ac.set(self.op+self.ac.val(),self.l)
  def _isz(self):
    self.core.ref(self.addr.val()).inc()
    if self.core.get(self.addr.val()) == 0:
      self.step()
  def _dca(self):
    self.core.set(self.addr.val(),self.ac.val())
  def _jms(self):
    self.core.set(self.addr.val(),self.pc())
    setpc(self.addr.val()+1)
  def _jmp(self):
    setpc(self.addr.val())
  def _iot(self):
    curr_dev = self.get_dev(self.dev.val())
    curr_dev.run(self,self.iot)
  def _opr(self):
    if self.i.val() == 0:
      self._opr1()
    else:
      self._opr2()
  def _opr1(self):
    [cla,cll,cma,cml,rar,ral,bsw,iac] = \
    init_instances(8, Register, 1)
    iac.set(self.bits.val(),bsw,ral,rar,cml,cma,cll,cla)
    if cla.val():
      self.ac.set(0)
    if cll.val():
      self.l.set(0)
    if cma.val():
      self.ac.inv()
    if cml.val():
      self.l.inv()
    if iac.val():
      self.ac.inc(l)
    if rar.val():
      self.ac.set(rot_r(self.ac.val(self.l), \
      01+bsw.val(),13),self.l)
    if ral.val():
      self.ac.set(rot_r(self.ac.val(self.l), \
      12-bsw.val(),13),self.l)
    if bsw.val() and not(rot_r.val() or rot_l.val()):
      self.ac.rar(5)
  def switch_register(self):
    return 0
  def _opr2(self):
    [cla,sma,sza,snl,andor,osr,hlt,group3] = \
       init_instances(8, Register, 1)
    group3.set(self.bits.val(),hlt,osr,andor,\
       snl,sza,sma,cla)
    if(group3.val()):
      self._opr3()
    if(cla.val()):
      self.ac.set(0)
    minus_flag = (self.ac.val()>>11)
    zero_flag  = self.ac.val()
    link_flag  = not self.l.val()
    skip = False
    if(sma.val()):
      skip = skip or minus_flag
    if(sza.val()):
      skip = skip or zero_flag
    if(snl.val()):
      skip = skip or link_flag
    if(andor.val()):
      skip = not skip
    if skip:
      self.step()
    if(osr.val()):
      self.ac.set(self.ac.val()|self.switch_register())
    if(hlt.val()):
      self.halt = True
  def _opr3(self):
    cla  = Register(1)
    mqa  = Register(1)
    sca  = Register(1)
    mql  = Register(1)
    cod3 = Register(3)
    _na_ = Register(1)
    _na_.set(self.bits.val(),cod3,mql,sca,mqa,cla)
    if(cla.val()):
      self.ac.set(0)
    if(mqa.val()):
      self.ac.set(self.ac.val()|self.mq.val())
    if(sca.val()):
      self.ac.set(self.sc.val())
    if(mql.val()):
      self.mq.set(self.ac.val())
      self.ac.set(0)
    mcode = cod3.val()
    operand = self.core.get(self.pc())
    mq_val  = self.mq.val()
    if   mcode == 1:
      self.sc.set(operand)
    elif mcode == 2:
      self.mq.set(operand*mq_val,self.ac)
    elif mcode == 3:
      dividend = self.mq.get(self.ac)
      self.ac.set(dividend % operand)
      self.mq.set(dividend / operand)
    elif mcode == 4:
      print 'Error: NMI has not yet been implemented'
      halt = True
    elif mcode == 5:
      result = self.mq.get(self.ac)
      self.mq.set(result << (operand + 1))
    elif mcode == 6:
      result = self.mq.get(self.ac)
      rev = False
      if result & 0x800000:
        rev = True
        result = 0x7fffff-result
      result >>= operand
      if rev:
        result = 0x7fffff-result
      self.mq.set(result,self.ac)
    elif mcode == 7:
      result = self.mq.get(self.ac)
      self.mq.set(result >> (operand + 1))
    if mcode:
      self.step()

def diglist(s):
  if not s:
    return []
  return [int(car(s))]+diglist(cdr(s))

def binlist(n,len):
  n %= 1 << len
  n += 1 << len
  return diglist(cdr(cdr(cdr(bin(n)))))

def stringselect(strs, bins):
  ret = ''
  for i in range(0,strs.__len__()):
    if bins[i]:
      ret += strs[i]+' '
  return ret

def myoct(n, len):
  n %= (1 << (3 * len))
  n += (1 << (3 * len))
  return cdr(cdr(oct(n)))

def string_ins(ins):
  ret = ''
  op = Register(3)
  op1 = Register(1)
  op2 = Register(1)
  op3 = Register(1)
  rem = Register(9)
  i = Register(1)
  z = Register(1)
  arg = Register(7)
  dev = Register(6)
  rem.set(ins,op)
  if op.val() < 6:
    ret = ['AND','TAD','ISZ','DCA','JMS','JMP'][op.val()]
    ret += ' '
    arg.set(ins,z,i)
    if i.val():
      ret += 'I '
    if z.val():
      ret += 'Z '
    ret += myoct(arg.val(),3)
  elif op.val() == 6:
    op1.set(ins,op2,op3,dev)
    op1 = op1.val()
    op2 = op2.val()
    op3 = op3.val()
    if dev.val() == 3:
      if op2:
        if op3:
          ret = 'KRB '
        else:
          ret = 'KCC '
      elif op3:
        ret = 'KRS '
      if op1:
        ret += 'KSF '
    elif dev.val() == 4:
      if op2:
        if op3:
          ret = 'TLS '
        else:
          ret = 'TCF '
      elif op3:
        ret = 'TPC '
      if op1:
        ret += 'TSF '
    else:
      op.set(ins)
      ret = 'IOT ('+oct(dev.val())+') '+oct(op.val())
  else:
    rem = binlist(ins,12)
    if not rem[3]:
      ret = stringselect(['CLA','CLL','CMA','CML'],rem[4:])
      if rem[11]:
        ret += 'IAC '
      if rem[10]:
        if rem[8]:
          ret += 'RTR '
        if rem[7]:
          ret += 'RTL '
        if not(rem[7] or rem[8]):
          ret += 'BSW '
      else:
        if rem[8]:
          ret += 'RAR '
        if rem[7]:
          ret += 'RAL '
    else:
      if not rem[11]:
        if rem[8]:
          sel = ['SPA','SNA','SZL']
        else:
          sel = ['SMA','SZA','SNL']
        ret = stringselect(sel, rem[5:])
        if rem[4]:
          ret += 'CLA '
        ret += stringselect(['OSR','HLT'],rem[9:])
      else:
        if rem[4]:
          ret += 'CLA '
        if rem[5]:
          if rem[7]:
            ret += 'CAM '
          else:
            ret += 'MQA '
        elif rem[7]:
          ret += 'MQL '
        if rem[6]:
          ret += 'SCA '
        ins %= 8
        if ins:
          ins -= 1
          ret += ['SCL','MUY','DVI','NMI','SHL','ASR','LSR'][ins]
  if ret == '':
    ret = 'NOP'
  return ret

def idx_first_ch(ch,s):
  for i in range(0,s.__len__()):
    if s[i]==ch:
      return i
  return i+1

def separate_ch(ch,s):
  idx = idx_first_ch(ch,s)
  return s[0:idx],s[idx+1:]

def forcelist(x):
  if not hasattr(x,'__len__'):
    return [x]
  return x

def remove_allfirst(ch,s):
  i = 0
  while s[i] == ch:
    i += 1
  return s[i:]

def remove_dup(ch,s):
  if s == '':
    return s
  s = remove_allfirst(ch,s)
  idx = idx_first_ch(ch,s)+1
  return s[:idx]+remove_dup(ch,s[idx:])

def delimit(ch,raw):
  if raw == '':
    return []
  _car,_cdr=separate_ch(ch,raw)
  return cons(_car,delimit(ch,_cdr))

def delimit_ws(raw):
  return delimit(' ',raw)

def try_oct(s):
  r = 0
  if not s:
    return 0
  for ch in s:
    if (ch < '0') or (ch > '7'):
      return 0
    r = r * 8 + (ord(ch) - ord('0'))
  return r + 1

def lookup(dic,key):
  try:
    return dic[key]
  except:
    return -1

op_dic = \
{'and':00000,'tad':01000,'isz':02000,'dca':03000,\
 'jms':04000}

op_arg_dic = \
{'i':00400,'z':00200}

mneumonic_dic = \
{'cla':07200,'cll':07100,'cma':07040,'cml':07020,
 'iac':07001,'rar':07010,'ral':07004,'rtr':07012,
 'rtl':07006,'bsw':07002,'sma':07500,
 'sza':07440,'snl':07420,'osr':07404,'hlt':07402,
 'skp':07410,'spa':07510,'sna':07450,
 'szl':07430,'mqa':07501,'sca':07441,
 'mql':07421,'cam':07621,'nop':07401,'scl':07403,
 'muy':07405,'dvi':07407,'nmi':07411,'shl':07413,
 'asr':07415,'lsr':07417}

def read_op_elems(args):
  global op_arg_dic
  ret = 0
  next_i = 0
  if not args:
    return -1
  for i in range(0,args.__len__()-1):
    op = lookup(op_arg_dic,args[i])
    if op != -1:
      ret |= op
    else:
      return -1
  op = try_oct(args[args.__len__()-1])
  if op:
    return ret | (op - 1)
  return -1

def make_ins(s):
  global op_dic
  global mneumonic_dic
  val = try_oct(s)
  idx = 0
  if val:
    return val-1
  elems = delimit_ws(s.lower())
  op = lookup(op_dic,elems[0])
  if op != -1:
    orv = read_op_elems(cdr(elems))
    if orv != -1:
      op |= orv
    else:
      op = -1
    return op
  for elem in elems:
    orv = lookup(mneumonic_dic,elem)
    if orv != -1:
      val |= orv
    else:
      return -1
  return val

def check_addr(x):
  if last(x) == ':':
    return try_oct(unlast(x))-1
  return -1

def check_word(line):
  return delimit_ws(line).__len__() == 1

def check_numeric(line):
  for ch in line:
    if ch < '0' or ch > '9':
      return False
  return True

def check_mneumonic(line):
  if check_word(line):
    if last(line) == ',':
      line = unlast(line)
      if not check_numeric(line):
        return line.lower()
  return -1

def line_interp(line,pdp8,*interactive):
  pc = check_addr(line)
  if pc != -1:
    pdp8.setpc(pc)
    if interactive:
      if car(interactive):
        print "PC AT "+myoct(pc,4)
    return
  line = delimit_ws(line)
  i = 0
  for i in range(0,line.__len__()):
    line[i] = pdp8.swap_ifdef(line[i])
  line = str_concat(line)
  print line
  op = make_ins(line)
  if op != -1:
    pdp8.setcurr(op)
    if interactive:
      if car(interactive):
        printline(pdp8.pc(),op)
    pdp8.step()
    return
  op = check_mneumonic(line)
  if op != -1:
    if interactive:
      print op.upper() + ' IS ' + myoct(pdp8.pc(),4)
    pdp8.add_def(op,pdp8.pc())

def make_mem(code,pdp8):
  lines = delimit('\n',code)
  for line in lines:
    line_interp(line,pdp8)

def printline(loc,x):
  print myoct(loc,4)+': ' +\
        myoct(  x,4)+' ~ '+\
        string_ins(x)

def interact(pdp8):
  unquit = True
  echo = True
  cmdnum = 0
  print 'Interactive mode with machine ('+pdp8.name+')'
  print 'Memory size is '+repr(pdp8.core.numwords)+' '\
         +repr(pdp8.core.wordsize)+'-bit words'
  print '(type \'#help#\' for a list of commands)'
  while unquit:
    num = 1
    cmd = raw_input(pdp8.name + ' > ')
    cmds = delimit_ws(cmd.lower())
    if not cmds:
      cmd == ''
      num = 0
    else:
      cmd = car(cmds)
      cmdnum = 0
    if cmds.__len__() == 2:
      num = try_oct(cmds[1])
      if num:
        num -= 1
    i = 0
    while i < num:
      if cmd == '#run' or cmdnum == 1:
        pdp8.run()
        cmdnum = 1
        i = num
      elif cmd == '#step' or cmdnum == 2:
        if echo:
          printline(pdp8.pc(),pdp8.getcurr())
        pdp8.runstep()
        cmdnum = 2
      elif cmd == '#done' or cmdnum == 3:
        unquit = False
        i = num
      elif cmd == '#page' or cmdnum == 4:
        printline(pdp8.pc(),pdp8.getcurr())
        pdp8.step()
        cmdnum = 4
      elif cmd == '#reset' or cmdnum == 5:
        pdp8.reset()
        cmdnum = 5
        i = num
      elif cmd == '#echo' or cmdnum == 6:
        echo = not echo
        print 'ECHO',
        if echo:
          print 'ON'
        else:
          print 'OFF'
        cmdnum = 7
      elif cmd == '#help':
        print 'Command     Action'
        print '#run         Starts the program at the current PC'
        print '#step n      Steps the program n times'
        print '#done        Exits the interactive mode'
        print '#page n      Prints n (octal) lines starting at PC'
        print '#reset       Resets the current machine'
        print '#echo        Toggles echo mode'
        print 'nnnn:        Sets PC to nnnn (octal)'
        print 'alias,       Sets vaue of \'alias\' to PC'
        print '(opcode)     Inputs the value of (opcode) to memory'
        i = num
      elif cmd != '':
        line_interp(cmd, pdp8, echo)
      i += 1

def main():
  machine = PDP8(4096,"DEFAULT8")
  interact(machine)