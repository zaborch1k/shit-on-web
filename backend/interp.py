# interp
class Interp:
    def __init__(self, prog):
        if prog:
            self.prog = list(prog.values())

    def eval(self, expr):
        etype = expr[0]

        if etype == 'num':
            num = self.do_int(expr[1])
            return num

        elif etype == 'binop':
            if expr[1] == '+':
                return self.eval(expr[2]) + self.eval(expr[3])
            elif expr[1] == '-':
                return self.eval(expr[2]) - self.eval(expr[3])
            elif expr[1] == '*':
                return self.eval(expr[2]) * self.eval(expr[3])
            elif expr[1] == '/':
                return self.do_int(float(self.eval(expr[2]) / self.eval(expr[3])))

        elif etype == 'UMINUS':
            return -1 * self.eval(expr[2])

        elif etype == 'var':
            var = expr[1]
            if var in self.vars.keys():
                if self.vars[var][0] == 'func':
                    self.error = 'недопустимое действие с процедурой'
                else:
                    return self.vars[var][0]
            else:
                self.error = 'обращение к неопределенной переменной'

    def assign(self, target, value, type=None):
        var = target
        if var in self.vars.keys() and type and self.vars[var][0] == 'func':
            self.error = 'объявление уже существующей процедуры'
        elif type:
            # if function
            self.vars[var] = (type, value)
        else:
            # if just variable
            value = self.eval(value)
            if not self.error:
                self.error = self.check_range_error(value)
            if not self.error:
                self.vars[var] = (value,)

    def check_range_error(self, num):
        msg = None
        if not isinstance(num, int):
            msg = 'не принимает нецелые значения'
        elif num < 1:
            msg = 'не может быть < 1'
        elif num > 1000:
            msg = 'не может быть > 1000'

        if msg:
            return f'неверное значение параметра команды ({msg})'
        else:
            return None

    def do_int(self, num):
        if num % 1 == 0:
            num = int(num)
        return num

    def run(self):
        self.vars = {}
        self.error = None
        self.qmove = []
        self.qpos = []
        self.pos = [1, 1]
        m = 21
        self.pc = 0

        while 1:
            try:
                if isinstance(self.prog[0], str):
                    self.error = self.prog[0]
                    self.prog = []
                    op = None

                else:
                    instr = self.prog[self.pc]
                    op = instr[0]

            except:
                return (self.qmove, self.error, self.qpos)

            if op == 'SET':
                target = instr[1]
                value = instr[2]
                self.assign(target, value)

            elif op in ('RIGHT', 'LEFT', 'DOWN', 'UP'):
                num = self.eval(instr[1])
                if not self.error:
                    self.error = self.check_range_error(num)

                if not self.error:
                    self.qmove.append(op)
                    if op == 'RIGHT':
                        self.pos[0] += num
                    elif op == 'LEFT':
                        self.pos[0] -= num
                    elif op == 'UP':
                        self.pos[1] += num
                    else:
                        self.pos[1] -= num
                    self.qmove.append(str(num))

            elif op == 'CALL':
                if instr[1] in self.vars:
                    del self.prog[self.pc]
                    self.prog.insert(self.pc+1, self.vars[instr[1]][1][0])
                    self.pc -= 1
                else:
                    self.error = 'вызов неопределенной процедуры'

            elif op == 'IFBLOCK':
                marker = False
                if instr[1] == 'RIGHT':
                    if self.pos[0] == m:
                        marker = True
                elif instr[1] == 'LEFT':
                    if self.pos[0] == 1:
                        marker = True
                elif instr[1] == 'UP':
                    if self.pos[1] == m:
                        marker = True
                else:
                    if self.pos[1] == 1:
                        marker = True

                if marker:
                    del self.prog[self.pc]
                    for z in instr[2]:
                        self.prog.insert(self.pc+1, z)
                    self.pc -= 1

            elif op == 'PROCEDURE':
                self.assign(instr[1], instr[2], 'func')

            elif op == 'REPEAT':
                val = self.eval(instr[1])
                if not self.error:
                    self.error = self.check_range_error(val)
                if not self.error:
                    del self.prog[self.pc]

                    for i in range(0, val):
                        for z in instr[2]:
                            self.prog.insert(self.pc+1, z)

                    self.pc -= 1

            if self.pos[0] > m or self.pos[1] > m or self.pos[0] < 1 or self.pos[1] < 1:
                del self.qmove[-1]
                self.error = 'попытка выйти за границы поля'

            if self.error:
                self.prog = {}
            else:
                self.qpos.append(self.pos.copy())
            self.pc += 1


def path(p):
    if getattr(sys, 'frozen', False):
        base = sys._MEIPASS
    else:
        base = os.getcwd()
    return os.path.join(base, p)


def get_data(data):
    return do_interp(data)


def do_interp(data):
    from backend.lexparse import parse
    i = None
    data = parse(data)
    i = Interp(data)
    return i.run()
