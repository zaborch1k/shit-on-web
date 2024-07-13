import ply.lex as lex
import ply.yacc as yacc 
from ply.lex import LexToken

import re

# лексер
keywords = (
    "SET",
    'RIGHT',
    'LEFT',
    'UP',
    'DOWN',
    'IFBLOCK',
    'ENDIF',
    'REPEAT',
    'ENDREPEAT',
    'PROCEDURE',
    'ENDPROC',
    'CALL'
)

tokens = keywords + (
    "DIVIDE",
    "EQUALS",
    "ID",
    "LPAREN",
    "MINUS",
    'NUMBER',
    "PLUS",
    "RPAREN",
    "TIMES",
    'NEWLINE',
    'INDENT',
    'DEDENT',
    'WS'
)

def t_ID(t):
    r"[a-zA-Z][a-zA-Z]*"
    if t.value in keywords:
        t.type = t.value
    return t

t_PLUS    = r"\+"
t_MINUS   = r"-"
t_TIMES   = r"\*"
t_DIVIDE  = r"/"
t_EQUALS  = r"="
t_LPAREN  = r"\("
t_RPAREN  = r"\)"
t_WS = r'[^\n\S]+'
#t_NEWLINE = r'\n'

def t_NEWLINE(t):
    r'\n'
    t.lexer.lineno += len(t.value)
    return t

def t_NUMBER(t):
    r"\d+"
    t.value = int(t.value)
    return t

def t_error(t):
    t.lexer.error = t.value[0]


# второй лексер, для обработки отступов
class IndentLex:
    def __init__(self, lexer):
        self.lexer = lexer
        self.tok = None
        self.data = None
        self.lineno = 0
        self.error = None

    def input(self, data):
        self.lexer.input(data)

    def token(self):
        if self.tok is None:
            self.tok = self._token()

        try:
            return next(self.tok)
        except StopIteration:
            return None

    def empty_tok(self):
        tok = LexToken()
        (tok.type,
         tok.value,
         tok.lineno,
         tok.lexpos) = ('', '', 0, 0)

        return tok

    def logical_lines(self):
        for t in self.lexer:
            tokens = []
            indent = 0

            while t.type != 'NEWLINE':
                if t.type != 'WS':
                    tokens.append(t)
                elif not tokens:
                    indent = len(t.value)
                t = self.lexer.token()
            tokens.append(t)

            if tokens[0].type == 'NEWLINE' and len(tokens) == 1:
                continue

            if tokens:
                yield tokens, indent
        yield 'EOF', 0

    def __iter__(self):
        return self._token()

    def _token(self):
        indent_stack = [0]

        for tokens, indent in self.logical_lines():
            indent = indent
            indent_tok = self.empty_tok()
            
            if tokens == 'EOF':
                while len(indent_stack) > 1:
                    indent_tok.type = 'DEDENT'
                    indent_stack.pop()
                    yield indent_tok
                break

            last_indent = indent_stack[-1]

            if last_indent < indent:
                indent_stack.append(indent)
                indent_tok.type = 'INDENT'
                yield indent_tok

            elif last_indent > indent:
                indent_tok.type = 'DEDENT'
                while indent_stack[-1] > indent:
                    indent_stack.pop()
                    yield indent_tok
                if indent_stack[-1] != indent:
                    self.error = 'неправильный уровень отступа'
            yield from tokens

# парсер
error = None   
            
def check_nest_lvl():
    global nest_lvl
    nest_lvl += 1
    err = None
    if nest_lvl > 3:
        err = True
    return err

def check_error():
    global error
    if not error:
        return None
    else:
        return 'continue'

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS')
)


def p_program(p):
    '''program : program statement
               | statement
               | '''
    global l 
    if len(p) == 2 and p[1]:
        p[0] = {}
        p[0][l] = p[1]
    elif len(p) == 3 and p[1]:
        p[1][l] = p[2]
        p[0] = p[1]
    l += 1


def p_block(p):
    '''block : NEWLINE INDENT groupstat DEDENT'''
    p[0] = (p[3])

def p_groupstat(p):
    '''groupstat : groupstat statement
                 | '''
    global group
    if len(p) == 3 and p[1]:
        if group:
            group = group + [p[2]]
        else:
            group = [p[2]]
        p[0] = group
    elif len(p) == 3 and p[2]:
        group = [p[2]]
        p[0] = group


def p_statement(p):
    '''statement : command NEWLINE'''
    p[0] = p[1]
    

def p_command_ifblock(p):
    '''command : IFBLOCK RIGHT block ENDIF
               | IFBLOCK DOWN block ENDIF
               | IFBLOCK UP block ENDIF
               | IFBLOCK LEFT block ENDIF'''
    global group
    group = None
    err = check_nest_lvl()
    if err:
        p[0] = 'превышение максимального уровня вложенности (больше 3)'
    else:
        p[0] = (p[1], p[2], p[3])

def p_command_ifblock_error(p):
    '''command : IFBLOCK error block ENDIF
               | IFBLOCK error groupstat ENDIF'''
    global error
    if p[2].value == '\n' or p[2].value == 'ENDIF':
        error = f"отстутствует параметр для {p[1]}"
    else:
        error = f"недопустимый параметр для IFBLOCK: {p[2].value}"

def p_command_ifblock_error1(p):
    '''command : IFBLOCK RIGHT error groupstat ENDIF
               | IFBLOCK DOWN error groupstat ENDIF
               | IFBLOCK UP error groupstat ENDIF
               | IFBLOCK LEFT error'''
    global error
    e = check_error()
    if not e:
        if p[3].value == '\n' or p[3].value == 'ENDIF' or not p[3]:
            error = f"отстутствует блок кода после IFBLOCK"
        else:
            error = f'неправильный уровень отступа после IFBLOCK {p[2]}'


def p_command_repeat(p):
    '''command : REPEAT expr block ENDREPEAT'''
    global group
    group = None
    err = check_nest_lvl()
    if err:
        p[0] = 'превышение максимального уровня вложенности (больше 3)'
    else:
        p[0] = (p[1], p[2], p[3])

def p_command_repeat_error(p):
    '''command : REPEAT error block ENDREPEAT
               | REPEAT error groupstat ENDREPEAT'''
    global error
    if p[2].value == '\n' or p[2].value == 'ENDREPEAT':
        error = f"отстутствует параметр для {p[1]}"
    else:
        error = f'недопустимый параметр для {p[1]}: {p[2].value}'

def p_command_repeat_error1(p):
    '''command : REPEAT expr error groupstat ENDREPEAT'''
    global error
    e = check_error()
    if not e:
        if p[3].value == 'ENDREPEAT' or p[3].value == '\n' or not p[3]:
            error = f'отстутствует блок кода после REPEAT'
        else:
            error = f'неправильный уровень отступа после REPEAT'


def p_command_procedure(p):
    '''command : PROCEDURE ID block ENDPROC'''
    global group
    group = None
    err = check_nest_lvl()
    if err:
        p[0] = 'превышение максимального уровня вложенности (больше 3)'
    else:
        p[0] = (p[1], p[2], p[3])

def p_command_procedure_error(p):
    '''command : PROCEDURE error block ENDPROC
               | PROCEDURE error groupstat ENDPROC '''
    global error
    if p[2].value == 'ENDPROC' or p[2].value == '\n' or not p[2]:
        error = f'отсутствует имя процедуры'
    else:
        error = f'недопустимое имя процедуры: {p[2].value}'

def p_command_procedure_error1(p):
    '''command : PROCEDURE ID error groupstat ENDPROC'''
    global error
    e = check_error()
    if not e:
        if p[3].value == 'ENDPROC' or p[3].value == '\n' or not p[3]:
            error = f'отстутствует блок кода после PROCEDURE'
        else:
            error = f'неправильный уровень отступа после PROCEDURE'


def p_command_call(p):
    '''command : CALL ID'''
    p[0] = (p[1], p[2])

def p_command_call_error(p):
    '''command : CALL error'''
    global error
    if p[2].value == '\n':
        error = f"отстутствует параметр для {p[1]}"
    else:
        error = f"недопустимый параметр для {p[1]}: '{p[2].value}' "


def p_command_dir(p):
    '''command : RIGHT expr
               | LEFT expr
               | UP expr
               | DOWN expr'''
    p[0] = p[1], p[2]

def p_command_dir_error(p):
    '''command : RIGHT error
               | LEFT error
               | UP error
               | DOWN error'''
    global error
    e = check_error()
    if not e:
        if p[2].value == '\n':
            error = f"отстутствует параметр для {p[1]}"
        else:
            error = f"недопустимый параметр для {p[1]}: '{p[2].value}' "


def p_command_set(p):
    '''command : SET ID EQUALS expr'''
    p[0] = (p[1], p[2], p[4])
    

def p_command_set_error(p):
    '''command : SET error'''
    global error
    if p[2].value == '\n':
        error = f"отсутствует имя переменной"
    else:
        error = f"недопустимое имя переменной: '{p[2].value}'"

def p_command_set_error2(p):
    '''command : SET ID error'''
    global error
    error = f"нет '=' в выражении SET"

def p_command_set_error3(p):
    '''command : SET ID EQUALS error'''
    global error
    if p[4].value == '\n':
        error = f"отстутствует значение переменной"
    else:
        error = f"недопустимое значение для переменной: '{p[4].value}'"


def p_expr(p):
    '''expr : expr PLUS factor
            | expr MINUS factor
            | factor'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('binop', p[2], p[1], p[3])

def p_expr_uminus(p):
    '''expr : MINUS expr %prec UMINUS'''
    p[0] = ('UMINUS', '-', p[2])

def p_factor(p):
    '''factor : factor TIMES fact
              | factor DIVIDE fact
              | fact'''
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('binop', p[2], p[1], p[3])


def p_fact_number(p):
    '''fact : NUMBER'''
    p[0] = ("num", p[1])

def p_fact_var(p):
    "fact : ID"
    p[0] = ("var", p[1])

def p_fact_paren(p):
    "fact : LPAREN expr RPAREN"
    p[0] = p[2]

def p_error(p):
    global perror
    if p:
        perror = f"недопустимая команда '{p.value}'"

# for debugging only

data = '''
IFBLOCK RIGHT
    RIGHT 3
ENDIF
'''
def ccount(s, word):
    return len(re.findall(word, s))

def check_no_end(start_stat, end_stat):
    error = None
    if start_stat[1] > end_stat[1]:
        error = f'нет {end_stat[0]} для {start_stat[0]}'
    elif start_stat[1] < end_stat[1]:
        error = error = f'нет {start_stat[0]} для {end_stat[0]}'
    return error

def parse(data):
    global l, nest_lvl, error, perror, group
    group = None
    perror = None
    error = None
    l = 0
    nest_lvl = 0
    data = data + '\n'

    repeat_count = ('REPEAT', ccount(data, '\\bREPEAT\\b'))
    endrep_count = ('ENDREPEAT', ccount(data, '\\bENDREPEAT\\b'))

    if_count = ('IFBLOCK', ccount(data, '\\bIFBLOCK\\b'))
    endif_count = ('ENDIF', ccount(data, '\\bENDIF\\b'))

    proc_count = ('PROCEDURE', ccount(data, '\\bPROCEDURE\\b'))
    endproc_count = ('ENDPROC', ccount(data, '\\bENDPROC\\b'))

    start_end = [[repeat_count, endrep_count], [if_count, endif_count], [proc_count, endproc_count]]

    while True:
        for i in start_end:
            err = check_no_end(i[0], i[1])
            if err:
                break
        break

    try:
        lexer1 = lex.lex()
        lexer = IndentLex(lexer1)
        if not lexer.error and not err:
            parser = yacc.yacc(debug=False, errorlog=yacc.NullLogger(), debuglog=None)
            p = parser.parse(data, lexer=lexer, debug=False)
        else:
            error = err
    except lex.LexError:
        error = f"недопустимый символ '{lexer1.error}'"

    if error:
        return {'0': error}
    elif perror:
        return {'0': perror}
    return p