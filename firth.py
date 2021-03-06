#!/bin/python3

class Op:
    def __init__(self, f):
        self.f = f

class BinOp(Op):
    pass

class Word:
    def __init__(self, name, values):
        self.name = name
        self.values = values

glossary = {
    '+' : BinOp(lambda args: args[0] + args[1]),
    '-' : BinOp(lambda args: args[0] - args[1]),
    '/' : BinOp(lambda args: args[0] / args[1]),
    '*' : BinOp(lambda args: args[0] * args[1])
}

stack = []
# debug = True
debug = False
NONE = 0
DEF = 1

def error(msg):
    print("Error: {}".format(msg))

def push(v):
    stack.append(v)

def pop():
    return stack.pop()

def peek():
    return stack[-1]

def repl():
    i = input()
    tokens = i.split(' ')
    result = 0
    state = NONE

    for t in tokens:
        state = ev(t, state)
        if result < 0:
            break

    if result == 0:
        print ("ok")

    if debug:
        print("STACK")
        print(stack)
        print("DICTIONARY")
        print(glossary)
        print('\n\n\n')

def ev(t, state):
    if t == '':
        return state

    if t == '.':
        print(pop())
        return state

    try:
        v = int(t)
        push(v)
        return state
    except:
        pass

    if ':' == t:
        state = DEF
        push(':')
        return DEF

    if ';' == t:
        if DEF == state:
            vals = [pop()]

            while vals[-1] != ':':
                vals.append(pop())

            define(vals[-2], list(reversed(vals[0:-2])))
            return NONE
        else:
            error("Unexpected semicolon")
            return -2

    if DEF == state:
        push(t)

    elif t in glossary:
        word = glossary[t]

        if isinstance(word, Word):
            for v in word.values:
                ev(v, state)
        elif isinstance(word, BinOp):
            push(word.f([pop(), pop()]))

    else:
        error("Undefined word")
        return -1

    return state

def define(name, values):
    if name in glossary:
        error("Word already defined")
        return -1

    glossary[name] = Word(name, values)

while 1:
    repl()
