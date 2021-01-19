#!/usr/bin/python3
# no support for: cuts, negation, ...

import copy, sys, re

class Atom():
    def __init__(self, name):
        self.name = name

    def __eq__ (self, other):
        return self.name == other.name

    def has_variables(self, other=None):
        return False

class Variable(Atom):
    def has_variables(self, other=None):
        return True or (other != None and self == other)

class Compound():
    def __init__(self, functor, arguments):
        self.functor = functor
        self.arguments = arguments

    def has_variables(self, other=None):
        for arg in self.arguments:
            if arg.has_variables(other):
                return True

        return False

class List:
    def __init__(self, elements):
        self.elements = elements
        self.name = None

    def __eq__ (self, other):
        if len(self.elements) != len(other.elements):
            return False

        equal = True

        for i, e in enumerate(self.elements):
            if e != other.elements[i]:
                equal = False

        return equal 

    def has_variables(self, other=None):
        for arg in self.elements:
            if arg.has_variables(other):
                return True

        return False

class Clause:
    def __init__(self, head, body):
        self.head = head 
        self.body = body

    def has_variables(self, other=None):
        if self.head.has_variables(other):
            return True
        else:
            for t in self.body:
                if t.has_variables(other):
                    return True
        return False

class Equation:
    def __init__(self, left, right):
        self.left = left
        self.right = right

class Conjunction:
    def __init__(self, terms):
        self.terms = terms

    def has_variables(self, other=None):
        for term in self.terms:
            if term.has_variables(other):
                return True
        return False

class Branch:
    def __init__(self, resolvent, G):
        self.resolvent = resolvent
        self.G = G


re_rule = re.compile('[a-z]+\(.*\)')
re_query = re.compile('[a-z]+\([a-zA-Z0-9_,\[\]\|]+\)[,?\.]')
re_atom = re.compile("([a-z][a-z_0-9]*)|(\'[a-zA-Z_0-9 ]+\')")
re_variable = re.compile('[A-Z_][A-Z0-9_]*')
re_args = re.compile('\(.+\)')
re_list = re.compile('\[.*\]')

rules = []

def parse_line(line):
    strip = line.strip()
    strip = strip.replace(' ', '')
    
    if re_rule.match(strip):
        parse_rule(strip)
    elif len(strip) > 0:
        print('error: line is not a rule. ('+strip+')')
        return False

    return True

def parse_rule(line):
    if line.find(':-') >= 0:
        parts = line.split(':-')
        head = parse_compound(parts[0])
        body = []
        body_parts = re_query.findall(parts[1])

        for term in body_parts:
            body.append(parse_compound(term[:-1]))

        rules.append(Clause(head, body))

    else:
        head = parse_compound(line)
        rules.append(Clause(head, []))

def parse_compound(term):
    functor = re_atom.findall(term)[0][0]
    args_str = re_args.findall(term.replace(' ', ''))[0]
    arg_parts = args_str[1:-1].split(',')
    args = []

    if args_str[1] == '[':
        args = [parse_term(args_str[1:-1])]
    else:
        for arg in arg_parts:
            args.append(parse_term(arg))

    return Compound(parse_term(functor), args)

def parse_conjunction(term):
    args_str = re_query.findall(term.replace(' ', ''))
    args = []

    for arg in args_str:
        args.append(parse_compound(arg[:-1]))

    if len(args) == 0:
        raise Exception("Invalid query.")

    return Conjunction(args)

def parse_term(term):
    if re_query.match(term):
        return parse_compound(term)
    # elif re_list.match(term):
    #     return parse_list(term)
    elif re_atom.match(term):
        return Atom(term)
    elif re_variable.match(term):
        return Variable(term)
    else:
        raise Exception("Invalid term", term)

def has_variable(term, x):
    return term.has_variables(x)

def replace(goal, substitution, variables=None):
    if isinstance(goal, Compound):
        for i, arg in enumerate(goal.arguments):
            replace(arg, substitution)

            for sub in substitution:
                if sub.left == arg:
                    goal.arguments[i] = sub.right

    elif isinstance(goal, Conjunction):
        for i, term in enumerate(goal.terms):
            replace(term, substitution, variables)

    elif isinstance(goal, Equation):
        replace(goal.left, substitution)
        replace(goal.right, substitution)

def unify(t1, t2):
    mgu = []
    stack = [Equation(t1, t2)]

    while len(stack) > 0:
        eq = stack.pop()

        if ((isinstance(eq.left, Variable) and isinstance(eq.right, Variable)
                and eq.left == eq.right) or
            (isinstance(eq.left, Atom) and isinstance(eq.right, Atom) and
                eq.left == eq.right)):
            continue
        if isinstance(eq.left, Variable):
            if isinstance(eq.right, Variable) and eq.left == eq.right:
                continue

            elif not has_variable(eq.right, eq.left):
                mgu.append(Equation(eq.left, eq.right))
                for t in stack:
                    replace(t, mgu)

        elif isinstance(eq.right, Variable):
            if not has_variable(eq.left, eq.right):
                mgu.append(Equation(eq.right, eq.left))
                for t in stack:
                    replace(t, mgu)

        elif (isinstance(eq.left, Compound) and isinstance(eq.right, Compound) and
                eq.left.functor == eq.right.functor):
            for i, arg in enumerate(eq.left.arguments):
                stack.append(Equation(arg, eq.right.arguments[i]))

        elif (isinstance(eq.left, List) and isinstance(eq.right, List) and
              len(eq.left.elements) == len(eq.right.elements)):
            for i, arg in enumerate(eq.left.elements):
                stack.append(Equation(arg, eq.right.elements[i]))

        else:
            return

    return mgu

def lookup_clause(goal):
    result = []

    for rule in rules:
        if rule.head.functor == goal.functor:
            args = rule.head.arguments

            if args == goal.arguments and not goal.has_variables():
                result.append(True)
            elif rule.has_variables() or goal.has_variables():
                s = unify(goal, rule.head)

                if not s == None:
                    result.append((s, rule.body))

    return result 

def resolve_clause(substitution, resolvent, G):
    if len(substitution) > 0:

        for i, goal in enumerate(resolvent):
            newgoal = copy.deepcopy(goal)
            replace(newgoal, substitution)
            resolvent[i] = newgoal

        replace(G, substitution)

def resolve(G):
    branch = [Branch(G.terms, copy.deepcopy(G))]
    done = []

    while len(branch) > 0:
        b = branch[0]

        while len(b.resolvent) > 0:
            did_branch = False
            goal = b.resolvent[0]

            clauses = lookup_clause(goal)

            if len(clauses) == 0:
                break

            for clause in clauses:
                if clause == True:
                    continue

                newb = Branch(b.resolvent[1:] + clause[1], copy.deepcopy(b.G))
                branch.append(newb)
                resolve_clause(clause[0], newb.resolvent, newb.G)
                did_branch = True

            if did_branch:
                break

            b.resolvent.pop(0)

        if len(b.resolvent) == 0:
            done.append(b)

        branch.remove(b)

    return done

def run_query(query_str):
    G = parse_conjunction(query_str)
    has_variables = G.has_variables()
    result = resolve(G)

    if len(result) > 0:
        if has_variables:
            for r in result:
                print_variables(G, r.G, {})
        else:
            print('true.')
    else:
        print('false.')

def test_query(query_str):
    print(query_str)
    run_query(query_str)
    print('')

def print_variables(G, H, printed):
    if isinstance(G, Conjunction):
        for i, t in enumerate(H.terms):
            print_variables(H.terms[i], G.terms[i], printed)
    elif isinstance(G, Compound):
        for i, t in enumerate(H.arguments):
            print_variables(H.arguments[i], G.arguments[i], printed)
    elif isinstance(G, Variable) and not G.name in printed:
        print('%s = %s.' %(G.name, H.name))
        printed[G.name] = 1

def get_input():
    while 1:
        try:
            data = input()

            if len(data) == 0:
                continue

            run_query(data)
        except KeyboardInterrupt:
            break
        except EOFError:
            break

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('no file')
        sys.exit()

    f = open(sys.argv[1])

    for line in f:
        res = parse_line(line)

        if not res:
            break

    get_input()
    f.close()
