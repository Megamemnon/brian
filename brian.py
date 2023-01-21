
import enum, sys 

ANSIESC='\033'
RESET=ANSIESC+'[0m'
VIOLET=ANSIESC+'[38;5;91m'
BLUE=ANSIESC+'[38;5;33m'
TEAL=ANSIESC+'[38;5;87m'
PINK=ANSIESC+'[38;5;206m'
GREEN=ANSIESC+'[38;5;47m'
YELLOW=ANSIESC+'[38;5;228m'
ORANGE=ANSIESC+'[38;5;222m'
RED=ANSIESC+'[38;5;160m'

PARENS='()'
LISTENS='[]'
CURLYENS='{}'
SEPARATORS=' \t\n\r'
BREAKING='()[]{}'

Rules = []
Program = []

class TokenType(enum.Enum):
  parenopen=1
  parenclose=2
  listopen=3
  listclose=4
  curlyopen=5
  curlyclose=6
  identifier=7
  operator=8
  text=9

def ispunctuation(char):
  if char.isprintable() and not char.isalnum():
    return True
  return False

class ASTNode():
  def __init__(self, nodetype, symbol, left, right, id):
    self.nodetype = nodetype
    self.symbol = symbol
    self.left = left
    self.right = right
    self.id = id

  def getFormula(self,paren=True):
    f=''
    match self.nodetype:
      case TokenType.operator:
        if paren and self.symbol!=',':
          f+='('
        f+=self.left.getFormula()
        if self.symbol=='$':
          f+='('
          f+=self.right.getFormula(False)
          f+=')'
        else:
          f+=self.symbol
          f+=self.right.getFormula()
        if paren and self.symbol!=',':
          f+=')'
      case TokenType.identifier|TokenType.text:
        f+=self.symbol
      case TokenType.listopen:
        f+='['
        f+=self.right.getFormula(False)
        f+=']'
      case TokenType.curlyopen:
        f+='{'
        f+=self.right.getFormula(False)
        f+='}'
    return f

  def equivalent(self, astnode):
    l=True
    r=True
    if astnode.nodetype==TokenType.identifier and astnode.symbol[0].isupper():
      return True
    if astnode.nodetype!=self.nodetype:
      return False
    if astnode.symbol!=self.symbol: 
      return False
    if self.left:
      if not astnode.left:
        return False
      else:
        l=self.left.equivalent(astnode.left)
    else:
      if astnode.left:
        return False
    if self.right:
      if not astnode.right:
        return False
      else:
        r=self.right.equivalent(astnode.right)
    else:
      if astnode.right:
        return False
    return l and r      

  def unify(self, astnode):
    if astnode.nodetype==TokenType.identifier and astnode.symbol[0].isupper():
      return [(astnode,self)]
    l=[]
    r=[]
    if astnode.nodetype!=self.nodetype:
      return None
    if astnode.symbol!=self.symbol:
      return None
    if self.left is not None:
      if astnode.left is None:
        return None
      l=self.left.unify(astnode.left)
    elif astnode.left is not None:
      return None
    if self.right is not None:
      if astnode.right is None:
        return None
      r=self.right.unify(astnode.right)
    elif astnode.right is not None:
      return None
    if l is None and r is None:
      return None
    unifier=[]
    if type(l)==list:
      unifier.extend(l)
    if type(r)==list:
      unifier.extend(r)
    return unifier

  def resolve(self, astnode):
    resolution=[]
    if self.equivalent(astnode):
      x=self.unify(astnode)
      if x is not None:
        resolution.append([self, x])
    if self.left is not None:
      x=self.left.resolve(astnode)
      if x is not None:
        resolution.extend(x)
    if self.right is not None:
      x=self.right.resolve(astnode)
      if x is not None:
        resolution.extend(x)
    if resolution==[]:
      return None
    return resolution

  def replaceVariable(self, var, newNode):
    l=False
    r=False
    if self.left is not None:
      if self.left.nodetype==TokenType.identifier and self.left.symbol==var:
        self.left=newNode.copy()
        l=True
      else:
        l=self.left.replaceVariable(var, newNode)
    if self.right is not None:
      if self.right.nodetype==TokenType.identifier and self.right.symbol==var:
        self.right=newNode.copy()
        r=True
      else:
        r=self.right.replaceVariable(var, newNode)
    if l or r:
      return True
    return False

  def replaceNode(self, currentNode, newNode):
    if self.left is not None:
      if self.left.id==currentNode.id:
        self.left=newNode
        return True
      elif self.left.replaceNode(currentNode, newNode)==True:
        return True
    if self.right is not None:
      if self.right.id==currentNode.id:
        self.right=newNode
        return True
      elif self.right.replaceNode(currentNode, newNode)==True:
        return True
    return False

  def copy(self):
    left=None
    right=None
    if self.left is not None:
      left=self.left.copy()
    if self.right is not None:
      right=self.right.copy()
    return ASTNode(self.nodetype, self.symbol, left, right, self.id)

def formulaToAST(formula):
  i=0               # index variable of formula string
  opstack=[]        # temp list of operators for converting formula to postix
  postfix=[]        # list of words in postfix order
  connectives=[]    # temp list of connectives
  output=[]         # temp list of words to make Nodes from
  node=None         # new ASTNode
  impliedop = False # an identifier prior to parenopen, listopen, or curlyopen
  priorword=''      #   implies the 'application' operator designated internally by '$'

  def getwordtype(w):
    if w[0]==PARENS[0]:
      return TokenType.parenopen
    if w[0]==PARENS[1]:
      return TokenType.parenclose
    if w[0]==LISTENS[0]:
      return TokenType.listopen
    if w[0]==LISTENS[1]:
      return TokenType.listclose
    if w[0]==CURLYENS[0]:
      return TokenType.curlyopen
    if w[0]==CURLYENS[1]:
      return TokenType.curlyclose
    if w[0]=='"':
      return TokenType.text
    if w[0]==' ':
      return TokenType.identifier
    if w[0].isalnum():
      return TokenType.identifier
    else:
      return TokenType.operator

  def getnextword(i):
    if i>=len(formula):
      return None
    w=''  # word
    c=formula[i]
    while c in SEPARATORS:
      i+=1
      if i>=len(formula):
        return None
      c=formula[i]
    if c in PARENS or c in LISTENS or c in CURLYENS or c==' ':
      return (c,i+1)
    if c=='"':
      textend=0
      while textend<2:
        w+=c
        i+=1
        if c=='"':
          textend+=1
        if i>=len(formula):
          w+='"'
          return (w,i)
        c=formula[i]
      return (w,i)
    if ispunctuation(c):      
      while ispunctuation(c):
        if c in BREAKING:
          return (w,i)
        w+=c
        i+=1
        if i>=len(formula):
          return (w,i)
        c=formula[i]
        if c in SEPARATORS:
          return (w,i)
    else:
      while c.isalnum():
        w+=c
        i+=1
        if i==len(formula):
          return (w,i)
        c=formula[i]
        if c in SEPARATORS:
          return (w,i)
    return (w,i)

  # convert formula to postfix
  wt=getnextword(i)
  if wt is not None:
    i=wt[1]
    word=wt[0]
  else:
    word=None
  while word is not None:
    wtype=getwordtype(word)
    match wtype:
      case TokenType.parenopen:
        postfix.append((word,wtype))
        opstack.append((word,wtype))
      case TokenType.parenclose:
        op=opstack.pop()
        while op[1]!=TokenType.parenopen:
          postfix.append(op)
          op=opstack.pop()
        postfix.append((word,wtype))
      case TokenType.listopen:
        postfix.append((word,wtype))
        opstack.append((word,wtype))
      case TokenType.listclose:
        op=opstack.pop()
        while op[1]!=TokenType.listopen:
          postfix.append(op)
          op=opstack.pop()
        postfix.append((word,wtype))
      case TokenType.curlyopen:
        postfix.append((word,wtype))
        opstack.append((word,wtype))
      case TokenType.curlyclose:
        op=opstack.pop()
        while op[1]!=TokenType.curlyopen:
          postfix.append(op)
          op=opstack.pop()
        postfix.append((word,wtype))
      case TokenType.identifier|TokenType.text:
        postfix.append((word,wtype))
      case TokenType.operator:
        if len(opstack)>0:
          # Comma is binary and right-associative
          if word==',':
            op=opstack.pop()
            if op[0]==',':
              opstack.append(op)
            else:
              while op[1]!=TokenType.parenopen and op[1]!=TokenType.listopen \
                and op[1]!=TokenType.curlyopen and op[0]!=',' \
                  and len(opstack)>0:
                postfix.append(op)
                op=opstack.pop()
              if op[1]==TokenType.parenopen or op[1]==TokenType.listopen \
                or op[1]==TokenType.curlyopen or op[0]==',':
                opstack.append(op)
          else:
            # handle all left-associative binary operators w/o special precedence
            op=opstack.pop()
            if op[1]!=TokenType.parenopen and op[1]!=TokenType.listopen \
              and op[1]!=TokenType.curlyopen:
              postfix.append(op)
            else:
              opstack.append(op)
        opstack.append((word,wtype))
    if impliedop:
      word=priorword
      impliedop=False
    else:
      wt=getnextword(i)
      if wt is not None:
        i=wt[1]
        word=wt[0]
        if wtype==TokenType.identifier and word in '([{':
          # add implied application operator
          priorword=word
          word='$'
          impliedop=True
      else:
        word=None 
  while len(opstack)>0:
    postfix.append(opstack.pop())
  
  postfix2=[]
  for term in postfix:
    if term[1]==TokenType.text:
      postfix2.append(('[',TokenType.listopen))
      terma=term[0][1:-1]
      for txt in terma:
        if txt==' ':
          postfix2.append((' ', TokenType.identifier))
        else:
          postfix2.append((txt, TokenType.identifier))
      for count in range(len(terma)-1):
        postfix2.append((',',TokenType.operator))
      postfix2.append((']',TokenType.listclose))
    else:
      postfix2.append(term)
  postfix=postfix2

  # convert postfix to AST
  id=1      # node id counter
  index=0   # postfix list index
  while index<len(postfix):
    wordtuple=postfix[index]
    match wordtuple[1]:
      case TokenType.identifier:
        node=ASTNode(wordtuple[1],wordtuple[0],None, None, id)
        id+=1
        output.append(node)
      case TokenType.text:
        node=ASTNode(wordtuple[1],wordtuple[0],None, None, id)
        id+=1
        output.append(node)
      case TokenType.listopen:
        output.append(wordtuple[0])
        node=ASTNode(wordtuple[1], wordtuple[0], None, None, id)
        id+=1
        connectives.append(node)
      case TokenType.listclose:
        tmpstack=[]
        right=output.pop()
        while right!=LISTENS[0]:
          if len(connectives)!=0:
            node=connectives.pop()
            node.right=right
            tmpstack.append(node)
          right=output.pop()
        output.extend(tmpstack)
      case TokenType.curlyopen:
        output.append(wordtuple[0])
        node=ASTNode(wordtuple[1], wordtuple[0], None, None, id)
        id+=1
        connectives.append(node)
      case TokenType.curlyclose:
        tmpstack=[]
        right=output.pop()
        while right!=CURLYENS[0]:
          if len(connectives)!=0:
            node=connectives.pop()
            node.right=right
            tmpstack.append(node)
          right=output.pop()
        output.extend(tmpstack)
      case TokenType.operator:
        right=output.pop()
        left=output.pop()
        node=ASTNode(wordtuple[1], wordtuple[0], left, right, id)
        id+=1
        output.append(node)
    index+=1
  while len(connectives)!=0:
    right=output.pop()
    node=connectives.pop()
    node.right=right
    output.append(node)
  return output[0]

def colorizeFormula(formula):
  paren=0
  newformula=''
  colors=[BLUE, PINK, TEAL, VIOLET]
  for c in formula:
    if c=='(':
      paren+=1
      color=colors[paren%4]
      newformula+=color+c+RESET
    elif c==')':
      newformula+=color+c+RESET
      paren-=1
      color=colors[paren%4]
    elif c in LISTENS:
      newformula+=GREEN+c+RESET
    elif c in CURLYENS:
      newformula+=ORANGE+c+RESET
    elif ispunctuation(c):
      newformula+=YELLOW+c+RESET
    else:
      newformula+=c
  return newformula

def getUnifierString(unifier):
  u='{'
  for c,var in enumerate(unifier):
    u+=var[0].getFormula(False)+'|'+var[1].getFormula(False)
    if c!=len(unifier)-1:
      u+=','
  u+='}'
  return u

def applyUnifier(node, unifier):
  if type(unifier) is list:
    for u in unifier:
      if node.symbol==u[0].symbol:
        node=u[1]
      else:
        node.replaceVariable(u[0].symbol, u[1])
  else:
    if node.symbol==unifier[0].symbol:
      node=unifier[1]
    else:
      node.replaceVariable(unifier[0].symbol,unifier[1])
  return node

def getUnifierErrors(unifier):
  uerrors=[]
  uclean=[]
  for u in unifier:
    if len(uclean)==0:
      uclean.append(u)
    else:
      found=False
      u1=u[1].getFormula(False)
      u0=u[0].getFormula(False)
      for uc in uclean:
        uc1=uc[1].getFormula(False)
        uc0=uc[0].getFormula(False)
        if u1==uc1 and u0!=uc0:
          uerrors.append(u)
          uerrors.append(uc)
          found=True
      if not found:
        uclean.append(u)
  uclean=[]
  for ue in uerrors:
    if len(uclean)==0:
      uclean.append(ue)
    else:
      ue1=ue[1].getFormula(False)
      ue0=ue[0].getFormula(False)
      for uc in uclean:
        uc1=uc[1].getFormula(False)
        uc0=uc[0].getFormula(False)
        if ue1==uc1 and ue0!=uc0:
          uclean.append(ue)
  return uclean

def applyRules():
  debug=False
  i=0
  for i in range(len(Program)):
    stmnt=Program[i]
    formula_ast=formulaToAST(stmnt)
    # apply user-provided transformation rules
    changed=True
    while changed:
      changed=False
      for rule in Rules:
        rule_ast = formulaToAST(rule)
        rterm=rule_ast.left
        resolutions=formula_ast.resolve(rterm)
        if resolutions is not None:
          for fnu in resolutions:
            matchedNode=fnu[0]
            rplc=rule_ast.right.copy()
            rulerhs=rplc.getFormula(False)
            for nu in fnu[1]:
              rplc=applyUnifier(rplc, nu)
            modrhs=rplc.getFormula(False)
            if rulerhs!=modrhs:
              if debug:
                print(f'DEBUG: Statement - {colorizeFormula(formula_ast.getFormula(False))}')
                print(f'DEBUG:   Rule - {colorizeFormula(rule)}')
                print(f'DEBUG:     Matched Node - {colorizeFormula(matchedNode.getFormula(False))}')
                print(f'DEBUG:       Transformed Node - {colorizeFormula(modrhs)}')
              if matchedNode.id==formula_ast.id:
                formula_ast=rplc
                changed=True
              else:
                formula_ast.replaceNode(matchedNode,rplc)
                changed=True
    Program[i]=formula_ast.getFormula(False)

    # run builtins
    builtin='print(X)'
    b_ast=formulaToAST(builtin)
    resolutions=formula_ast.resolve(b_ast)
    if resolutions is not None:
      for fnu in resolutions:
        for nu in fnu[1]:
          term=nu[1].getFormula(False)
          if term[0]=='[' and term[-1]==']':
            term=term[1:-1]
            termlist=term.split(',')
            term=''
            for termx in termlist:
              term+=termx
          print(f'output: {colorizeFormula(term)}')
    builtin='debug(t)'
    b_ast=formulaToAST(builtin)
    resolutions=formula_ast.resolve(b_ast)
    if resolutions is not None:
      debug=True
    builtin='debug(f)'
    b_ast=formulaToAST(builtin)
    resolutions=formula_ast.resolve(b_ast)
    if resolutions is not None:
      for fnu in resolutions:
        for nu in fnu[1]:
          debug=False


def loadFile(filepath):
  with open(filepath, "r") as f:
    buffer=f.readlines()
  for i,b in enumerate(buffer):
    line=b
    if b[-1]=='\n':
      line=b[:-1]
    l=line.split()
    if len(l)>0:
      if l[0][0]!='#':
        try:
          ast=formulaToAST(line)
          line=ast.getFormula(False)
        except:
          print(f'Unable to parse line {i}: {line}')
          sys.exit(0)
        if ast.symbol=='->':
          Rules.append(line)
        else:
          Program.append(line)

def printList(l):
  for c,entry in enumerate(l):
    print(f'  {c+1}  {entry}')

def main():
  print(f'Brian v0.1 Copyright (c) 2022 Brian O\'Dell')
  if len(sys.argv)<2:
    print(f'usage: {sys.argv[0]} program')
  loadFile(sys.argv[1])
  print('Rules')
  printList(Rules)
  print('Program')
  printList(Program)
  applyRules()
  print('Transformed Program')
  printList(Program)

if __name__=='__main__':
  main()
