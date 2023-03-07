
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'leftORleftANDnonassocINFSUPBOOLEQUALINFEQUALSUPEQUALleftPLUSMINUSleftTIMESDIVIDEAND BOOLEQUAL COMA DIVIDE ELSE EQUALS FOR FUNC IF INF INFEQUAL LACCOLADE LPAREN MINUS NAME NUMBER OR PLUS PRINT PRINTSTR RACCOLADE RETURN RPAREN SEMI SHARP SUP SUPEQUAL TIMES WHILEstart : bloc bloc : bloc statement SEMI\n            | statement SEMIstatement : NAME EQUALS expressionstatement : PRINT LPAREN expression RPARENstatement : PRINTSTR LPAREN strings RPARENstatement : IF LPAREN expression RPAREN LACCOLADE bloc RACCOLADE\n                 | IF LPAREN expression RPAREN LACCOLADE bloc RACCOLADE ELSE LACCOLADE bloc RACCOLADEstatement : WHILE LPAREN expression RPAREN LACCOLADE bloc RACCOLADEstatement : FOR LPAREN NAME EQUALS expression SEMI expression SEMI NAME EQUALS expression RPAREN LACCOLADE bloc RACCOLADEstatement : FUNC NAME LPAREN RPAREN LACCOLADE bloc RACCOLADE\n                 | FUNC NAME LPAREN param RPAREN LACCOLADE bloc RACCOLADEstatement : NAME LPAREN RPAREN\n                 | NAME LPAREN expression RPARENstatement : SHARP stringsexpression : expression PLUS expression\n                  | expression MINUS expression\n                  | expression TIMES expression\n                  | expression DIVIDE expression\n                  | expression INF expression \n                  | expression SUP expression\n                  | expression INFEQUAL expression \n                  | expression SUPEQUAL expression \n                  | expression AND expression \n                  | expression OR expression\n                  | expression BOOLEQUAL expression expression : MINUS expressionexpression : expression COMA expressionexpression : LPAREN expression RPARENexpression : NUMBERexpression : NAMEparam : NAME COMA param\n             | NAME EQUALS expression COMA param\n             | NAME EQUALS expression\n             | NAMEstrings : NAME strings\n               | NAME'
    
_lr_action_items = {'NAME':([0,2,10,11,13,14,15,16,17,18,19,20,23,24,27,28,37,39,40,41,42,43,44,45,46,47,48,49,50,58,75,76,78,79,80,82,83,84,87,88,92,94,96,100,102,103,107,108,],[4,4,21,23,-3,25,25,25,23,25,25,36,23,-2,25,25,59,25,25,25,25,25,25,25,25,25,25,25,25,25,4,4,59,25,4,4,4,25,4,4,97,4,101,4,4,25,4,4,]),'PRINT':([0,2,13,24,75,76,80,82,83,87,88,94,100,102,107,108,],[5,5,-3,-2,5,5,5,5,5,5,5,5,5,5,5,5,]),'PRINTSTR':([0,2,13,24,75,76,80,82,83,87,88,94,100,102,107,108,],[6,6,-3,-2,6,6,6,6,6,6,6,6,6,6,6,6,]),'IF':([0,2,13,24,75,76,80,82,83,87,88,94,100,102,107,108,],[7,7,-3,-2,7,7,7,7,7,7,7,7,7,7,7,7,]),'WHILE':([0,2,13,24,75,76,80,82,83,87,88,94,100,102,107,108,],[8,8,-3,-2,8,8,8,8,8,8,8,8,8,8,8,8,]),'FOR':([0,2,13,24,75,76,80,82,83,87,88,94,100,102,107,108,],[9,9,-3,-2,9,9,9,9,9,9,9,9,9,9,9,9,]),'FUNC':([0,2,13,24,75,76,80,82,83,87,88,94,100,102,107,108,],[10,10,-3,-2,10,10,10,10,10,10,10,10,10,10,10,10,]),'SHARP':([0,2,13,24,75,76,80,82,83,87,88,94,100,102,107,108,],[11,11,-3,-2,11,11,11,11,11,11,11,11,11,11,11,11,]),'$end':([1,2,13,24,],[0,-1,-3,-2,]),'SEMI':([3,12,22,23,25,26,29,30,38,51,53,54,55,62,63,64,65,66,67,68,69,70,71,72,73,74,77,89,90,91,93,99,104,109,],[13,24,-15,-37,-31,-4,-30,-13,-36,-27,-14,-5,-6,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-28,-29,84,-7,-9,96,-11,-12,-8,-10,]),'EQUALS':([4,36,59,97,101,],[14,58,79,79,103,]),'LPAREN':([4,5,6,7,8,9,14,15,16,18,19,21,27,28,39,40,41,42,43,44,45,46,47,48,49,50,58,79,84,92,103,],[15,16,17,18,19,20,28,28,28,28,28,37,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,]),'RACCOLADE':([13,24,82,83,87,94,102,108,],[-3,-2,89,90,93,99,104,109,]),'MINUS':([14,15,16,18,19,25,26,27,28,29,31,32,34,35,39,40,41,42,43,44,45,46,47,48,49,50,51,52,58,62,63,64,65,66,67,68,69,70,71,72,73,74,77,79,84,86,91,92,97,103,105,],[27,27,27,27,27,-31,40,27,27,-30,40,40,40,40,27,27,27,27,27,27,27,27,27,27,27,27,-27,40,27,-16,-17,-18,-19,40,40,40,40,40,40,40,40,-29,40,27,27,40,40,27,-31,27,40,]),'NUMBER':([14,15,16,18,19,27,28,39,40,41,42,43,44,45,46,47,48,49,50,58,79,84,92,103,],[29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,]),'RPAREN':([15,23,25,29,31,32,33,34,35,37,38,51,52,59,61,62,63,64,65,66,67,68,69,70,71,72,73,74,85,86,97,98,105,],[30,-37,-31,-30,53,54,55,56,57,60,-36,-27,74,-35,81,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-28,-29,-32,-34,-31,-33,106,]),'PLUS':([25,26,29,31,32,34,35,51,52,62,63,64,65,66,67,68,69,70,71,72,73,74,77,86,91,97,105,],[-31,39,-30,39,39,39,39,-27,39,-16,-17,-18,-19,39,39,39,39,39,39,39,39,-29,39,39,39,-31,39,]),'TIMES':([25,26,29,31,32,34,35,51,52,62,63,64,65,66,67,68,69,70,71,72,73,74,77,86,91,97,105,],[-31,41,-30,41,41,41,41,41,41,41,41,-18,-19,41,41,41,41,41,41,41,41,-29,41,41,41,-31,41,]),'DIVIDE':([25,26,29,31,32,34,35,51,52,62,63,64,65,66,67,68,69,70,71,72,73,74,77,86,91,97,105,],[-31,42,-30,42,42,42,42,42,42,42,42,-18,-19,42,42,42,42,42,42,42,42,-29,42,42,42,-31,42,]),'INF':([25,26,29,31,32,34,35,51,52,62,63,64,65,66,67,68,69,70,71,72,73,74,77,86,91,97,105,],[-31,43,-30,43,43,43,43,-27,43,-16,-17,-18,-19,None,None,None,None,43,43,None,43,-29,43,43,43,-31,43,]),'SUP':([25,26,29,31,32,34,35,51,52,62,63,64,65,66,67,68,69,70,71,72,73,74,77,86,91,97,105,],[-31,44,-30,44,44,44,44,-27,44,-16,-17,-18,-19,None,None,None,None,44,44,None,44,-29,44,44,44,-31,44,]),'INFEQUAL':([25,26,29,31,32,34,35,51,52,62,63,64,65,66,67,68,69,70,71,72,73,74,77,86,91,97,105,],[-31,45,-30,45,45,45,45,-27,45,-16,-17,-18,-19,None,None,None,None,45,45,None,45,-29,45,45,45,-31,45,]),'SUPEQUAL':([25,26,29,31,32,34,35,51,52,62,63,64,65,66,67,68,69,70,71,72,73,74,77,86,91,97,105,],[-31,46,-30,46,46,46,46,-27,46,-16,-17,-18,-19,None,None,None,None,46,46,None,46,-29,46,46,46,-31,46,]),'AND':([25,26,29,31,32,34,35,51,52,62,63,64,65,66,67,68,69,70,71,72,73,74,77,86,91,97,105,],[-31,47,-30,47,47,47,47,-27,47,-16,-17,-18,-19,-20,-21,-22,-23,-24,47,-26,47,-29,47,47,47,-31,47,]),'OR':([25,26,29,31,32,34,35,51,52,62,63,64,65,66,67,68,69,70,71,72,73,74,77,86,91,97,105,],[-31,48,-30,48,48,48,48,-27,48,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,48,-29,48,48,48,-31,48,]),'BOOLEQUAL':([25,26,29,31,32,34,35,51,52,62,63,64,65,66,67,68,69,70,71,72,73,74,77,86,91,97,105,],[-31,49,-30,49,49,49,49,-27,49,-16,-17,-18,-19,None,None,None,None,49,49,None,49,-29,49,49,49,-31,49,]),'COMA':([25,26,29,31,32,34,35,51,52,59,62,63,64,65,66,67,68,69,70,71,72,73,74,77,86,91,97,105,],[-31,50,-30,50,50,50,50,-27,50,78,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,50,-29,50,92,50,78,50,]),'LACCOLADE':([56,57,60,81,95,106,],[75,76,80,88,100,107,]),'ELSE':([89,],[95,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'start':([0,],[1,]),'bloc':([0,75,76,80,88,100,107,],[2,82,83,87,94,102,108,]),'statement':([0,2,75,76,80,82,83,87,88,94,100,102,107,108,],[3,12,3,3,3,12,12,12,3,12,3,12,3,12,]),'strings':([11,17,23,],[22,33,38,]),'expression':([14,15,16,18,19,27,28,39,40,41,42,43,44,45,46,47,48,49,50,58,79,84,92,103,],[26,31,32,34,35,51,52,62,63,64,65,66,67,68,69,70,71,72,73,77,86,91,73,105,]),'param':([37,78,92,],[61,85,98,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> start","S'",1,None,None,None),
  ('start -> bloc','start',1,'p_start','TP1_sans_precedence.py',250),
  ('bloc -> bloc statement SEMI','bloc',3,'p_bloc','TP1_sans_precedence.py',258),
  ('bloc -> statement SEMI','bloc',2,'p_bloc','TP1_sans_precedence.py',259),
  ('statement -> NAME EQUALS expression','statement',3,'p_statement_assign','TP1_sans_precedence.py',266),
  ('statement -> PRINT LPAREN expression RPAREN','statement',4,'p_statement_expr','TP1_sans_precedence.py',270),
  ('statement -> PRINTSTR LPAREN strings RPAREN','statement',4,'p_statement_print_str','TP1_sans_precedence.py',274),
  ('statement -> IF LPAREN expression RPAREN LACCOLADE bloc RACCOLADE','statement',7,'p_statement_if','TP1_sans_precedence.py',279),
  ('statement -> IF LPAREN expression RPAREN LACCOLADE bloc RACCOLADE ELSE LACCOLADE bloc RACCOLADE','statement',11,'p_statement_if','TP1_sans_precedence.py',280),
  ('statement -> WHILE LPAREN expression RPAREN LACCOLADE bloc RACCOLADE','statement',7,'p_statement_while','TP1_sans_precedence.py',285),
  ('statement -> FOR LPAREN NAME EQUALS expression SEMI expression SEMI NAME EQUALS expression RPAREN LACCOLADE bloc RACCOLADE','statement',15,'p_statement_for','TP1_sans_precedence.py',289),
  ('statement -> FUNC NAME LPAREN RPAREN LACCOLADE bloc RACCOLADE','statement',7,'p_statement_funct','TP1_sans_precedence.py',293),
  ('statement -> FUNC NAME LPAREN param RPAREN LACCOLADE bloc RACCOLADE','statement',8,'p_statement_funct','TP1_sans_precedence.py',294),
  ('statement -> NAME LPAREN RPAREN','statement',3,'p_statement_call_function','TP1_sans_precedence.py',300),
  ('statement -> NAME LPAREN expression RPAREN','statement',4,'p_statement_call_function','TP1_sans_precedence.py',301),
  ('statement -> SHARP strings','statement',2,'p_statement_sharp','TP1_sans_precedence.py',306),
  ('expression -> expression PLUS expression','expression',3,'p_expression_binop','TP1_sans_precedence.py',309),
  ('expression -> expression MINUS expression','expression',3,'p_expression_binop','TP1_sans_precedence.py',310),
  ('expression -> expression TIMES expression','expression',3,'p_expression_binop','TP1_sans_precedence.py',311),
  ('expression -> expression DIVIDE expression','expression',3,'p_expression_binop','TP1_sans_precedence.py',312),
  ('expression -> expression INF expression','expression',3,'p_expression_binop','TP1_sans_precedence.py',313),
  ('expression -> expression SUP expression','expression',3,'p_expression_binop','TP1_sans_precedence.py',314),
  ('expression -> expression INFEQUAL expression','expression',3,'p_expression_binop','TP1_sans_precedence.py',315),
  ('expression -> expression SUPEQUAL expression','expression',3,'p_expression_binop','TP1_sans_precedence.py',316),
  ('expression -> expression AND expression','expression',3,'p_expression_binop','TP1_sans_precedence.py',317),
  ('expression -> expression OR expression','expression',3,'p_expression_binop','TP1_sans_precedence.py',318),
  ('expression -> expression BOOLEQUAL expression','expression',3,'p_expression_binop','TP1_sans_precedence.py',319),
  ('expression -> MINUS expression','expression',2,'p_expression_uminus','TP1_sans_precedence.py',324),
  ('expression -> expression COMA expression','expression',3,'p_expression_suite','TP1_sans_precedence.py',328),
  ('expression -> LPAREN expression RPAREN','expression',3,'p_expression_group','TP1_sans_precedence.py',332),
  ('expression -> NUMBER','expression',1,'p_expression_number','TP1_sans_precedence.py',336),
  ('expression -> NAME','expression',1,'p_expression_name','TP1_sans_precedence.py',340),
  ('param -> NAME COMA param','param',3,'p_param_funct','TP1_sans_precedence.py',344),
  ('param -> NAME EQUALS expression COMA param','param',5,'p_param_funct','TP1_sans_precedence.py',345),
  ('param -> NAME EQUALS expression','param',3,'p_param_funct','TP1_sans_precedence.py',346),
  ('param -> NAME','param',1,'p_param_funct','TP1_sans_precedence.py',347),
  ('strings -> NAME strings','strings',2,'p_print_name','TP1_sans_precedence.py',357),
  ('strings -> NAME','strings',1,'p_print_name','TP1_sans_precedence.py',358),
]
