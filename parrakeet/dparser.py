# Copyright (c) 2003 Brian Sabbey

import pydparser, sys, types, dl, os, md5

class Nothing: pass
Reject = Nothing()

class Loc:
    'members: buf, idx, previous_col, col, line'
    
class Parser:
    def __init__(self, modules=None, initial_skip_space_fn=None,
                 syntax_error_fn=None, ambiguity_fn=None,
                 make_token=None,
                 dont_fixup_internal_productions=0,
                 dont_merge_epsilon_trees=0,
                 commit_actions_interval=0,
                 error_recovery=0):
        self.file_prefix = "d_parser_mach_gen"
        self.parser = None
        self.dl_parser = None
        self.actions = []
        self.sig = md5.new()

        if not modules:
            try:
                raise RuntimeError
            except RuntimeError:
                e,b,t = sys.exc_info()
            
            dicts = [t.tb_frame.f_back.f_globals]
        else:
            if type(modules) == list:
                dicts = [module.__dict__ for module in modules]
            else:
                dicts = modules

        functions = [val for dict in dicts for name, val in dict.items() 
			 if (isinstance(val, types.FunctionType)) and name[0:2] == 'd_']
        functions.sort(lambda x, y: cmp(x.func_code.co_filename, y.func_code.co_filename)
                         or cmp(x.func_code.co_firstlineno, y.func_code.co_firstlineno))

        self.filename = self.file_prefix + ".g"
        filename = self.filename
        g_file = open(self.filename, "w")
        for f in functions:
            if f.__doc__:
                g_file.write(f.__doc__)
                self.sig.update(f.__doc__)
            else:
                raise "action missing doc string:\t" + f.__name__
            g_file.write(";\n${action}\n");
            ac = f.func_code.co_argcount
            self.actions.append((f, ac))
            if not (ac == 1 or ac == 2):
                raise "action with wrong number of arguments:\t" + f.__name__
        g_file.close()

        if self.sig_changed():
            if os.system("make_dparser " + filename):
                raise "make_dparser error, see above"
            command = "cc -I/usr/local/include -shared -fPIC -o " + filename + ".so " + filename + ".d_parser.c"
            if os.system(command):
                raise "error running the command:\n\t%s" % command
            open(self.file_prefix + ".md5", "w").write("%s\n" % repr(self.sig.digest()))

        self.dl_parser = dl.open("./" + filename + ".so")
        tables = self.dl_parser.sym("parser_tables_gram")
        self.loc = Loc()
        self.parser = pydparser.make_parser(tables, Reject, make_token, self.loc, self.actions,
                                            initial_skip_space_fn,
                                            syntax_error_fn,
                                            ambiguity_fn,
                                            dont_fixup_internal_productions,
                                            dont_merge_epsilon_trees,
                                            commit_actions_interval,
                                            error_recovery or syntax_error_fn != None)
        del self.sig
        self.pydparser = pydparser

    def __del__(self):
        self.pydparser.del_parser(self.parser)
        
    def sig_changed(self):
        try:
            line = open(self.file_prefix + ".md5", "r").readline()
            if line and eval(line) == self.sig.digest():
                return 0
        except IOError, SyntaxError:
            pass
 
        return 1

    def parse(self, buf):
        self.loc.buf = buf
        return pydparser.run_parser(self.parser, buf)
