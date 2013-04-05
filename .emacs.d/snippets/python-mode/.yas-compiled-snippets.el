;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
                     '(("cont" "def __contains__(self, el):\n    $0" "__contains__" nil nil nil nil nil nil)
                       ("ent" "def __enter__(self):\n    $0\n\n    return self" "__enter__" nil nil nil nil nil nil)
                       ("ex" "def __exit__(self, type, value, traceback):\n    $0" "__exit__" nil nil nil nil nil nil)
                       ("getit" "def __getitem__(self, ${1:key}):\n    $0" "__getitem__" nil nil nil nil nil nil)
                       ("len" "def __len__(self):\n    $0" "__len__" nil nil nil nil nil nil)
                       ("new" "def __new__(mcs, name, bases, dict):\n    $0\n    return type.__new__(mcs, name, bases, dict)\n" "__new__" nil nil nil nil nil nil)
                       ("setit" "def __setitem__(self, ${1:key}, ${2:val}):\n    $0" "__setitem__" nil nil nil nil nil nil)
                       ("a" "__all__ = [\n        $0\n]" "all" nil nil nil nil nil nil)
                       ("arg" "parser.add_argument('-$1', '--$2',\n                    $0)\n" "arg" nil nil nil nil nil nil)
                       ("arg" "parser.add_argument('${1:varname}', $0)" "arg_positional" nil nil nil nil nil nil)
                       ("ass" "assert $0" "assert" nil nil nil nil nil nil)
                       ("ae" "self.assertEqual($1, $2)" "assertEqual" nil nil nil nil nil nil)
                       ("ar" "self.assertRaises(${1:Exception}, ${2:fun})" "assertRaises" nil nil nil nil nil nil)
                       ("at" "self.assertTrue($0)" "assertTrue" nil nil nil nil nil nil)
                       ("__" "def __${1:$$(yas/choose-value '(\"str\" \"cmp\" \"iter\" \"eq\" \"repr\"))}__(self${2:, other}):\n    $0" "builtin" nil
                        ("definitions")
                        nil nil nil nil)
                       ("cm" "@classmethod\ndef ${1:meth}(cls, $0):\n    " "classmethod" nil nil nil nil nil nil)
                       ("cls" "class ${1:class}:\n      $0" "cls" nil nil nil nil nil nil)
                       ("cprof" "try:\n     import cProfile as profile\nexcept ImportError:\n     import profile" "cprof" nil nil nil nil nil nil)
                       ("dec" "def ${1:decorator}(func):\n    $2\n    def _$1(*args, **kwargs):\n        $3\n        ret = func(*args, **kwargs)\n        $4\n        return ret\n    return _$1" "dec" nil
                        ("definitions")
                        nil nil nil nil)
                       ("def" "def ${1:fun}(${2:args}):\n    $0" "def" nil
                        ("definitions")
                        nil nil nil nil)
                       ("dt" "def test_${1:long_name}(self):\n    $0" "deftest" nil nil nil nil nil nil)
                       ("ds" "\"\"\"\n$0\n\"\"\"\n" "docstring" nil nil nil nil nil nil)
                       ("doc" ">>> ${1:function calls}\n${2:desired output}\n$0" "doctest" nil nil nil nil nil nil)
                       ("eq" "def __eq__(self, other):\n    return self.$1 == other.$1" "eq" nil
                        ("overloading")
                        nil nil nil nil)
                       ("for" "for ${var} in ${collection}:\n    $0" "for ... in ... : ..." nil nil nil nil nil nil)
                       ("from" "from ${1:lib} import ${2:funs}" "from" nil
                        ("general")
                        nil nil nil nil)
                       ("fsf" "#############################################################################\n##                                                                         ##\n## Fetched from http://www.secdev.org/python/tunproxy.py                   ##\n## ${1:filename}.py --- small demo program for tunneling over UDP with tun/tap  ##\n##                                                                         ##\n##                                                                         ##\n## This program is free software; you can redistribute it and/or modify it ##\n## under the terms of the GNU General Public License as published by the   ##\n## Free Software Foundation; either version 2, or (at your option) any     ##\n## later version.                                                          ##\n##                                                                         ##\n## This program is distributed in the hope that it will be useful, but     ##\n## WITHOUT ANY WARRANTY; without even the implied warranty of              ##\n## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       ##\n## General Public License for more details.                                ##\n##                                                                         ##\n#############################################################################\n" "fsf" nil nil nil nil nil nil)
                       ("f" "def ${1:function}($0):\n    " "function" nil nil nil nil nil nil)
                       ("if" "if ${1:cond}:\n   $0" "if" nil
                        ("control structure")
                        nil nil nil nil)
                       ("ife" "if $1:\n   $2\nelse:\n   $0" "ife" nil
                        ("control structure")
                        nil nil nil nil)
                       ("ifm" "if __name__ == '__main__':\n   ${1:main()}" "ifmain" nil nil nil nil nil nil)
                       ("imp" "import ${1:lib}${2: as ${3:alias}}\n$0" "import" nil
                        ("general")
                        nil nil nil nil)
                       ("init" "def __init__(self${1:, args}):\n    ${2:\"${3:docstring}\"\n    }$0" "init" nil
                        ("definitions")
                        nil nil nil nil)
                       ("int" "import code; code.interact(local=locals())" "interact" nil nil nil nil nil nil)
                       ("iso" "# -*- coding: iso-8859-15 -*-\n$0" "iso" nil
                        ("general")
                        nil nil nil nil)
                       ("iter" "def __iter__(self):\n    return ${1:iter($2)}" "iter" nil
                        ("overloading")
                        nil nil nil nil)
                       ("lam" "lambda ${1:x}: $0" "lambda" nil nil nil nil nil nil)
                       ("li" "[${1:el} for $1 in ${2:list}]\n$0" "list" nil
                        ("definitions")
                        nil nil nil nil)
                       ("ln" "logger = logging.getLogger(__name__)" "logger_name" nil nil nil nil nil nil)
                       ("log" "logger = logging.getLogger(\"${1:name}\")\nlogger.setLevel(logging.${2:level})\n" "logging" nil nil nil nil nil nil)
                       ("main" "def main():\n    $0" "main" nil nil nil nil nil nil)
                       ("mt" "__metaclass__ = type" "metaclass" nil nil nil nil nil nil)
                       ("m" "def ${1:method}(self${2:, $3}):\n    $0" "method" nil nil nil nil nil nil)
                       ("not_impl" "raise NotImplementedError" "not_impl" nil nil nil nil nil nil)
                       ("np" "import numpy as np\n$0" "np" nil
                        ("general")
                        nil nil nil nil)
                       ("pack" "struct.pack(\"!${1:fmt}\", $0)" "pack" nil nil nil nil nil nil)
                       ("pargs" "def parse_arguments():\n    parser = argparse.ArgumentParser(description='$1')\n    $0\n    return parser.parse_args()" "parse_args" nil nil nil nil nil nil)
                       ("pars" "parser = argparse.ArgumentParser(description='$1')\n$0" "parser" nil nil nil nil nil nil)
                       ("ps" "pass" "pass" nil nil nil nil nil nil)
                       ("p" "print($0)" "print" nil nil nil nil nil nil)
                       ("prop" "def ${1:foo}():\n   doc = \"\"\"${2:Doc string}\"\"\"\n   def fget(self):\n       return self._$1\n\n   def fset(self, value):\n       self._$1 = value\n\n   def fdel(self):\n       del self._$1\n   return locals()\n$1 = property(**$1())\n\n$0\n" "prop" nil nil nil nil nil nil)
                       ("tu" "import pudb; pudb.set_trace()" "pudb" nil nil nil nil nil nil)
                       ("reg" "${1:regexp} = re.compile(r\"${2:expr}\")\n$0" "reg" nil
                        ("general")
                        nil nil nil nil)
                       ("repr" "def __repr__(self):\n    $0" "repr" nil nil nil nil nil nil)
                       ("r" "return $0" "return" nil nil nil nil nil nil)
                       ("scapy" "from scapy.all import $0" "scapy" nil nil nil nil nil nil)
                       ("script" "#!/usr/bin/env python\n\ndef main():\n   pass\n\nif __name__ == '__main__':\n   main()\n" "script" nil nil nil nil nil nil)
                       ("." "self.$0" "self" nil nil nil nil nil nil)
                       ("s" "self" "self_without_dot" nil nil nil nil nil nil)
                       ("sn" "self.$1 = $1" "selfassign" nil nil nil nil nil nil)
                       ("setdef" "${1:var}.setdefault(${2:key}, []).append(${3:value})" "setdef" nil nil nil nil nil nil)
                       ("setup" "from setuptools import setup\n\npackage = '${1:name}'\nversion = '${2:0.1}'\n\nsetup(name=package,\n      version=version,\n      description=\"${3:description}\",\n      url='${4:url}'$0)\n" "setup" nil nil nil nil nil nil)
                       ("size" "sys.getsizeof($0)" "size" nil nil nil nil nil nil)
                       ("sm" "@staticmethod\ndef ${1:func}($0):\n" "static" nil nil nil nil nil nil)
                       ("str" "def __str__(self):\n    $0" "str" nil
                        ("overloading")
                        nil nil nil nil)
                       ("super" "super(${1:Class}, self).${2:function}(${3:args})" "super" nil nil nil nil nil nil)
                       ("tcs" "class Test${1:toTest}(unittest.TestCase):\n      $0\n" "test_class" nil
                        ("definitions")
                        nil nil nil nil)
                       ("tf" "import unittest\n${1:from ${2:test_file} import *}\n\n$0\n\nif __name__ == '__main__':\n    unittest.main()" "test_file" nil
                        ("definitions")
                        nil nil nil nil)
                       ("tr" "import pdb; pdb.set_trace()" "trace" nil nil nil nil nil nil)
                       ("tri" "\"\"\"$0\n\"\"\"" "triple-quote" nil nil nil nil nil nil)
                       ("try" "try:\n    $1\nexcept ${2:Exception}:\n    $0" "try" nil nil nil nil nil nil)
                       ("try" "try:\n    $1\nexcept $2:\n    $3\nelse:\n    $0" "tryelse" nil nil nil nil nil nil)
                       ("un" "def __unicode__(self):\n    $0" "unicode" nil nil nil nil nil nil)
                       ("utf" "# -*- encoding: utf-8 -*-\n$0" "utf8" nil
                        ("general")
                        nil nil nil nil)
                       ("wh" "while ${1:True}:\n      $0" "while" nil nil nil nil nil nil)
                       ("with" "with ${1:expr}${2: as ${3:alias}}:\n     $0" "with" nil
                        ("control structure")
                        nil nil nil nil)
                       ("fw" "from __future__ import with_statement" "with_statement" nil nil nil nil nil nil)
                       ("yaml" "${1:res} = yaml.load(open(${2:file}))\n$0" "yaml" nil
                        ("general")
                        nil nil nil nil)))


;;; Do not edit! File generated at Thu Apr  4 16:55:24 2013
