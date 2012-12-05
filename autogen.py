import sys
import os.path
import subprocess
import glob
import re

"""
This script takes a directory name, which should contain python files that
represent python implementations of builtins.  There should be one function or
class definition per file.  It will output the contents of a file in Racket
syntax containing the core syntax representations of the definitions. These
representations will be defined to the name of the file that contains them.
"""

def main():
    if len(sys.argv) < 2:
        print("Not enough args; RTFM")
        return
    
    dirname = sys.argv[1]
    path = os.path.join(dirname, "*.py")
    files = glob.glob(path)
    file_names = map(lambda p: os.path.basename(p).replace(".py", ""), files)
    print("#lang plai-typed")
    print("(require \"python-core-syntax.rkt\")")

    for f, name in zip(files, file_names):
        process_file(f, name)

def process_file(filename, defname):
    core_syntax = subprocess.check_output("cat " + filename + \
            " | racket python-main.rkt --python-path ./python-bin --get-core-syntax", \
            shell=True, universal_newlines=True)

    core_syntax = re.sub("^\(.*?\(", "(", core_syntax)
    core_syntax = re.sub("\)$", "", core_syntax)
    core_syntax = re.sub("'\(\)", "empty", core_syntax)

    def id_list_fixer(m):
        items = map(lambda s: "'" + s, m.group(1).split(" "))
        return "(list " + " ".join(items) + ")"

    core_syntax = re.sub("'\((.*?)\)", id_list_fixer, core_syntax)

    print("(define " + defname + " " + core_syntax + ")")
    

if __name__ == "__main__":
    main()
