The final project for CS173 - An interpreter for Python

nuts and bolts:

python-syntax -> define surface syntax
python-core-syntax -> define core syntax


get-structured-python -> json -> surface syntax
python-desugar -> surface syntax to core syntax
python-interp -> core syntax to values




command line run:
echo "<python code>" | racket python-main.rkt --python-path <path> --interp

use interp-py to let your python run the case for you


