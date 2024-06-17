#/bin/bash
MODEL=GInolic

python pareto.py ./sets/${MODEL}_S*.setDVO -o 6-8 -e 0.015 0.015 1.0 --output ${MODEL}.referenceDVO --delimiter=" "
cut -d ' ' -f 7-9 ${MODEL}.referenceDVO >> ${MODEL}.reference