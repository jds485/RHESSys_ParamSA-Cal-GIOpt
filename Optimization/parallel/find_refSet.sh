#/bin/bash
MODEL=LakeComo

python pareto.py ./sets/${MODEL}_S*.set -o 0-2 -e 0.5 25.0 0.05 --output ${MODEL}.reference --delimiter=" "