#This shell script is used to rename output files after they have all been generated.
#Make output directory in the permanent storage location
mkdir /nv/vol288/quinnlab-value/js4yd/Baisman30m_MorrisSA/output

cd /scratch/js4yd/MorrisSA/RHESSysRuns/output
for l in $(ls)
do
#There are 2 kinds of output files. Some have P9998 for adding 9998 to the number. All others, want to get the number after the underscore and subtract 1, then rename the file with that new number. 

#Set delimiter
IFS='_'

#Read file
read -ra ADDR <<< "$l" # file l is read into an array as tokens separated by IFS

#Want an if statement here for the value of ADDR[1]
if [ "${ADDR[1]}" == "P9998" ]
then
PLUSNINES=9998
SUBTHREE="${ADDR[2]:0:-4}"
i=$(expr $SUBTHREE + $PLUSNINES)
else
SUBONE=1
SUBTWO="${ADDR[1]:0:-4}"
i=$(expr $SUBTWO - $SUBONE)
fi
cp /scratch/js4yd/MorrisSA/RHESSysRuns/output/"$l" /nv/vol288/quinnlab-value/js4yd/Baisman30m_MorrisSA/output/Run_"$i".out
done
#Reset default delimiter
IFS=' '