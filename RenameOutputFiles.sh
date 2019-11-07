#Make output directory in the permanent storage location
mkdir /nv/vol288/quinnlab-value/js4yd/Baisman30m_MorrisSA/output

cd /scratch/js4yd/MorrisSA/RHESSysRuns/output
for l in $(ls)
do
#Want to get the number after the underscore and subtract 1, then rename the file with that new number. 
#Set delimiter
IFS='_'
read -ra ADDR <<< "$l" # file l is read into an array as tokens separated by IFS
SUBONE=1
SUBTWO="${ADDR[1]:0:1}"
i=$(expr $SUBTWO - $SUBONE)
mv /scratch/js4yd/MorrisSA/RHESSysRuns/output/"$l" /nv/vol288/quinnlab-value/js4yd/Baisman30m_MorrisSA/output/Run_"$i".out
done
#Reset default delimiter
IFS=' '