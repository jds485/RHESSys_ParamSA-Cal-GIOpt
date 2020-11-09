ORIG='/scratch/js4yd/Bais910Hill30mDREAMzs-10Ch'
DEST='/nv/vol288/quinnlab-value/js4yd/Bais910Hill30mDREAMzs-10Ch'

cd "$ORIG"/RHESSysRuns/
mv ./OutputWorkspace-10Ch_s400.RData "$DEST"/RHESSysRuns/
mv ./C* "$DEST"/RHESSysRuns/
cp ./B* "$DEST"/RHESSysRuns/
cp ./D* "$DEST"/RHESSysRuns/
cp ./M* "$DEST"/RHESSysRuns/
cp ./R* "$DEST"/RHESSysRuns/
mv ./counter.csv "$DEST"/RHESSysRuns/
mv ./delta.csv "$DEST"/RHESSysRuns/
mv ./l* "$DEST"/RHESSysRuns/       
mv ./Iter* "$DEST"/RHESSysRuns/    
mv ./iter.csv "$DEST"/RHESSysRuns/
mv ../Run_MoveDREAM-10Ch-s400.out "$DEST"/